"""
clique
======

Python implementation of CLIQUE: Conditional Local Importance by
Quantile Expectation.

This module implements a model-agnostic, grid-based local feature
importance method. For each observation and each feature, CLIQUE holds
every other feature fixed and sweeps the feature of interest over a grid
of representative values (either quantiles of its observed distribution
or an evenly spaced range). The resulting change in predictive loss,
relative to the observation's own baseline loss, defines that feature's
local importance for that observation.

Two public entry points are provided:

``clique``
    A convenience wrapper that fits its own models via k-fold
    cross-validation and computes local importance from out-of-fold
    predictions, so no separate holdout set is required. Supports
    regression (univariate and multivariate), binary classification,
    and multiclass classification (against hard labels or predicted
    probabilities), inferring the target type automatically from
    ``Y``'s dtype.

``clique_eval``
    Computes local importance from a single, already-fitted ``model``
    and an explicit train/test split. Supports the same target types as
    ``clique``. Use this when model fitting should be handled entirely
    by the caller.

Both entry points share a set of private helpers (``_predict_model``,
``_compute_base_loss``, ``_generate_grid_values``, ``_binarize_labels``)
so that prediction dispatch, grid construction, loss computation, and
label binarization behave identically regardless of which entry point
is used.

Shared vocabulary
------------------
``model``
    Any fitted object exposing a ``predict`` method (and optionally
    ``predict_proba``), or a fitted :class:`TorchMLP` wrapper --
    detected automatically via ``isinstance(model, TorchMLP)``, so no
    separate flag is needed to say a model is a Torch model.
``loss_function``
    A callable ``loss_function(truth, pred) -> array`` computing
    elementwise loss. Defaults to :func:`default_loss` (absolute error).
``class_loss``
    For classification targets, whether to compute loss against
    predicted class labels (``False``) or predicted probabilities
    (``True``, requires ``predict_proba``).

Author
------
Kelvyn K. Bladen

License
-------
MIT License
"""

from __future__ import annotations

from typing import Callable, Optional, Union

import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from torch.utils.data import DataLoader, TensorDataset

from sklearn.base import clone
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.multioutput import MultiOutputRegressor
from sklearn.model_selection import KFold
from sklearn.preprocessing import LabelBinarizer

# =============================================================================
# PyTorch model metadata
# =============================================================================

__author__ = "Kelvyn K. Bladen"
__version__ = "0.1.0"

__all__ = [
    "MLP",
    "TorchMLP",
    "build_mlp",
    "train_model",
    "default_loss",
    "clique_eval",
    "clique",
]

# =============================================================================
# PyTorch model utilities
# =============================================================================

class MLP(nn.Module):
    """
    Fully-connected feedforward network used as the default PyTorch
    backbone for CLIQUE.

    Architecture: four hidden layers (512, 256, 128, 64 units), each
    followed by ReLU, batch normalization, and dropout (p=0.2), with a
    final linear projection to ``output_dim``.

    Parameters
    ----------
    input_dim : int
        Number of input features.
    output_dim : int
        Number of output units (e.g. number of regression targets or
        classes).
    """

    def __init__(self, input_dim: int, output_dim: int):
        super().__init__()
        self.network = nn.Sequential(
            nn.Linear(input_dim, 512),
            nn.ReLU(),
            nn.BatchNorm1d(512),
            nn.Dropout(0.2),

            nn.Linear(512, 256),
            nn.ReLU(),
            nn.BatchNorm1d(256),
            nn.Dropout(0.2),

            nn.Linear(256, 128),
            nn.ReLU(),
            nn.BatchNorm1d(128),
            nn.Dropout(0.2),

            nn.Linear(128, 64),
            nn.ReLU(),
            nn.BatchNorm1d(64),
            nn.Dropout(0.2),

            nn.Linear(64, output_dim)
        )

    def forward(self, x):
        return self.network(x)


class TorchMLP:
    """
    Thin wrapper bundling an :class:`MLP`, its optimizer, and its loss
    criterion, exposing a scikit-learn-like ``predict`` interface (and,
    in classification mode, ``predict_proba``) so it can be used
    interchangeably with other estimators inside :func:`clique` and
    :func:`clique_eval`.

    By default the wrapper is in regression mode: ``predict`` returns
    the network's raw output and training uses ``nn.HuberLoss``.
    Passing ``classes`` switches it into classification mode: the
    network is trained to distinguish those classes, ``predict``
    returns labels drawn from ``classes`` (not raw logits or indices),
    and ``predict_proba`` becomes available. Any loss module can be
    supplied via ``criterion`` -- see the ``Notes`` section for how
    targets are encoded to match it.

    Parameters
    ----------
    input_dim : int
        Number of input features.
    output_dim : int
        Number of output units: number of regression targets in
        regression mode, or number of network output units in
        classification mode (typically the number of classes, e.g. for
        ``nn.CrossEntropyLoss``; use ``1`` for a single-logit head
        paired with e.g. ``nn.BCEWithLogitsLoss``).
    lr : float, default=5e-4
        Learning rate for the Adam optimizer.
    criterion : torch.nn.Module, optional
        Loss module used during training. Defaults to
        ``nn.CrossEntropyLoss()`` when ``classes`` is provided,
        otherwise ``nn.HuberLoss(delta=1.0)``.
    classes : array-like, optional
        Class labels the network should learn to distinguish. Providing
        this switches the wrapper into classification mode. The order
        of ``classes`` determines ``predict_proba``'s column order, so
        pass classes in the same (sorted) order used elsewhere in the
        pipeline (e.g. ``np.unique(Y)``, as :func:`clique` does).

    Attributes
    ----------
    model : MLP
        The underlying network.
    optimizer : torch.optim.Adam
        Optimizer over ``model.parameters()``.
    criterion : torch.nn.Module
        Training loss criterion.
    classes_ : numpy.ndarray or None
        Class labels in classification mode; ``None`` in regression
        mode.

    Notes
    -----
    In classification mode, raw class labels passed to
    :func:`train_model` are converted to a tensor matching
    ``criterion`` as follows:

    - ``nn.CrossEntropyLoss`` / ``nn.NLLLoss``: a ``long`` tensor of
      class indices, shape ``(n,)``.
    - Any other loss, with ``output_dim == 1``: a ``float32`` tensor of
      0/1 class indices, shape ``(n, 1)`` -- suited to
      ``nn.BCEWithLogitsLoss`` for binary classification.
    - Any other loss, with ``output_dim > 1``: a one-hot ``float32``
      tensor, shape ``(n, n_classes)``.
    """

    def __init__(
        self,
        input_dim: int,
        output_dim: int,
        lr: float = 5e-4,
        criterion: Optional[nn.Module] = None,
        classes=None
    ):
        self.model = MLP(input_dim, output_dim)
        self.optimizer = torch.optim.Adam(
            self.model.parameters(),
            lr=lr
        )
        self.output_dim = output_dim
        self.classes_ = None if classes is None else np.asarray(classes)
        self._label_to_index = (
            None if self.classes_ is None
            else {label: i for i, label in enumerate(self.classes_)}
        )

        if criterion is None:
            criterion = (
                nn.CrossEntropyLoss() if self.classes_ is not None
                else nn.HuberLoss(delta=1.0)
            )
        self.criterion = criterion

    def _forward(self, X, batch_size: int = 1024) -> np.ndarray:
        """
        Batched raw forward pass through the network (no
        post-processing), in evaluation mode.

        Parameters
        ----------
        X : array-like of shape (n_samples, n_features)
        batch_size : int, default=1024

        Returns
        -------
        numpy.ndarray of shape (n_samples, output_dim)
            Raw network output (regression values, or classification
            logits).
        """
        self.model.eval()
        X = torch.as_tensor(X, dtype=torch.float32)
        loader = torch.utils.data.DataLoader(
            X, batch_size=batch_size, shuffle=False
        )

        outputs = []
        with torch.no_grad():
            for xb in loader:
                outputs.append(self.model(xb).cpu())

        outputs = torch.cat(outputs, dim=0).numpy()
        return outputs

    def predict(self, X, batch_size: int = 1024) -> np.ndarray:
        """
        Run batched inference in evaluation mode.

        Parameters
        ----------
        X : array-like of shape (n_samples, n_features)
            Input features.
        batch_size : int, default=1024
            Number of rows per inference batch.

        Returns
        -------
        numpy.ndarray
            In regression mode: predictions of shape
            ``(n_samples, output_dim)``. In classification mode: hard
            class labels drawn from ``classes_``, shape ``(n_samples,)``.
        """
        raw = self._forward(X, batch_size=batch_size)

        if self.classes_ is None:
            return raw

        if self.output_dim == 1:
            idx = (raw.ravel() > 0).astype(int)
        else:
            idx = raw.argmax(axis=1)

        return self.classes_[idx]

    def predict_proba(self, X, batch_size: int = 1024) -> np.ndarray:
        """
        Predicted class probabilities. Only available in classification
        mode (i.e. the wrapper was constructed with ``classes``).

        Parameters
        ----------
        X : array-like of shape (n_samples, n_features)
            Input features.
        batch_size : int, default=1024
            Number of rows per inference batch.

        Returns
        -------
        numpy.ndarray of shape (n_samples, n_classes)
            Predicted probabilities, columns ordered to match
            ``classes_``. Computed via a sigmoid (single-logit head) or
            softmax (multi-logit head) applied to the network's raw
            output, regardless of the training criterion.

        Raises
        ------
        AttributeError
            If the wrapper is in regression mode (``classes_ is None``).
        """
        if self.classes_ is None:
            raise AttributeError(
                "predict_proba is only available in classification "
                "mode; construct TorchMLP (or build_mlp) with "
                "`classes=...` to enable it."
            )

        raw = self._forward(X, batch_size=batch_size)

        if self.output_dim == 1:
            p1 = 1.0 / (1.0 + np.exp(-raw.ravel()))
            return np.column_stack([1.0 - p1, p1])

        shifted = raw - raw.max(axis=1, keepdims=True)
        exp = np.exp(shifted)
        return exp / exp.sum(axis=1, keepdims=True)


def build_mlp(
    input_dim: int,
    output_dim: int,
    lr: float = 5e-4,
    criterion: Optional[nn.Module] = None,
    classes=None
) -> TorchMLP:
    """
    Factory function constructing a :class:`TorchMLP`.

    Intended to be passed as ``model_builder`` to :func:`clique`. For
    regression targets, :func:`clique` calls
    it as ``model_builder(n_features, n_targets)``; for classification
    targets, as ``model_builder(n_features, n_classes, classes=classes)``.

    Parameters
    ----------
    input_dim : int
        Number of input features.
    output_dim : int
        Number of output units (regression targets, or classes in
        classification mode -- see :class:`TorchMLP`).
    lr : float, default=5e-4
        Learning rate forwarded to :class:`TorchMLP`.
    criterion : torch.nn.Module, optional
        Loss module forwarded to :class:`TorchMLP`. Defaults to
        ``nn.CrossEntropyLoss()`` when ``classes`` is provided,
        otherwise ``nn.HuberLoss(delta=1.0)``. Pass e.g.
        ``nn.BCEWithLogitsLoss()`` (with ``output_dim=1``) for a
        single-logit binary head instead.
    classes : array-like, optional
        Class labels; providing this builds a classification-mode
        :class:`TorchMLP` (enables ``predict_proba``). Omit for
        regression.

    Returns
    -------
    TorchMLP
        A freshly initialized, untrained wrapper.
    """
    return TorchMLP(input_dim, output_dim, lr=lr, criterion=criterion, classes=classes)


def _encode_targets(wrapper: TorchMLP, y) -> "torch.Tensor":
    """
    Convert raw targets into a torch tensor matching ``wrapper``'s task
    and loss criterion (see the ``Notes`` section of :class:`TorchMLP`
    for the exact encoding rules).

    Parameters
    ----------
    wrapper : TorchMLP
        Wrapper whose ``classes_``, ``criterion``, and ``output_dim``
        determine the target encoding.
    y : array-like of shape (n_samples,)
        Raw targets: regression values, or class labels.

    Returns
    -------
    torch.Tensor
        Encoded targets, ready to pair with the network's output in
        ``wrapper.criterion``.
    """
    y = np.asarray(y)

    if wrapper.classes_ is None:
        return torch.as_tensor(y, dtype=torch.float32)

    idx = np.array([wrapper._label_to_index[label] for label in y])

    if isinstance(wrapper.criterion, (nn.CrossEntropyLoss, nn.NLLLoss)):
        return torch.as_tensor(idx, dtype=torch.long)

    if wrapper.output_dim == 1:
        return torch.as_tensor(idx, dtype=torch.float32).reshape(-1, 1)

    onehot = np.eye(len(wrapper.classes_), dtype=np.float32)[idx]
    return torch.as_tensor(onehot, dtype=torch.float32)


def train_model(
    wrapper: TorchMLP,
    X_train,
    y_train,
    X_val=None,
    y_val=None,
    epochs: int = 50,
    batch_size: int = 32,
    verbose: bool = False
) -> TorchMLP:
    """
    Train a :class:`TorchMLP` wrapper in place using mini-batch gradient
    descent.

    Works for both regression and classification wrappers: targets are
    encoded via :func:`_encode_targets` to match ``wrapper.criterion``,
    so any loss module supported there (``HuberLoss``,
    ``CrossEntropyLoss``, ``BCEWithLogitsLoss``, or a custom module)
    trains correctly without further changes to this function.

    Parameters
    ----------
    wrapper : TorchMLP
        Wrapper containing the model, optimizer, and loss criterion to
        train. Mutated in place.
    X_train : array-like
        Training features.
    y_train : array-like
        Training targets: regression values, or class labels (raw
        labels, not pre-encoded indices -- ``wrapper`` handles that).
    X_val, y_val : array-like, optional
        Validation features and targets. If provided, validation loss is
        reported alongside training loss when ``verbose`` is True.
    epochs : int, default=50
        Number of training epochs.
    batch_size : int, default=32
        Mini-batch size for training.
    verbose : bool, default=False
        If True, print training (and validation) loss every 10 epochs.

    Returns
    -------
    TorchMLP
        The same ``wrapper`` instance, now trained.
    """
    X_train = torch.as_tensor(X_train, dtype=torch.float32)
    y_train = _encode_targets(wrapper, y_train)

    train_loader = DataLoader(
        TensorDataset(X_train, y_train),
        batch_size=batch_size,
        shuffle=True
    )

    if X_val is not None:
        X_val = torch.as_tensor(X_val, dtype=torch.float32)
        y_val = _encode_targets(wrapper, y_val)

    model = wrapper.model
    optimizer = wrapper.optimizer
    criterion = wrapper.criterion

    for epoch in range(epochs):
        model.train()
        train_loss = 0.0

        for xb, yb in train_loader:
            optimizer.zero_grad()
            preds = model(xb)
            loss = criterion(preds, yb)
            loss.backward()
            optimizer.step()
            train_loss += loss.item()

        # ---- validation ----
        if X_val is not None:
            model.eval()
            with torch.no_grad():
                val_preds = model(X_val)
                val_loss = criterion(val_preds, y_val).item()
        else:
            val_loss = None

        if verbose and (epoch + 1) % 10 == 0:
            msg = f"Epoch {epoch + 1}: train={train_loss:.4f}"
            if val_loss is not None:
                msg += f", val={val_loss:.4f}"
            print(msg)

    return wrapper


# =============================================================================
# Loss functions
# =============================================================================

def default_loss(truth, pred) -> np.ndarray:
    """
    Elementwise absolute error, the default loss used throughout CLIQUE.

    Parameters
    ----------
    truth : array-like
        Ground-truth values (or one-hot/binarized labels).
    pred : array-like
        Predicted values (or predicted probabilities), same shape as
        ``truth``.

    Returns
    -------
    numpy.ndarray
        Elementwise ``abs(truth - pred)``.
    """
    return np.abs(truth - pred)


# =============================================================================
# Internal prediction / loss helpers (shared by clique and clique_eval)
# =============================================================================

def _predict_model(
    model,
    X,
    batch_size: int = 4096,
    class_loss: bool = False
) -> np.ndarray:
    """
    Dispatch a prediction call to ``model``, normalizing across
    scikit-learn estimators, models exposing ``predict_proba``, and
    :class:`TorchMLP` wrappers.

    Parameters
    ----------
    model : object
        Fitted estimator or :class:`TorchMLP` wrapper. Torch wrappers
        are detected automatically via ``isinstance(model, TorchMLP)``.
    X : array-like of shape (n_samples, n_features)
        Input features.
    batch_size : int, default=4096
        Batch size forwarded to Torch model predictions. Ignored for
        non-Torch estimators.
    class_loss : bool, default=False
        If True and ``model`` exposes ``predict_proba``, returns
        predicted probabilities instead of hard labels. Applies to both
        ordinary estimators and :class:`TorchMLP` wrappers built in
        classification mode.

    Returns
    -------
    numpy.ndarray
        Model predictions (labels, probabilities, or regression
        outputs, depending on the flags above).
    """
    if isinstance(model, TorchMLP):
        if class_loss and hasattr(model, "predict_proba"):
            return model.predict_proba(X, batch_size=batch_size)
        return model.predict(
            X,
            batch_size=batch_size
        )

    if class_loss and hasattr(model, "predict_proba"):
        return model.predict_proba(X)

    return model.predict(X)


def _binarize_labels(Y, classes: Optional[np.ndarray] = None):
    """
    One-hot binarize class labels for probability-based loss
    computation, always returning at least two columns (matching the
    two columns scikit-learn's ``predict_proba`` returns for binary
    classifiers).

    Parameters
    ----------
    Y : array-like of shape (n_samples,)
        Class labels.
    classes : array-like, optional
        Fixed set of classes defining the binarization's column order.
        Pass this whenever labels are binarized in slices that might
        not individually contain every class -- e.g. one
        cross-validation fold at a time in :func:`clique` -- so that
        columns stay aligned with each fold model's ``predict_proba``
        output. Defaults to inferring classes from ``Y`` itself, which
        is sufficient whenever ``Y`` already reflects the full label
        set (as in :func:`clique_eval`).

    Returns
    -------
    truth : numpy.ndarray of shape (n_samples, n_classes)
        Binarized targets (always >= 2 columns).
    label_binarizer : sklearn.preprocessing.LabelBinarizer
        The fitted binarizer, exposing ``.classes_``.
    """
    lb = LabelBinarizer()
    if classes is not None:
        lb.fit(classes)
        truth = lb.transform(Y)
    else:
        truth = lb.fit_transform(Y)

    if truth.shape[1] == 1:
        truth = np.column_stack([1 - truth, truth])

    return truth, lb


def _compute_base_loss(
    model,
    X,
    Y,
    loss_function: Optional[Callable] = None,
    class_loss: bool = False,
    batch_size: int = 4096,
    classes: Optional[np.ndarray] = None
):
    """
    Compute each observation's baseline predictive loss under ``model``,
    branching by target type (regression, classification against hard
    labels, or classification against predicted probabilities).

    Parameters
    ----------
    model : object
        Fitted estimator or :class:`TorchMLP` wrapper.
    X : array-like of shape (n_samples, n_features)
        Input features.
    Y : array-like of shape (n_samples,) or (n_samples, n_targets)
        Ground-truth targets.
    loss_function : callable, optional
        ``loss_function(truth, pred) -> array``, elementwise. Defaults
        to :func:`default_loss`.
    class_loss : bool, default=False
        Whether to score classification targets against predicted
        probabilities rather than hard labels.
    batch_size : int, default=4096
        Batch size forwarded to Torch model predictions.
    classes : array-like, optional
        Fixed set of classes to binarize against when ``class_loss`` is
        True (see :func:`_binarize_labels`). Forwarded unchanged; has no
        effect for regression targets or hard-label classification.

    Returns
    -------
    preds : numpy.ndarray
        Raw model predictions.
    base_loss : numpy.ndarray of shape (n_samples,)
        Per-observation baseline loss, summed (regression, probability
        loss) or taken directly (0/1 accuracy loss) across output
        dimensions.
    """
    if loss_function is None:
        loss_function = default_loss

    preds = _predict_model(
        model,
        X,
        batch_size=batch_size,
        class_loss=class_loss
    )

    # ----------------------------------------------------
    # Regression
    # ----------------------------------------------------
    if np.issubdtype(np.asarray(Y).dtype, np.number):
        Y = np.asarray(Y)
        if Y.ndim == 1:
            Y = Y.reshape(-1, 1)

        preds = np.asarray(preds)
        if preds.ndim == 1:
            preds = preds.reshape(-1, 1)

        base_loss = loss_function(Y, preds).sum(axis=1)
        return preds, base_loss

    # ----------------------------------------------------
    # Classification accuracy loss
    # ----------------------------------------------------
    if not class_loss:
        truth = np.asarray(Y)
        preds = np.asarray(preds)
        return preds, (truth != preds).astype(float)

    # ----------------------------------------------------
    # Probability-based class loss
    # ----------------------------------------------------
    truth_mat, _ = _binarize_labels(Y, classes=classes)

    preds = np.asarray(preds)
    base_loss = loss_function(truth_mat, preds).mean(axis=1)
    return preds, base_loss


def _generate_grid_values(
    x,
    nsim: int = 25,
    quantile_grid: bool = True
) -> np.ndarray:
    """
    Build the sweep grid used to perturb a single feature.

    Numeric features are swept over ``nsim`` quantiles (or an evenly
    spaced range) of their observed values. Categorical features are
    swept over their observed unique levels, repeated in proportion to
    their frequency so that the grid has approximately ``nsim`` points
    in total -- except that every observed level is guaranteed at least
    one grid point, even if its frequency alone would round down to
    zero. This means the realized grid can be slightly larger than
    ``nsim`` when there are many rare categories, but no category is
    silently dropped from the perturbation sweep.

    Parameters
    ----------
    x : array-like of shape (n_samples,)
        Observed values of the feature being perturbed.
    nsim : int, default=25
        Target number of grid points.
    quantile_grid : bool, default=True
        For numeric features: use quantiles (True) or an evenly spaced
        range between the min and max (False). Ignored for categorical
        features.

    Returns
    -------
    numpy.ndarray
        Grid of values to substitute for this feature.
    """
    x = np.asarray(x)

    if np.issubdtype(x.dtype, np.number):
        if quantile_grid:
            vals = np.quantile(x, np.linspace(0, 1, nsim))
        else:
            vals = np.linspace(np.min(x), np.max(x), nsim)
        return np.unique(vals)

    # categorical -- guarantee every level appears at least once
    vals, counts = np.unique(x, return_counts=True)
    reps = np.round(nsim * counts / counts.sum()).astype(int)
    reps = np.maximum(reps, 1)
    out = np.repeat(vals, reps)
    return out

# =============================================================================
# Public API: clique (cross-validated, fits its own models)
# =============================================================================

def clique(
    X,
    Y,
    model_builder: Union[str, Callable, object] = "rf",
    folds: int = 5,
    nsim: int = 25,
    quantile_grid: bool = True,
    loss_function: Optional[Callable] = None,
    class_loss: bool = False,
    epochs: int = 50,
    batch_size: int = 64,
    random_state: int = 123,
    verbose: bool = False
) -> dict:
    """
    CLIQUE local importance using cross-validation.

    Supports regression (univariate and multivariate), binary
    classification, and multiclass classification (against hard labels
    or predicted probabilities) -- the same target types handled by
    :func:`clique_eval`. The target type is inferred automatically from
    ``Y``'s dtype: numeric targets are treated as regression, anything
    else (e.g. string or object labels) as classification.

    ``clique`` fits its own models using k-fold cross-validation and
    computes local importance from out-of-fold predictions, so every
    observation is scored by a model that never saw it during training.
    Prediction dispatch, baseline loss, grid construction, and label
    binarization are delegated to the same helpers used by
    :func:`clique_eval` (:func:`_predict_model`, :func:`_compute_base_loss`,
    :func:`_generate_grid_values`, :func:`_binarize_labels`, and
    ``loss_function``), so both entry points behave identically wherever
    their functionality overlaps. For probability-based classification
    loss, the set of classes is fixed once across the whole dataset (via
    ``classes=`` on the shared helpers) so that a class missing from a
    particular fold's test slice doesn't misalign probability columns.

    Parameters
    ----------
    X : array-like of shape (n_samples, n_features)
        Feature matrix.
    Y : array-like of shape (n_samples,) or (n_samples, n_targets)
        Target values. Numeric dtype is treated as regression (multiple
        columns supported for multivariate regression). Non-numeric
        dtype (e.g. strings) is treated as classification and must be a
        single column of class labels.
    model_builder : {"rf"}, callable, or object, default="rf"
        How to obtain a model for each fold:

        - ``"rf"``: for regression targets, fit a
          :class:`~sklearn.ensemble.RandomForestRegressor` (wrapped in
          :class:`~sklearn.multioutput.MultiOutputRegressor` when ``Y``
          has more than one column); for classification targets, fit a
          :class:`~sklearn.ensemble.RandomForestClassifier`.
        - a callable: invoked fresh for each fold to construct a model
          (e.g. :func:`build_mlp`). Called as
          ``model_builder(n_features, n_targets)`` for regression
          targets, or ``model_builder(n_features, n_classes,
          classes=classes)`` for classification targets -- so a
          classification-capable builder must accept a ``classes``
          keyword argument, as :func:`build_mlp` does. If the returned
          object is a :class:`TorchMLP`, it is trained via
          :func:`train_model`; any other object is trained via
          ``.fit(X, y)``.
        - any other (non-callable) object exposing ``.fit`` /
          ``.predict`` (and, for probability-based classification loss,
          ``.predict_proba``): used directly for every fold.
    folds : int, default=5
        Number of cross-validation folds. Must be >= 2 and <= the
        number of samples.
    nsim : int, default=25
        Number of grid points evaluated per feature. Must be >= 2.
    quantile_grid : bool, default=True
        If True, grid points are quantiles of the feature's observed
        values; otherwise they are evenly spaced between its min and
        max.
    loss_function : callable, optional
        ``loss_function(truth, pred) -> array``, elementwise. Defaults
        to :func:`default_loss` (absolute error).
    class_loss : bool, default=False
        For classification targets, score against predicted
        probabilities (True, requires ``predict_proba``) instead of
        hard labels (False). Ignored for regression targets.
    epochs : int, default=50
        Training epochs, used only when the fold model is a
        :class:`TorchMLP`. Must be >= 1.
    batch_size : int, default=64
        Mini-batch size for Torch training. Must be >= 1. Grid
        predictions are always batched at 4096 regardless of this
        setting.
    random_state : int, default=123
        Seed for the k-fold split and for the default random forest.
    verbose : bool, default=False
        If True, print progress: per-fold Torch training loss (every 10
        epochs) and "Finished variable j/p" as each feature is
        processed.

    Returns
    -------
    dict
        ``{"models": list of per-fold fitted models,
        "local_imp": pandas.DataFrame of shape (n_samples, n_features)}``

    Raises
    ------
    ValueError
        If ``folds < 2``, ``folds`` exceeds the number of samples,
        ``nsim < 2``, ``epochs < 1``, ``batch_size < 1``, ``X`` and
        ``Y`` have mismatched row counts, ``Y`` is a non-numeric
        (classification) target with fewer than 2 classes or with more
        than one column, or ``class_loss=True`` is requested with a
        pre-instantiated (non-callable, non-``"rf"``) ``model_builder``
        that has no ``predict_proba``.
    """
    if loss_function is None:
        loss_function = default_loss

    # ============================================================
    # Validate arguments
    # ============================================================
    if folds < 2:
        raise ValueError(f"folds must be >= 2, got {folds}")
    if nsim < 2:
        raise ValueError(f"nsim must be >= 2, got {nsim}")
    if epochs < 1:
        raise ValueError(f"epochs must be >= 1, got {epochs}")
    if batch_size < 1:
        raise ValueError(f"batch_size must be >= 1, got {batch_size}")
    if len(X) != len(Y):
        raise ValueError(
            f"X and Y must have the same number of rows "
            f"(got {len(X)} and {len(Y)})."
        )
    if folds > len(X):
        raise ValueError(
            f"folds ({folds}) cannot exceed the number of samples "
            f"({len(X)})."
        )
    # ============================================================
    # Convert features once
    # ============================================================
    X_df = pd.DataFrame(X)
    feature_names = X_df.columns.tolist()
    X_np = X_df.to_numpy(dtype=np.float32)
    n, p = X_np.shape

    # ============================================================
    # Detect target type and convert targets
    # ============================================================
    Y_arr = np.asarray(Y)
    is_regression = np.issubdtype(Y_arr.dtype, np.number)

    if is_regression:
        Y_np = pd.DataFrame(Y).to_numpy(dtype=np.float32)
        if Y_np.ndim == 1:
            Y_np = Y_np.reshape(-1, 1)
        yp = Y_np.shape[1]
        classes = None
    else:
        if Y_arr.ndim > 1 and Y_arr.shape[1] > 1:
            raise ValueError(
                "clique() only supports a single classification "
                "target; Y must be 1-D (or a single column) when it "
                "holds non-numeric class labels."
            )
        Y_np = Y_arr.reshape(-1)
        classes = np.unique(Y_np)
        if len(classes) < 2:
            raise ValueError(
                f"classification targets need at least 2 classes, "
                f"got {len(classes)} unique label(s)."
            )
        if (
            class_loss
            and model_builder != "rf"
            and not callable(model_builder)
            and not hasattr(model_builder, "predict_proba")
        ):
            raise ValueError(
                "class_loss=True requires the fold model to expose "
                "predict_proba; the pre-instantiated model_builder "
                "passed in has no such method."
            )
        yp = len(classes)

    # ============================================================
    # CV splits (stored once)
    # ============================================================
    cv = KFold(n_splits=folds, shuffle=True, random_state=random_state)
    splits = list(cv.split(X_np))

    # ============================================================
    # Train fold models
    # ============================================================
    models = []
    for train_idx, test_idx in splits:

        # ----------------------------------------
        # Build model
        # ----------------------------------------
        if model_builder == "rf":
            if is_regression:
                model = RandomForestRegressor(
                    n_estimators=100,
                    random_state=random_state,
                    n_jobs=-1
                )
                if yp > 1:
                    model = MultiOutputRegressor(model)
            else:
                model = RandomForestClassifier(
                    n_estimators=100,
                    random_state=random_state,
                    n_jobs=-1
                )
        elif callable(model_builder):
            if is_regression:
                model = model_builder(p, yp)
            else:
                model = model_builder(p, yp, classes=classes)
        else:
             model = clone(model_builder)

        # ----------------------------------------
        # Fit model
        # ----------------------------------------
        if isinstance(model, TorchMLP):
            model = train_model(
                model,
                X_train=X_np[train_idx],
                y_train=Y_np[train_idx],
                X_val=X_np[test_idx],
                y_val=Y_np[test_idx],
                epochs=epochs,
                batch_size=batch_size,
                verbose=verbose
            )
        else:
            model.fit(X_np[train_idx], Y_np[train_idx])

        models.append(model)

    # ============================================================
    # Base out-of-fold loss
    # ============================================================
    base_loss = np.zeros(n, dtype=np.float32)
    for fold, (_, test_idx) in enumerate(splits):
        model = models[fold]
        _, fold_base_loss = _compute_base_loss(
            model,
            X_np[test_idx],
            Y_np[test_idx],
            loss_function=loss_function,
            class_loss=class_loss,
            batch_size=1024,
            classes=classes
        )
        base_loss[test_idx] = fold_base_loss

    # ============================================================
    # Local importance matrix
    # ============================================================
    local_imp = np.zeros((n, p), dtype=np.float32)

    # ============================================================
    # Main importance loop
    # ============================================================
    for j in range(p):
        grid_vals = _generate_grid_values(
            X_np[:, j],
            nsim=nsim,
            quantile_grid=quantile_grid
        )
        n_grid = len(grid_vals)

        imp_accum = np.zeros(n, dtype=np.float32)

        # ------------------------------------------------------
        # Fold-specific predictions
        # ------------------------------------------------------
        for fold, (_, test_idx) in enumerate(splits):
            model = models[fold]
            X_test = X_np[test_idx]
            Y_test_fold = Y_np[test_idx]
            n_test = len(test_idx)

            # Stacked matrix of shape (n_grid * n_test, p), with
            # column j swept across the grid.
            X_big = np.repeat(X_test, repeats=n_grid, axis=0)
            X_big[:, j] = np.tile(grid_vals, n_test)

            pred_big = _predict_model(
                model,
                X_big,
                batch_size=4096,
                class_loss=class_loss
            )

            # =================================================
            # Regression
            # =================================================
            if is_regression:
                pred_big = np.asarray(pred_big)
                pred_big = pred_big.reshape(
                    n_test, n_grid, yp
                ).transpose(1, 0, 2)

                new_loss = loss_function(
                    Y_test_fold[None, :, :],
                    pred_big
                ).sum(axis=2)

            # =================================================
            # Classification accuracy
            # =================================================
            elif not class_loss:
                pred_big = np.asarray(pred_big)
                pred_big = pred_big.reshape(n_test, n_grid).T

                new_loss = (
                    pred_big != np.asarray(Y_test_fold)
                ).astype(float)

            # =================================================
            # Probability loss
            # =================================================
            else:
                truth, _ = _binarize_labels(Y_test_fold, classes=classes)
                k = truth.shape[1]

                pred_big = np.asarray(pred_big)
                pred_big = pred_big.reshape(
                    n_test, n_grid, k
                ).transpose(1, 0, 2)

                new_loss = loss_function(
                    truth[None, :, :],
                    pred_big
                ).mean(axis=2)

            delta = (new_loss - base_loss[test_idx][None, :]).mean(axis=0)
            imp_accum[test_idx] = delta

        local_imp[:, j] = imp_accum
        if verbose:
            print(f"Finished variable {j + 1}/{p}")

    # ============================================================
    # Return same structure as original
    # ============================================================
    local_imp_df = pd.DataFrame(local_imp, columns=feature_names)

    return {
        "models": models,
        "local_imp": local_imp_df
    }

# =============================================================================
# Public API: clique_eval (pretrained model, explicit train/test split)
# =============================================================================

def clique_eval(
    model,
    X_train,
    Y_train,
    X_test=None,
    Y_test=None,
    nsim: int = 25,
    quantile_grid: bool = True,
    loss_function: Optional[Callable] = None,
    class_loss: bool = False,
    batch_size: int = 4096,
    verbose: bool = False
) -> pd.DataFrame:
    """
    CLIQUE local importance using a single, already-fitted model.

    Supports regression (univariate and multivariate), binary
    classification, and multiclass classification (against hard labels
    or predicted probabilities). ``X_train``/``Y_train`` establish the
    reference distribution each feature's grid is drawn from; importance
    is computed and returned for every row of ``X_test`` (or, if omitted,
    every row of ``X_train``).

    Unlike :func:`clique`, which fits its own models and uses k-fold 
    cross-validation, ``clique_eval`` requires an already-fitted model
    and an explicit train/test split. Prediction dispatch, grid
    construction, and loss computation are delegated to the same
    helpers used by :func:`clique` (:func:`_predict_model`,
    :func:`_generate_grid_values`, and ``loss_function``), so both entry
    points behave identically wherever their functionality overlaps.

    Parameters
    ----------
    model : object
        A fitted estimator exposing ``predict`` (and, if ``class_loss``
        is True, ``predict_proba``), or a fitted :class:`TorchMLP` --
        detected automatically via ``isinstance(model, TorchMLP)``.
    X_train : array-like of shape (n_train, n_features)
        Reference feature matrix used to build each feature's grid.
    Y_train : array-like
        Reference targets. Only used directly when ``X_test`` is
        omitted (see below).
    X_test : array-like of shape (n_test, n_features), optional
        Feature matrix to compute local importance for. Defaults to
        ``X_train``.
    Y_test : array-like, optional
        Targets corresponding to ``X_test``. Defaults to ``Y_train``
        when ``X_test`` is omitted.
    nsim : int, default=25
        Number of grid points evaluated per feature. Must be >= 2.
    quantile_grid : bool, default=True
        If True, grid points are quantiles of the feature's observed
        values; otherwise they are evenly spaced between its min and
        max. Ignored for categorical features.
    loss_function : callable, optional
        ``loss_function(truth, pred) -> array``, elementwise. Defaults
        to :func:`default_loss`.
    class_loss : bool, default=False
        For classification targets, score against predicted
        probabilities (True, requires ``predict_proba``) instead of
        hard labels (False).
    batch_size : int, default=4096
        Batch size used for grid predictions (and Torch inference).
        Must be >= 1.
    verbose : bool, default=False
        If True, print progress ("Finished variable j/p") as each
        feature is processed.

    Returns
    -------
    pandas.DataFrame of shape (n_test, n_features)
        Local importance values, one row per test observation and one
        column per feature.

    Raises
    ------
    ValueError
        If ``nsim < 2``, ``batch_size < 1``, ``X_train``/``Y_train`` (or
        ``X_test``/``Y_test``) have mismatched row counts, ``X_test``
        has a different number of features than ``X_train``, or
        ``class_loss=True`` is requested for a model with no
        ``predict_proba``.
    """
    if loss_function is None:
        loss_function = default_loss

    # ============================================================
    # Validate arguments
    # ============================================================
    if nsim < 2:
        raise ValueError(f"nsim must be >= 2, got {nsim}")
    if batch_size < 1:
        raise ValueError(f"batch_size must be >= 1, got {batch_size}")
    if class_loss and not hasattr(model, "predict_proba"):
        raise ValueError(
            "class_loss=True requires `model` to expose predict_proba; "
            "got an object with no such method."
        )
    if len(X_train) != len(Y_train):
        raise ValueError(
            f"X_train and Y_train must have the same number of rows "
            f"(got {len(X_train)} and {len(Y_train)})."
        )
    if (X_test is None) != (Y_test is None):
        raise ValueError(
            "X_test and Y_test must be provided together, or both "
            "omitted."
        )
    if X_test is not None and len(X_test) != len(Y_test):
        raise ValueError(
            f"X_test and Y_test must have the same number of rows "
            f"(got {len(X_test)} and {len(Y_test)})."
        )

    X_train = pd.DataFrame(X_train)
    if X_test is None:
        X_test = X_train.copy()
        Y_test = Y_train
    X_test = pd.DataFrame(X_test)

    if X_test.shape[1] != X_train.shape[1]:
        raise ValueError(
            f"X_test must have the same number of features as "
            f"X_train (got {X_test.shape[1]} and {X_train.shape[1]})."
        )

    feature_names = X_train.columns.tolist()
    X_ref = X_train.to_numpy()
    X_eval = X_test.to_numpy()

    # =====================================================
    # Base predictions
    # =====================================================
    preds, base_loss = _compute_base_loss(
        model=model,
        X=X_eval,
        Y=Y_test,
        loss_function=loss_function,
        class_loss=class_loss,
        batch_size=batch_size
    )

    n = X_eval.shape[0]
    p = X_eval.shape[1]
    local_imp = np.zeros((n, p), dtype=np.float32)

    # =====================================================
    # Variable loop
    # =====================================================
    for j in range(p):
        grid_vals = _generate_grid_values(
            X_ref[:, j],
            nsim=nsim,
            quantile_grid=quantile_grid
        )
        n_grid = len(grid_vals)

        X_big = np.repeat(X_eval, repeats=n_grid, axis=0)
        X_big[:, j] = np.tile(grid_vals, n)

        pred_big = _predict_model(
            model,
            X_big,
            batch_size=batch_size,
            class_loss=class_loss
        )

        # =================================================
        # Regression
        # =================================================
        if np.issubdtype(np.asarray(Y_test).dtype, np.number):
            Y = np.asarray(Y_test)
            if Y.ndim == 1:
                Y = Y.reshape(-1, 1)
            yp = Y.shape[1]

            pred_big = np.asarray(pred_big)
            pred_big = pred_big.reshape(n, n_grid, yp).transpose(1, 0, 2)

            new_loss = loss_function(
                Y[None, :, :],
                pred_big
            ).sum(axis=2)

        # =================================================
        # Classification accuracy
        # =================================================
        elif not class_loss:
            pred_big = np.asarray(pred_big)
            pred_big = pred_big.reshape(n, n_grid).T

            new_loss = (pred_big != np.asarray(Y_test)).astype(float)

        # =================================================
        # Probability loss
        # =================================================
        else:
            truth, _ = _binarize_labels(Y_test)
            k = truth.shape[1]

            pred_big = np.asarray(pred_big)
            pred_big = pred_big.reshape(n, n_grid, k).transpose(1, 0, 2)

            new_loss = loss_function(
                truth[None, :, :],
                pred_big
            ).mean(axis=2)

        delta = (new_loss - base_loss[None, :]).mean(axis=0)
        local_imp[:, j] = delta

        if verbose:
            print(f"Finished variable {j + 1}/{p}")

    local_imp = pd.DataFrame(local_imp, columns=feature_names)
    return local_imp