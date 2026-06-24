# Main function
#' clique
#' @importFrom caret createFolds train trainControl
#' @importFrom dplyr arrange select
#' @importFrom stats model.frame
#' @importFrom fastDummies dummy_cols
#' @importFrom future multisession plan
#' @importFrom future.apply future_lapply
#' @importFrom rlang .data
#' @description Implements the CLIQUE method for local variable importance
#'   estimation. The procedure fits a predictive model using cross-validation
#'   and evaluates changes in prediction error under controlled perturbations
#'   of each feature, enabling observation-level assessment of feature
#'   importance.
#' @param x \code{x} is an object where samples are in rows and features are
#'   in columns. This could be a data frame or matrix with column names.
#' @param y A numeric or factor vector containing the response feature.
#' @param formula An object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#'   By default the variables are taken from \code{environment(formula)}.
#' @param method A character string specifying the model to be passed to
#'   \code{caret::train}. Valid options correspond to models registered in caret.
#'   See \code{caret::modelLookup()} for available methods.
#' @param tuneGrid A data frame with tuning parameters for the specified model.
#'   See \code{caret::train} for model-specific tuning structures.
#' @param folds Number of folds used for cross-validation. Default is 5.
#' @param parallel Logical; if TRUE, computations are run in parallel.
#'   Default is TRUE.
#' @param cores Number of CPU cores to use when \code{parallel = TRUE}.
#'   Default is 5.
#' @param seed Random seed for reproducibility. Default is 123.
#'   Set to \code{NULL} to disable.
#' @param nsim Number of grid points used to replace each variable.
#'   Default is 25.
#' @param quantile_grid Logical; if TRUE, replacement points are chosen
#'   using quantiles of the variable. If FALSE, a uniform grid is used.
#'   Default is TRUE.
#' @param class_loss Logical; if FALSE simply computes individual errors based
#'   on classification accuracy. If TRUE, averages the \code{loss_function}
#'   across class level probabilities when computing individual errors, serving
#'   as a loss generalized version of a Brier Score. Only applicable for
#'   classification problems. Default is FALSE.
#' @param loss_function A function for computing the loss/error between model
#'   predictions and the response. Argument is necessary for regression or
#'   if \code{class_loss = TRUE}. Default is Absolute Error.
#'
#' @return A list containing:
#'   \item{bestTune}{Best hyper-parameter combination selected by caret.}
#'   \item{results}{A data frame of performance metrics for all hyper-parameter
#'   combinations.}
#'   \item{finalModel}{A fitted model trained using the optimal tuning
#'   parameters.}
#'   \item{local_imp}{A data frame of CLIQUE values (local variable
#'   importance measures).}
#'
#' @seealso
#' \code{\link{clique_eval}}
#' @export
#'
#' @examples
#' v <- clique(x = iris[1:4], y = iris$Species,
#'             method = "rf", cores = 2)
#' v$local_imp

clique <- function(x, y, formula, data,
                   method = "rf", tuneGrid = NULL, folds = 5,
                   parallel = TRUE, cores = 5, seed = 123,
                   nsim = 25, quantile_grid = TRUE,
                   class_loss = FALSE,
                   loss_function = function(truth, predictions)
                     abs(truth - predictions)
) {

  # Did user provide x and y?
  using_xy <- !missing(x) && !missing(y)

  # Did user provide formula interface?
  using_formula <- !missing(formula) && !missing(data)

  # If neither interface is supplied
  if (!using_xy && !using_formula) {
    stop("You must provide either (x & y) or (formula & data).")
  }

  if (!using_xy) {
    model_frame <- model.frame(formula, data = data)
    x <- model_frame[, -1, drop = FALSE]
    y <- model_frame[, 1]
  }

  if (length(y) != nrow(x)) {
    stop("x and y have incompatible dimensions.")
  }

  if (is.factor(y)) {
    y <- factor(y, labels = make.names(levels(y)))
  }

  x <- as.data.frame(x)

  # Set up cross-validation folds
  set.seed(seed)
  flds <- caret::createFolds(y, k = folds, returnTrain = TRUE)
  set.seed(seed)
  flds0 <- caret::createFolds(y, k = folds, list = FALSE, returnTrain = FALSE)

  # Parallel setup
  if (parallel) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    future::plan(future::multisession, workers = cores)
  }

  # Train the global model: invalid connect
  global_model <- caret::train(
    x, y,
    method = method,
    tuneGrid = tuneGrid,
    trControl = caret::trainControl(
      method = "cv",
      index = flds,
      savePredictions = "final",
      classProbs = is.factor(y)
    )
  )

  # Train models for each fold
  mods <- train_models(x, y, folds, flds0, method,
                       global_model$bestTune)

  global_model$pred <- global_model$pred |> dplyr::arrange(.data$rowIndex)

  # Calculate errors
  if (!is.factor(y)) {
    truth <- global_model$pred$obs
    predictions <- global_model$pred$pred
    m <- loss_function(truth, predictions)
  } else if (class_loss) {
    truth <- fastDummies::dummy_cols(as.data.frame(y))[, -1]
    predictions <- global_model$pred |> dplyr::select(levels(y))
    m <- rowMeans(loss_function(truth, predictions))
  } else {
    truth <- factor(global_model$pred$obs,
                    levels = levels(y))
    predictions <- factor(global_model$pred$pred,
                          levels = levels(y))
    m <- as.numeric(truth != predictions)
  }

  x_loc <- clique_compute(
    x_ref = x,
    x_eval = x,
    y_eval = y,
    truth = truth,
    predictions = predictions,
    m = m,
    mods = mods,
    flds0 = flds0,
    seed = seed,
    nsim = nsim,
    quantile_grid = quantile_grid,
    class_loss = class_loss,
    loss_function = loss_function,
    typ = ifelse(class_loss & is.factor(y), "prob", "raw")
  )

  list(
    bestTune = global_model$bestTune,
    results = global_model$results,
    finalModel = global_model$finalModel,
    local_imp = x_loc
  )
}

#' clique_eval
#' @importFrom dplyr select
#' @importFrom stats model.frame predict
#' @importFrom fastDummies dummy_cols
#' @importFrom future multisession plan
#' @description Computes CLIQUE local variable importance values using a
#'   pre-trained predictive model. The procedure evaluates changes in
#'   prediction error under controlled perturbations of each feature and can
#'   be applied to either the training data or an external test dataset.
#'   For model assessment and interpretation of generalization behavior,
#'   evaluation on an independent test dataset is recommended, as
#'   importance values computed on training observations may reflect
#'   overfitting or optimistic prediction performance. Consider using
#'   \code{clique} to evaluate training data through cross-validation.
#' @param model A fitted predictive model with a compatible
#'   \code{predict()} method. Models generated by \code{caret::train} are
#'   recommended to ensure support.
#' @param x_train A data frame object where samples are in rows and features
#'   are in columns. Used to construct the reference distribution for
#'   feature perturbations.
#' @param y_train A numeric or factor vector containing the response feature
#'   corresponding to \code{x_train}.
#' @param x_test Optional test data where samples are in rows and features
#'   are in columns. If omitted, CLIQUE values are computed for
#'   \code{x_train}.
#' @param y_test Optional response vector corresponding to \code{x_test}.
#'   Must be provided for \code{x_test} to be evaluated. If omitted,
#'   \code{y_train} is used to evaluate \code{x_train}.
#' @param formula An object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the variables used by
#'   the model.
#' @param data_train A data frame containing the training variables in the
#'   model.
#' @param data_test Optional test data frame for evaluating.
#'   If omitted, \code{data_train} is used.
#' @param parallel Logical; if TRUE, computations are run in parallel.
#'   Default is FALSE.
#' @param cores Number of CPU cores to use when \code{parallel = TRUE}.
#'   Default is 5.
#' @param seed Random seed for reproducibility. Default is 123.
#'   Set to \code{NULL} to disable.
#' @param nsim Number of grid points used to replace each variable.
#'   Default is 25.
#' @param quantile_grid Logical; if TRUE, replacement points are chosen
#'   using quantiles of the variable. If FALSE, a uniform grid is used.
#'   Default is TRUE.
#' @param class_loss Logical; if FALSE simply computes individual errors based
#'   on classification accuracy. If TRUE, averages the \code{loss_function}
#'   across class level probabilities when computing individual errors, serving
#'   as a loss-generalized version of a Brier Score. Only applicable for
#'   classification problems. Default is FALSE.
#' @param loss_function A function for computing the loss/error between model
#'   predictions and the response. Argument is necessary for regression or
#'   if \code{class_loss = TRUE}. Default is Absolute Error.
#'
#' @return A data frame of CLIQUE values (local variable importance measures),
#'   with one row per evaluated observation and one column per feature.
#'   Larger values indicate greater local importance of the corresponding
#'   feature for the observation.
#'
#' @section Note:
#'   When available, users are encouraged to provide an independent test
#'   dataset through \code{x_test} and \code{y_test}. If explanation of
#'   training observations is desired, users are encouraged to do so via
#'   the cross-validation framework found in \code{clique}. Computing CLIQUE
#'   values on raw training observations may yield importance estimates that
#'   reflect patterns learned from the training data, whereas test-set or
#'   cross-validation evaluation better characterizes feature importance
#'   for out-of-sample predictions.
#'
#' @seealso
#' \code{\link{clique}}
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' train_ind <- sample(1:150, 120)
#' test_ind <- (1:150)[!(1:150 %in% train_ind)]
#'
#' model <- caret::train(
#'   x = iris[train_ind, 1:4], y = iris$Species[train_ind],
#'   method = "rf"
#' )
#'
#' ## get training data local importance
#' v_train <- clique_eval(
#'   model = model,
#'   x_train = iris[train_ind, 1:4],
#'   y_train = iris$Species[train_ind],
#'   cores = 2
#' )
#'
#' head(v_train)
#'
#' ## get test data local importance
#' v_test <- clique_eval(
#'   model = model,
#'   x_train = iris[train_ind, 1:4],
#'   y_train = iris$Species[train_ind],
#'   x_test = iris[test_ind, 1:4],
#'   y_test = iris$Species[test_ind],
#'   cores = 2
#' )
#'
#' head(v_test)
clique_eval <- function(model, x_train, y_train,
                        x_test, y_test,
                        formula, data_train, data_test,
                        parallel = FALSE, cores = 5, seed = 123,
                        nsim = 25, quantile_grid = TRUE,
                        class_loss = FALSE,
                        loss_function = function(truth, predictions)
                          abs(truth - predictions)
) {
  # clique_eval, clique_newdata, clique_model

  # Did user provide x and y?
  using_xy <- !missing(x_train) && !missing(y_train)

  # Did user provide formula interface?
  using_formula <- !missing(formula) && !missing(data_train)

  # If neither interface is supplied
  if (!using_xy && !using_formula) {
    stop("You must provide either (x_train & y_train) or (formula & data_train).")
  }

  if (!missing(x_test) && missing(y_test)) {
    stop("y_test must be supplied when x_test is provided.")
  }

  if(missing(data_test) && using_formula){
    data_test <- data_train
  }

  if (!using_xy) {
    model_frame <- model.frame(formula, data = data_train)
    x_train <- model_frame[, -1, drop = FALSE]
    y_train <- model_frame[, 1]
    model_frame <- model.frame(formula, data = data_test)
    x_test <- model_frame[, -1, drop = FALSE]
    y_test <- model_frame[, 1]
  }

  if(missing(x_test)){
    x_test <- x_train
    y_test <- y_train
  }

  if (length(y_train) != nrow(x_train)) {
    stop("x_train and y_train have incompatible dimensions.")
  }

  if (length(y_test) != nrow(x_test)) {
    stop("x_test and y_test have incompatible dimensions.")
  }

  x_train <- as.data.frame(x_train)
  x_test  <- as.data.frame(x_test)

  x_all <- rbind(x_train, x_test)

  # if (is.factor(y_train)) {
  #   y_train <- factor(y_train, labels = make.names(levels(y_train)))
  #   y_test <- factor(y_test, labels = make.names(levels(y_test)))
  # }

  # Parallel setup
  if (parallel) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    future::plan(future::multisession, workers = cores)
  }

  # Calculate test errors
  if (!is.factor(y_test)) {
    truth <- y_test
    predictions <- predict(model, x_test)
    m <- loss_function(truth, predictions)
  } else if (class_loss) {
    levs <- levels(as.factor(y_train))
    y_test <- factor(y_test, levels = levs)
    truth <- fastDummies::dummy_cols(as.data.frame(y_test))[, -1]
    predictions <- predict(model, x_test, type = "prob") |>
      dplyr::select(levs)
    m <- rowMeans(loss_function(truth, predictions))
  } else {
    levs <- levels(as.factor(y_train))
    truth <- factor(y_test, levels = levs)
    predictions <- factor(
      predict(model, x_test),
      levels = levs
    )
    m <- as.numeric(truth != predictions)
  }

  x_loc <- clique_compute(
    x_ref = x_all,
    x_eval = x_test,
    y_eval = y_test,
    truth = truth,
    predictions = predictions,
    m = m,
    model = model,
    seed = seed,
    nsim = nsim,
    quantile_grid = quantile_grid,
    class_loss = class_loss,
    loss_function = loss_function,
    typ = ifelse(class_loss & is.factor(y_test), "prob", "raw")
  )

  x_loc
}

#' Internal CLIQUE computation engine
#' @importFrom future.apply future_lapply
#' @keywords internal
clique_compute <- function(
    x_ref,
    x_eval,
    y_eval,
    truth,
    predictions,
    m,
    model = NULL,
    mods = NULL,
    flds0 = NULL,
    seed = TRUE,
    nsim = 25,
    quantile_grid = TRUE,
    class_loss = FALSE,
    loss_function,
    typ = "raw"
) {

  x_loc <- future.apply::future_lapply(
    seq_len(ncol(x_eval)),
    function(ii) {

      get_var_loc(
        ii = ii,
        x_grid = x_ref,
        x_eval = x_eval,
        y = y_eval,
        truth = truth,
        nsim = nsim,
        m = m,
        typ = typ,
        quantile_grid = quantile_grid,
        class_loss = class_loss,
        loss_function = loss_function,
        predictions = predictions,
        model = model,
        mods = mods,
        flds0 = flds0
      )

    },
    future.seed = seed
  ) |> as.data.frame()

  colnames(x_loc) <- colnames(x_eval)

  x_loc
}

#' Internal helper to train the CV models
#' @importFrom caret train trainControl
#' @keywords internal
train_models <- function(x, y, folds, flds0, method, tuneGrid) {
  models <- vector("list", length(folds))
  for (k in seq_len(folds)) {
    models[[k]] <- caret::train(
      x[flds0 != k, ], y[flds0 != k],
      method = method,
      tuneGrid = tuneGrid,
      trControl = caret::trainControl(method = "none",
                                      classProbs = is.factor(y))
    )
  }
  return(models)
}

#' Internal helper function to generate grid values
#' @importFrom stats quantile
#' @importFrom tidyr uncount
#' @importFrom dplyr pull
#' @importFrom rlang .data
#' @keywords internal
generate_grid_values <- function(xi, nsim, quantile_grid) {
  if (!is.numeric(xi)) {
    grid_v <- round(nsim * table(xi) / length(xi))
    # gd = cheapr::gcd(grid_v) # install
    # gd <- min(pracma::gcd(grid_v[1], grid_v))
    grid_vals <- grid_v |> as.data.frame() |>
      tidyr::uncount(.data$Freq) |>
      dplyr::pull(1)
  } else {
    grid_vals <- if (quantile_grid) {
      quantile(xi, probs = seq(0, 1, length = nsim)) # Quantile-based grid
    } else {
      seq(min(xi), max(xi), length = nsim) # Uniform grid
    }
  }
  grid_vals
}

#' Internal helper function to update predictions and calculate errors
#' @importFrom stats predict
#' @keywords internal
calculate_new_m <- function(
    grid_val,
    x,
    y,
    ii,
    truth,
    typ,
    class_loss,
    loss_function,
    model = NULL,
    mods = NULL,
    flds0 = NULL,
    predictions
) {

  x_new <- x
  x_new[[ii]] <- grid_val

  ## Obtain predictions
  if (!is.null(mods)) {

    for (k in seq_along(mods)) {

      if (class_loss && is.factor(y)) {

        predictions[flds0 == k, ] <- predict(mods[[k]],
                                             x_new[flds0 == k, ],
                                             type = typ)

      } else {

        predictions[flds0 == k] <- predict(mods[[k]],
                                           x_new[flds0 == k, ],
                                           type = typ)

      }
    }

  } else {

    if (class_loss && is.factor(y)) {
      predictions <- predict(model, x_new, type = "prob")
    } else {
      predictions <- predict(model, x_new)
    }

  }

  ## Compute loss
  if (!is.factor(y)) {

    loss_function(truth, predictions)

  } else if (class_loss) {

    rowMeans(loss_function(truth, predictions))

  } else {

    as.numeric(truth != predictions)

  }
}

#' Internal helper function that generates grids for a variable and obtains and
#' aggregates all new errors to vector matching the length of the response.
#' @keywords internal
get_var_loc <- function(
    ii,
    x_grid,
    x_eval,
    y,
    truth,
    nsim,
    m,
    typ,
    quantile_grid,
    class_loss,
    loss_function,
    predictions,
    model = NULL,
    mods = NULL,
    flds0 = NULL
) {

  grid_vals <- generate_grid_values(x_grid[[ii]], nsim, quantile_grid)

  new_m_mat <- sapply(
    grid_vals,
    calculate_new_m,
    x = x_eval,
    y = y,
    ii = ii,
    truth = truth,
    typ = typ,
    class_loss = class_loss,
    loss_function = loss_function,
    predictions = predictions,
    model = model,
    mods = mods,
    flds0 = flds0
  )

  if (nrow(x_eval) == 1) {

    mean(new_m_mat) - m

  } else {

    rowMeans(new_m_mat) - m

  }
}
