# Main function
#' clique
#' @importFrom caret createFolds train trainControl
#' @importFrom dplyr %>% arrange select pull
#' @importFrom tidyr uncount
#' @importFrom stats model.frame quantile predict
#' @importFrom pracma gcd
#' @importFrom fastDummies dummy_cols
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom future.apply future_lapply
#' @importFrom rlang .data
#' @description Implements the CLIQUE method for local variable importance
#'   estimation. The procedure fits a predictive model using cross-validation
#'   and evaluates changes in prediction error under controlled perturbations
#'   of each feature, enabling observation-level assessment of feature
#'   importance.
#' @param formula an object of class "\link{formula}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param data a data frame containing the variables in the model.
#'   By default the variables are taken from \code{environment(formula)}.
#' @param method A character string specifying the model to be passed to
#'   \code{caret::train}. Valid options correspond to models registered in caret.
#'   See \code{caret::modelLookup()} for available methods.
#' @param tuneGrid A data frame with tuning parameters for the specified model.
#'   See \code{caret::train} for model-specific tuning structures.
#' @param parallel Logical; if TRUE, computations are run in parallel.
#'   Default is TRUE.
#' @param cores Number of CPU cores to use when \code{parallel = TRUE}.
#'   Default is 5.
#' @param seed Random seed for reproducibility. Default is 123.
#'   Set to \code{NULL} to disable.
#' @param nsim Number of grid points used to replace each variable.
#'   Default is 25.
#' @param folds Number of folds used for cross-validation. Default is 5.
#' @param quantile_grid Logical; if TRUE, replacement points are chosen
#'   using quantiles of the variable. If FALSE, a uniform grid is used.
#'   Default is TRUE.
#' @param brier_score Logical; if TRUE, uses the Brier score instead of
#'   classification accuracy when computing individual errors.
#'   Only applicable for classification problems. Default is FALSE.
#'
#' @return A list containing:
#'   \item{bestTune}{Best hyper-parameter combination selected by caret.}
#'   \item{results}{A data frame of performance metrics for all hyper-parameter
#'   combinations.}
#'   \item{finalModel}{A fitted model trained using the optimal tuning
#'   parameters.}
#'   \item{local_imp}{A data frame of CLIQUE values (local variable
#'   importance measures).}
#' @export
#'
#' @examples
#' v <- clique(formula = factor(Species) ~ ., data = iris,
#'             method = "rf", cores = 2)
#' v$local_imp

clique <- function(formula, data,
                   method = "rf", tuneGrid = NULL,
                   parallel = TRUE, cores = 5, seed = 123,
                   nsim = 25, folds = 5, quantile_grid = TRUE,
                   brier_score = FALSE) {
  # Set up the data
  model_frame <- model.frame(formula, data = data)
  x <- model_frame[, -1]
  y <- model_frame[, 1]
  nc <- ncol(x)
  if (is.factor(y)) {
    y <- factor(y, labels = make.names(levels(y)))
  }

  # Set up cross-validation folds
  set.seed(seed)
  flds <- caret::createFolds(y, k = folds, returnTrain = TRUE)
  set.seed(seed)
  flds0 <- caret::createFolds(y, k = folds, list = FALSE, returnTrain = FALSE)

  # Parallel setup
  if (parallel) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    # on.exit({
    #   parallel::stopCluster(cl)
    #   env <- foreach:::.foreachGlobals
    #   rm(list=ls(name=env), pos=env)
    # })
    on.exit({
      parallel::stopCluster(cl)
    }, add = TRUE)
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
      classProbs = TRUE
    )
  )

  # Train models for each fold
  mods <- train_models(x, y, folds, flds0, method,
                       global_model$finalModel$tuneValue)

  global_model$pred = global_model$pred %>% dplyr::arrange(.data$rowIndex)

  # Calculate errors
  if (!is.factor(y)) {
    truth <- global_model$pred$obs
    predictions <- global_model$pred$pred
    m <- abs(truth - predictions)
  } else if (brier_score) {
    truth <- fastDummies::dummy_cols(as.data.frame(y))[, -1]
    predictions <- global_model$pred %>% dplyr::select(levels(y))
    m <- rowMeans(abs(truth - predictions))
  } else {
    truth <- global_model$pred$obs
    predictions <- global_model$pred$pred
    m <- as.numeric(truth != predictions)
  }

  # Task function for parallel computation
  x_loc <- future.apply::future_lapply(
    seq_len(nc),
    function(ii) {
      get_var_loc(ii, x = x, y = y, truth = truth, nsim = nsim, mods = mods,
                  flds0 = flds0, m = m, typ = ifelse(brier_score, "prob", "raw"),
                  quantile_grid = quantile_grid, predictions = predictions,
                  brier_score = brier_score)
    }
  ) %>% as.data.frame()
  colnames(x_loc) <- colnames(x)

  # Output
  list(
    bestTune = global_model$bestTune,
    results = global_model$results,
    finalModel = global_model$finalModel,
    local_imp = x_loc
  )
}

#' Internal helper to train the CV models
#' @keywords internal
train_models <- function(x, y, folds, flds0, method, tuneGrid) {
  models <- vector("list", length(folds))
  for (k in seq_len(folds)) {
    models[[k]] <- caret::train(
      x[flds0 != k, ], y[flds0 != k],
      method = method,
      tuneGrid = tuneGrid,
      trControl = caret::trainControl(method = "none",
        classProbs = TRUE)
    )
  }
  return(models)
}

#' Internal helper function to generate grid values
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
#' @keywords internal
calculate_new_m <- function(grid_val, x, y, ii, mods, flds0, truth, typ,
                            predictions, brier_score) {
  x_new <- x
  x_new[, ii] <- grid_val

  for (k in seq_along(mods)) {
    if(brier_score){
      predictions[flds0 == k,] = predict(mods[[k]], x_new[flds0 == k,],
                                         type = typ)
    } else {
      predictions[flds0 == k] = predict(mods[[k]], x_new[flds0 == k,],
                                        type = typ)
    }
  }
  if(!is.factor(y)){
    return(abs(truth - predictions))
  } else if (brier_score){
    # Calculate the row-wise mean squared error
    return(rowMeans(abs(truth - predictions)))
  } else {
    return(as.numeric(truth != predictions))
  }
}

#' Internal helper function that generates grids for a variable and obtains and
#' aggregates all new errors to vector matching the length of the response.
#' @keywords internal
get_var_loc <- function(ii, x, y, truth, nsim, mods, flds0, m, typ,
                  quantile_grid, predictions, brier_score) { # depends on y

  # Generate grid values for the selected variable
  grid_vals <- generate_grid_values(x[, ii], nsim, quantile_grid)

  # Apply over all grid values
  new_m_mat <- sapply(grid_vals, calculate_new_m, x = x, y = y, ii = ii,
                      mods = mods, flds0 = flds0, truth = truth, typ = typ,
                      predictions = predictions, brier_score = brier_score)

  # Calculate the mean error for each observation and adjust by baseline errors
  rowMeans(new_m_mat) - m
}

# library(caret)
# library(dplyr)
# library(parallel)
# library(doParallel)
# library(foreach)
# library(future.apply)
# library(fastDummies)
# library(pracma)
#
# library(randomForest)
# library(tidyverse)
# library(patchwork)
# library(randomForestVIP)
# library(scales)
# library(ggeasy)
