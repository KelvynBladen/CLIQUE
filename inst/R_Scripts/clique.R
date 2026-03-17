library(randomForest)
library(tidyverse)
library(patchwork)
library(randomForestVIP)
library(caret)
library(scales)
library(ggeasy)

train_models <- function(x, y, folds, flds0, method, tuneGrid) { # caret
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


# Helper function to generate grid values
generate_grid_values <- function(xi, y, nsim, quantile_grid) {
  if (!is.numeric(xi)) {
    grid_v <- round(nsim * table(y) / length(y))
    gd <- min(pracma::gcd(grid_v[1], grid_v))
    grid_vals <- (grid_v / gd) |> as.data.frame() |> uncount(Freq) |> pull(1)
  } else {
    grid_vals <- if (quantile_grid) {
      quantile(xi, probs = seq(0, 1, length = nsim)) # Quantile-based grid
    } else {
      seq(min(xi), max(xi), length = nsim) # Uniform grid
    }
  }
  grid_vals
}

# Update predictions and calculate errors
calculate_new_m <- function(grid_val, x, ii, mods, flds0, truth, typ,
                            predictions) {
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
  predictions
  if(!is.factor(y)){
    return(abs(truth - predictions))
  } else if (brier_score){
    # Calculate the row-wise mean squared error
    return(rowMeans(abs(truth - predictions)))
  } else {
    return(as.numeric(truth != predictions))
  }
}

get_var_loc <- function(ii, x, y, truth, nsim, mods, flds0, m, typ,
                  quantile_grid, predictions) { # depends on y

  # Generate grid values for the selected variable
  grid_vals <- generate_grid_values(x[, ii], y, nsim, quantile_grid)

  # Apply over all grid values
  new_m_mat <- sapply(grid_vals, calculate_new_m, x = x, ii = ii,
                      mods = mods, flds0 = flds0, truth = truth, typ = typ,
                      predictions = predictions)

  # Calculate the mean error for each observation and adjust by baseline errors
  rowMeans(new_m_mat) - m
}


# Main function
clique <- function(formula = medv ~ ., data = Boston,
                   method = "rf", tuneGrid = tuneGrid,
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

  # Train the global model
  global_model <- caret::train(
    x, y,
    method = method,
    tuneGrid = tuneGrid,
    trControl = trainControl(
      method = "cv",
      index = flds,
      savePredictions = "final",
      classProbs = TRUE
    )
  )


  # Parallel setup
  if (parallel) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      parallel::stopCluster(cl)
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
    })
  }

  # Train models for each fold
  mods <- train_models(x, y, folds, flds0, method,
                       global_model$finalModel$tuneValue)

  global_model$pred = global_model$pred %>% dplyr::arrange(rowIndex)

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
            quantile_grid = quantile_grid, predictions = predictions)
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
