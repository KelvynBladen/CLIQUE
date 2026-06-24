# library(caret)
# train_ind = sample(1:150, 120)
# test_ind = (1:150)[!(1:150 %in% train_ind)]
#
#
# model <- caret::train(
#   x = iris[train_ind, 1:4], y = iris$Species[train_ind],
#   method = "rf",
#   tuneGrid = NULL,
#   trControl = caret::trainControl(
#     method = "cv",
#     classProbs = TRUE
#   )
# )
#
# res = clique_test(model, x_train = iris[train_ind,1:4],
#                   y_train = iris$Species[train_ind],
#                   x_test = iris[test_ind, 1:4], y_test = iris$Species[test_ind])
#
# res
#
# res1 = clique_test(model, x_train = iris[train_ind,1:4],
#                   y_train = iris$Species[train_ind])
#
# res1
#
# clique_test <- function(model, x_train, y_train,
#                         x_test, y_test,
#                         formula, data_train, data_test,
#                         parallel = FALSE, cores = 5, seed = 123,
#                         nsim = 25, quantile_grid = TRUE,
#                         class_loss = FALSE,
#                         loss_function = function(truth, predictions)
#                           abs(truth - predictions)
# ) {
#
#   # Did user provide x and y?
#   using_xy <- !missing(x_train) && !missing(y_train)
#
#   # Did user provide formula interface?
#   using_formula <- !missing(formula) && !missing(data_train)
#
#   # If neither interface is supplied
#   if (!using_xy && !using_formula) {
#     stop("You must provide either (x_train & y_train) or (formula & data_train).")
#   }
#
#   if(missing(data_test) && using_formula){
#     data_test = data_train
#   }
#
#   if (!using_xy) {
#     model_frame <- model.frame(formula, data = data_train)
#     x_train <- model_frame[, -1]
#     y_train <- model_frame[, 1]
#     model_frame <- model.frame(formula, data = data_test)
#     x_test <- model_frame[, -1]
#     y_test <- model_frame[, 1]
#   }
#
#   if(missing(x_test)){
#     x_test = x_train
#     y_test = y_train
#   }
#
#   if (length(y_train) != nrow(x_train)) {
#     stop("x_train and y_train have incompatible dimensions.")
#   }
#
#   if (length(y_test) != nrow(x_test)) {
#     stop("x_test and y_test have incompatible dimensions.")
#   }
#
#   x_all = rbind(x_train, x_test)
#
#   nc <- ncol(x_all)
#   # if (is.factor(y_train)) {
#   #   y_train <- factor(y_train, labels = make.names(levels(y_train)))
#   #   y_test <- factor(y_test, labels = make.names(levels(y_test)))
#   # }
#
#   # Parallel setup
#   if (parallel) {
#     cl <- parallel::makeCluster(cores)
#     doParallel::registerDoParallel(cl)
#     # on.exit({
#     #   parallel::stopCluster(cl)
#     #   env <- foreach:::.foreachGlobals
#     #   rm(list=ls(name=env), pos=env)
#     # })
#     on.exit({
#       parallel::stopCluster(cl)
#     }, add = TRUE)
#   }
#
#   # Calculate test errors
#   if (!is.factor(y_test)) {
#     truth <- y_test
#     predictions <- predict(model, x_test)
#     m <- loss_function(truth, predictions)
#   } else if (class_loss) {
#     truth <- fastDummies::dummy_cols(as.data.frame(y_test))[, -1]
#     predictions <- predict(model, x_test, type = "prob") |>
#       dplyr::select(levels(y_test))
#     m <- rowMeans(loss_function(truth, predictions))
#   } else {
#     truth <- y_test
#     predictions <- predict(model, x_test)
#     m <- as.numeric(truth != predictions)
#   }
#
#   # Task function for parallel computation
#   x_loc <- future.apply::future_lapply(
#     seq_len(nc),
#     function(ii) {
#       get_var_loc(model = model, ii, x_all = x_all, x = x_test, y = y_test,
#                   truth = truth, nsim = nsim, m = m,
#                   typ = ifelse(class_loss & is.factor(y_test), "prob", "raw"),
#                   quantile_grid = quantile_grid,
#                   class_loss = class_loss, loss_function = loss_function)
#     },
#     future.seed = T
#   ) |> as.data.frame()
#   colnames(x_loc) <- colnames(x_test)
#
#   x_loc
# }
#
#
#
# generate_grid_values <- function(xi, nsim, quantile_grid) {
#   if (!is.numeric(xi)) {
#     grid_v <- round(nsim * table(xi) / length(xi))
#     grid_vals <- grid_v |> as.data.frame() |>
#       tidyr::uncount(.data$Freq) |>
#       dplyr::pull(1)
#   } else {
#     grid_vals <- if (quantile_grid) {
#       quantile(xi, probs = seq(0, 1, length = nsim)) # Quantile-based grid
#     } else {
#       seq(min(xi), max(xi), length = nsim) # Uniform grid
#     }
#   }
#   grid_vals
# }
#
# calculate_new_m <- function(model, grid_val, x, y, ii, truth, typ,
#                             class_loss, loss_function) {
#   x_new <- x
#   x_new[, ii] <- grid_val
#
#   predictions = predict(model, x_new, type = typ)
#
#   if(!is.factor(y)){
#     return(loss_function(truth, predictions))
#   } else if (class_loss & is.factor(y)){
#     return(rowMeans(loss_function(truth, predictions)))
#   } else {
#     return(as.numeric(truth != predictions))
#   }
# }
#
# get_var_loc <- function(model, ii, x_all, x, y, truth, nsim, m, typ,
#                         quantile_grid, class_loss, loss_function) { # depends on y
#
#   # Generate grid values for the selected variable
#   grid_vals <- generate_grid_values(x_all[, ii], nsim, quantile_grid)
#
#   # Apply over all grid values
#   new_m_mat <- sapply(grid_vals, calculate_new_m, model = model,
#                       x = x, y = y, ii = ii,
#                       truth = truth, typ = typ,
#                       class_loss = class_loss,
#                       loss_function = loss_function)
#
#   if(nrow(x) == 1){
#     mean(new_m_mat) - m
#   } else {
#     # Calculate the mean error for each observation and adjust by baseline errors
#     rowMeans(new_m_mat) - m
#   }
# }

