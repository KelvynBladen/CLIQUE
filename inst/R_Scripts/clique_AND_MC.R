library(fastDummies)
library(caret)
library(rpart)
library(tidyverse)
library(GGally)
library(treeshap)
library(lime)
library(mlbench)
library(featureImportance)
library(mlr)
tuneGrid <- expand.grid(mtry = 2)

source("R/clique.R")

library(doParallel)
stopImplicitCluster()
registerDoSEQ()
ctrl <- trainControl(method = "none",
                     allowParallel = FALSE
)

n <- 400 # increase higher 1000

# mae = data.frame(mat.or.vec(50, 5))
# colnames(mae) = c("v1", "LIME", "SHAP", "ICI", "CLIQUE")
mae = read.csv("mae_and.csv")

for(i in 1:50){
  tictoc::tic()
  set.seed(i)
  v1 = runif(n, -1, 1)
  v2 = runif(n, -1, 1)
  v3 = runif(n, -1, 1)
  y = ifelse(v1 < -1/3, 0, 
             ifelse(v2 > -1/3, 1, 0))
  df <- data.frame(v1, v2, v3, y)
  
  # Shap #
  mod = randomForest::randomForest(factor(y) ~ ., df, mtry = 2)
  rf_unified <- treeshap::randomForest.unify(mod, df)
  rf_res <- treeshap(rf_unified, df, interactions = T)
  shap = as.data.frame(rf_res$shaps)
  print("shap")
  
  # LIME #
  rt = caret::train(df[, 1:3], factor(df[[4]]), method = "rf",
                    tuneGrid = tuneGrid, trControl = ctrl)
  explanation <- lime(df[, 1:3], rt, bin_continuous = T, n_bins = 20,
                      use_density = T)
  elim = explain(df[, 1:3], explanation, labels = "1", n_features = 3)
  
  lime = elim %>% dplyr::select(case, feature, feature_weight) %>%
    pivot_wider(names_from = feature, values_from = c(feature_weight)) %>%
    dplyr::select(-case) %>% as.data.frame()
  print("lime")
  
  # ICI #
  task1 = mlr::makeRegrTask(data = df, target = "y")
  learner <- mlr::makeLearner("regr.randomForest")
  model <- mlr::train(learner, task1)
  
  # Compute feature importance
  ici <- featureImportance(model, data = df, target = "y",
                           n.feat.perm = 25,
                           local = TRUE)
  
  iii = ici$importance |>
    pivot_wider(id_cols = c(replace.id, n.feat.perm), names_from = features,
                values_from = mse) |>
    group_by(replace.id) |>
    summarize(v1 = mean(v1)) |>
    dplyr::select(!replace.id)
  print("ici")
  
  # CLIQUE #
  xg = clique(formula = factor(y) ~ ., data = df, parallel = T, cores = 5, 
              seed = 123, method = "rf", tuneGrid = tuneGrid, brier_score = T,
              nsim = 25, folds = 5, quantile_grid = TRUE)
  print("clique")
  
  # Combine
  d1 = data.frame(v1 = df[,1],
                  LIME = lime$v1,
                  SHAP = shap$v1,
                  ICI = iii$v1,
                  CLIQUE = xg$local_imp$v1
  )
  
  sc = apply(d1, 2, FUN = function(x){max(abs(x))})
  vr = d1 |> filter(v2 < -1/3)
  
  mae[i,] = colMeans(abs(vr / sc))
  write.csv(mae, "mae_and.csv", row.names = F)
  print(i)
  tictoc::toc()
}
