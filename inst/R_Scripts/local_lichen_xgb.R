library(randomForest)
library(randomForestVIP)
library(caret)
library(xgboost)
library(patchwork)
# lichen = randomForestVIP::lichen
# lichen$LobaOreg = factor(lichen$LobaOreg)
# lichen$StandAgeClass = as.numeric(lichen$StandAgeClass) - 1
# lichen$ReserveStatus = as.numeric(lichen$ReserveStatus) - 1
#
# write.csv(lichen, file = "lichen.csv", row.names = F)
load("data/lichen.rda")
cor(lichen[,c("MinTempAve", "ACONIF","Elevation","AmbVapPressAve")])

# library(phateR)
# rfphate in python
# save phatel to csv, import in R

# slich = scale(lichen[,-1])
# phates <- phate(slich)
# summary(phates)
# ggplot(phates, aes(x = PHATE1, y = PHATE2, color = lichen$LobaOreg)) +
#   geom_point()

# phatel = read.csv("lichen_phate.csv", header = T)
# colnames(phatel) = c("PHATE1", "PHATE2")
# ggplot(phatel, aes(x = PHATE1, y = PHATE2, color = lichen$LobaOreg)) +
#   geom_point()
# ggplot(phatel, aes(x = PHATE1, y = PHATE2, color = lichen$MinTempAve > 450)) +
#   geom_point()
# ggplot(phatel, aes(x = PHATE1, y = PHATE2, color = lichen$MinTempDiff > -275)) +
#   geom_point()
#
# ggplot(phatel, aes(x = PHATE1, y = PHATE2,
#                    color = xm2$local_imp$MinTempAve)) +
#   geom_point()
#
# z = abs(xm2$local_imp$MinTempAve) > 0.12
# ggplot(phatel, aes(x = PHATE1, y = PHATE2,
#                    color = z)) +
#   geom_point()
# table(z, phatel$PHATE1 > -0.02)
#
# ggplot(phatel, aes(x = PHATE1, y = PHATE2,
#                    color = abs(xm2$local_imp$ACONIF) > 0.1)) +
#   geom_point()
#
# ggpairs(phatel)

#############################

ctrl = trainControl(method = "cv", number = 5)
n_samples = 30
xgboost::xgb.set.config(verbosity = 0)

set.seed(12345)
random_grid <- data.frame(
  nrounds = round(10^(runif(n_samples, log10(10), log10(1000)))),
  eta = 5*10^(runif(n_samples, log10(0.001), log10(0.1))),
  max_depth = round(4^(runif(n_samples, log(0.5, base = 4), log(16.5, base = 4)))),
  colsample_bytree = runif(n_samples, 0.5, 1),
  subsample = runif(n_samples, 0.5, 1),
  gamma = 0,
  min_child_weight = 1
)
tictoc::tic()
rt = caret::train(lichen[,-1], lichen$LobaOreg, method = "xgbTree",
                  trControl = ctrl, tuneGrid = random_grid, verbose = F)
tictoc::toc()
rt$results
rt$bestTune
rt$finalModel


X <- lichen[, -1]
y <- lichen$LobaOreg

pred_fun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

vi <- vip::vi_permute(
  object = rt,
  feature_names = colnames(X),
  train = X,
  target = y,
  metric = "Accuracy",   # or "Accuracy" for classification
  pred_wrapper = pred_fun,
  nsim = 50         # number of permutations
)


vv = vip::vip(rt, method = 'permute', target = y, metric = 'accuracy',
              pred_wrapper = pred_fun)

df = data.frame(var = vi$Variable, imp = vi$Importance, sd = vi$StDev)

df1 = df |> dplyr::arrange(desc(imp)) |> dplyr::top_n(10, wt = imp)
lev = df1 |> dplyr::pull(var)
df1$var = factor(df1$var, levels = rev(lev))
df1

ggy = ggplot(df1, aes(sqrt(imp), var)) + geom_point(size = 1.5) +
  scale_x_continuous(limits = c(0, 0.26), breaks = 0:5 * 0.05) +
  ggeasy::easy_remove_y_axis("title") +
  xlab("sqrt(MeanDecreaseAccuracy)") +
  theme(axis.title.x = element_text(size = 10))


pa = pdp::partial(rt, pred.var = "ACONIF", prob = T, which.class = 2L)
pt = pdp::partial(rt, pred.var = "MinTempAve", prob = T, which.class = 2L)
colnames(pa)[1] = "value"
colnames(pt)[1] = "value"


dp = data.frame(name = rep(c("ACONIF", "MinTempAve"), each = 51), rbind(pa, pt))
dp
pdpy = ggplot(dp, aes(x = value, y = yhat)) +
  geom_smooth(span = 0.25, se = F, color = "black", linewidth = 0.6) +
  # geom_line() +
  facet_wrap(~name, scales = "free_x") +
  ylab("LobaOreg Predicted Probability") +
  ggeasy::easy_remove_x_axis("title") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 10))

ggy + pdpy + plot_layout(widths = c(1,2))

ggsave("inst/plots/lichen_global_wide_xgb.pdf", dpi = 1600,
       width = 7.5, height = 2.5)


# Shap #
tictoc::tic()
set.seed(12345)
mod_unified <- treeshap::xgboost.unify(rt$finalModel, lichen)
mod_res <- treeshap::treeshap(mod_unified, lichen)
shapy = as.data.frame(mod_res$shaps)
tictoc::toc()

# LIME #
library(lime)
library(dplyr)
library(tidyr)
tictoc::tic()
explanation <- lime(lichen[,-1], rt, bin_continuous = T,
                    n_bins = 20, use_density = T)
elim = explain(lichen[,-1], explanation, n_features = 33, labels = "1")
lime = elim %>% dplyr::select(case, feature, feature_weight) %>%
  pivot_wider(names_from = feature, values_from = c(feature_weight)) %>%
  dplyr::select(-case) %>% as.data.frame()
tictoc::toc()

dim(shapy)
dim(lime)

#### ICI #####
library(mlbench)
library(featureImportance)
library(mlr)

# Create a task
task1 = mlr::makeClassifTask(data = lichen, target = "LobaOreg")
learner <- mlr::makeLearner(
  "classif.xgboost",
  predict.type = "prob",   # important for AUC / logloss / SHAP
  nrounds = 81,
  eta = 0.4245321,
  max_depth = 10,
  subsample = 0.7608888,
  colsample_bytree = 0.804738
)
model <- mlr::train(learner, task1)
model$task.desc
predict(model, newdata = lichen)

# Compute feature importance

ici <- featureImportance(model, data = lichen, target = "LobaOreg",
                         n.feat.perm = 25, measure = list(brier),
                         local = TRUE)

hist(ici$importance$brier)

iii = ici$importance |>
  pivot_wider(id_cols = c(replace.id, n.feat.perm), names_from = features,
              values_from = brier) |>
  group_by(replace.id) |>
  summarize(across(-n.feat.perm, mean, na.rm = TRUE)) |>
  dplyr::select(!replace.id)

ggplot(lichen, aes(x = MinTempAve, y = iii$MinTempAve)) +
  geom_point() + ylim(-0.05, 1)

ggplot(df, aes(x = v1, y = iii$v1, color = v3 > 0)) +
  geom_point() + ylim(-0.05, 1)
##############

source("R/clique.R")
tictoc::tic()
xg2 = clique(formula = LobaOreg ~ ., data = lichen, parallel = T, cores = 5,
             seed = 123, method = "xgbTree", tuneGrid = rt$bestTune,
             nsim = 25, folds = 5, quantile_grid = TRUE, brier_score = T)
tictoc::toc()

boxplot(xg2$local_imp$ACONIF ~ lichen$MinTempAve > 65)

# 62 to 107
# 219 to 241

MinTempAve = lichen$MinTempAve

dmtemp = data.frame(vals = lichen$MinTempAve,
                    #CLIP = pm2$local_imp$MinTempAve,
                    ICI = iii$MinTempAve,
                    CLIQUE = xg2$local_imp$MinTempAve,
                    SHAP = shapy$MinTempAve,
                    LIME = lime$MinTempAve)

ggplot(dmtemp, aes(vals, CLIQUE)) + geom_point()

dacon = data.frame(vals = lichen$ACONIF,
                   #CLIP = pm2$local_imp$ACONIF,
                   ICI = iii$ACONIF,
                   # CLIQUE = xm2$local_imp$ACONIF,
                   CLIQUE = xg2$local_imp$ACONIF,
                   SHAP = shapy$ACONIF,
                   LIME = lime$ACONIF)

dacon$MinTempAve = lichen$MinTempAve > 65

dacon1 = dacon |> mutate(CLIQUE = CLIQUE / max(abs(CLIQUE)),
                         SHAP = SHAP / max(abs(SHAP)),
                         LIME = LIME / max(abs(LIME)),
                         ICI = ICI / max(abs(ICI)))


dacon1 = dacon1 |> pivot_longer(cols = 2:5)
dacon1$name = factor(dacon1$name, levels = c("LIME", "SHAP", "ICI", "CLIQUE"))


ggplot(dacon1, aes(x = MinTempAve, y = value)) +
  geom_boxplot(coef = 10000) +
  #geom_blank(data = blank_data, aes(x = x, y = y)) + # Add the blank data
  facet_wrap(~ name, nrow = 1, scales = "fixed") +
  scale_y_continuous(limits = c(-1, 1)) +
  expand_limits(y = 0) +
  labs(x = "MinTempAve > 65 degrees",
       y = "Scaled ACONIF Importance Values ")

ggsave("inst/plots/lichen_aconif_wide_xgb.pdf", dpi = 1600,
       width = 8, height = 2.6)
