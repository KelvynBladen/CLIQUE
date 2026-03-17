
library(latex2exp)
library(randomForest)
library(dplyr)

# concrete = read.csv("C:/Users/kelvy/Downloads/Concrete_Data.csv", header = T)
load("data/concrete.rda")
colnames(concrete) = c("Cement", "Slag", "FlyAsh",
                   "Water", "Superplast", "CoarseAgg",
                   "FineAgg", "Age", "Strength")

concrete$Strength
summary(concrete$Strength)
sum(concrete$Strength < 79.99)/length(concrete$Strength)
sum(concrete$Strength < 40.27)/length(concrete$Strength)

set.seed(123)

rc = randomForest(Strength ~ ., data = concrete, mtry = 5, importance = T)
rc
viv = vivid::vivi(concrete, rc, "Strength", importanceType = '%IncMSE')
vivid::viviHeatmap(viv)

vpp = vivid::pdpPairs(concrete, rc, "Strength", vars = c("Age", "Cement"),
                     probability = T)
vpp


g = randomForestVIP::ggvip(rc)
# to plot
ggg = g$accuracy_vip
pcc = pdp_compare(rc, var_vec = c("Age", "Cement"))
pdpc = pcc$full_num
# to plot
pd1 = pdpc +
  scale_y_continuous(limits = c(18, 54), breaks = c(18, 30, 42, 54))
pcc$Age
sort(unique(concrete$Age))
table(concrete$Age > 75)

pdpy = pcc$Age / pcc$Cement + plot_layout(axis_titles = "collect") &
  scale_y_continuous(limits = c(18, 54),
                     breaks = c(18, 30, 42, 54)) & theme_bw() &
  ylab("concrete Strength Predictions")
pdpy


ggg + pdpy

ggsave("conc_global.pdf", dpi = 1600, width = 6, height = 3.5)

pccAge = pcc$Age +
  labs(x = "Age (days)",
       y = "Concrete Strength Predictions") +
  scale_y_continuous(limits = c(18, 54),
                     breaks = c(18, 30, 42, 54)) +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10))

pccCement = pcc$Cement +
  labs(x = TeX("Cement (kg / $m ^ 3$)"),
       y = "Concrete Strength Predictions") +
  scale_y_continuous(limits = c(18, 54),
                     breaks = c(18, 30, 42, 54)) +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10))

ggg = ggg + theme(axis.title = element_text(size = 11),
            axis.text = element_text(size = 10))

ggg + pccAge + pccCement + plot_layout(axes = "collect")

ggsave("inst/plots/conc_global_wide.pdf", dpi = 1600, width = 8.4, height = 2.8)

library(rpart)
trec = rpart(Strength ~ ., data = concrete,
            control = rpart.control(minsplit = 2, cp = 0.02))
rpart.plot::rpart.plot(trec)

concrete[1,]
concrete[3,]

# Shap #

tictoc::tic()
set.seed(123)
rc = randomForest(Strength ~ ., data = concrete, mtry = 5, importance = T)
set.seed(1234)
rf_unified <- treeshap::randomForest.unify(rc, concrete)
rf_res <- treeshap::treeshap(rf_unified, concrete)
shap = as.data.frame(rf_res$shaps)
tictoc::toc()

# LIME #
library(lime)
set.seed(1234)
tictoc::tic()
rt = caret::train(concrete[,-9], concrete$Strength, method = "rf")
explanation <- lime(concrete[,-9], rt, bin_continuous = T, n_bins = 20,
                    use_density = T)
elim = explain(concrete[,-9], explanation, n_features = 8)
lime = elim %>% dplyr::select(case, feature, feature_weight) %>%
  pivot_wider(names_from = feature, values_from = c(feature_weight)) %>%
  dplyr::select(-case) %>% as.data.frame()
tictoc::toc()
##############

tuneGrid <- expand.grid(mtry = 5)

set.seed(1234)
tictoc::tic()
pm3 = local_perm_imp_cm(formula = Strength ~ ., data = concrete,
                        method = "rf", tuneGrid = tuneGrid, nsim = 25,
                        folds = 5)
tictoc::toc()

set.seed(1234)
tictoc::tic()
xm3 = local_grid_imp_cm(formula = Strength ~ ., data = concrete,
                        method = "rf", tuneGrid = tuneGrid, nsim = 25,
                        folds = 5, quantile_grid = T)
tictoc::toc()

source("inst/R_Scripts/clique.R")
set.seed(123)
tictoc::tic()
xg3 = clique(formula = Strength ~ ., data = concrete, parallel = T, cores = 5,
             seed = 123,
             method = "rf", tuneGrid = tuneGrid,
             nsim = 25, folds = 5, quantile_grid = TRUE, brier_score = F)
tictoc::toc()

library(mlbench)
library(featureImportance)
library(mlr)

# Create a regression task
task1 = mlr::makeRegrTask(data = concrete, target = "Strength")
learner <- mlr::makeLearner("regr.randomForest")
model <- mlr::train(learner, task1)

# Compute feature importance
ici <- featureImportance(model, data = concrete, target = "Strength",
                         n.feat.perm = 25,
                         local = TRUE)

iii = ici$importance |>
  pivot_wider(id_cols = c(row.id, n.feat.perm), names_from = features,
              values_from = mse) |>
  group_by(row.id) |>
  summarize(v1 = mean(v1), v2 = mean(v2), v3 = mean(v3)) |>
  dplyr::select(!row.id)
iii

ggplot(df, aes(x = v1, y = iii$v1)) +
  geom_point() + ylim(-0.05, 1)
ggplot(df, aes(x = v1, y = iii$v1, color = v3 > 0)) +
  geom_point() + ylim(-0.05, 1)


iii = ici$importance |>
  pivot_wider(id_cols = c(replace.id, n.feat.perm), names_from = features,
              values_from = mse) |>
  group_by(replace.id) |>
  summarize(Age = mean(Age), Cement = mean(Cement),
            Slag = mean(Slag), FlyAsh = mean(FlyAsh),
            Water = mean(Water), Superplast = mean(Superplast),
            CoarseAgg = mean(CoarseAgg), FineAgg = mean(FineAgg)) |>
  dplyr::select(!replace.id)


dage = data.frame(vals = concrete$Age,
                  CLIP = pm3$local_imp$Age,
                  CLIQUE_old = xm3$local_imp$Age,
                  CLIQUE = xg3$local_imp$Age,
                  SHAP = shap$Age,
                  LIME = lime$Age,
                  ICI = iii$Age
                  )

ggpairs(dage)
ggplot(dage, aes(vals, local)) + geom_point()

dcem = data.frame(Cement = concrete$Cement,
                  CLIP = pm3$local_imp$Cement,
                  CLIQUE_old = xm3$local_imp$Cement,
                  CLIQUE = xg3$local_imp$Cement,
                  SHAP = shap$Cement,
                  LIME = lime$Cement,
                  ICI = iii$Cement
                  )

ggpairs(dcem, mapping = aes(colour = concrete$Age > 75))

ggplot(dcem, aes(vals, local, colour = concrete$Age < 75)) + geom_point()
# good #
# to plot
Age = concrete$Age

summary(dcem)
pivot_longer()
dcem$Age = concrete$Age > 75
dcem = dcem |> select(c(1,4:8))
dcem1 = dcem |> mutate(CLIQUE = CLIQUE / max(abs(CLIQUE)),
               SHAP = SHAP / max(abs(SHAP)),
               LIME = LIME / max(abs(LIME)),
               ICI = ICI / max(abs(ICI)))
dcemp = dcem1 |> pivot_longer(cols = 2:5)
dcemp$name = factor(dcemp$name, levels = c("LIME", "SHAP", "ICI", "CLIQUE"))

summary(dcemp)

blank_data <- dcemp |> group_by(name) |>
  summarise(r = min(value) / (max(value) - min(value)),
            x = 1.5,
            y = -0.436 * (max(value) - min(value)))

blank_data1 <- dcemp |> group_by(name) |>
  summarise(r = max(value) / (max(value) - min(value)),
            x = 1.5,
            y = 1.04 * (max(value) - min(value)))
blank_data = rbind(blank_data, blank_data1)


ggplot(dcemp, aes(x = Age, y = value)) +
  # geom_violin() +
  geom_boxplot(coef = 5) +
  #geom_blank(data = blank_data, aes(x = x, y = y)) + # Add the blank data
  facet_wrap(~ name, nrow = 1, scales = "fixed") +
  scale_y_continuous(limits = c(-0.8, 1), breaks = -1:2/2) +
  expand_limits(y = 0) +
  labs(x = "Age > 75 days",
       y = "Scaled Cement Importance Values ")

ggsave("inst/plots/conc_cement_wide2.pdf", dpi = 1600, width = 8, height = 2.6)

gla = ggplot(dcem, aes(Age > 75, LIME)) +
  geom_boxplot(coef = 10) + ylab("Cement LIME") + ylim(-16, 21)
gs = ggplot(dcem, aes(Age > 75, SHAP)) +
  geom_boxplot(coef = 10) + ylab("Cement SHAP") + ylim(-16, 21)
gp = ggplot(dcem, aes(Age > 75, CLIP)) +
  geom_boxplot(coef = 10) + ylab("Cement CLIP")  +
  scale_y_continuous(limits = c(-670, 1200), breaks = seq(-600, 1200, 600))
gq = ggplot(dcem, aes(Age > 75, CLIQUE_old)) +
  geom_boxplot(coef = 10) + ylab("Cement CLIQUE") +
  scale_y_continuous(limits = c(-670, 1200), breaks = seq(-600, 1200, 600))
gq1 = ggplot(dcem, aes(Age > 75, CLIQUE)) +
  geom_boxplot(coef = 10) + ylab("Cement CLIQUE") +
  scale_y_continuous(limits = c(-20, 300), breaks = seq(0, 300, 100))
gi = ggplot(dcem, aes(Age > 75, ICI)) +
  geom_boxplot(coef = 10) + ylab("Cement ICI") +
  scale_y_continuous(limits = c(-20, 310), breaks = seq(0, 300, 100))
summary(dcem)
gla + gs + gp + gq +
  plot_layout(axis_titles = "collect", axes = "collect")

ggsave("conc_cement_by_age.pdf", dpi = 1600, width = 5.6, height = 4)

gb1 = ggplot(dcem, aes(Age > 75, CLIQUE)) +
  geom_boxplot(coef = 10) + ylab("Cement Local Importance")

gs1 = ggplot(dcem, aes(Age > 75, SHAP)) +
  geom_boxplot() + ylab("Cement SHAPs")
gb1 + gs1

design <- "
  A
  A
  A
  B
  B
"
wrap_plots(gb1, gs1, ncol = 1, design = design) +
  plot_layout(axis_titles = "collect")
ggsave("conc_cement.pdf", dpi = 1600, width = 7, height = 10)

db = dcem %>% group_by(Age) %>%
  summarise(clique_mean = mean(CLIQUE),
            clique_med = median(CLIQUE),
            shap_abs_mean = mean(abs(SHAP)),
            shap_abs_med = median(abs(SHAP)))

dbf = data.frame(t(db)[2:5,])
colnames(dbf) = c("Age < 75", "Age > 75")
dbf$Ratio = dbf$`Age < 75` / dbf$`Age > 75`
dbf = round(dbf, 2)
dbf


lmr1 = lm(Strength ~ ., concrete[concrete$Age < 75, ])
s1 = summary(lmr1)


lmr2 = lm(Strength ~ ., concrete[concrete$Age > 75, ])
s2 = summary(lmr2)

s1$coefficients[2,c(1,3)] / s2$coefficients[2,c(1,3)]

q1 = xm3$local_imp[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(val = val/max(abs(val)),
                          var = factor(var, levels = var))
gq1 = ggplot(q1, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-5, 1250), breaks = seq(0, 1200, 300)) +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for Obs. 1")

q1 = xg3$local_imp[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(val = val/max(abs(val)),
                          var = factor(var, levels = var))
gq1 = ggplot(q1, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  labs(x = "CLIQUE", y = "Obs. 1")

s1 = shap[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gs1 = ggplot(s1, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "SHAP", y = "Obs. 1")

l1 = lime[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gl1 = ggplot(l1, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "LIME", y = "Obs. 1")

i1 = iii[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gi1 = ggplot(i1, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  labs(x = "ICI", y = "Obs. 1")

obs = 4
q4 = xm3$local_imp[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(val = val/max(abs(val)),
                          var = factor(var, levels = var))
gq4 = ggplot(q4, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for Obs. 4")

q4 = xg3$local_imp[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(val = val/max(abs(val)),
                          var = factor(var, levels = var))
gq4 = ggplot(q4, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  labs(x = "CLIQUE", y = "Obs. 4")

# the variable/value most helpful for distinguishing strength is
# Water, superplast, and Age

s4 = shap[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gs4 = ggplot(s4, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "SHAP", y = "Obs. 4")

l4 = lime[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gl4 = ggplot(l4, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "LIME", y = "Obs. 4")

i4 = iii[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gi4 = ggplot(i4, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  labs(x = "ICI", y = "Obs. 4")


obs = 8
q8 = xm3$local_imp[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(val = val/max(abs(val)),
                          var = factor(var, levels = var))
gq8 = ggplot(q8, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for Obs. 8")

q8 = xg3$local_imp[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(val = val/max(abs(val)),
                          var = factor(var, levels = var))
gq8 = ggplot(q8, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  labs(x = "CLIQUE", y = "Obs. 8")

# the variable/value most helpful for distinguishing strength is
# Water, superplast, and Age

s8 = shap[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gs8 = ggplot(s8, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "SHAP", y = "Obs. 8")

l8 = lime[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gl8 = ggplot(l8, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "LIME", y = "Obs. 8")


i8 = iii[obs,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(val = val/max(abs(val)),
                               var = factor(var, levels = var))
gi8 = ggplot(i8, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.2)) +
  labs(x = "ICI", y = "Obs. 8")

# wrap_plots(gq1, gq4, gs1, gs4, gl1, gl4, ncol = 2)
#
# ggsave("conc_local_imp.pdf", dpi = 1600, width = 6, height = 6)

wrap_plots(gl1, gs1, gi1, gq1,
           gl4, gs4, gi4, gq4,
           gl8, gs8, gi8, gq8,
           ncol = 4) +
  plot_layout(axis_titles = "collect", axes = "collect")

ggsave("inst/plots/conc_local_imp_wide2.pdf", dpi = 1600,
       width = 8.8, height = 4.4)
