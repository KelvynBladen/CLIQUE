
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

dage = data.frame(vals = concrete$Age,
                  CLIP = pm3$local_imp$Age,
                  CLIQUE = xm3$local_imp$Age,
                  SHAP = shap$Age#,
                  #LIME = lime$Age
                  )


ggpairs(dage)
ggplot(dage, aes(vals, local)) + geom_point()

dcem = data.frame(Cement = concrete$Cement,
                  CLIP = pm3$local_imp$Cement,
                  CLIQUE = xm3$local_imp$Cement,
                  SHAP = shap$Cement#,
                  #LIME = lime$Cement
                  )

ggpairs(dcem, mapping = aes(colour = concrete$Age > 75))

ggplot(dcem, aes(vals, local, colour = concrete$Age < 75)) + geom_point()
# good #
# to plot
Age = concrete$Age

summary(dcem$CLIQUE)

gla = ggplot(dcem, aes(Age > 75, LIME)) +
  geom_boxplot(coef = 10) + ylab("Cement LIME") + ylim(-16, 21)
gs = ggplot(dcem, aes(Age > 75, SHAP)) +
  geom_boxplot(coef = 10) + ylab("Cement SHAP") + ylim(-16, 21)
gp = ggplot(dcem, aes(Age > 75, CLIP)) +
  geom_boxplot(coef = 10) + ylab("Cement CLIP")  +
  scale_y_continuous(limits = c(-670, 1200), breaks = seq(-600, 1200, 600))
gq = ggplot(dcem, aes(Age > 75, CLIQUE)) +
  geom_boxplot(coef = 10) + ylab("Cement CLIQUE") +
  scale_y_continuous(limits = c(-670, 1200), breaks = seq(-600, 1200, 600))

gla + gs + gp + gq +
  plot_layout(axis_titles = "collect", axes = "collect")

ggsave("conc_cement_by_age.pdf", dpi = 1600, width = 5.6, height = 4)

gb1 = ggplot(dcem, aes(Age > 75, local)) +
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

db = dcem %>% group_by(concrete$Age > 75) %>% summarise(loc_mean = mean(CLIQUE),
                                               loc_med = median(CLIQUE),
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
  arrange(val) %>% mutate(var = factor(var, levels = var))
gq1 = ggplot(q1, aes(x = val, y = var)) + geom_point() +
  scale_x_continuous(limits = c(-5, 1250), breaks = seq(0, 1200, 300)) +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for Obs. 1")

s1 = shap[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(var = factor(var, levels = var))
gs1 = ggplot(s1, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("SHAP for Obs. 1")

l1 = lime[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(var = factor(var, levels = var))
gl1 = ggplot(l1, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("LIME for Obs. 1")

q3 = xm3$local_imp[3,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(var = factor(var, levels = var))
gq3 = ggplot(q3, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for Obs. 3")

# the variable/value most helpful for distinguishing strength is
# Water, superplast, and Age

s3 = shap[3,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(var = factor(var, levels = var))
gs3 = ggplot(s3, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("SHAP for Obs. 3")

l3 = lime[3,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(abs(val)) %>% mutate(var = factor(var, levels = var))
gl3 = ggplot(l3, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("LIME for Obs. 3")

wrap_plots(gq1, gq3, gs1, gs3, gl1, gl3, ncol = 2)

ggsave("conc_local_imp.pdf", dpi = 1600, width = 6, height = 6)

dwat = data.frame(vals = concrete$Water,
                  CLIQUE = xm3$local_imp$Water)
ggplot(dwat, aes(vals, CLIQUE)) + geom_point()
ggplot(dwat, aes(vals, CLIQUE, colour = concrete$Age < 50)) + geom_point()
ggplot(dwat, aes(concrete$Age < 50, CLIQUE)) +
  geom_boxplot()

