
library(randomForestVIP)
lichen = randomForestVIP::lichen
lichen$LobaOreg = factor(lichen$LobaOreg)
lichen$StandAgeClass = as.numeric(lichen$StandAgeClass) - 1
lichen$ReserveStatus = as.numeric(lichen$ReserveStatus) - 1


write.csv(lichen, file = "lichen.csv", row.names = F)

cor(lichen[,c("MinTempAve", "ACONIF","Elevation","AmbVapPressAve")])

library(phateR)
# rfphate in python
# save phatel to csv, import in R
slich = scale(lichen[,-1])
phates <- phate(slich)
summary(phates)
ggplot(phates, aes(x = PHATE1, y = PHATE2, color = lichen$LobaOreg)) + 
  geom_point()

phatel = read.csv("lichen_phate.csv", header = T)
colnames(phatel) = c("PHATE1", "PHATE2")
ggplot(phatel, aes(x = PHATE1, y = PHATE2, color = lichen$LobaOreg)) + 
  geom_point()
ggplot(phatel, aes(x = PHATE1, y = PHATE2, color = lichen$MinTempAve > 450)) + 
  geom_point()
ggplot(phatel, aes(x = PHATE1, y = PHATE2, color = lichen$MinTempDiff > -275)) + 
  geom_point()

ggplot(phatel, aes(x = PHATE1, y = PHATE2, 
                   color = xm2$local_imp$MinTempAve)) + 
  geom_point()

z = abs(xm2$local_imp$MinTempAve) > 0.12
ggplot(phatel, aes(x = PHATE1, y = PHATE2, 
                   color = z)) + 
  geom_point()
table(z, phatel$PHATE1 > -0.02)

ggplot(phatel, aes(x = PHATE1, y = PHATE2, 
                   color = abs(xm2$local_imp$ACONIF) > 0.1)) + 
  geom_point()

ggpairs(phatel)

set.seed(12345)
ry = randomForest(LobaOreg ~ ., data = lichen, mtry = 10, importance = T)
ry

gy = randomForestVIP::ggvip(ry, num_var = 10)
ggy = gy$accuracy_vip
#ggy = gy$both_vips
pc1 = pdp_compare(ry, var_vec = c("ACONIF", "MinTempAve"))
pdpy = pc1$full_num + scale_y_continuous(limits = c(.13, .39),
                                  breaks = c(.13, .26, .39)) +
  ylab("LobaOreg Predicted Probability")


pdpy = pc1$ACONIF / pc1$MinTempAve + plot_layout(axis_titles = "collect") & 
  scale_y_continuous(limits = c(.13, .39),
                     breaks = c(.13, .26, .39)) & theme_bw() &
  ylab("LobaOreg Predicted Probability")

hist(lichen$MinTempAve)


t1 <- wrap_elements(panel = textGrob('Global Variable Importances'))
t2 <- wrap_elements(panel = textGrob('Partial Dependence Plots'))

design <- "
  AB
"
ggy + pdpy

ggsave("lichen_global1.pdf", dpi = 1600, width = 8, height = 6)
ggsave("lichen_global1.pdf", dpi = 1600, width = 6, height = 3.5)

# tre = rpart(LobaOreg ~ ., data = lichen, 
#             control = rpart.control(minsplit = 2, cp = 0.05))
# rpart.plot::rpart.plot(tre)

# Shap #
tictoc::tic()
set.seed(12345)
ry = randomForest(LobaOreg ~ ., data = lichen, mtry = 10, importance = T)
rfy_unified <- treeshap::randomForest.unify(ry, lichen)
rfy_res <- treeshap::treeshap(rfy_unified, lichen)
shapy = as.data.frame(rfy_res$shaps)
tictoc::toc()

# LIME #
library(lime)
tictoc::tic()
rt = caret::train(lichen[,-1], lichen$LobaOreg, method = "rf")
explanation <- lime(lichen[,-1], rt, bin_continuous = T, n_bins = 20, use_density = T)
elim = explain(lichen[,-1], explanation, n_features = 33, labels = "1")
lime = elim %>% dplyr::select(case, feature, feature_weight) %>% 
  pivot_wider(names_from = feature, values_from = c(feature_weight)) %>% 
  dplyr::select(-case) %>% as.data.frame()
tictoc::toc()

dim(shapy)
dim(lime)

tuneGrid <- expand.grid(mtry = 10)
tictoc::tic()
pm2 = local_perm_imp_cm(formula = LobaOreg ~ ., data = lichen, 
                        method = "rf", tuneGrid = tuneGrid, nsim = 25,
                        folds = 5)
tictoc::toc()
tictoc::tic()
xm2 = local_grid_imp_cm(formula = LobaOreg ~ ., data = lichen, 
                        method = "rf", tuneGrid = tuneGrid, nsim = 25,
                        folds = 5, quantile_grid = T)
tictoc::toc()

pm = pdp::partial(ry, pred.var = c("MinTempAve"), prob = T, which.class = 2)
pm
# 62 to 107
# 219 to 241

MinTempAve = lichen$MinTempAve

dmtemp = data.frame(vals = lichen$MinTempAve, 
                   CLIP = pm2$local_imp$MinTempAve,
                   CLIQUE = xm2$local_imp$MinTempAve,
                   SHAP = shapy$MinTempAve,
                   LIME = lime$MinTempAve)

ggplot(dmtemp, aes(vals, CLIQUE)) + geom_point()
ggpairs(dmtemp, mapping = aes(colour = lichen$LobaOreg))

dacon = data.frame(vals = lichen$ACONIF, 
                   CLIP = pm2$local_imp$ACONIF,
                   CLIQUE = xm2$local_imp$ACONIF,
                   SHAP = shapy$ACONIF,
                   LIME = lime$ACONIF)
ggpairs(dacon, mapping = aes(colour = lichen$LobaOreg))
ggpairs(dacon, mapping = aes(colour = MinTempAve > 65))

ggplot(dacon, aes(vals, CLIQUE)) + geom_point()

ggplot(dacon, aes(vals, CLIQUE, colour = MinTempAve > 65)) + 
  geom_point()
ggplot(dacon, aes(vals, CLIQUE, colour = lichen$LobaOreg)) + 
  geom_point()
ggpairs(dacon, mapping = aes(colour = lichen$LobaOreg))
ggpairs(dacon, mapping = aes(colour = MinTempAve > 65))

gap = ggplot(dacon, aes(MinTempAve > 65, CLIP)) + 
  geom_boxplot(coef = 200) + ylab("ACONIF CLIP") + ylim(-.4, .4)

gag = ggplot(dacon, aes(MinTempAve > 65, CLIQUE)) + 
  geom_boxplot(coef = 200) + ylab("ACONIF CLIQUE") + ylim(-.4, .4)

gas = ggplot(dacon, aes(MinTempAve > 65, SHAP)) + 
  geom_boxplot(coef = 25) + ylab("ACONIF SHAP") + ylim(-.2, .23)

gal = ggplot(dacon, aes(MinTempAve > 65, LIME)) + 
  geom_boxplot(coef = 25) + ylab("ACONIF LIME") + ylim(-.2, .23)

gal + gas + gap + gag +
  plot_layout(axis_titles = "collect", axes = "collect")

#ggsave("lichen_aconif2.pdf", dpi = 1600, width = 6, height = 6)
ggsave("lichen_aconif2k.pdf", dpi = 1600, width = 5.6, height = 4)

ggplot(dacon, aes(MinTempAve > 75, CLIQUE)) + 
  geom_boxplot() + ylab("CLIQUE_ACONIF")

ggplot(dacon, aes(MinTempAve > 165, CLIQUE, colour = lichen$LobaOreg)) + 
  geom_boxplot()


dc = dacon %>% group_by(lichen$MinTempAve > 65) %>% 
  summarise(perm_mean = mean(CLIP),
            perm_abs_mean = mean(abs(CLIP)),
            loc_mean = mean(local),
            loc_abs_mean = mean(abs(local)),
            shap_abs_mean = mean(abs(SHAP)),
            lime_abs_mean = mean(abs(LIME)))

dcf = data.frame(t(dc)[2:7,])
colnames(dcf) = c("MinTempAve < 65", "MinTempAve > 65")
dcf$Ratio = dcf$`MinTempAve > 65` / dcf$`MinTempAve < 65`
dcf = round(dcf, 4)
dcf


lmy1 = glm(LobaOreg ~ ., lichen[lichen$MinTempAve < 65, ], 
           family = binomial())
sy1 = summary(lmy1)
sy1

lmy2 = glm(LobaOreg ~ ., lichen[lichen$MinTempAve > 65, ], 
           family = binomial())
sy2 = summary(lmy2)

sy2$coefficients
round(sy2$coefficients[30,c(1,3)] / sy1$coefficients[30,c(1,3)], 4)

lmy1 = glm(LobaOreg ~ ACONIF, lichen[lichen$MinTempAve < 65, ], 
           family = binomial())
sy1 = summary(lmy1)
sy1

lmy2 = glm(LobaOreg ~ ACONIF, lichen[lichen$MinTempAve > 65, ], 
           family = binomial())
sy2 = summary(lmy2)

round(sy2$coefficients[2,c(1,3)] / sy1$coefficients[2,c(1,3)], 4)


py1 = pm2$local_imp[1,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gp1 = ggplot(py1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIP for Obs. 1") + xlim(0, 0.05)

ly1 = xm2$local_imp[1,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gl1 = ggplot(ly1, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for Obs. 1")

sy1 = shapy[1,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(abs(val), n = 8) %>% arrange(abs(val)) %>% 
  mutate(var = factor(var, levels = var))
gs1 = ggplot(sy1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + 
  xlab("SHAP for Obs. 1") + xlim(-0.06, 0.06)

sl1 = lime[1,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(abs(val), n = 8) %>% arrange(abs(val)) %>% 
  mutate(var = factor(var, levels = var))
gsl1 = ggplot(sl1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + 
  xlab("LIME for Obs. 1") + xlim(-0.06, 0.1)

py2 = pm2$local_imp[2,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gp2 = ggplot(py2, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIP for Obs. 2") + xlim(0, 0.04)

ly2 = xm2$local_imp[2,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gl2 = ggplot(ly2, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for Obs. 2") + 
  scale_x_continuous(limits = c(0, 0.011), breaks = seq(0, 0.01, by = 0.005))

sy2 = shapy[2,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(abs(val), n = 8) %>% arrange(abs(val)) %>% 
  mutate(var = factor(var, levels = var))
gs2 = ggplot(sy2, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + 
  xlab("SHAP for Obs. 2") + xlim(-0.045, 0)

sl2 = lime[2,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(abs(val), n = 8) %>% arrange(abs(val)) %>% 
  mutate(var = factor(var, levels = var))
gsl2 = ggplot(sl2, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + 
  xlab("LIME for Obs. 2") + 
  scale_x_continuous(limits = c(-0.09, 0.03), breaks = seq(-0.09, 0.03, 0.03))

#gp1 + gp2 + gl1 + gl2 + gs1 + gs2 + gsl1 + gsl2
wrap_plots(gl1, gl2, gs1, gs2, gsl1, gsl2, ncol = 2)

ggsave("lichen_local_imp1.pdf", dpi = 1600, width = 6, height = 6)

lichen[xm2$local_imp$ACONIF > 0.45 & lichen$MinTempAve < 80, ]
lichen[xm2$local_imp$ACONIF > 0.15 & lichen$ACONIF > 250 & lichen$LobaOreg == 0, ]

dmoi = data.frame(vals = lichen$MoistIndexAve, 
                   CLIQUE = xm2$local_imp$MoistIndexAve)
ggplot(dmoi, aes(vals, CLIQUE)) + geom_point()
ggplot(dmoi, aes(vals, CLIQUE, colour = minTemp > 165)) + 
  geom_point()
ggplot(dmoi, aes(minTemp > 165, CLIQUE)) + 
  geom_boxplot()

delv = data.frame(vals = lichen$Elevation, 
                  CLIQUE = xm2$local_imp$Elevation)
ggplot(delv, aes(vals, CLIQUE)) + geom_point()
ggplot(delv, aes(vals, CLIQUE, colour = minTemp > 165)) + 
  geom_point()
ggplot(delv, aes(minTemp > 165, CLIQUE)) + 
  geom_boxplot()


dpa = data.frame(vals = lichen$PrecipAve, 
                  CLIQUE = xm2$local_imp$PrecipAve)
ggplot(dpa, aes(vals, CLIQUE)) + geom_point()
ggplot(dpa, aes(vals, CLIQUE, colour = minTemp > 165)) + 
  geom_point()
ggplot(dpa, aes(minTemp > 165, CLIQUE)) + 
  geom_boxplot()


x1 = xm2$local_imp[1,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(var = factor(var, levels = var))
ggplot(x1, aes(x = val, y = var)) + geom_point()

x2 = xm2$local_imp[2,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(var = factor(var, levels = var))
ggplot(x2, aes(x = val, y = var)) + geom_point()

x3 = xm2$local_imp[3,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(var = factor(var, levels = var))
ggplot(x3, aes(x = val, y = var)) + geom_point()

x4 = xm2$local_imp[4,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  arrange(val) %>% mutate(var = factor(var, levels = var))
ggplot(x4, aes(x = val, y = var)) + geom_point()
