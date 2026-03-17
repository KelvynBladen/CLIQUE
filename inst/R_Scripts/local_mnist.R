library(randomForest)
library(tidyverse)
library(patchwork)
library(randomForestVIP)
library(caret)
library(scales)
library(ggeasy)
source("inst/R_Scripts/pdp_multiclass.R")

tuneGrid = NULL
local_grid_imp_cm <- function(formula = medv ~ ., data = Boston,
                              method = "rf", tuneGrid = tuneGrid,
                              nsim = 25, folds = 5, quantile_grid = TRUE) {
  model_frame <- model.frame(formula, data = data)
  x <- model_frame[, -1]
  y <- model_frame[, 1]
  nc <- ncol(x)
  
  if (is(y, "factor")) {
    y = factor(y, labels = make.names(levels(y)))
  }
  
  set.seed(123)
  flds <- createFolds(y, k = folds, returnTrain = T)
  set.seed(123)
  flds0 <- createFolds(y, k = folds, list = F, returnTrain = F)
  
  model <- train(x, y,
                 # formula, data = data,
                 method = method,
                 tuneGrid = tuneGrid,
                 trControl = trainControl(method = "cv",
                                          index = flds,
                                          savePredictions = "final",
                                          classProbs = T))
  
  mods = list()
  for(k in 1:folds){
    mods[[k]] <- train(x[flds0 != k,], y[flds0 != k],
                       # formula, data = data,
                       method = method,
                       tuneGrid = model$finalModel$tuneValue,
                       trControl = trainControl(classProbs = T))
  }
  
  
  output = list()
  output$bestTune = model$bestTune
  output$results = model$results
  output$finalModel = model$finalModel
  
  model$pred = model$pred %>% arrange(rowIndex)
  
  x_loc <- x
  x_loc[, ] <- 0
  
  if (!is(y, "factor")) {
    p = model$pred$pred
    truth = model$pred$obs
    m <- (p - truth)^2
  } else {
    yd <- fastDummies::dummy_cols(as.data.frame(y))[, -1]
    p = model$pred %>% dplyr::select(levels(y))
    m <- rowMeans((yd - p)^2)
  }
  
  for (i in seq_len(nc)) {
    if(!inherits(x[,i], "numeric")){
      # perhaps do a permutation for low level factors!!
      # maybe convert factors to characters!!
      grid_vals = unique(x[,i])
    } else {
      grid_vals = ifelse(rep(quantile_grid, nsim),
                         quantile(x[,i], probs = seq(0, 1, length = nsim)), #quantile
                         seq(min(x[,i]), max(x[,i]), length = nsim)) #uniform
    }
    
    for (j in grid_vals) {
      x_new <- x
      x_new[, i] <- j
      
      if (!is(y, "factor")) {
        pr <- truth
        for(k in 1:folds){
          pr[flds0 == k] = predict(mods[[k]], x_new[flds0 == k,], type = "raw")
        }
        new_m <- (pr - truth)^2
      } else {
        pr <- yd
        for(k in 1:folds){
          pr[flds0 == k,] = predict(mods[[k]], x_new[flds0 == k,],
                                    type = "prob")
        }
        new_m <- rowMeans((yd - pr)^2)
      }
      x_loc[, i] <- x_loc[, i] + ((new_m - m) / nsim)
    }
  }
  output$local_imp = x_loc
  return(output)
}

# tuneGrid = NULL
# local_grid_imp_cm <- function(formula = medv ~ ., data = Boston,
#                               method = "rf", tuneGrid = tuneGrid,
#                               nsim = 25, folds = 5, quantile_grid = TRUE) {
#   model_frame <- model.frame(formula, data = data)
#   x <- model_frame[, -1]
#   y <- model_frame[, 1]
#   nc <- ncol(x)
#
#   if (is(y, "factor")) {
#     y = factor(y, labels = make.names(levels(y)))
#   }
#
#   set.seed(123)
#   flds <- createFolds(y, k = folds, returnTrain = T)
#   set.seed(123)
#   flds0 <- createFolds(y, k = folds, list = F, returnTrain = F)
#
#   model <- caret::train(x, y,
#                         # formula, data = data,
#                         method = method,
#                         tuneGrid = tuneGrid,
#                         trControl = trainControl(method = "cv",
#                                                  index = flds,
#                                                  savePredictions = "final",
#                                                  classProbs = T))
#
#   mods = list()
#   for(k in 1:folds){
#     mods[[k]] <- caret::train(x[flds0 != k,], y[flds0 != k],
#                               # formula, data = data,
#                               method = method,
#                               tuneGrid = model$finalModel$tuneValue,
#                               trControl = trainControl(classProbs = T))
#   }
#
#
#   output = list()
#   output$bestTune = model$bestTune
#   output$results = model$results
#   output$finalModel = model$finalModel
#
#   model$pred = model$pred %>% arrange(rowIndex)
#
#   x_loc <- x
#   x_loc[, ] <- 0
#
#   if (!is(y, "factor")) {
#     p = model$pred$pred
#     truth = model$pred$obs
#     m <- abs(p - truth)
#   } else {
#     yd <- fastDummies::dummy_cols(as.data.frame(y))[, -1]
#     p = model$pred %>% dplyr::select(levels(y))
#     m <- rowMeans(abs(yd - p))
#   }
#
#   for (i in seq_len(nc)) {
#     if(!inherits(x[,i], "numeric")){
#       # perhaps do a permutation for low level factors!!
#       # maybe convert factors to characters!!
#       grid_vals = unique(x[,i])
#     } else {
#       grid_vals = ifelse(rep(quantile_grid, nsim),
#                          quantile(x[,i], probs = seq(0, 1, length = nsim)), #quantile
#                          seq(min(x[,i]), max(x[,i]), length = nsim)) #uniform
#     }
#
#     for (j in grid_vals) {
#       x_new <- x
#       x_new[, i] <- j
#
#       if (!is(y, "factor")) {
#         pr <- truth
#         for(k in 1:folds){
#           pr[flds0 == k] = predict(mods[[k]], x_new[flds0 == k,], type = "raw")
#         }
#         new_m <- abs(pr - truth)
#       } else {
#         pr <- yd
#         for(k in 1:folds){
#           pr[flds0 == k,] = predict(mods[[k]], x_new[flds0 == k,],
#                                     type = "prob")
#         }
#         new_m <- rowMeans(abs(yd - pr))
#       }
#       x_loc[, i] <- x_loc[, i] + ((new_m - m) / nsim)
#     }
#   }
#   output$local_imp = x_loc
#   return(output)
# }

# mnist = read.csv("mnist_small.csv", header = T)
# mnist$y = factor(mnist$y)
#
mnisto = read.csv("inst/extdata/mnist_small.csv", header = T)
mnisto$y = factor(mnisto$y)

# pmnist = pivot_longer(mnist, cols = 1:64)
# pmnist$x1 = rep(1:8, times = 14376)
# pmnist$x2 = rep(rep(1:8, each = 8), times = 1797)

load("data/mnist.rda")
load("data/long_mnist.rda")

pmnist = long_mnist

colnames(mnist)[1:65] = c(paste0("x",pmnist$x1,"y",pmnist$x2)[1:64], "z")

ggplot(pmnist[1:64,], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[65:128,], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*2-1):(64*3),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*3-1):(64*4),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*4-1):(64*5),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*5-1):(64*6),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*6-1):(64*7),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*7-1):(64*8),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*8-1):(64*9),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()
ggplot(pmnist[(64*9-1):(64*10),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse()


ap = pmnist %>% group_by(y, x1, x2) %>% summarise(value = mean(value))

g0 = ggplot(ap[1:64,], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g0
ggplot(ap[65:128,], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
ggplot(ap[(64*2+1):(64*3),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g3 = ggplot(ap[(64*3+1):(64*4),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g3
ggplot(ap[(64*4+1):(64*5),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g5 = ggplot(ap[(64*5+1):(64*6),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g5
g6 = ggplot(ap[(64*6+1):(64*7),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g6
ggplot(ap[(64*7+1):(64*8),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g8 = ggplot(ap[(64*8+1):(64*9),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g8
g9 = ggplot(ap[(64*9+1):(64*10),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
g9

g0 + scale_fill_continuous(limits = c(0, 16)) +
  scale_x_continuous(breaks = 1:8)
g0 + g3 + g6 + g9 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect") &
  scale_fill_continuous(limits = c(0, 16)) &
  scale_x_continuous(breaks = 1:8) & ylab("y") & xlab("x") &
  ggeasy::easy_remove_legend()

ggsave("mnist_datx.pdf", dpi = 1600, width = 6.4, height = 6)

g5 + g6 + g8 + g9 +
  plot_layout(guides = "collect", axis_titles = "collect") &
  scale_fill_continuous(limits = c(0, 16)) &
  scale_x_continuous(breaks = 1:8) & ylab("y") & xlab("x")
ggsave("mnist_dat1.pdf", dpi = 1600, width = 7.2, height = 5.4)

app = pmnist %>% group_by(x1, x2, y) %>% summarise(mean = mean(value),
                                                   sd = sd(value))

set.seed(123)
library(phateR)
ptrain <- phate(mnist[, -65])
summary(ptrain)

write.csv(ptrain$embedding, "phate_mnist.csv")
ptrain = read.csv("inst/R_Scripts/phate_mnist.csv")

ggpairs(ptrain, mapping = aes(color=mnist$z))

dat = data.frame(ptrain, y = mnist$z)

gg = ggplot(dat, aes(PHATE1, PHATE2)) +
  geom_point(alpha = 0.6) + theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Digit")

gd = ggplot(ptrain, aes(PHATE1, PHATE2,
                        colour = mnist$z)) +
  geom_point(alpha = 0.6) + theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Digit")

######################################################
set.seed(421)
rff = randomForest(z ~ ., mtry = 16,
                   data = mnist,
                   importance = T)
rff

gf = randomForestVIP::ggvip(rff, num_var = 8)
# to plot
gfg = gf$accuracy_vip
gfg
library(pdp)

pcf = pdp_compare(x = rff)
pcf$full_num

# pdp_multiclass
p = pdp_multiclass(rff, mnist, "z", pred.var = "x4y6")
p$color #cutpoint about 3.5

p2 = pdp_multiclass(rff, mnist, "z", pred.var = "x3y6")
p2$color # about 7.5

p5 = pdp_multiclass(rff, mnist, "z", pred.var = "x6y3")
p5$color # 2.5

p6 = pdp_multiclass(rff, mnist, "z", pred.var = "x3y4")
p6$color # 6.5
################

p$color + p2$color + p5$color + p6$color  +
  plot_layout(guides = "collect", axis_titles = "collect") &
  ylim(0.035, 0.215)

pp = p$color / p2$color +
  plot_layout(guides = "collect", axis_titles = "collect") &
  ylim(0.04, 0.16) & ylab("Digit Predicted Probability")

gfg + pp

ggsave("mnist_global.pdf", dpi = 1600, width = 8, height = 6)
ggsave("mnist_global.pdf", dpi = 1600, width = 6, height = 4.5)


# Shap #
# rf_unified <- treeshap::randomForest.unify(rff, mnist)
# rf_res <- treeshap::treeshap(rf_unified, mnist)
# shap = as.data.frame(rf_res$shaps)
# dim(shap)

# LIME #
# library(lime)
# rt = caret::train(mnist[,-65], mnist$z, method = "rf")
# explanation <- lime(mnist[,-65], rt, bin_continuous = T, n_bins = 20, use_density = T)
# elim = explain(mnist[,-65], explanation, n_features = 64, n_labels = 10)
# lime = elim %>% dplyr::select(case, feature, feature_weight) %>%
#   pivot_wider(names_from = feature, values_from = c(feature_weight)) %>%
#   dplyr::select(-case) %>% as.data.frame()

tuneGrid <- expand.grid(mtry = 16)
tictoc::tic()
xm4 = local_grid_imp_cm(formula = z ~ ., data = mnist,
                        method = "rf", tuneGrid = tuneGrid, nsim = 25,
                        folds = 5, quantile_grid = T)
tictoc::toc()

source("inst/R_Scripts/clique.R")
tictoc::tic()
xg4 = clique(formula = z ~ ., data = mnist, parallel = T, cores = 5,
             seed = 123, method = "rf", tuneGrid = tuneGrid,
             nsim = 25, folds = 5, quantile_grid = TRUE, brier_score = T)
tictoc::toc()
###############################################
library(ggplot2)

plot(xm4$local_imp$x4y6, xg4$local_imp$x4y6)
summary(xg4$local_imp$x4y6)
g4g = ggplot(ptrain, aes(PHATE1, PHATE2,
                         colour = xg4$local_imp$x4y6)) +
  geom_point() +
  scale_colour_gradient2(low = muted("green"), mid = "white",
                         high = muted("purple"),
                         midpoint = mean(xg4$local_imp$x4y6), breaks = -2:4 / 50) +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("x4y6 CLIQUE")

cz <- cut(xm4$local_imp$x4y6,
          breaks = quantile(xm4$local_imp$x4y6, 0:3/3),
          labels= c("Low", "Mid", "High"), include.lowest=TRUE)
g4 = ggplot(ptrain, aes(PHATE1, PHATE2,
                        colour = cz)) +
  geom_point() + scale_colour_manual(values = c("green", "grey90", "purple")) +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("x4y6 CLIQUE")

g4
summary(xg4$local_imp$x3y6)
g3g = ggplot(ptrain, aes(PHATE1, PHATE2,
                         colour = xg4$local_imp$x3y6)) +
  geom_point() +
  scale_colour_gradient2(midpoint = mean(xg4$local_imp$x3y6)) +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("x3y6 CLIQUE")

cc <- cut(xm4$local_imp$x3y6,
          breaks = quantile(xm4$local_imp$x3y6, 0:3/3),
          labels=c("Low", "Mid", "High"), include.lowest=TRUE)
g3 = ggplot(ptrain, aes(PHATE1, PHATE2,
                        colour = cc)) +
  geom_point() + scale_colour_manual(values = c("red", "grey90", "blue")) +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("x3y6 CLIQUE")

gg + gd + g4 + g3 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")

ggsave("mnist_phatek.pdf", dpi = 1600, width = 6.5, height = 5.5)

gg + gd + g4g + g3g +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect") &
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11))

ggsave("inst/plots/mnist_phate_grad_wide1.pdf", dpi = 1600, width = 8.8, height = 6.8)

pdf = data.frame(ptrain)

gdd = ggplot(pdf, aes(PHATE1, PHATE2,
                      colour = mnist$z == 1)) +
  geom_point() + theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Digit = 1") +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))


phate_full = data.frame(ptrain)
phate_sub = phate_full[mnist$z %in% c(1, 9, 5), ]

v1 = as.numeric(rownames(phate_sub[c(305, 1),]))
v5 = as.numeric(rownames(phate_sub[c(267, 438),]))


gpp = ggplot(pdf, aes(PHATE1, PHATE2)) +
  geom_point(pdf[!pdf$PHATE1 %in% pdf$PHATE1[v1],], mapping = aes(color = "C")) +
  geom_point(pdf[pdf$PHATE1 %in% pdf$PHATE1[v1[1]],], mapping = aes(color = "A")) +
  geom_point(pdf[pdf$PHATE1 %in% pdf$PHATE1[v1[2]],], mapping = aes(color = "B")) +
  scale_color_manual(values = c(C = "grey90", A = "#2CA02C", B = "#9467BD"),
                     breaks = c("A", "B")) +
  annotate("text", x = 0.029, y = 0.032, label = "A") +
  annotate("text", x = -0.0043, y = 0.006, label = "B") +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_remove_legend()
gpp

#!!!!!
pmnist1ak = pivot_longer(mnisto[1009, ], cols = 1:64)
pmnist1ak$x1 = rep(1:8, times = 8)
pmnist1ak$x2 = rep(1:8, each = 8)
ap1ak = pmnist1ak %>% group_by(x1, x2) %>% summarise(value = median(value))
g1ak = ggplot(ap1ak, aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)

pmnist1bk = pivot_longer(mnisto[2, ], cols = 1:64)
pmnist1bk$x1 = rep(1:8, times = 8)
pmnist1bk$x2 = rep(1:8, each = 8)
ap1bk = pmnist1bk %>% group_by(x1, x2) %>% summarise(value = median(value))
g1bk = ggplot(ap1bk, aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)

g1ak + g1bk + plot_layout(axes = "collect", guides = "collect")

xt1 = xg4$local_imp[1009,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 6) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggt1 = ggplot(xt1, aes(x = val, y = var)) + geom_point(size = 2) +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for A") + xlim(0, 0.06)

xb1 = xg4$local_imp[2,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 6) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggb1 = ggplot(xb1, aes(x = val, y = var)) + geom_point(size = 2) +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for B") + xlim(0, 0.06)

ggt1 + ggb1

patchwork::wrap_plots(gdd, gpp,
                      g1ak + easy_remove_legend() + xlab("A") +
                        easy_remove_axes("y") +
                        easy_remove_axes("x", c("text","ticks")),
                      g1bk  + easy_remove_legend() + xlab("B") +
                        easy_remove_axes("y") +
                        easy_remove_axes("x", c("text","ticks")),
                      ggt1, ggb1, ncol = 2) +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
ggsave("mnist_samples_ex.pdf", dpi = 1600, width = 5.2, height = 6.2)

title_size = 14
text_size = 12
patchwork::wrap_plots(gdd + theme(axis.title = element_text(size = title_size),
                                  axis.text = element_text(size = text_size)),
                      g1ak + easy_remove_legend() + xlab("A") +
                        easy_remove_axes("y", "title") +
                        scale_x_continuous(breaks = 1:8) +
                        theme(axis.title = element_text(size = title_size),
                              axis.text = element_text(size = text_size)),
                      ggt1 + theme(axis.title = element_text(size = title_size),
                                   axis.text = element_text(size = text_size)),
                      gpp + theme(axis.title = element_text(size = title_size),
                                  axis.text = element_text(size = text_size)),
                      g1bk + easy_remove_legend() + xlab("B") +
                        easy_remove_axes("y", "title") +
                        scale_x_continuous(breaks = 1:8) +
                        theme(axis.title = element_text(size = title_size),
                              axis.text = element_text(size = text_size)),
                      ggb1 + theme(axis.title = element_text(size = title_size),
                                   axis.text = element_text(size = text_size)),
                      ncol = 3)
ggsave("inst/plots/mnist_samples_wide2.pdf", dpi = 1600, width = 9, height = 6)

gdd + ggt1 +  gpp + ggb1 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
ggsave("mnist_samples1.pdf", dpi = 1600, width = 6, height = 6)



pdf$D1 = ifelse(mnist$z == 1, "D1", "D2")
gdd = ggplot(pdf, aes(PHATE1, PHATE2, color = D1)) +
  geom_point(pdf[pdf$D1 == "D1",], mapping = aes(color = "1")) +
  geom_point(pdf[pdf$D1 == "D2",], mapping = aes(color = "2")) +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_remove_legend_title() +
  scale_color_manual(values = c("1" = "#00BFC4", "2" = "grey90"),
                     breaks = c("1")) +
  theme(legend.position = c(0.87, 0.1),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "white", color = "black"))

gdd

gpp1 = ggplot(pdf, aes(PHATE1, PHATE2)) +
  geom_point(pdf[!pdf$PHATE1 %in% pdf$PHATE1[mnist$z == 1],],
             mapping = aes(color = "0")) +
  geom_point(pdf[pdf$PHATE1 %in% pdf$PHATE1[mnist$z == 1 & pdf$PHATE1 > 0.02],],
             mapping = aes(color = "1A")) +
  geom_point(pdf[pdf$PHATE1 %in% pdf$PHATE1[mnist$z == 1 & pdf$PHATE1 < 0.02],],
             mapping = aes(color = "1B")) +
  scale_color_manual(values = c("grey90", "blue", "red")) +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Sampled")

gdd / gpp1 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
ggsave("mnist_group1.pdf", dpi = 1600, width = 6, height = 6)


x0 = xg4$local_imp[mnist$z == 0,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g0 = ggplot(x0, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 0") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x1 = xg4$local_imp[mnist$z == 1,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g1 = ggplot(x1, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 1") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x2 = xg4$local_imp[mnist$z == 2,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g2 = ggplot(x2, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 2") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x3 = xg4$local_imp[mnist$z == 3,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g3 = ggplot(x3, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 3") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x4 = xg4$local_imp[mnist$z == 4,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g4 = ggplot(x4, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 4") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x5 = xg4$local_imp[mnist$z == 5,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g5 = ggplot(x5, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 5") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x6 = xg4$local_imp[mnist$z == 6,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g6 = ggplot(x6, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 6") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x7 = xg4$local_imp[mnist$z == 7,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g7 = ggplot(x7, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 7") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x8 = xg4$local_imp[mnist$z == 8,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g8 = ggplot(x8, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 8") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

x9 = xg4$local_imp[mnist$z == 9,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
g9 = ggplot(x9, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 9") +
  scale_x_continuous(limits = c(0, 0.093), breaks = 0:3 * 0.03)

g0 + g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9
wrap_plots(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 5) +
  plot_layout(axes = "collect")
ggsave("inst/plots/mnist_agg_local1.pdf", dpi = 1600, width = 7.7, height = 3.3)
ggsave("inst/plots/mnist_agg_local.pdf", dpi = 1600, width = 11, height = 8)

wrap_plots(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 2) +
  plot_layout(axes = "collect")
ggsave("mnist_agg_local1.pdf", dpi = 1600, width = 4.8, height = 6.4)


# zz = xm4$local_imp$x3y6 > 0.01
# ggplot(ptrain, aes(PHATE1, PHATE2,
#                              colour = zz)) +
#   geom_point()
# table(zz, mnist$z)
#
# zzz = xm4$local_imp$x3y6 < 0
# ggplot(ptrain, aes(PHATE1, PHATE2,
#                              colour = zzz)) +
#   geom_point()
# table(zzz, mnist$z)
#
# ggplot(ptrain, aes(PHATE1, PHATE2, colour = mnist$z)) +
#   geom_point() #+ facet_wrap(~mnist$x4y6 > 4.5)
# ggplot(ptrain, aes(PHATE1, PHATE2, colour = mnist$x4y6 > 4.5)) +
#   geom_point()
#
# ggplot(ptrain, aes(PHATE1, PHATE2, colour = mnist$z)) +
#   geom_point() + facet_wrap(~mnist$x3y6 > 7.5)
# ggplot(ptrain, aes(PHATE1, PHATE2, colour = mnist$x3y6 > 7.5)) +
#   geom_point()
# ggplot(ptrain, aes(PHATE1, PHATE2, colour = mnist$x3y6 > 7.5)) +
#   geom_point() + facet_wrap(~mnist$z)
#
# ggplot(ptrain, aes(PHATE1, PHATE2, colour = mnist$z)) +
#   geom_point() + ggh4x::facet_grid2(rows = vars(mnist$x4y6 > 4.5),
#                    cols = vars(mnist$x3y6 > 7.5)) +
#   theme(strip.placement = "outside")
#
# aa = factor(ifelse(mnist$x3y6 > 7.5,
#        ifelse(mnist$x4y6 > 4.5, "AA", "Aa"),
#        ifelse(mnist$x4y6 > 4.5, "aA", "aa")))
# ggpairs(ptrain, mapping = aes(color=aa))
#
# ggplot(ptrain, aes(PHATE1, PHATE2, colour = aa)) +
#   geom_point() + facet_wrap(~mnist$z)


table(as.numeric(mnist$x3y6 > 7.5), mnist$x4y6 > 4.5, mnist$z)
# 0 = mnist$x3y6 > 7.5 & mnist$x4y6 < 4.5   Tf
# 1 = mnist$x4y6 > 4.5 & mnist$x3y6 < 7.5   fT
# 2 = mnist$x4y6 > 4.5                      fT
# 3 = mnist$x4y6 < 4.5 & mnist$x3y6 < 7.5   FF
# 4 = mnist$x4y6 > 4.5                      tT
# 5 = mnist$x3y6 < 7.5                      Ff
# 6 = mnist$x3y6 > 7.5                      Tt
# 7 = mnist$x4y6 > 4.5 & mnist$x3y6 < 7.5   fT
# 8 = mnist$x4y6 > 4.5 & mnist$x3y6 > 7.5   tt
# 9 = mnist$x3y6 < 7.5 & mnist$x4y6 < 4.5   FF


# 3 = mnist$x4y6 < 4.5 & mnist$x3y6 < 7.5   FF
# 5 = mnist$x3y6 < 7.5                      Ff
# 9 = mnist$x3y6 < 7.5 & mnist$x4y6 < 4.5   FF

# 1 = mnist$x4y6 > 4.5 & mnist$x3y6 < 7.5   fT
# 2 = mnist$x4y6 > 4.5                      fT
# 7 = mnist$x4y6 > 4.5 & mnist$x3y6 < 7.5   fT

# 0 = mnist$x3y6 > 7.5 & mnist$x4y6 < 4.5   Tf

# 4 = mnist$x4y6 > 4.5                      tT
# 6 = mnist$x3y6 > 7.5                      Tt
# 8 = mnist$x4y6 > 4.5 & mnist$x3y6 > 7.5   tt


d1 = data.frame(vals = mnist$x4y6,
                CLIQUE = xg4$local_imp$x4y6)

ggplot(d1, aes(vals, CLIQUE)) + geom_point()
mnist$z
summary(d1$CLIQUE)
ggg = ggplot(d1, aes(mnist$z, CLIQUE)) + geom_boxplot(coef = 25) +
  xlab("Digit") +
  ylab("x4y6 CLIQUE") +
  # scale_y_continuous(limits = c(-0.032, 0.044),
  #                    breaks = seq(-0.02, 0.04, by = 0.02)) +
  scale_y_continuous(limits = c(-0.044, 0.08),
                     breaks = seq(-0.04, 0.08, by = 0.04))
ggg1 = ggplot(d1, aes(mnist$x3y6 > 7.5, CLIQUE)) + geom_boxplot(coef = 25) +
  xlab("x3y6 > 7.5") +
  ylab("x4y6 CLIQUE") +
  # scale_y_continuous(limits = c(-0.032, 0.044),
  #                    breaks = seq(-0.02, 0.04, by = 0.02)) +
  scale_y_continuous(limits = c(-0.044, 0.08),
                     breaks = seq(-0.04, 0.08, by = 0.04))
ggg + ggg1
#ggplot(d1, aes(mnist$x3y6 > 7.5, local, colour = mnist$z)) + geom_boxplot()

dg = d1 %>% group_by(mnist$x3y6 > 7.5) %>% summarise(mean = mean(CLIQUE),
                                                     median = median(CLIQUE))
dgf = data.frame(t(dg)[2:3,])
colnames(dgf) = c("x3y6 < 7.5", "x3y6 > 7.5")
dgf$Ratio = dgf$`x3y6 < 7.5` / dgf$`x3y6 > 7.5`
dcf = round(dgf, 4)
dcf

d2 = data.frame(vals = mnist$x3y6,
                CLIQUE = xg4$local_imp$x3y6)
summary(d2$CLIQUE)
ggplot(d2, aes(vals, CLIQUE)) + geom_point()
ggg2 = ggplot(d2, aes(mnist$z, CLIQUE)) +
  #geom_violin(width = 2) +
  geom_boxplot(coef = 25) +
  xlab("Digit") +
  ylab("x3y6 CLIQUE") +
  # scale_y_continuous(limits = c(-0.0132, 0.022),
  #                    breaks = seq(-0.01, 0.02, by = 0.01)) +
  scale_y_continuous(limits = c(-0.03, 0.06),
                     breaks = seq(-0.03, 0.06, by = 0.03))
ggg3 = ggplot(d2, aes(mnist$x4y6 > 4.5, CLIQUE)) + geom_boxplot(coef = 25) +
  xlab("x4y6 > 4.5") +
  ylab("x3y6 CLIQUE") +
  # scale_y_continuous(limits = c(-0.0132, 0.022),
  #                    breaks = seq(-0.01, 0.02, by = 0.01)) +
  scale_y_continuous(limits = c(-0.03, 0.06),
                     breaks = seq(-0.03, 0.06, by = 0.03))

ggg3
design <- "
  AABBB
  CCDDD
"

ggg1 + ggg + ggg3 + ggg2  +
  plot_layout(guides = "collect", axes = "collect",
              axis_titles = "collect_y", widths = c(2,4))
ggsave("inst/plots/mnist_box_wide1.pdf", dpi = 1600, width = 5, height = 3.5)

ggg1 + ggg3 +
  plot_layout(guides = "collect", axis_titles = "collect")
ggsave("mnist_boxvk.pdf", dpi = 1600, width = 5, height = 2.5)


ggg / ggg2 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
ggsave("mnist_box_digk.pdf", dpi = 1600, width = 4.5, height = 4.5)

ggplot(mnist, aes(z, x4y6)) + geom_boxplot(coef = 100) +
  xlab("Digit") +
  ylab("x4y6 Value")
table(mnist$z, mnist$x4y6 > 0)
table(mnist$z)
(143 + 152) / (143 + 152 + 40 + 28)
103 / (103 + 75)
# ggplot(d2, aes(mnist$x6y3 > 4.5, local)) + geom_boxplot()
# ggplot(d2, aes(mnist$x4y6 > 4.5, local, colour = mnist$z)) + geom_boxplot()


dg2 = d2 %>% group_by(mnist$x4y6 > 4.5) %>% summarise(mean = mean(local),
                                                      median = median(local))
dgf2 = data.frame(t(dg2)[2:3,])
colnames(dgf2) = c("x4y6 < 4.5", "x4y6 > 4.5")
dgf2$Ratio = dgf2$`x4y6 < 4.5` / dgf2$`x4y6 > 4.5`
dgf2 = round(dgf2, 4)
dgf2


# d3 = data.frame(vals = mnist$x6y3,
#                 local = xm4$local_imp$x6y3)
#
# ggplot(d3, aes(vals, local)) + geom_point()
# ggplot(d3, aes(mnist$z, local)) + geom_boxplot()
# ggplot(d3, aes(mnist$x3y6 > 7.5, local)) + geom_boxplot()
# ggplot(d3, aes(mnist$x4y6 > 4.5, local)) + geom_boxplot()


loc = xm4$local_imp
loc$obs = rownames(loc)
loc$z = mnist$z

ploc = pivot_longer(loc, cols = 1:64)
ploc$x1 = rep(1:8, times = 14376)
ploc$x2 = rep(rep(1:8, each = 8), times = 1797)

lap = ploc %>% group_by(z, x1, x2) %>% summarise(value = mean(value))

p0 = ggplot(lap[1:64,], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)

ggplot(lap[(64*1+1):(64*2),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
ggplot(lap[(64*2+1):(64*3),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
p3 = ggplot(lap[(64*3+1):(64*4),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
ggplot(lap[(64*4+1):(64*5),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
ggplot(lap[(64*5+1):(64*6),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
p6 = ggplot(lap[(64*6+1):(64*7),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
ggplot(lap[(64*7+1):(64*8),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
ggplot(lap[(64*8+1):(64*9),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)
p9 = ggplot(lap[(64*9+1):(64*10),], aes(x = x1, y = x2, fill = value)) +
  geom_tile() + scale_y_reverse(breaks = 1:8)

p0 + p3 + p6 + p9 +
  plot_layout(guides = "collect", axis_titles = "collect") &
  scale_fill_continuous(limits = c(-0.001, 0.046)) &
  scale_x_continuous(breaks = 1:8) & ylab("y") & xlab("x")
ggsave("mnist_dat.pdf", dpi = 1600, width = 7.2, height = 5.4)

ll0 = xm4$local_imp[1,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll0, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 0")

ll1 = xm4$local_imp[2,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll1, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 1")

ll2 = xm4$local_imp[3,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll2, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 2")

ll3 = xm4$local_imp[4,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll3, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 3")

ll4 = xm4$local_imp[5,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll4, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 4")


ll5 = xm4$local_imp[6,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll5, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 5")

ll6 = xm4$local_imp[7,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll6, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 6")

ll7 = xm4$local_imp[8,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll7, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 7")

ll8 = xm4$local_imp[9,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll8, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 8")

ll9 = xm4$local_imp[10,] %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>%
  mutate(var = factor(var, levels = var))
ggplot(ll9, aes(x = val, y = var)) + geom_point() +
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE Importance for Obs. 9")