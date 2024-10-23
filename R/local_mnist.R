
mnist = read.csv("mnist_small.csv", header = T)
mnist$y = factor(mnist$y)


mnisto = read.csv("mnist_small.csv", header = T)
mnisto$y = factor(mnisto$y)

pmnist = pivot_longer(mnist, cols = 1:64)
pmnist$x1 = rep(1:8, times = 14376)
pmnist$x2 = rep(rep(1:8, each = 8), times = 1797)

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
  plot_layout(guides = "collect", axis_titles = "collect") & 
  scale_fill_continuous(limits = c(0, 16)) & 
  scale_x_continuous(breaks = 1:8) & ylab("y") & xlab("x")
ggsave("mnist_dat.pdf", dpi = 1600, width = 7.2, height = 5.4)

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

ggpairs(ptrain$embedding, mapping = aes(color=mnist$z))

dat = data.frame(ptrain$embedding, y = mnist$z)

gg = ggplot(ptrain$embedding, aes(PHATE1, PHATE2)) + 
  geom_point() + theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Digit")

gd = ggplot(ptrain$embedding, aes(PHATE1, PHATE2, 
                             colour = mnist$z)) + 
  geom_point() + theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Digit")

ggplot(ptrain$embedding, aes(PHATE1, PHATE2, 
                             colour = xm4$local_imp$x4y6)) + 
  geom_point() 


cz <- cut(xm4$local_imp$x4y6, 
          breaks = quantile(xm4$local_imp$x4y6, 0:3/3), 
          labels= c("Low", "Mid", "High"), include.lowest=TRUE)
g4 = ggplot(ptrain$embedding, aes(PHATE1, PHATE2, 
                             colour = cz)) + 
  geom_point() + scale_colour_manual(values = c("green", "grey90", "purple")) + 
  theme_bw() + ggeasy::easy_remove_gridlines() + 
  ggeasy::easy_add_legend_title("x4y6 CLIQUE")



ggplot(ptrain$embedding, aes(PHATE1, PHATE2, 
                             colour = xm4$local_imp$x3y6)) + 
  geom_point() 

cc <- cut(xm4$local_imp$x3y6, 
                  breaks = quantile(xm4$local_imp$x3y6, 0:3/3), 
                  labels=c("Low", "Mid", "High"), include.lowest=TRUE)
g3 = ggplot(ptrain$embedding, aes(PHATE1, PHATE2, 
                             colour = cc)) + 
  geom_point() + scale_colour_manual(values = c("red", "grey90", "blue")) + 
  theme_bw() + ggeasy::easy_remove_gridlines() + 
  ggeasy::easy_add_legend_title("x3y6 CLIQUE")

gg + gd + g4 + g3 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")


ggsave("mnist_phatek.pdf", dpi = 1600, width = 6.5, height = 5.5)

pdf = data.frame(ptrain$embedding)

gdd = ggplot(pdf, aes(PHATE1, PHATE2, 
                                  colour = mnist$z == 1)) + 
  geom_point() + theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Digit = 1")


gpp = ggplot(pdf, aes(PHATE1, PHATE2)) + 
  geom_point(pdf[!pdf$PHATE1 %in% pdf$PHATE1[v1],], mapping = aes(color = "0")) +
  geom_point(pdf[pdf$PHATE1 %in% pdf$PHATE1[v1[1]],], mapping = aes(color = "A")) +
  geom_point(pdf[pdf$PHATE1 %in% pdf$PHATE1[v1[2]],], mapping = aes(color = "B")) +
  scale_color_manual(values = c("grey90", "blue", "red")) +
  theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Sampled")
wrap_plots()
aa = gdd + gpp +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
aa
bb = g1ak + easy_remove_legend() + g1bk + easy_remove_legend() + 
  plot_layout(axis_titles = "collect", axes = "collect")
cc = ggt1 + ggb1 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
aa / bb / cc
easy_remove_axes()
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

gdd + ggt1 +  gpp + ggb1 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
ggsave("mnist_samples1.pdf", dpi = 1600, width = 6, height = 6)

d21 = xm4$local_imp[mnist$z == 1 & pdf$PHATE1 > 0.02,] %>% 
  dplyr::select(xh$var) %>%
  pivot_longer(everything(), names_to = "var", values_to = "val")
d21$var = factor(d21$var, levels = levels(xh$var))

d22 = xm4$local_imp[mnist$z == 1 & pdf$PHATE1 < 0.02,] %>% 
  dplyr::select(xl$var) %>%
  pivot_longer(everything(), names_to = "var", values_to = "val")
d22$var = factor(d22$var, levels = levels(xl$var))

xl$var
gg21 = ggplot(d21, aes(y = var, x = val)) + 
  geom_boxplot() + 
  xlab("CLIQUE 1A") + ggeasy::easy_remove_y_axis("title") +
  scale_x_continuous(limits = c(-0.01, 0.027), 
                     breaks = seq(-0.01, 0.02, by = 0.01))

gg22 = ggplot(d22, aes(y = var, x = val)) + 
  geom_boxplot() + 
  xlab("CLIQUE 1B") + ggeasy::easy_remove_y_axis("title") +
  scale_x_continuous(limits = c(-0.01, 0.027), 
                     breaks = seq(-0.01, 0.02, by = 0.01))

xh = xm4$local_imp[mnist$z == 1 & pdf$PHATE1 > 0.02,] %>% 
  sapply(., median) %>% #colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gh = ggplot(xh, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 1A") + xlim(0, 0.0152)

xl = xm4$local_imp[mnist$z == 1 & pdf$PHATE1 < 0.02,] %>% 
  sapply(., median) %>% #colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gl = ggplot(xl, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 1B") + xlim(0, 0.0152)
gh + gl
gdd = ggplot(pdf, aes(PHATE1, PHATE2, 
                      colour = mnist$z == 1)) + 
  geom_point() + theme_bw() + ggeasy::easy_remove_gridlines() +
  ggeasy::easy_add_legend_title("Digit = 1")

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

gg21 / gg22
gdd + gg21 + gpp1 + gg22 +
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")
ggsave("mnist_group1.pdf", dpi = 1600, width = 6, height = 6)

pmnist1a = pivot_longer(mnisto[mnist$z == 1 & pdf$PHATE1 > 0.02, ], cols = 1:64)
pmnist1b = pivot_longer(mnisto[mnist$z == 1 & pdf$PHATE1 < 0.02, ], cols = 1:64)

pmnist1a$x1 = rep(1:8, times = 216)
pmnist1a$x2 = rep(rep(1:8, each = 8), times = 27)

pmnist1b$x1 = rep(1:8, times = 1240)
pmnist1b$x2 = rep(rep(1:8, each = 8), times = 155)

ap1a = pmnist1a %>% group_by(x1, x2) %>% summarise(value = median(value))
ap1b = pmnist1b %>% group_by(x1, x2) %>% summarise(value = median(value))

g1a = ggplot(ap1a, aes(x = x1, y = x2, fill = value)) + 
  geom_tile() + scale_y_reverse(breaks = 1:8)
g1b = ggplot(ap1b, aes(x = x1, y = x2, fill = value)) + 
  geom_tile() + scale_y_reverse(breaks = 1:8)
g1a + g1b + plot_layout(axes = "collect", guides = "collect")


mnisto[mnist$z == 1 & pdf$PHATE1 > 0.02, ]
mnisto[mnist$z == 1 & pdf$PHATE1 < 0.02, ]
#!!!!!
pmnist1ak = pivot_longer(mnisto[1009, ], cols = 1:64)
pmnist1ak$x1 = rep(1:8, times = 8)
pmnist1ak$x2 = rep(1:8, each = 8)
ap1ak = pmnist1ak %>% group_by(x1, x2) %>% summarise(value = median(value))
g1ak = ggplot(ap1ak, aes(x = x1, y = x2, fill = value)) + 
  geom_tile() + scale_y_reverse(breaks = 1:8)

g1ak + g1bk

pmnist1bk = pivot_longer(mnisto[2, ], cols = 1:64)
pmnist1bk$x1 = rep(1:8, times = 8)
pmnist1bk$x2 = rep(1:8, each = 8)
ap1bk = pmnist1bk %>% group_by(x1, x2) %>% summarise(value = median(value))
g1bk = ggplot(ap1bk, aes(x = x1, y = x2, fill = value)) + 
  geom_tile() + scale_y_reverse(breaks = 1:8)

g1ak + g1bk + plot_layout(axes = "collect", guides = "collect")




msub = mnist[mnist$z %in% c(1, 9, 5), ]
loc_sub = xm4$local_imp[mnist$z %in% c(1, 9, 5), ]


phate_full = data.frame(ptrain$embedding)
phate_sub = phate_full[mnist$z %in% c(1, 9, 5), ]

ggplot(phate_sub, aes(PHATE1, PHATE2, 
                                  colour = msub$z)) + 
  geom_point() + theme_bw() +
  ggeasy::easy_add_legend_title("Digit")



t1 = phate_sub$PHATE1 > 0.02
b1 = phate_sub$PHATE1 < 0.02 & msub$z == 1

ct1 = phate_sub[t1,]
ct1

ggplot(phate_sub, aes(PHATE1, PHATE2, 
                      colour = PHATE1 == PHATE1[305])) + 
  geom_point() + theme_bw() +
  ggeasy::easy_add_legend_title("Digit")


cb1 = phate_sub[b1,]
cb1
phate_sub
ggplot() + 
  geom_point(data = phate_sub[phate_sub$PHATE1 != phate_sub$PHATE1[1],], 
             aes(PHATE1, PHATE2, colour = "")) + 
  geom_point(data = phate_sub[phate_sub$PHATE1 == phate_sub$PHATE1[1],], 
             aes(PHATE1, PHATE2, colour = "A")) + 
  theme_bw() + scale_color_manual(values = c("black", "red")) +
  ggeasy::easy_add_legend_title("Digit")


t5 = phate_sub$PHATE2 > -0.01 & phate_sub$PHATE1 < 0.005 & msub$z == 5
b5 = !(phate_sub$PHATE2 > -0.01 & phate_sub$PHATE1 < 0.005) & msub$z == 5

t5

ct5 = phate_sub[t5 & phate_sub$PHATE1 > 0 & phate_sub$PHATE1 < 0.002 &
            phate_sub$PHATE2 > -0.003 & phate_sub$PHATE2 < -0.002,]
ct5

cb5 = phate_sub[b5 & phate_sub$PHATE2 < -0.016 & 
                  phate_sub$PHATE1 > 0.01 & phate_sub$PHATE1 < 0.011 &
                  phate_sub$PHATE2 > -0.019,]
cb5

ggplot(phate_sub, aes(PHATE1, PHATE2, 
                      colour = PHATE1 == PHATE1[267])) + 
  geom_point() + theme_bw() +
  ggeasy::easy_add_legend_title("Digit")


ggplot(phate_sub, aes(PHATE1, PHATE2, 
                      colour = PHATE1 == PHATE1[438])) + 
  geom_point() + theme_bw() +
  ggeasy::easy_add_legend_title("Digit")


v1 = as.numeric(rownames(phate_sub[c(305, 1),]))
v5 = as.numeric(rownames(phate_sub[c(267, 438),]))
v1
v5
cz[v5]
cc[v1]


xt1 = xm4$local_imp[1009,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
ggt1 = ggplot(xt1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for A") + xlim(0, 0.016)

#!!!!!!!

xb1 = xm4$local_imp[2,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
ggb1 = ggplot(xb1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE for B") + xlim(0, 0.016)

ggt1 + ggb1

xt5 = xm4$local_imp[886,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
ggplot(xt5, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE") + xlim(0, 0.05)

xb5 = xm4$local_imp[1448,] %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
ggplot(xb5, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE") + xlim(0, 0.06)



xt1 = xm4$local_imp[as.numeric(rownames(phate_sub[t1, ])),] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gt1 = ggplot(xt1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE") + xlim(0, 0.03)

xb1 = xm4$local_imp[as.numeric(rownames(phate_sub[b1, ])),] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gb1 = ggplot(xb1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE") + xlim(0, 0.02)

gt1 + gb1

xt5 = xm4$local_imp[as.numeric(rownames(phate_sub[t5, ])),] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gt5 = ggplot(xt5, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE") + xlim(0, 0.05)

xb5 = xm4$local_imp[as.numeric(rownames(phate_sub[b5, ])),] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 8) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
gb5 = ggplot(xb5, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE") + xlim(0, 0.05)

gt1 + gb1
gt5 + gb5

x0 = xm4$local_imp[mnist$z == 0,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g0 = ggplot(x0, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 0") + xlim(0, 0.05)

x1 = xm4$local_imp[mnist$z == 1,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g1 = ggplot(x1, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 1") + xlim(0, 0.05)

x2 = xm4$local_imp[mnist$z == 2,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g2 = ggplot(x2, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 2") + xlim(0, 0.05)

x3 = xm4$local_imp[mnist$z == 3,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g3 = ggplot(x3, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 3") + xlim(0, 0.05)

x4 = xm4$local_imp[mnist$z == 4,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g4 = ggplot(x4, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 4") + xlim(0, 0.05)

x5 = xm4$local_imp[mnist$z == 5,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g5 = ggplot(x5, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 5") + xlim(0, 0.05)

x6 = xm4$local_imp[mnist$z == 6,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g6 = ggplot(x6, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 6") + xlim(0, 0.05)

x7 = xm4$local_imp[mnist$z == 7,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g7 = ggplot(x7, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 7") + xlim(0, 0.05)

x8 = xm4$local_imp[mnist$z == 8,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g8 = ggplot(x8, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 8") + xlim(0, 0.05)

x9 = xm4$local_imp[mnist$z == 9,] %>% colMeans() %>%
  t() %>% as.data.frame() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  slice_max(val, n = 5) %>% arrange(val) %>% 
  mutate(var = factor(var, levels = var))
g9 = ggplot(x9, aes(x = val, y = var)) + geom_point() + 
  ggeasy::easy_remove_y_axis("title") + xlab("CLIQUE 9") + xlim(0, 0.05)

g0 + g1 + g2 + g3 + g4 + g5 + g6 + g7 + g8 + g9
wrap_plots(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 5) + 
  plot_layout(axes = "collect")
ggsave("mnist_agg_local.pdf", dpi = 1600, width = 11, height = 8)

wrap_plots(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 2) + 
  plot_layout(axes = "collect")
ggsave("mnist_agg_local1.pdf", dpi = 1600, width = 4.8, height = 6.4)




zz = xm4$local_imp$x3y6 > 0.01
ggplot(ptrain$embedding, aes(PHATE1, PHATE2, 
                             colour = zz)) + 
  geom_point() 
table(zz, mnist$z)

zzz = xm4$local_imp$x3y6 < 0
ggplot(ptrain$embedding, aes(PHATE1, PHATE2, 
                             colour = zzz)) + 
  geom_point() 
table(zzz, mnist$z)

ggplot(ptrain$embedding, aes(PHATE1, PHATE2, colour = mnist$z)) + 
  geom_point() #+ facet_wrap(~mnist$x4y6 > 4.5)
ggplot(ptrain$embedding, aes(PHATE1, PHATE2, colour = mnist$x4y6 > 4.5)) + 
  geom_point()

ggplot(ptrain$embedding, aes(PHATE1, PHATE2, colour = mnist$z)) + 
  geom_point() + facet_wrap(~mnist$x3y6 > 7.5)
ggplot(ptrain$embedding, aes(PHATE1, PHATE2, colour = mnist$x3y6 > 7.5)) + 
  geom_point()
ggplot(ptrain$embedding, aes(PHATE1, PHATE2, colour = mnist$x3y6 > 7.5)) + 
  geom_point() + facet_wrap(~mnist$z)


ggplot(ptrain$embedding, aes(PHATE1, PHATE2, colour = mnist$z)) + 
  geom_point() + ggh4x::facet_grid2(rows = vars(mnist$x4y6 > 4.5), 
                   cols = vars(mnist$x3y6 > 7.5)) +
  theme(strip.placement = "outside") 

aa = factor(ifelse(mnist$x3y6 > 7.5, 
       ifelse(mnist$x4y6 > 4.5, "AA", "Aa"),
       ifelse(mnist$x4y6 > 4.5, "aA", "aa")))
ggpairs(ptrain$embedding, mapping = aes(color=aa))

ggplot(ptrain$embedding, aes(PHATE1, PHATE2, colour = aa)) + 
  geom_point() + facet_wrap(~mnist$z)


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


set.seed(421)
rff = randomForest(z ~ ., mtry = 16,
                   data = mnist,
                   importance = T)
rff

ggvip()
gf = randomForestVIP::ggvip(rff, num_var = 15)
# to plot
gfg = gf$accuracy_vip
gfg
library(pdp)

pcf = pdp_compare(x = rff)
pcf$full_num

p = pdp_multiclass(rff, mnist, "z", pred.var = "x4y6")
p$color #cutpoint about 3.5

p2 = pdp_multiclass(rff, mnist, "z", pred.var = "x3y6")
p2$color # about 7.5

p5 = pdp_multiclass(rff, mnist, "z", pred.var = "x6y3")
p5$color # 2.5

p6 = pdp_multiclass(rff, mnist, "z", pred.var = "x3y4")
p6$color # 6.5

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
rf_unified <- treeshap::randomForest.unify(rff, mnist)
rf_res <- treeshap::treeshap(rf_unified, mnist)
shap = as.data.frame(rf_res$shaps)
dim(shap)

# LIME #
# library(lime)
# rt = caret::train(mnist[,-65], mnist$z, method = "rf")
# explanation <- lime(mnist[,-65], rt, bin_continuous = T, n_bins = 20, use_density = T)
# elim = explain(mnist[,-65], explanation, n_features = 64, n_labels = 10)
# lime = elim %>% dplyr::select(case, feature, feature_weight) %>% 
#   pivot_wider(names_from = feature, values_from = c(feature_weight)) %>% 
#   dplyr::select(-case) %>% as.data.frame()

tuneGrid <- expand.grid(mtry = 16)
xm4 = local_grid_imp_cm(formula = z ~ ., data = mnist, 
                        method = "rf", tuneGrid = tuneGrid, nsim = 25,
                        folds = 5, quantile_grid = T)

d1 = data.frame(vals = mnist$x4y6, 
                CLIQUE = xm4$local_imp$x4y6)

ggplot(d1, aes(vals, CLIQUE)) + geom_point()
mnist$z
summary(d1$CLIQUE)
ggg = ggplot(d1, aes(mnist$z, CLIQUE)) + geom_boxplot(coef = 25) + 
  xlab("Digit") + 
  ylab("x4y6 CLIQUE") + 
  scale_y_continuous(limits = c(-0.032, 0.044), 
                     breaks = seq(-0.02, 0.04, by = 0.02))
ggg1 = ggplot(d1, aes(mnist$x3y6 > 7.5, CLIQUE)) + geom_boxplot(coef = 25) + 
  xlab("x3y6 > 7.5") + 
  ylab("x4y6 CLIQUE") + 
  scale_y_continuous(limits = c(-0.032, 0.044), 
                     breaks = seq(-0.02, 0.04, by = 0.02))
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
                CLIQUE = xm4$local_imp$x3y6)
summary(d2$CLIQUE)
ggplot(d2, aes(vals, CLIQUE)) + geom_point()
ggg2 = ggplot(d2, aes(mnist$z, CLIQUE)) + 
  #geom_violin(width = 2) + 
  geom_boxplot(coef = 25) + 
  xlab("Digit") + 
  ylab("x3y6 CLIQUE") +
  scale_y_continuous(limits = c(-0.0132, 0.022), 
                     breaks = seq(-0.01, 0.02, by = 0.01))
ggg3 = ggplot(d2, aes(mnist$x4y6 > 4.5, CLIQUE)) + geom_boxplot(coef = 25) +
  xlab("x4y6 > 4.5") + 
  ylab("x3y6 CLIQUE") +
  scale_y_continuous(limits = c(-0.016, 0.022), 
                     breaks = seq(-0.01, 0.02, by = 0.01))

ggg3
design <- "
  AABBB
  CCDDD
"

ggg1 + ggg + ggg3 + ggg2  +
  plot_layout(guides = "collect", axis_titles = "collect_y", design = design)
ggsave("mnist_box.pdf", dpi = 1600, width = 6, height = 6)

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
