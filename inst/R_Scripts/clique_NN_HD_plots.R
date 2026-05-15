
library(fastDummies)
library(caret)
library(rpart)
library(tidyverse)
library(GGally)
library(treeshap)
library(lime)
library(patchwork)
library(tictoc)

term = "50_mar"

d1 = read.csv(paste0("inst/results_data/df_v1_HD_",term,".csv"))
d6 = read.csv(paste0("inst/results_data/df_v6_HD_",term,".csv"))

names(d1) = c("v1", "CLIQUE", "ICI", "SHAP", "LIME", "z")
d1$z = factor(d1$z)

names(d6) = c("v6", "CLIQUE", "ICI", "SHAP", "LIME", "z")
d6$z = factor(d6$z)


gq = ggplot(d1, aes(x = v1, y = CLIQUE, color = z)) +
  geom_point()
gs = ggplot(d1, aes(x = v1, y = SHAP, color = z)) +
  geom_point()
gl = ggplot(d1, aes(x = v1, y = LIME, color = z)) +
  geom_point()
gi = ggplot(d1, aes(x = v1, y = ICI, color = z)) +
  geom_point()

patch = gl + gs + gi + gq +
  plot_layout(guides = "collect", axes = "collect",
              axis_titles = "collect")
patch
# ggsave("corn2patch_v1.jpg", dpi = 1200, width = 4.4, height = 4.4)

d1n = d1 |> mutate(CLIQUE = CLIQUE / max(abs(CLIQUE)),
                   SHAP = SHAP / max(abs(SHAP)),
                   LIME = LIME / max(abs(LIME)),
                   ICI = ICI / max(abs(ICI)))


d1n = d1n |> pivot_longer(cols = 2:5)
d1n$name = factor(d1n$name, levels = c("LIME", "SHAP", "ICI", "CLIQUE"))


ggplot(d1n, aes(x = z, y = value)) +
  geom_boxplot(coef = 10000) +
  facet_wrap(~ name, nrow = 1, scales = "fixed") +
  scale_y_continuous(limits = c(-1, 1)) +
  expand_limits(y = 0) +
  labs(x = "z",
       y = "Scaled V1 Importance Values")


ggsave(paste0("inst/plots/HD_",term,"_v1_imps.pdf"), dpi = 1600,
       width = 8, height = 2.6)

d6n = d6 |> mutate(CLIQUE = CLIQUE / max(abs(CLIQUE)),
                   SHAP = SHAP / max(abs(SHAP)),
                   LIME = LIME / max(abs(LIME)),
                   ICI = ICI / max(abs(ICI)))


d6n = d6n |> pivot_longer(cols = 2:5)
d6n$name = factor(d6n$name, levels = c("LIME", "SHAP", "ICI", "CLIQUE"))


ggplot(d6n, aes(x = z, y = value)) +
  geom_boxplot(coef = 10000) +
  facet_wrap(~ name, nrow = 1, scales = "fixed") +
  scale_y_continuous(limits = c(-1, 1)) +
  expand_limits(y = 0) +
  labs(x = "z",
       y = "Scaled V6 Importance Values")

ggsave(paste0("inst/plots/HD_",term,"_v6_imps.pdf"), dpi = 1600,
       width = 8, height = 2.6)

d1n$var = "v1"
d6n$var = "v6"
colnames(d1n)[1] = "v"
colnames(d6n)[1] = "v"
dn = rbind(d1n, d6n)

ggplot(dn, aes(x = z, y = value)) +
  geom_boxplot(coef = 10000) +
  ggh4x::facet_grid2(var ~ name, scales = "fixed") +
  scale_y_continuous(limits = c(-1, 1)) +
  expand_limits(y = 0) +
  labs(x = "z",
       y = "Scaled Importance Values")

ggsave(paste0("inst/plots/HD_",term,".pdf"), dpi = 1600,
       width = 7.5, height = 3.3)
