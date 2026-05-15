
library(dplyr)
library(ggplot2)
library(lime)

n <- 400
set.seed(444)
v1 = runif(n, -1, 1)
v2 = runif(n, -1, 1)
v3 = runif(n, -1, 1)

y = ifelse(v1 < -1/3, 0,
           ifelse(v2 > -1/3, 1, 0))
y = as.factor(y)
df <- data.frame(v1, v2, v3, y)

sup = factor(ifelse(v2 < -1/3, 0, ifelse(v1 < -1/3, 2, 1)))

tuneGrid <- expand.grid(mtry = 2)
ctrl <- trainControl(method = "none",
                     allowParallel = FALSE
)
rt = caret::train(df[, 1:3], factor(df[[4]]), method = "rf",
                  tuneGrid = tuneGrid, trControl = ctrl)

med_dist <- median(dist(df[, 1:3]))
k_vals <- med_dist * c(0.1, 0.25, 0.5, 0.75, 1, 1.5, 2)
k_vals <- med_dist * c(0.15, 0.4, 0.8, 1.3, 2)
k_vals <- med_dist * c(0.2, 0.4, 0.8, 1.6, 3.2)

0.75 * sqrt(3)

settings <- expand.grid(
    use_density = c(TRUE, FALSE),
    bin_continuous = TRUE,
    n_bins = c(10, 20)
  )

results <- vector(mode = "list", length = 20)
idx <- 1

for (j in 1:nrow(settings)) {
  tictoc::tic()
  explainer <- lime(
    df[, 1:3], rt,
    use_density = settings$use_density[j],
    bin_continuous = settings$bin_continuous[j],
    n_bins = settings$n_bins[j]
  )
  tictoc::toc()

  for (k in k_vals) {
    tictoc::tic()
    elim <- explain(
      df[, 1:3], explainer,
      labels = "1",
      n_features = 3,
      dist_fun = "euclidean",
      kernel_width = k
    )

    # extract v1 contribution
    dtmp <- elim %>%
      filter(feature == "v1") %>%
      mutate(
        kernel_width = k,
        use_density = settings$use_density[j],
        bin_continuous = settings$bin_continuous[j],
        n_bins = settings$n_bins[j]
      )

    results[[idx]] <- dtmp

    tictoc::toc()
    print(idx)
    idx <- idx + 1
  }
}

d1 <- bind_rows(results)

lime = d1 %>%
  dplyr::select(case, feature, feature_weight,
                kernel_width, use_density, bin_continuous, n_bins) %>%
  pivot_wider(names_from = feature, values_from = c(feature_weight)) %>%
  as.data.frame()


library(arrow)
#write_parquet(lime, "inst/results_data/lime_res.parquet")
#lime <- read_parquet("inst/results_data/lime_res.parquet")

colnames(lime)[5] = "lime"

df$case <- 1:nrow(df)

lime$case = rep(1:400, times = 20)

d2 <- lime %>%
  left_join(df, by = c("case"))

ggplot(d2, aes(x = v1, y = lime, color = v2 > -1/3)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_grid(n_bins + use_density ~ round(kernel_width, 2)) +
  theme_bw(base_size = 12)

colnames(d2)[c(1,4)] = c("k_width", "bins")
d2$k_width = round(d2$k_width, 2)

ggplot(d2 |> filter(use_density), aes(x = v1, y = lime, color = v2 > -1/3)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_grid(bins ~ k_width, labeller = "label_both") +
  scale_x_continuous(limits = c(-1.05, 1.05)) +
  #scale_y_continuous(limits = c(-1.01, 1.01)) +
  ylab("LIME") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 10))

ggsave("inst/plots/lime_SA_NEW1.pdf", dpi = 1600,
       width = 9.3, height = 3)

region = rep(sup, times = 20)

ggplot(d2, aes(x = v1, y = lime, color = region)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_grid(use_density + n_bins ~ round(kernel_width, 2)) +
  theme_bw(base_size = 12)

ggplot(d2 |> filter(kernel_width < 0.3),
       aes(x = v1, y = lime, color = v2 > -1/3)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_grid(use_density ~ n_bins, labeller = "label_both") +
  theme_bw(base_size = 12)

region = rep(sup, times = 4)

ggplot(d2 |> filter(kernel_width < 0.3),
       aes(x = v1, y = lime, color = region)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_grid(use_density ~ n_bins, labeller = "label_both") +
  theme_bw(base_size = 12)
