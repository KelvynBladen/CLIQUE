## code to prepare `DATASET` dataset goes here

lichen = randomForestVIP::lichen
lichen$LobaOreg = factor(lichen$LobaOreg)
lichen$StandAgeClass = as.numeric(lichen$StandAgeClass) - 1
lichen$ReserveStatus = as.numeric(lichen$ReserveStatus) - 1
usethis::use_data(lichen, overwrite = TRUE)

concrete = MAVE::Concrete
colnames(concrete)
colnames(concrete) = c("Cement", "Slag", "FlyAsh",
                   "Water", "Superplast", "CoarseAgg",
                   "FineAgg", "Age", "Strength")
usethis::use_data(concrete, overwrite = TRUE)

mnist = read.csv("inst/extdata/mnist_small.csv", header = T)
mnist$y = factor(mnist$y)

long_mnist = pivot_longer(mnist, cols = 1:64)
long_mnist$x1 = rep(1:8, times = 14376)
long_mnist$x2 = rep(rep(1:8, each = 8), times = 1797)

# colnames(mnist)[1:65] = c(paste0("x",long_mnist$x1,
#                                  "y",long_mnist$x2)[1:64], "z")

usethis::use_data(mnist, overwrite = TRUE)
usethis::use_data(long_mnist, overwrite = TRUE)
