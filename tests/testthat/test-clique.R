test_that("clique returns expected output structure", {

  set.seed(123)

  fit <- clique(
    x = iris[, 1:4],
    y = iris$Species,
    method = "rf",
    cores = 1,
    parallel = FALSE
  )

  expect_type(fit, "list")

  expect_named(
    fit,
    c("bestTune", "results", "finalModel", "local_imp")
  )

  expect_s3_class(fit$local_imp, "data.frame")

  expect_equal(
    dim(fit$local_imp),
    c(nrow(iris), 4)
  )

  expect_equal(
    colnames(fit$local_imp),
    colnames(iris[,1:4])
  )

})

test_that("formula and x-y interfaces agree", {

  set.seed(123)

  fit_xy <- clique(
    x = iris[,1:4],
    y = iris$Species,
    method = "rf",
    parallel = FALSE
  )

  set.seed(123)

  fit_formula <- clique(
    formula = Species ~ .,
    data = iris,
    method = "rf",
    parallel = FALSE
  )

  expect_equal(
    fit_xy$local_imp,
    fit_formula$local_imp
  )

})

test_that("clique requires x-y or formula-data", {

  expect_error(
    clique(),
    "either"
  )

})

test_that("clique catches incompatible dimensions", {

  expect_error(
    clique(
      x = iris[,1:4],
      y = iris$Species[-1]
    ),
    "incompatible"
  )

})

test_that("clique is reproducible", {

  fit1 <- clique(
    x = iris[,1:4],
    y = iris$Species,
    seed = 123,
    parallel = FALSE
  )

  fit2 <- clique(
    x = iris[,1:4],
    y = iris$Species,
    seed = 123,
    parallel = FALSE
  )

  expect_equal(
    fit1$local_imp,
    fit2$local_imp
  )

})

test_that("regression works", {

  fit <- clique(
    x = mtcars[, -1],
    y = mtcars$mpg,
    method = "rf",
    parallel = FALSE
  )

  expect_equal(
    dim(fit$local_imp),
    c(nrow(mtcars), ncol(mtcars)-1)
  )

})

test_that("class_loss mode runs successfully", {

  fit <- clique(
    x = iris[,1:4],
    y = iris$Species,
    method = "rf",
    class_loss = TRUE,
    parallel = FALSE
  )

  expect_s3_class(
    fit$local_imp,
    "data.frame"
  )

})

test_that("clique is reproducible", {

  fit1 <- clique(
    x = iris[,1:4],
    y = iris$Species,
    seed = 123,
    parallel = TRUE,
    cores = 2
  )

  fit2 <- clique(
    x = iris[,1:4],
    y = iris$Species,
    seed = 123,
    parallel = TRUE,
    cores = 2
  )

  expect_equal(
    fit1$local_imp,
    fit2$local_imp
  )

})
