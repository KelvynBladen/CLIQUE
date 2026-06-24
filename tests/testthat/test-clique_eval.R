test_that("clique_eval works on training data", {

  model <- caret::train(
    x = iris[,1:4],
    y = iris$Species,
    method = "rf"
  )

  out <- clique_eval(
    model = model,
    x_train = iris[,1:4],
    y_train = iris$Species,
  )

  expect_s3_class(out, "data.frame")

  expect_equal(
    dim(out),
    c(150, 4)
  )

})

test_that("clique_eval works on test data", {

  set.seed(123)

  train_ind <- sample(150, 120)

  model <- caret::train(
    x = iris[train_ind,1:4],
    y = iris$Species[train_ind],
    method = "rf"
  )

  out <- clique_eval(
    model = model,
    x_train = iris[train_ind,1:4],
    y_train = iris$Species[train_ind],
    x_test = iris[-train_ind,1:4],
    y_test = iris$Species[-train_ind]
  )

  expect_equal(
    dim(out),
    c(30,4)
  )

})

test_that("y_test required when x_test supplied", {

  model <- caret::train(
    x = iris[,1:4],
    y = iris$Species,
    method = "rf"
  )

  expect_error(
    clique_eval(
      model = model,
      x_train = iris[,1:4],
      y_train = iris$Species,
      x_test = iris[1:10,1:4]
    ),
    "y_test"
  )

})

test_that("single observation evaluation works", {

  model <- caret::train(
    x = iris[,1:4],
    y = iris$Species,
    method = "rf"
  )

  out <- clique_eval(
    model = model,
    x_train = iris[,1:4],
    y_train = iris$Species,
    x_test = iris[1,1:4,drop=FALSE],
    y_test = iris$Species[1]
  )

  expect_equal(
    dim(out),
    c(1,4)
  )

})

################################################################################

test_that("clique_eval works when y_test contains fewer classes than y_train", {

  set.seed(123)

  train_ind <- c(1:40, 51:90, 101:140)
  test_ind  <- 141:150  # mostly Setosa-free subset

  model <- caret::train(
    x = iris[train_ind, 1:4],
    y = iris$Species[train_ind],
    method = "rf"
  )

  x_test <- iris[test_ind, 1:4]
  y_test <- droplevels(iris$Species[test_ind])

  expect_no_error({

    out <- clique_eval(
      model = model,
      x_train = iris[train_ind, 1:4],
      y_train = iris$Species[train_ind],
      x_test = x_test,
      y_test = y_test
    )

  })

})

test_that("clique_eval works for regression", {

  set.seed(123)

  train_ind <- sample(seq_len(nrow(mtcars)), 25)

  model <- caret::train(
    x = mtcars[train_ind, -1],
    y = mtcars$mpg[train_ind],
    method = "lm"
  )

  out <- clique_eval(
    model = model,
    x_train = mtcars[train_ind, -1],
    y_train = mtcars$mpg[train_ind],
    x_test = mtcars[-train_ind, -1],
    y_test = mtcars$mpg[-train_ind]
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), length(mtcars$mpg[-train_ind]))
  expect_equal(ncol(out), ncol(mtcars) - 1)

})

test_that("clique_eval works with class_loss = TRUE", {

  set.seed(123)

  train_ind <- sample(seq_len(nrow(iris)), 120)

  model <- caret::train(
    x = iris[train_ind, 1:4],
    y = iris$Species[train_ind],
    method = "rf"
  )

  out <- clique_eval(
    model = model,
    x_train = iris[train_ind, 1:4],
    y_train = iris$Species[train_ind],
    x_test = iris[-train_ind, 1:4],
    y_test = iris$Species[-train_ind],
    class_loss = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(ncol(out), 4)

})

test_that("clique_eval catches incompatible training dimensions", {

  model <- caret::train(
    x = iris[, 1:4],
    y = iris$Species,
    method = "rf"
  )

  expect_error(
    clique_eval(
      model = model,
      x_train = iris[, 1:4],
      y_train = iris$Species[-1]
    ),
    "incompatible dimensions"
  )

})

test_that("clique_eval catches incompatible test dimensions", {

  model <- caret::train(
    x = iris[, 1:4],
    y = iris$Species,
    method = "rf"
  )

  expect_error(
    clique_eval(
      model = model,
      x_train = iris[, 1:4],
      y_train = iris$Species,
      x_test = iris[1:10, 1:4],
      y_test = iris$Species[1:9]
    ),
    "incompatible dimensions"
  )

})

test_that("clique_eval requires y_test when x_test is supplied", {

  model <- caret::train(
    x = iris[, 1:4],
    y = iris$Species,
    method = "rf"
  )

  expect_error(
    clique_eval(
      model = model,
      x_train = iris[, 1:4],
      y_train = iris$Species,
      x_test = iris[1:10, 1:4]
    ),
    "y_test must be supplied"
  )

})

test_that("clique_eval requires x_test when y_test is supplied", {

  model <- caret::train(
    x = iris[, 1:4],
    y = iris$Species,
    method = "rf"
  )

  expect_error(
    clique_eval(
      model = model,
      x_train = iris[, 1:4],
      y_train = iris$Species,
      y_test = iris$Species[1:10]
    ),
    "x_test must be supplied"
  )

})

test_that("formula and x-y interfaces agree", {

  set.seed(123)

  train_ind <- sample(seq_len(nrow(iris)), 120)

  model <- caret::train(
    x = iris[train_ind, 1:4],
    y = iris$Species[train_ind],
    method = "rf"
  )

  out_xy <- clique_eval(
    model = model,
    x_train = iris[train_ind, 1:4],
    y_train = iris$Species[train_ind],
    x_test = iris[-train_ind, 1:4],
    y_test = iris$Species[-train_ind],
    nsim = 10,
    seed = 123
  )

  train_df <- iris[train_ind, ]
  test_df  <- iris[-train_ind, ]

  out_formula <- clique_eval(
    model = model,
    formula = Species ~ .,
    data_train = train_df,
    data_test = test_df,
    nsim = 10,
    seed = 123
  )

  expect_equal(out_xy, out_formula)

})
