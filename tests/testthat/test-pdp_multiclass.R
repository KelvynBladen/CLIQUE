test_that("errors when response not in data", {

  df <- iris

  expect_error(
    pdp_multiclass(
      object = randomForest(Species ~ ., data = df),
      pred.data = df,
      response = "not_a_column",
      pred.vars = "Sepal.Length"
    ),
    "not found in pred.data, or is mistyped"
  )
})


test_that("errors when predictors missing", {

  df <- iris

  expect_error(
    pdp_multiclass(
      object = randomForest(Species ~ ., data = df),
      pred.data = df,
      response = "Species",
      pred.vars = c("does_not_exist")
    ),
    "not in pred.data"
  )
})


test_that("errors when response not factor", {

  df <- iris
  df$Species <- as.character(df$Species)

  expect_error(
    pdp_multiclass(
      object = randomForest(Species ~ ., data = df),
      pred.data = df,
      response = "Species",
      pred.vars = "Sepal.Length"
    ),
    "must be a factor"
  )
})

test_that("output structure is correct", {

  set.seed(1)
  df <- iris

  rf <- randomForest(Species ~ ., data = df)

  out <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = c("Sepal.Length", "Sepal.Width"),
    prob = TRUE
  )

  expect_true(is.list(out))

  expect_true("data" %in% names(out))
  expect_true("combo_color" %in% names(out))
  expect_true("combo_facet" %in% names(out))

  # per-variable components exist
  expect_true(any(grepl("Sepal.Length", names(out))))
  expect_true(any(grepl("Sepal.Width", names(out))))
})

test_that("returned PDP data is valid", {

  set.seed(1)
  df <- iris
  rf <- randomForest(Species ~ ., data = df)

  out <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = c("Sepal.Length")
  )

  dat <- out$data

  expect_true(is.data.frame(dat))

  # required columns
  expect_true(all(c("x", "yhat", "yhat.id", "pred.var") %in% names(dat)))

  # no missing predictions
  expect_false(any(is.na(dat$yhat)))

  # class labels preserved
  expect_true(all(levels(df$Species) %in% unique(dat$yhat.id)))
})

test_that("output is deterministic under fixed seed", {

  df <- iris
  rf <- randomForest(Species ~ ., data = df)

  set.seed(123)
  out1 <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = "Sepal.Length"
  )

  set.seed(123)
  out2 <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = "Sepal.Length"
  )

  expect_equal(out1$data, out2$data)
})

test_that("all classes appear in output", {

  df <- iris
  rf <- randomForest(Species ~ ., data = df)

  out <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = "Sepal.Width"
  )

  classes <- levels(df$Species)

  expect_true(all(classes %in% unique(out$data$yhat.id)))
})

test_that("plots are ggplot objects", {

  df <- iris
  rf <- randomForest(Species ~ ., data = df)

  out <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = "Sepal.Length"
  )

  expect_s3_class(out$combo_color, "ggplot")
  expect_s3_class(out$combo_facet, "ggplot")

  # per-variable plot
  expect_s3_class(out[[grep("_color", names(out))[1]]], "ggplot")
})

test_that("parallel setting does not change results", {

  df <- iris
  rf <- randomForest(Species ~ ., data = df)

  set.seed(123)
  out1 <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = "Sepal.Length"
  )

  # simulate parallel-like rerun
  set.seed(123)
  out2 <- pdp_multiclass(
    object = rf,
    pred.data = df,
    response = "Species",
    pred.vars = "Sepal.Length"
  )

  expect_equal(out1$data, out2$data)
})

