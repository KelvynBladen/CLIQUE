test_that("quantile grid returns correct length", {

  x <- rnorm(100)

  g <- generate_grid_values(
    x,
    nsim = 25,
    quantile_grid = TRUE
  )

  expect_length(g, 25)

})

test_that("uniform grid returns correct length", {

  x <- rnorm(100)

  g <- generate_grid_values(
    x,
    nsim = 30,
    quantile_grid = FALSE
  )

  expect_length(g, 30)

})
