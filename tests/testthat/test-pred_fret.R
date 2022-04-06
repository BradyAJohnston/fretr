test_that("FRET prediction works.", {
  expect_equal(round(fret_pred(100), 5), 0.02419)
  expect_equal(round(fret_pred(c(100, 54)), 5), c(0.02419, 0.5))
})

test_that("Strings don't calculate FRET.", {
  testthat::expect_error({
    fret_pred("100")
  })
})
