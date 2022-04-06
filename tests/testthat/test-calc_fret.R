test_that("Testing fret_calc", {
  expect_equal(fret_calc(0, 1), 1)
  expect_equal(fret_calc(0.5, 0.5), 0.5)
  expect_equal(fret_calc(c(0, 0.5), c(1, 0.5)), c(1, 0.5))
})

test_that("Testing fret_calc", {
  expect_error(fret_calc("0", 1))
  expect_error(fret_calc(0, "1"))
})
