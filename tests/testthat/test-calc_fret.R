test_that("Testing calc_fret", {
  expect_equal(calc_fret(0, 1), 1)
  expect_equal(calc_fret(0.5, 0.5), 0.5)
  expect_equal(calc_fret(c(0, 0.5), c(1, 0.5)), c(1, 0.5))
})

test_that("Testing calc_fret", {
  expect_error(calc_fret("0", 1))
  expect_error(calc_fret(0, "1"))
})
