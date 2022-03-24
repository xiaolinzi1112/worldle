test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("summation works", {
  expect_equal(2 + 2, 4)
})

test_that("sure failure no more", {
  expect_error(expect_equal(TRUE, FALSE))
})
