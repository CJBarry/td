library(td)
context("test td values")

test_that("expected dates", {
  expect_equal(td(30, 12, 1899), 0)
  expect_equal(td(29, 2, 1900), NA_real_)
  expect_equal(td(31, 7, 2000), 36738)
  expect_equal(td(4, 5, 1970), 25692)
  expect_equal(td(31, 3, 3000), 401858)
  expect_identical(monthdays(1:13, 1900),
                   c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, NA))
  expect_identical(monthdays(1:13, 1904),
                   c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, NA))
  expect_identical(monthdays(1:13, 2000),
                   c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, NA))
  expect_equal(yeardays(1900), 365)
  expect_equal(yeardays(2000), 366)
})

test_that("invtd", {
  expect_equal(invtd(0), "30/12/1899")
  expect_equal(invtd(.99), "30/12/1899")
  expect_equal(invtd(335250), "17/11/2817")
  expect_equal(invtd(39859), "15/2/2009")
  expect_equal(invtd(23097), "27/3/1963")
})
