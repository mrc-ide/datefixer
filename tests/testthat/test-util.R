test_that("can convert from dates to integers", {
  expect_equal(date_to_int("1970-02-09"), 39)
  expect_equal(date_to_int("2020-02-09", origin = "2020-01-01"), 39)
  
  expect_equal(date_to_int(c("1970-02-09", "1970-03-01")), c(39, 59))
  expect_equal(date_to_int(array(c("1970-02-09", "1970-03-01", 
                                   "1970-03-21", "1970-04-10"),
                                 c(2, 2))),
               array(c(39, 59, 79, 99), c(2, 2)))
})

test_that("can convert from integers to dates", {
  expect_equal(int_to_date(39), as.Date("1970-02-09"))
  expect_equal(int_to_date(39, origin = "2020-01-01"), as.Date("2020-02-09"))
  
  expect_equal(int_to_date(c(39, 59)), as.Date(c("1970-02-09", "1970-03-01")))
  expect_equal(int_to_date(array(c(39, 59, 79, 99), c(2, 2))),
    array(as.Date(c("1970-02-09", "1970-03-01", "1970-03-21", "1970-04-10")),
          c(2, 2)))
})
