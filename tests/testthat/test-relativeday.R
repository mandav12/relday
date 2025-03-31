library(testthat)
library(relday)

test_that("relative_day computes correct relative days", {
  test_data <- tibble(
    TRTSDT = as.Date("2024-03-01"),
    ASTDT = as.Date("2024-03-05"),
    AENDT = as.Date("2024-03-10")
  )

  result <- relative_day(test_data, reference_date = TRTSDT, source_vars = c("ASTDT", "AENDT"))

  # Check correct relative day calculations
  expect_equal(result$ASTDY, 5)
  expect_equal(result$AENDY, 10)
})


