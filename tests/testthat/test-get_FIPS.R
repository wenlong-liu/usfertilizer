context("get_FIPS")

###############
# test the inputs of the function
test_that("If the county name is wake, the FIPS # is 37183",{
  expect_that(get_FIPS(counties = "wake")$FIPS[1], equals("37183" ))
})

# fixed = TRUE to compare the true string value.
test_that("If the input is year, throw the error",{
  expect_error(get_FIPS(years = 1990), "unused argument (years = 1990)", fixed = TRUE)
})

test_that("If county name is DeKalb, the length is 6, the third value is 17037, the 6th name
          is DeKalb, TN.",{
  FIPS_test = get_FIPS(counties = "DeKalb")
  expect_that(length(FIPS_test$FIPS), equals(6))
  expect_that(FIPS_test$FIPS[3], equals("17037"))
  expect_that(FIPS_test$County[6],equals("DeKalb, TN"))
})

