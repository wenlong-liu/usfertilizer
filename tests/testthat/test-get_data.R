context("get_data")

###############
# test the inputs of the function
# produce some error data.
year1_wrong = 128355
year_error = c(1, 1903, 2019, 24556, -1, 50, 22222222222, -8882039 )
county_error = c("aer", "wes","3w2")
state_error = c("2w", "esa", 0)
FIPS_error = c(10000000,332,-10,3)
long_error = c(-000, 1332,35, 99, "abc")
fert_error = c(1,"2,'S")

# test year input.
test_that("Check to make sure the error info is right.",
          {expect_error(get_data(years = year1_wrong),"Years 128355 are not available in the dataset.\n\n               Please deleted them and try again.\n",
                        fixed = TRUE)})

for (year in year_error){
test_that("If the years are out of limit, we can retrieve data from the dataset.",{
  expect_error(get_data(years = year))
})
}

# test fertilizer input.
for (fert in fert_error){
  test_that("If the fertilizer type is not right, we can not retrieve data from the dataset.",{
    expect_that(get_data(fert_type =  fert), throws_error())
  })
}

# test county input.
for (county in county_error){
  test_that("If the county names are in correct or out of bound, we can not retrieve data from the dataset.",{
    expect_that(get_data(counties = county), throws_error())
  })
}

# test state input.
for (state in state_error){
  test_that("If the county names are in correct or out of bound, we can not retrieve data from the dataset.",{
    expect_that(get_data(states = state ), throws_error())
  })
}

# test FIPS input.
for (FIPS in FIPS_error){
  test_that("If the FIPS codes are incorrect or out of bound, we can not retrieve data from the dataset.",{
    expect_that(get_data(FIPSs =  FIPS ), throws_error())
  })
}

# test the lattitude and longtitude.
test_that("If the FIPS codes are incorrect or out of bound, we can not retrieve data from the dataset.",{
  expect_that(get_data(long_max = long_error[1],  long_min = long_error[2],
                       lat_max = long_error[3], lat_min = long_error[4] ), throws_error())
  })

###############
# test parameters of the function.
county = "Lincoln"
FIPS = 37109
state = "NC"
year = 2004
fert.type = "N"

# data = get_data(years = 2004, counties = "Lincoln",states = "NC") dim = [4,11]
#
test_that("Test the output dataframe has the right dimention.",{
  expect_that(dim(get_data(years = year, counties = county,states = state))[1], equals(4))
})

# test the parameter, overlap_state_county.
# data = get_data(years = 2004, counties = "Lincoln",states = "NC", overlap_state_county = F)
# dim(data) = [488,11]

test_that("Test the output dataframe has the right dimention without overlapping.",{
  expect_that(dim(get_data(years = year, counties = county,states = state,
                           overlap_state_county = F))[1], equals(488))
})

test_that("test county and FIPS can not input at the same time.",{
  expect_that(get_data(years = year, counties = county,states = state, FIPSs = FIPS,
                           overlap_state_county = F), throws_error())
})

test_that("Test gives a warning if no data selected.",{
  expect_that(get_data(years = year, counties = county,states = "SC",
                       overlap_state_county = T), gives_warning())
})

test_that("Combine_state_county will give the result like Wake, NC.",{
  expect_that(get_data(years = year, counties = county,states = state,
                       combine_state_county = T)$County[1], equals("Lincoln, NC"))
})

data_upper = get_data(years = year, counties = county,states = state)
data_lower = get_data(years = year, counties = county,states = tolower(state))

test_that("The capalized state name or county name will not affect the results. ",{
  expect_that(data_upper, equals(data_lower))

  data_lower = get_data(years = year, counties = toupper(county),states = state)
  expect_that(data_upper, equals(data_lower))

  data_upper = get_data(years = year, counties = county,fert_type = fert.type,states = state)
  data_lower = get_data(years = year, counties = county,fert_type = tolower(fert.type), states = state)
  expect_that(data_upper, equals(data_lower))

})

test_that("The overlapping value TRUE requres both state and county filters", {
  expect_that(get_data(years=year,counties = county, overlap_state_county = F), throws_error())
})
