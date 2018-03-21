context("data_overview")

wake = get_data(counties = "wake")

test_that("Test the data overview of Wake county data.",
          {
            expect_equal(data_overview(wake), NULL)
          })
