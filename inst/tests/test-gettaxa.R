context("Tests of getTaxa")

test_that("The arguments are checked",{
  default <- mangalAPI()
  expect_error(getTaxa(default, 1), "The id argument must be a string")
  expect_error(getTaxa("default", "1"), "The API argument must be a valid mangalAPI object")
})

test_that("The 404 error code is recognized",{
  default <- mangalAPI()
  expect_error(getTaxa(default, "0"), "This taxa cannot be found in the database, use listTaxa for a list of taxa")
})