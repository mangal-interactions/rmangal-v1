context("Test of the first contact with the API")

test_that("A default call is a success",{
  default <- mangalAPI()
  expect_equal(sayHi(default)$method, "GET")
  expect_equal(sayHi(default)$api, "0.9a")
})