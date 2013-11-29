context("Test of mangalAPI")

test_that("The API url/usr/pwd works",{
  expect_warning(mangalAPI(usr='test'), "No password has been provided")
  expect_warning(mangalAPI(pwd='test'), "No username has been provided")
})