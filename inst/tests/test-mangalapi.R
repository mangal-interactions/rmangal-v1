context("Test of mangalAPI")

test_that("The API url/usr/pwd works",{
  expect_warning(mangalapi(usr='test'), "No password has been provided")
  expect_warning(mangalapi(pwd='test'), "No username has been provided")
})
