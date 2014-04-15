context("Test of mangalAPI")

test_that("The default API works",{
  expect_is(mangalapi(), "list")
})

