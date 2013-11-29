context("Tests of listPop")

test_that("The arguments are checked",{
  default <- mangalAPI()
  taxa <- getTaxa(default, "1")
  taxa_numid <- taxa
  taxa_numid$id <- 1
  taxa_noid <- taxa
  taxa_noid$id <- NULL
  expect_error(listPop(default, "1"), "The taxa argument must be a list")
  expect_error(listPop(default, taxa_numid), "The id attribute of taxa must be a character")
  expect_error(listPop(default, taxa_noid), "he taxa object must have a id attribute")
  expect_error(listPop("default", taxa), "The API argument must be a valid mangalAPI object")
})