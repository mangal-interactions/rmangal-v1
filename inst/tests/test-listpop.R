context("Tests of listPop")

test_that("The arguments are checked",{
  default <- mangalAPI()
  taxa <- getTaxa(default, "1")
  taxa_numid <- taxa
  taxa_numid$id <- 1
  taxa_noid <- taxa
  taxa_noid$id <- NULL
  expect_error(listPop(default, "1"), "Objects must be passed as lists")
  expect_error(listPop(default, taxa_numid), "The id attribute must be a character")
  expect_error(listPop(default, taxa_noid), "The object must have a ID attribute")
  expect_error(listPop("default", taxa), "The API argument must be a valid mangalAPI object")
})