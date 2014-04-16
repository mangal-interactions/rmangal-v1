context("Test of the taxa methods")

test_that("listTaxa and getTaxa work as they should",{
          api <- mangalapi()
          list_taxa <- listTaxa(api)
          expect_is(list_taxa, "list")
          expect_is(list_taxa[[1]], "list")
          taxa_1 <- getTaxa(api, list_taxa[[1]]$id)
          expect_equal(list_taxa[[1]]$name, taxa_1$name)
})
