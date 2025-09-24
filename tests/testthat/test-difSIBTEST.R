test_that("difSIBTEST - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")

  # Loading of the verbal data
  data(verbal)

  # Excluding the "Anger" variable
  verbal <- verbal[, colnames(verbal) != "Anger"]

  # Three equivalent settings of the data matrix and the group membership
  expect_snapshot((r1 <- difSIBTEST(verbal, group = 25, focal.name = 1)))
  # saveRDS(r1, file = "tests/testthat/fixtures/difSIBTEST_examples_1.rds")
  r1_expected <- readRDS(test_path("fixtures", "difSIBTEST_examples_1.rds"))
  expect_equal(r1, r1_expected)
  expect_equal(r1, difSIBTEST(verbal, group = "Gender", focal.name = 1))
  expect_equal(r1, difSIBTEST(verbal[, 1:24], group = verbal[, 25], focal.name = 1))

  # Test for nonuniform DIF
  expect_snapshot((r2 <- difSIBTEST(verbal, group = 25, focal.name = 1, type = "nudif")))
  # saveRDS(r2, file = "tests/testthat/fixtures/difSIBTEST_examples_2.rds")
  r2_expected <- readRDS(test_path("fixtures", "difSIBTEST_examples_2.rds"))
  expect_equal(r2, r2_expected)

  # Multiple comparisons adjustment using Benjamini-Hochberg method
  expect_snapshot(difSIBTEST(verbal, group = 25, focal.name = 1, p.adjust.method = "BH"))

  # With item purification
  expect_snapshot((r3 <- difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE)))
  # saveRDS(r3, file = "tests/testthat/fixtures/difSIBTEST_examples_3.rds")
  r3_expected <- readRDS(test_path("fixtures", "difSIBTEST_examples_3.rds"))
  expect_equal(r3, r3_expected)
  expect_snapshot(difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE, nrIter = 5))

  # With items 1 to 5 set as anchor items
  expect_snapshot(difSIBTEST(verbal, group = "Gender", focal.name = 1, anchor = 1:5))
  expect_snapshot(difSIBTEST(verbal, group = "Gender", focal.name = 1, anchor = 1:5, purify = TRUE))

  # With combination of item purification and multiple comparisons adjustment
  expect_snapshot(difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "simple"))
  expect_snapshot((r4 <- difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "combined")))
  # saveRDS(r4, file = "tests/testthat/fixtures/difSIBTEST_examples_4.rds")
  r4_expected <- readRDS(test_path("fixtures", "difSIBTEST_examples_4.rds"))
  expect_equal(r4, r4_expected)

})
