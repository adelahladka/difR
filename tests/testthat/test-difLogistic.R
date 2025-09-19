test_that("difLogistic - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")

  # Loading of the verbal data
  data(verbal)

  # Excluding the "Anger" variable
  anger <- verbal[, colnames(verbal) == "Anger"]
  verbal <- verbal[, colnames(verbal) != "Anger"]

  # Testing both DIF effects simultaneously
  # Three equivalent settings of the data matrix and the group membership
  expect_snapshot((r1 <- difLogistic(verbal, group = 25, focal.name = 1)))
  # saveRDS(r1, file = "tests/testthat/fixtures/difLogistic_examples_1.rds")
  r1_expected <- readRDS(test_path("fixtures", "difLogistic_examples_1.rds"))
  expect_equal(r1, r1_expected)
  expect_equal(r1, difLogistic(verbal, group = "Gender", focal.name = 1))
  expect_equal(r1, difLogistic(verbal[, 1:24], group = verbal[, 25], focal.name = 1))

  # Returning all covariance matrices of model parameters
  expect_snapshot((r2 <- difLogistic(verbal, group = 25, focal.name = 1, all.cov = TRUE)))
  # saveRDS(r2, file = "tests/testthat/fixtures/difLogistic_examples_2.rds")
  r2_expected <- readRDS(test_path("fixtures", "difLogistic_examples_2.rds"))
  expect_equal(r2, r2_expected)

  # Testing both DIF effects with the Wald test
  expect_snapshot(difLogistic(verbal, group = 25, focal.name = 1, criterion = "Wald"))

  # Testing nonuniform DIF effect
  expect_snapshot(difLogistic(verbal, group = 25, focal.name = 1, type = "nudif"))

  # Testing uniform DIF effect
  expect_snapshot(difLogistic(verbal, group = 25, focal.name = 1, type = "udif"))

  # Multiple comparisons adjustment using Benjamini-Hochberg method
  expect_snapshot(difLogistic(verbal, group = 25, focal.name = 1, p.adjust.method = "BH"))

  # With item purification
  expect_snapshot((r3 <- difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE)))
  # saveRDS(r3, file = "tests/testthat/fixtures/difLogistic_examples_3.rds")
  r3_expected <- readRDS(test_path("fixtures", "difLogistic_examples_3.rds"))
  expect_equal(r3, r3_expected)
  expect_snapshot(difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE, nrIter = 5))

  # With combination of item purification and multiple comparisons adjustment
  expect_snapshot(difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "simple"))
  expect_snapshot((r4 <- difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "combined")))
  # saveRDS(r4, file = "tests/testthat/fixtures/difLogistic_examples_4.rds")
  r4_expected <- readRDS(test_path("fixtures", "difLogistic_examples_4.rds"))
  expect_equal(r4, r4_expected)

  # With items 1 to 5 set as anchor items
  expect_snapshot(difLogistic(verbal, group = 25, focal.name = 1, anchor = 1:5))

  # Using anger trait score as the matching criterion
  expect_snapshot(difLogistic(verbal,group = 25, focal.name = 1, match = anger))

  # Using trait anger score as the group variable (i.e. testing
  # for DIF with respect to trait anger score)
  expect_snapshot(difLogistic(verbal[, 1:24], group = anger, member.type = "cont"))

})
