test_that("difMH - examples at help page", {
  # skip_on_cran()
  # skip_on_os("linux")

  # Loading of the verbal data
  data(verbal)

  # Excluding the "Anger" variable
  verbal <- verbal[colnames(verbal) != "Anger"]

  # Three equivalent settings of the data matrix and the group membership
  expect_snapshot((r1 <- difMH(verbal, group = 25, focal.name = 1)))
  # saveRDS(r1, file = "tests/testthat/fixtures/difMH_examples_1.rds")
  r1_expected <- readRDS(test_path("fixtures", "difMH_examples_1.rds"))
  expect_equal(r1, r1_expected)
  expect_equal(r1, difMH(verbal, group = "Gender", focal.name = 1))
  expect_equal(r1, difMH(verbal[, 1:24], group = verbal[, 25], focal.name = 1))

  # With log odds-ratio statistic
  expect_snapshot((r2 <- difMH(verbal, group = 25, focal.name = 1, MHstat = "logOR")))
  # saveRDS(r2, file = "tests/testthat/fixtures/difMH_examples_2.rds")
  r2_expected <- readRDS(test_path("fixtures", "difMH_examples_2.rds"))
  expect_equal(r2, r2_expected)

  # With exact inference
  expect_snapshot((r3 <- difMH(verbal, group = 25, focal.name = 1, exact = TRUE)))
  # saveRDS(r3, file = "tests/testthat/fixtures/difMH_examples_3.rds")
  r3_expected <- readRDS(test_path("fixtures", "difMH_examples_3.rds"))
  expect_equal(r3, r3_expected)

  # Multiple comparisons adjustment using Benjamini-Hochberg method
  expect_snapshot(difMH(verbal, group = 25, focal.name = 1, p.adjust.method = "BH"))

  # With item purification
  expect_snapshot((r4 <- difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE)))
  # saveRDS(r4, file = "tests/testthat/fixtures/difMH_examples_4.rds")
  r4_expected <- readRDS(test_path("fixtures", "difMH_examples_4.rds"))
  expect_equal(r4, r4_expected)
  expect_snapshot(difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, nrIter = 5))

  # With combination of item purification and multiple comparisons adjustment
  expect_snapshot(difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "simple"))
  expect_snapshot((r5 <- difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, p.adjust.method = "BH", puriadjType = "combined")))
  # saveRDS(r5, file = "tests/testthat/fixtures/difMH_examples_5.rds")
  r5_expected <- readRDS(test_path("fixtures", "difMH_examples_5.rds"))
  expect_equal(r5, r5_expected)

  # Without continuity correction and with 0.01 significance level
  expect_snapshot(difMH(verbal, group = "Gender", focal.name = 1, alpha = 0.01, correct = FALSE))

  # With items 1 to 5 set as anchor items
  expect_snapshot(difMH(verbal, group = "Gender", focal.name = 1, anchor = 1:5))
  expect_snapshot(difMH(verbal, group = "Gender", focal.name = 1, anchor = 1:5, purify = TRUE))

})
