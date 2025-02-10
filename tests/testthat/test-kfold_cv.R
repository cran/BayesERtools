set.seed(1234)
data(d_sim_binom_cov_hgly2)

ermod_bin <- dev_ermod_bin(
  data = d_sim_binom_cov_hgly2 |> head(100),
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
  chains = 2,
  iter = 1000
)

cv_results <- run_kfold_cv(ermod_bin, k = 3, seed = 123)

# Test for other models are covered in test-eval_ermod.R with kfold-cv eval
test_that("binary model", {
  expect_equal(
    names(cv_results$d_truth),
    c(".row", "truth", "fold_id")
  )
  expect_equal(
    names(cv_results$d_sim),
    c(".row", ".draw", "pred", "fold_id")
  )
  expect_equal(dim(cv_results$d_sim), c(100 * 1000, 4))
})
