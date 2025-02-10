d_sim_binom_cov <- d_sim_binom_cov

id_to_sample <- seq(1, max(d_sim_binom_cov$ID), by = 5)

df_er_ae_covsel_test <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "ae_covsel_test",
    ID %in% id_to_sample
  ) |>
  dplyr::mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10, BHBA1C_5 = BHBA1C / 5
  )


# Create  ermod_bin objects for testing ---------------------------------------
set.seed(1234)
ermod_bin <- dev_ermod_bin(
  data = df_er_ae_covsel_test,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
  verbosity_level = 0,
  # Need large number of samples to calculate p_direction precisely
  chains = 4, iter = 2000
)

ermod_bin_cov_sel <- new_ermod_bin_cov_sel(
  mod = extract_mod(ermod_bin),
  data = df_er_ae_covsel_test,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov_candidates = c("BAGE_10", "BWT_10", "BHBA1C_5", "RACE", "VISC"),
  var_cov = c("BHBA1C_5"),
  var_selected = c("AUCss_1000", "BHBA1C_5"),
  cv_method = c("LOO", "kfold"),
  cvvs = NULL,
  rk = NULL
)


test_that("p_direction", {
  expect_equal(p_direction(ermod_bin_cov_sel, as_num = TRUE), 0.9955)
  expect_equal(
    p_direction(ermod_bin_cov_sel, as_num = TRUE, as_p = TRUE),
    0.009
  )
})
