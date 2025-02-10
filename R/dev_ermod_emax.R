# Emax ------------------------------------------------------------------------

#' Develop Emax model for continuous and binary endpoint
#'
#' These functions are used to develop an Emax model with continuous or binary
#' endpoint.
#' You can also specify covariates to be included in the model; note that only
#' categorical covariates are allowed.
#'
#' @export
#' @inheritParams dev_ermod_bin
#' @param l_var_cov a names list of categorical covariate variables in
#' character vector. See details in the `param.cov` argument of
#'  [rstanemax::stan_emax()] or [rstanemax::stan_emax_binary()]
#' @param gamma_fix Hill coefficient, default fixed to 1.
#' See details in [rstanemax::stan_emax()] or [rstanemax::stan_emax_binary()]
#' @param e0_fix See details in [rstanemax::stan_emax()] or
#' [rstanemax::stan_emax_binary()]
#' @param emax_fix See details in [rstanemax::stan_emax()] or
#'  [rstanemax::stan_emax_binary()]
#' @param priors See details in [rstanemax::stan_emax()] or
#' [rstanemax::stan_emax_binary()]
#' @param seed Random seed for Stan model execution, see details in
#' [rstan::sampling()] which is used in [rstanemax::stan_emax()] or
#' [rstanemax::stan_emax_binary()]
#'
#' @return An object of class `ermod_emax`.or `ermod_bin_emax`.
#'
#' @examples
#' \donttest{
#' data_er_cont <- rstanemax::exposure.response.sample
#'
#' ermod_emax <-
#'   dev_ermod_emax(
#'     data = data_er_cont,
#'     var_exposure = "exposure",
#'     var_resp = "response"
#'   )
#'
#' plot_er(ermod_emax, show_orig_data = TRUE)
#'
#' data_er_cont_cov <- rstanemax::exposure.response.sample.with.cov
#'
#' ermod_emax_w_cov <-
#'   dev_ermod_emax(
#'     data = data_er_cont_cov,
#'     var_exposure = "conc",
#'     var_resp = "resp",
#'     l_var_cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1")
#'   )
#' }
#'
dev_ermod_emax <- function(
    data,
    var_resp,
    var_exposure,
    l_var_cov = NULL,
    gamma_fix = 1,
    e0_fix = NULL,
    emax_fix = NULL,
    priors = NULL,
    verbosity_level = 1,
    chains = 4,
    iter = 2000,
    seed = sample.int(.Machine$integer.max, 1)) {
  rlang::check_installed("rstanemax", version = "0.1.7")

  input_args <- capture_selected_args(
    c(
      "gamma_fix", "e0_fix", "emax_fix",
      "priors", "chains", "iter"
    ),
    environment()
  )

  stopifnot(verbosity_level %in% c(0, 1, 2, 3))
  refresh <- dplyr::if_else(verbosity_level >= 3, iter %/% 4, 0)

  check_data_columns(
    data = data,
    var_exposure = var_exposure,
    var_resp = var_resp,
    var_cov = unlist(l_var_cov)
  )

  formula <-
    stats::formula(paste(var_resp, "~", var_exposure))

  mod <- rstanemax::stan_emax(
    formula,
    data = data,
    gamma.fix = gamma_fix,
    e0.fix = e0_fix,
    emax.fix = emax_fix,
    priors = priors,
    param.cov = l_var_cov,
    refresh = refresh,
    chains = chains,
    iter = iter,
    seed = seed
  )

  new_ermod_emax(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    l_var_cov = l_var_cov,
    input_args = input_args
  )
}


#' Exposure metrics selection for Emax models
#'
#' This functions is used to develop an Emax model with binary and continuous
#' endpoint, using various exposure metrics and selecting the best one.
#'
#' @export
#' @inheritParams dev_ermod_emax
#' @inheritParams dev_ermod_bin_exp_sel
#'
#' @return An object of class `ermod_emax_exp_sel` or `ermod_bin_emax_exp_sel`.
#'
#' @examples
#' \donttest{
#' data_er_cont <- rstanemax::exposure.response.sample
#' noise <- 1 + 0.5 * stats::rnorm(length(data_er_cont$exposure))
#' data_er_cont$exposure2 <- data_er_cont$exposure * noise
#' # Replace exposure < 0 with 0
#' data_er_cont$exposure2[data_er_cont$exposure2 < 0] <- 0
#'
#' ermod_emax_exp_sel <-
#'   dev_ermod_emax_exp_sel(
#'     data = data_er_cont,
#'     var_resp = "response",
#'     var_exp_candidates = c("exposure", "exposure2")
#'   )
#'
#' ermod_emax_exp_sel
#' }
#'
dev_ermod_emax_exp_sel <- function(
    data,
    var_resp,
    var_exp_candidates,
    verbosity_level = 1,
    chains = 4,
    iter = 2000,
    gamma_fix = 1,
    e0_fix = NULL,
    emax_fix = NULL,
    priors = NULL,
    seed = sample.int(.Machine$integer.max, 1)) {
  fun_dev_ermod <-
    purrr::partial(
      dev_ermod_emax,
      gamma_fix = gamma_fix,
      e0_fix = e0_fix,
      emax_fix = emax_fix,
      priors = priors,
      seed = seed
    )

  l_out <-
    .dev_ermod_exp_sel(
      data = data,
      var_resp = var_resp,
      var_exp_candidates = var_exp_candidates,
      verbosity_level = verbosity_level,
      chains = chains,
      iter = iter,
      fun_dev_ermod = fun_dev_ermod
    )

  new_ermod_emax_exp_sel(l_out)
}

# Binary Emax -----------------------------------------------------------------

#' @export
#' @rdname dev_ermod_emax
#' @examples
#' \donttest{
#' data_er_bin <- rstanemax::exposure.response.sample.binary
#'
#' ermod_bin_emax <-
#'   dev_ermod_bin_emax(
#'     data = data_er_bin,
#'     var_exposure = "conc",
#'     var_resp = "y"
#'   )
#'
#' plot_er(ermod_bin_emax, show_orig_data = TRUE)
#'
#' ermod_bin_emax_w_cov <-
#'   dev_ermod_bin_emax(
#'     data = data_er_bin,
#'     var_exposure = "conc",
#'     var_resp = "y_cov",
#'     l_var_cov = list(emax = "sex")
#'   )
#' }
#'
dev_ermod_bin_emax <- function(
    data,
    var_resp,
    var_exposure,
    l_var_cov = NULL,
    gamma_fix = 1,
    e0_fix = NULL,
    emax_fix = NULL,
    priors = NULL,
    verbosity_level = 1,
    chains = 4,
    iter = 2000,
    seed = sample.int(.Machine$integer.max, 1)) {
  rlang::check_installed("rstanemax", version = "0.1.7")

  input_args <- capture_selected_args(
    c(
      "gamma_fix", "e0_fix", "emax_fix",
      "priors", "chains", "iter"
    ),
    environment()
  )

  stopifnot(verbosity_level %in% c(0, 1, 2, 3))
  refresh <- dplyr::if_else(verbosity_level >= 3, iter %/% 4, 0)

  check_data_columns(
    data = data,
    var_exposure = var_exposure,
    var_resp = var_resp,
    var_cov = unlist(l_var_cov),
    is_binary = TRUE
  )

  formula <-
    stats::formula(paste(var_resp, "~", var_exposure))

  mod <- rstanemax::stan_emax_binary(
    formula,
    data = data,
    gamma.fix = gamma_fix,
    e0.fix = e0_fix,
    emax.fix = emax_fix,
    priors = priors,
    param.cov = l_var_cov,
    refresh = refresh,
    chains = chains,
    iter = iter,
    seed = seed
  )

  new_ermod_bin_emax(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    l_var_cov = l_var_cov,
    input_args = input_args
  )
}


#' @export
#' @rdname dev_ermod_emax_exp_sel
#'
#' @examples
#' \donttest{
#' data_er_bin <- rstanemax::exposure.response.sample.binary
#'
#' noise <- 1 + 0.5 * stats::rnorm(length(data_er_bin$conc))
#' data_er_bin$conc2 <- data_er_bin$conc * noise
#' data_er_bin$conc2[data_er_bin$conc2 < 0] <- 0
#'
#' ermod_bin_emax_exp_sel <-
#'   dev_ermod_bin_emax_exp_sel(
#'     data = data_er_bin,
#'     var_resp = "y",
#'     var_exp_candidates = c("conc", "conc2")
#'   )
#' }
#'
dev_ermod_bin_emax_exp_sel <- function(
    data,
    var_resp,
    var_exp_candidates,
    verbosity_level = 1,
    chains = 4,
    iter = 2000,
    gamma_fix = 1,
    e0_fix = NULL,
    emax_fix = NULL,
    priors = NULL,
    seed = sample.int(.Machine$integer.max, 1)) {
  fun_dev_ermod <-
    purrr::partial(
      dev_ermod_bin_emax,
      gamma_fix = gamma_fix,
      e0_fix = e0_fix,
      emax_fix = emax_fix,
      priors = priors,
      seed = seed
    )

  l_out <-
    .dev_ermod_exp_sel(
      data = data,
      var_resp = var_resp,
      var_exp_candidates = var_exp_candidates,
      verbosity_level = verbosity_level,
      chains = chains,
      iter = iter,
      fun_dev_ermod = fun_dev_ermod
    )

  new_ermod_bin_emax_exp_sel(l_out)
}
