#' @name p_direction
#' @importFrom bayestestR p_direction
#' @export
bayestestR::p_direction

#' Probability of Direction (pd)
#'
#' Compute the **Probability of Direction** (***pd***). Although differently
#' expressed, this index is fairly similar (*i.e.*, is strongly correlated) to
#' the frequentist **p-value**. See [bayestestR::p_direction()] and
#' `vignette("overview_of_vignettes", package = "bayestestR")` >
#' "Probability of Direction (pd)" page for details.
#' For converting **pd** to a frequentist **p-value**,
#' see [bayestestR::pd_to_p()].
#'
#' For the class `ermod_bin_*`, it only calculates the **pd** for
#' the exposure variable.
#'
#' @export
#' @rdname p_direction
#' @param x An object of class \code{ermod_bin_*}
#' @param null The null hypothesis value. Default is 0.
#' @param as_num If `TRUE`, the output is converted to a numeric value.
#' @param as_p If `TRUE`, the p-direction (pd) values are converted to a
#' frequentist p-value using [bayestestR::pd_to_p()]. Only works when
#' `as_num = TRUE`.
#' @param direction What type of p-value is requested or provided with
#' as_p = TRUE. Can be `"two-sided"` (default, two tailed) or `"one-sided"`
#' (one tailed).
#' @param ... Additional arguments passed to [bayestestR::p_direction()].
#'
#' @return See [bayestestR::p_direction()] for details.
#' @examples
#' \donttest{
#' df_er_dr2 <-
#'   d_sim_binom_cov |>
#'   dplyr::filter(
#'     AETYPE == "dr2",
#'     ID %in% seq(1, 500, by = 5)
#'   ) |>
#'   dplyr::mutate(AUCss_1000 = AUCss / 1000, BHBA1C_5 = BHBA1C / 5)
#'
#' ermod_bin <- dev_ermod_bin(
#'   data = df_er_dr2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000",
#'   var_cov = "BHBA1C_5"
#' )
#'
#' p_direction(ermod_bin, as_num = TRUE, as_p = TRUE)
#' }
#'
p_direction.ermod_bin <- function(
    x,
    null = 0,
    as_p = FALSE,
    as_num = FALSE,
    direction = "two-sided",
    ...) {
  if (as_p && !as_num) {
    stop("as_p = TRUE only works when as_num = TRUE")
  }

  out_p_direction <-
    extract_mod(x) |>
    bayestestR::p_direction(
      parameters = extract_var_exposure(x),
      null = null,
      ...
    )

  if (!as_num) {
    return(out_p_direction)
  }

  out <- as.numeric(out_p_direction)

  if (as_p) out <- bayestestR::pd_to_p(out, direction = direction)

  return(out)
}
