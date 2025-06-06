% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dev_ermod_emax.R
\name{dev_ermod_emax_exp_sel}
\alias{dev_ermod_emax_exp_sel}
\alias{dev_ermod_bin_emax_exp_sel}
\title{Exposure metrics selection for Emax models}
\usage{
dev_ermod_emax_exp_sel(
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
  seed = sample.int(.Machine$integer.max, 1)
)

dev_ermod_bin_emax_exp_sel(
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
  seed = sample.int(.Machine$integer.max, 1)
)
}
\arguments{
\item{data}{Input data for E-R analysis}

\item{var_resp}{Response variable name in character}

\item{var_exp_candidates}{Candidate exposure variable names
in character vector}

\item{verbosity_level}{Verbosity level. 0: No output, 1: Display steps,
2: Display progress in each step, 3: Display MCMC sampling.}

\item{chains}{Number of chains for Stan.}

\item{iter}{Number of iterations for Stan.}

\item{gamma_fix}{Hill coefficient, default fixed to 1.
See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or \code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{e0_fix}{See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{emax_fix}{See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{priors}{See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{seed}{Random seed for Stan model execution, see details in
\code{\link[rstan:stanmodel-method-sampling]{rstan::sampling()}} which is used in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}
}
\value{
An object of class \code{ermod_emax_exp_sel} or \code{ermod_bin_emax_exp_sel}.
}
\description{
This functions is used to develop an Emax model with binary and continuous
endpoint, using various exposure metrics and selecting the best one.
}
\examples{
\donttest{
data_er_cont <- rstanemax::exposure.response.sample
noise <- 1 + 0.5 * stats::rnorm(length(data_er_cont$exposure))
data_er_cont$exposure2 <- data_er_cont$exposure * noise
# Replace exposure < 0 with 0
data_er_cont$exposure2[data_er_cont$exposure2 < 0] <- 0

ermod_emax_exp_sel <-
  dev_ermod_emax_exp_sel(
    data = data_er_cont,
    var_resp = "response",
    var_exp_candidates = c("exposure", "exposure2")
  )

ermod_emax_exp_sel
}

\donttest{
data_er_bin <- rstanemax::exposure.response.sample.binary

noise <- 1 + 0.5 * stats::rnorm(length(data_er_bin$conc))
data_er_bin$conc2 <- data_er_bin$conc * noise
data_er_bin$conc2[data_er_bin$conc2 < 0] <- 0

ermod_bin_emax_exp_sel <-
  dev_ermod_bin_emax_exp_sel(
    data = data_er_bin,
    var_resp = "y",
    var_exp_candidates = c("conc", "conc2")
  )
}

}
