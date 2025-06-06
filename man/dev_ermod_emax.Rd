% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dev_ermod_emax.R
\name{dev_ermod_emax}
\alias{dev_ermod_emax}
\alias{dev_ermod_bin_emax}
\title{Develop Emax model for continuous and binary endpoint}
\usage{
dev_ermod_emax(
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
  seed = sample.int(.Machine$integer.max, 1)
)

dev_ermod_bin_emax(
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
  seed = sample.int(.Machine$integer.max, 1)
)
}
\arguments{
\item{data}{Input data for E-R analysis}

\item{var_resp}{Response variable name in character}

\item{var_exposure}{Exposure variable names in character}

\item{l_var_cov}{a names list of categorical covariate variables in
character vector. See details in the \code{param.cov} argument of
\code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or \code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{gamma_fix}{Hill coefficient, default fixed to 1.
See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or \code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{e0_fix}{See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{emax_fix}{See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{priors}{See details in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}

\item{verbosity_level}{Verbosity level. 0: No output, 1: Display steps,
2: Display progress in each step, 3: Display MCMC sampling.}

\item{chains}{Number of chains for Stan.}

\item{iter}{Number of iterations for Stan.}

\item{seed}{Random seed for Stan model execution, see details in
\code{\link[rstan:stanmodel-method-sampling]{rstan::sampling()}} which is used in \code{\link[rstanemax:stan_emax]{rstanemax::stan_emax()}} or
\code{\link[rstanemax:stan_emax_binary]{rstanemax::stan_emax_binary()}}}
}
\value{
An object of class \code{ermod_emax}.or \code{ermod_bin_emax}.
}
\description{
These functions are used to develop an Emax model with continuous or binary
endpoint.
You can also specify covariates to be included in the model; note that only
categorical covariates are allowed.
}
\examples{
\dontshow{if (BayesERtools:::.if_run_ex_plot_er()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
data_er_cont <- rstanemax::exposure.response.sample

ermod_emax <-
  dev_ermod_emax(
    data = data_er_cont,
    var_exposure = "exposure",
    var_resp = "response"
  )

plot_er(ermod_emax, show_orig_data = TRUE)

data_er_cont_cov <- rstanemax::exposure.response.sample.with.cov

ermod_emax_w_cov <-
  dev_ermod_emax(
    data = data_er_cont_cov,
    var_exposure = "conc",
    var_resp = "resp",
    l_var_cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1")
  )
}
\dontshow{\}) # examplesIf}
\dontshow{if (BayesERtools:::.if_run_ex_plot_er()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
data_er_bin <- rstanemax::exposure.response.sample.binary

ermod_bin_emax <-
  dev_ermod_bin_emax(
    data = data_er_bin,
    var_exposure = "conc",
    var_resp = "y"
  )

plot_er(ermod_bin_emax, show_orig_data = TRUE)

ermod_bin_emax_w_cov <-
  dev_ermod_bin_emax(
    data = data_er_bin,
    var_exposure = "conc",
    var_resp = "y_cov",
    l_var_cov = list(emax = "sex")
  )
}
\dontshow{\}) # examplesIf}
}
