% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{Calculate_Sides_STRATA_Single_CR}
\alias{Calculate_Sides_STRATA_Single_CR}
\title{Utility function to calculate repeated values used in Cox Log-Likelihood calculation with STRATA and without derivative and with competing risks
\code{Calculate_Sides_STRATA_Single_CR} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group but not derivatives but with competing risks}
\usage{
Calculate_Sides_STRATA_Single_CR(
  RiskFail,
  RiskGroup,
  totalnum,
  ntime,
  R,
  Rls1,
  Lls1,
  cens_weight,
  nthreads,
  debugging,
  STRATA_vals,
  KeepConstant
)
}
\arguments{
\item{RiskFail}{matrix of indexes for event rows in each risk group}

\item{RiskGroup}{list of string indices for every risk group}

\item{totalnum}{total number of parameters}

\item{ntime}{number of risk groups}

\item{R}{risk vector}

\item{Rls1}{First Risk sum storage}

\item{Lls1}{Second Risk sum storage}

\item{cens_weight}{vector of censoring weights}

\item{nthreads}{number of threads available}

\item{debugging}{additional boolean for verbosity in testing}

\item{STRATA_vals}{vector of strata identifier values}

\item{KeepConstant}{binary vector to identify constant parameters}
}
\value{
Updates matrices in place: risk storage matrices
}
\description{
Utility function to calculate repeated values used in Cox Log-Likelihood calculation with STRATA and without derivative and with competing risks
\code{Calculate_Sides_STRATA_Single_CR} Called to update repeated sum calculations, Uses list of event rows and risk matrices, Performs calculation of sums of risk in each group but not derivatives but with competing risks
}
