% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{Intercept_Bound}
\alias{Intercept_Bound}
\title{Utility function to keep intercept parameters within the range of possible values
\code{Intercept_Bound} Called to update the parameter list in the event that intercepts leave the bounds of possible values}
\usage{
Intercept_Bound(
  nthreads,
  totalnum,
  beta_0,
  dbeta,
  dfc,
  df0,
  KeepConstant,
  debugging,
  tform
)
}
\arguments{
\item{nthreads}{number of threads available}

\item{totalnum}{total number of parameters}

\item{beta_0}{parameter estimates}

\item{dbeta}{parameter change vector}

\item{dfc}{vector matching subterm number to matrix column}

\item{df0}{matrix of covariate values}

\item{KeepConstant}{binary vector to identify constant parameters}

\item{debugging}{additional boolean for verbosity in testing}

\item{tform}{subterm types}
}
\value{
Updates vector in place: parameter vector
}
\description{
Utility function to keep intercept parameters within the range of possible values
\code{Intercept_Bound} Called to update the parameter list in the event that intercepts leave the bounds of possible values
}
