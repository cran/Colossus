% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{Write_Time_Dep}
\alias{Write_Time_Dep}
\title{Generates csv file with time-dependent columns
\code{Write_Time_Dep} Called directly from R, Defines a new matrix which interpolates time-dependent values on a grid}
\usage{
Write_Time_Dep(
  df0_Times,
  df0_dep,
  df0_const,
  df0_event,
  dt,
  filename,
  tform_tdep,
  tu,
  iscox,
  nthreads
)
}
\arguments{
\item{df0_Times}{Matrix with (starting time, ending time)}

\item{df0_dep}{matrix with pairs of (covariate at start, covariate at end) for each time-dependent covariate}

\item{df0_const}{matrix with values that are held constant}

\item{df0_event}{matrix with event status, zero up to the last entry for each original row}

\item{dt}{spacing in time}

\item{filename}{file to save the data to}

\item{tform_tdep}{vector with types of time dependent variables}

\item{tu}{Event time vector}

\item{iscox}{boolean of cox formatting is used}

\item{nthreads}{number of threads available}
}
\value{
saves a dataframe to be used with time-dependent covariate analysis
}
\description{
Generates csv file with time-dependent columns
\code{Write_Time_Dep} Called directly from R, Defines a new matrix which interpolates time-dependent values on a grid
}
