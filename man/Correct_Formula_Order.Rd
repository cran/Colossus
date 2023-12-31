% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utility.R
\name{Correct_Formula_Order}
\alias{Correct_Formula_Order}
\title{Corrects the order of terms/formula/etc
\code{Correct_Formula_Order} checks the order of formulas given and corrects any ordering issues}
\usage{
Correct_Formula_Order(
  Term_n,
  tform,
  keep_constant,
  a_n,
  names,
  der_iden = 0,
  verbose = FALSE
)
}
\arguments{
\item{Term_n}{term numbers for each element of the model}

\item{tform}{list of string function identifiers, used for linear/step}

\item{keep_constant}{binary values to denote which parameters to change}

\item{a_n}{list of initial parameter values, used to determine number of parameters}

\item{names}{columns for elements of the model, used to identify data columns}

\item{der_iden}{number for the subterm to test derivative at, only used for testing runs with a single varying parameter}

\item{verbose}{boolean to control if additional information is printed to the console}
}
\value{
returns the corrected lists
}
\description{
Corrects the order of terms/formula/etc
\code{Correct_Formula_Order} checks the order of formulas given and corrects any ordering issues
}
\examples{
library(data.table)
## basic example code reproduced from the starting-description vignette

Term_n <- c(0,1,1,0,0)
tform <- c("loglin",'quad_slope','lin', "lin_int", "lin_slope")
keep_constant <- c(0,0,0,1,0)
a_n <- c(1,2,3,4,5)
names <- c("a","a","a","a","a")
val <- Correct_Formula_Order(Term_n, tform, keep_constant, a_n, names)
Term_n <- val$Term_n
tform <- val$tform
keep_constant <- val$keep_constant
a_n <- val$a_n
der_iden <- val$der_iden
names <- val$names

}
