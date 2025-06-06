% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utility.R
\name{Likelihood_Ratio_Test}
\alias{Likelihood_Ratio_Test}
\title{Defines the likelihood ratio test}
\usage{
Likelihood_Ratio_Test(alternative_model, null_model)
}
\arguments{
\item{alternative_model}{the new model of interest in list form, output from a Poisson regression}

\item{null_model}{a model to compare against, in list form}
}
\value{
returns the score statistic
}
\description{
\code{Likelihood_Ratio_Test} uses two models and calculates the ratio
}
\examples{
library(data.table)
# In an actual example, one would run two seperate RunCoxRegression regressions,
#    assigning the results to e0 and e1
e0 <- list("name" = "First Model", "LogLik" = -120)
e1 <- list("name" = "New Model", "LogLik" = -100)
score <- Likelihood_Ratio_Test(e1, e0)

}
