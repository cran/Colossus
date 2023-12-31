% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cox_Regression.R
\name{RunCoxRegression_Single}
\alias{RunCoxRegression_Single}
\title{Performs basic Cox Proportional Hazards calculation with no derivative
\code{RunCoxRegression_Single} uses user provided data, time/event columns, vectors specifying the model, and options and returns the log-likelihood}
\usage{
RunCoxRegression_Single(
  df,
  time1,
  time2,
  event0,
  names,
  Term_n,
  tform,
  keep_constant,
  a_n,
  modelform,
  fir,
  control
)
}
\arguments{
\item{df}{a data.table containing the columns of interest}

\item{time1}{column used for time period starts}

\item{time2}{column used for time period end}

\item{event0}{column used for event status}

\item{names}{columns for elements of the model, used to identify data columns}

\item{Term_n}{term numbers for each element of the model}

\item{tform}{list of string function identifiers, used for linear/step}

\item{keep_constant}{binary values to denote which parameters to change}

\item{a_n}{list of initial parameter values, used to determine number of parameters}

\item{modelform}{string specifying the model type}

\item{fir}{term number for the initial term, used for models of the form T0*f(Ti) in which the order matters}

\item{control}{list of parameters controlling the convergence}
}
\value{
returns a list of the final results
}
\description{
Performs basic Cox Proportional Hazards calculation with no derivative
\code{RunCoxRegression_Single} uses user provided data, time/event columns, vectors specifying the model, and options and returns the log-likelihood
}
\examples{
library(data.table)
## basic example code reproduced from the starting-description vignette

df <- data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
           "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
             "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
          "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
                      "a"=c(0,   1,   1,   0,   1,   0,   1),
                      "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
                      "c"=c(10,  11,  10,  11,  12,  9,   11),
                      "d"=c(0,   0,   0,   1,   1,   1,   1))
# For the interval case
time1 <- "Starting_Age"
time2 <- "Ending_Age"
event <- "Cancer_Status"
names <- c('a','b','c','d')
Term_n <- c(0,1,1,2)
tform <- c("loglin","lin","lin","plin")
modelform <- "M"
fir <- 0
a_n <- c(1.1, -0.1, 0.2, 0.5) #used to test at a specific point

keep_constant <- c(0,0,0,0)

control <- list("Ncores"=2,'verbose'=FALSE, 'ties'='breslow','double_step'=1)

e <- RunCoxRegression_Single(df, time1, time2, event, names, Term_n, tform,
                             keep_constant, a_n, modelform, fir, control)

}
