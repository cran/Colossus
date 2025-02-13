## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(survival)

## ----eval=TRUE----------------------------------------------------------------
data(cancer, package = "survival")

cancer$status <- as.integer(cancer$status == 2)
cancer$time <- cancer$time / 100

cancer$erate <- 0.5

## ----eval=TRUE----------------------------------------------------------------
pyr <- "time"
event <- "status"
names <- c("erate")
term_n <- c(0)
tform <- c("lin")
keep_constant <- c(0)
a_n <- c(1)
modelform <- "M"
fir <- 0
der_iden <- 0
control <- list(
  "ncores" = 1, "maxiter" = 20, "halfmax" = 5, "epsilon" = 1e-9,
  "deriv_epsilon" = 1e-9, "verbose" = 1
)
e <- RunPoissonRegression_Omnibus(
  cancer, pyr, event, names, term_n, tform,
  keep_constant, a_n, modelform, fir, der_iden, control
)

print(e$beta_0)

## ----eval=TRUE----------------------------------------------------------------
names <- c("erate", "sex")
term_n <- c(0, 1)
tform <- c("lin", "lin")
keep_constant <- c(1, 0)
a_n <- c(1, 1)
e <- RunPoissonRegression_Omnibus(
  cancer, pyr, event, names, term_n, tform,
  keep_constant, a_n, modelform,
  control = control
)
print(e$beta_0)

## ----eval=TRUE----------------------------------------------------------------
names <- c("erate", "sex")
term_n <- c(0, 1)
tform <- c("lin", "lin")
keep_constant <- c(0, 0)
a_n <- c(1, 1)

e <- RunPoissonRegression_Omnibus(
  cancer, pyr, event, names, term_n, tform,
  keep_constant, a_n, modelform,
  control = control
)

print(e$beta_0)

