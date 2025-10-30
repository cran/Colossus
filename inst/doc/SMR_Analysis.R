## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)
library(survival)
library(dplyr)

## ----eval=TRUE----------------------------------------------------------------
data(cancer, package = "survival")
cancer %>% setDT()
df <- copy(cancer)

cancer$status <- as.integer(cancer$status == 2)
cancer$time <- cancer$time / 100

cancer$erate <- 0.5

## ----eval=TRUE----------------------------------------------------------------
a_n <- c(1)
control <- list(
  "ncores" = 1, "maxiter" = 20, "halfmax" = 5, "epsilon" = 1e-9,
  "deriv_epsilon" = 1e-9, "verbose" = 2
)
e <- PoisRun(Poisson(time, status) ~ linear(erate), cancer, a_n = a_n, control = control)
print(e)

## ----eval=TRUE----------------------------------------------------------------
a_n <- c(1, 1)

e <- PoisRun(Poisson(time, status) ~ linear(erate, 0) + linear(sex, 1), cancer, a_n = a_n, control = control)
print(e)

