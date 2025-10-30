## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)

## -----------------------------------------------------------------------------
model <- Cox(t0, t1, event) ~ loglinear(a, r0, s0)

a_n <- c(0.1, 0.1, 0.1)

## ----eval=FALSE---------------------------------------------------------------
# dose_index <- c("r0", "s0")
# # The two columns in the model to replace are the radiation and sleeping covariates
# dose_realizations <- matrix(
#   c("r0", "r1", "r2", "r3", "r4", "s0", "s1", "s2", "s3", "s4"),
#   nrow = 2
# )
# # columns to be used for realizations 0-4, rows for each column being replaced

## ----eval=FALSE---------------------------------------------------------------
# e_fma <- CoxRunMulti(model, df, a_n = a_n, realization_columns = realization_columns, realization_index = realization_index, fma = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# e_mcml <- CoxRunMulti(model, df, a_n = a_n, realization_columns = realization_columns, realization_index = realization_index, mcml = TRUE)

