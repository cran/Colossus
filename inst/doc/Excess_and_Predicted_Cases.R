## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)

## ----eval=FALSE---------------------------------------------------------------
# a_n <- c(0.1, 0.1)
# model <- Pois(pyr, event) ~ loglinear(x, 0) + linear(D, 1) + Multiplicative()
# poisres <- PoisRun(model, df, a_n = a_n)

## ----eval=FALSE---------------------------------------------------------------
# e <- EventAssignment(poisres, df)
# 
# e0 <- e$predict
# e1 <- e$caused
# 
# BK <- e0[, 1]
# EX <- e0[, 2]
# Total <- e0[, 3]

