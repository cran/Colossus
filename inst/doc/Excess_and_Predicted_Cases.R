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
# model <- Pois(pyr, event) ~ loglinear(x, 0) + plinear(D, 1) + Multiplicative()
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

## ----eval=FALSE---------------------------------------------------------------
# e <- EventAssignment(poisres, df, check_num = 2, z = 1.96)
# 
# e_lower <- e$lower_limit$caused
# e_mid <- e$midpoint$caused
# e_high <- e$upper_limit$caused
# 
# EX_low <- e_lower[, 2]
# EX_mid <- e_mid[, 2]
# EX_high <- e_high[, 2]
# 
# p_bound <- LikelihoodBound(poisres, df, para_number = 2, alpha = 0.05)
# e <- EventAssignment(p_bound, df)

