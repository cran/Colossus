## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)
library(parallel)

## ----eval=FALSE---------------------------------------------------------------
# df_Dose <- fread("EX_DOSE.csv")

## ----eval=FALSE---------------------------------------------------------------
# model_ERR <- Cox(age_entry, age_exit, nonCLL) ~ plinear(cumulative_dose, 0) + loglinear(factor(SES_CAT), factor(YOB_CAT), sexm)

## ----eval=FALSE---------------------------------------------------------------
# control <- list("Ncores" = 2, "maxiter" = 100, "verbose" = 2, "epsilon" = 1e-9, "der_epsilon" = 1e-9)

## ----eval=FALSE---------------------------------------------------------------
# e <- CoxRun(model_ERR, df_dose, control = control)
# print(e)

## ----eval=FALSE---------------------------------------------------------------
# # HR
# model_HR <- Cox(age_entry, age_exit, nonCLL) ~ loglinear(cumulative_dose, factor(SES_CAT), factor(YOB_CAT), sexm)
# e <- CoxRun(model_HR, df_dose, control = control)
# print(e)
# 
# # Categorical
# model_categ <- Cox(age_entry, age_exit, nonCLL) ~ loglinear(factor(dose_cat), factor(SES_CAT), factor(YOB_CAT), sexm)
# e <- CoxRun(model_categ, df_dose, control = control)
# print(e)

