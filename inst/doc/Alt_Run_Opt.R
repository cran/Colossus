## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)

## ----eval=FALSE---------------------------------------------------------------
# Strat_Col <- "s0"
# e <- CoxRun_Strata(Cox(time1, time2, event, s0) ~ loglinear(dose), df,
#   a_n = a_n, control = control
# )
# Strat_Cols <- c("s0", "s1", "s2")
# e <- CoxRun_Strata(Cox(time1, time2, event, c(s0, s1, s2)) ~ loglinear(dose), df,
#   a_n = a_n, control = control
# )

## ----eval=FALSE---------------------------------------------------------------
# Strat_Col <- c("e")
# e <- PoisRun(Poisson_Strata(pyr, event, e) ~ loglinear(dose), df,
#   a_n = a_n, control = control
# )

## ----eval=FALSE---------------------------------------------------------------
# e <- CoxRun(Cox(time1, time2, event) ~ loglinear(dose), df,
#   a_n = a_n, control = control, single = TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# pdata <- finegray(Surv(time2, event) ~ ., data = df)
# 
# e <- CoxRun(FineGray(fgstart, fgstop, fgstatus, fgwt) ~ loglinear(dose), pdata,
#   a_n = a_n, control = control
# )

## ----eval=TRUE----------------------------------------------------------------
a <- c(0, 0, 0, 1, 1, 1)
b <- c(1, 1, 1, 2, 2, 2)
c <- c(0, 1, 2, 2, 1, 0)
d <- c(1, 1, 0, 0, 1, 1)
e <- c(0, 1, 1, 1, 0, 0)
df <- data.table("t0" = a, "t1" = b, "e0" = c, "e1" = d, "fac" = e)
time1 <- "t0"
time2 <- "t1"
df$pyr <- df$t1 - df$t0
pyr <- "pyr"
events <- c("e0", "e1")

## ----eval=TRUE----------------------------------------------------------------
model_1 <- Pois(pyr, e0) ~ loglin(fac, 0)
model_2 <- Pois(pyr, e1) ~ loglin(fac, 0)
model_s <- Pois(pyr) ~ plinear(t0, 0)
formula_list <- list(model_1, model_2, "shared" = model_s)

## ----eval=TRUE----------------------------------------------------------------
get_form_joint(formula_list, df)

## ----eval=TRUE----------------------------------------------------------------
control <- list(
  "ncores" = 1, "lr" = 0.75, "maxiter" = 10, "halfmax" = 5, "epsilon" = 1e-6,
  "deriv_epsilon" = 1e-6, "verbose" = 2, "ncores" = 2
)
e <- PoisRunJoint(formula_list, df, control = control)
print(e)

