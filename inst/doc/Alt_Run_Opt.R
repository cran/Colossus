## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)

## ----eval=FALSE---------------------------------------------------------------
# Strat_Col <- "e"
# e <- RunCoxRegression_Strata(
#   df, time1, time2, event, names, term_n, tform, keep_constant,
#   a_n, modelform,
#   control = control, strat_col = Strat_Col
# )

## ----eval=FALSE---------------------------------------------------------------
# Strat_Col <- c("e")
# e <- RunPoissonRegression_Strata(
#   df, pyr, event, names, term_n, tform, keep_constant,
#   a_n, modelform,
#   control = control, strat_col = Strat_Col
# )

## ----eval=FALSE---------------------------------------------------------------
# e <- RunCoxRegression_Basic(
#   df, time1, time2, event, names,
#   keep_constant, a_n,
#   control = control
# )

## ----eval=FALSE---------------------------------------------------------------
# e <- RunCoxRegression_Single(
#   df, time1, time2, event, names, term_n, tform,
#   a_n, modelform,
#   control = control
# )
# 
# e <- RunPoissonRegression_Single(
#   df, pyr, event, names, term_n, tform,
#   a_n, modelform,
#   control = control
# )

## ----eval=FALSE---------------------------------------------------------------
# pdata <- finegray(Surv(time2, event) ~ ., data = df)
# 
# e <- RunCoxRegression_CR(
#   pdata, "fgstart", "fgstop", "fgstatus", names, term_n, tform, keep_constant,
#   a_n, modelform,
#   control = control, cens_weight = "fgwt"
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
names_e0 <- c("fac")
names_e1 <- c("fac")
names_shared <- c("t0", "t0")
term_n_e0 <- c(0)
term_n_e1 <- c(0)
term_n_shared <- c(0, 0)
tform_e0 <- c("loglin")
tform_e1 <- c("loglin")
tform_shared <- c("quad_slope", "loglin_top")
keep_constant_e0 <- c(0)
keep_constant_e1 <- c(0)
keep_constant_shared <- c(0, 0)
a_n_e0 <- c(-0.1)
a_n_e1 <- c(0.1)
a_n_shared <- c(0.001, -0.02)
name_list <- list("shared" = names_shared, "e0" = names_e0, "e1" = names_e1)
term_n_list <- list("shared" = term_n_shared, "e0" = term_n_e0, "e1" = term_n_e1)
tform_list <- list("shared" = tform_shared, "e0" = tform_e0, "e1" = tform_e1)
keep_constant_list <- list(
  "shared" = keep_constant_shared,
  "e0" = keep_constant_e0, "e1" = keep_constant_e1
)
a_n_list <- list("shared" = a_n_shared, "e0" = a_n_e0, "e1" = a_n_e1)

## ----eval=TRUE----------------------------------------------------------------
Joint_Multiple_Events(
  df, events, name_list, term_n_list,
  tform_list, keep_constant_list, a_n_list
)

## ----eval=TRUE----------------------------------------------------------------
modelform <- "M"
control <- list(
  "ncores" = 1, "lr" = 0.75, "maxiter" = 10, "halfmax" = 5, "epsilon" = 1e-6,
  "deriv_epsilon" = 1e-6, "verbose" = 2
)
Strat_Col <- "f"
e <- RunPoissonRegression_Joint_Omnibus(
  df, pyr, events, name_list, term_n_list,
  tform_list, keep_constant_list, a_n_list,
  modelform,
  control = control, strat_col = Strat_Col
)
Interpret_Output(e)

## ----eval=FALSE---------------------------------------------------------------
# a_n <- list(c(1, 1, 1), c(1, 2, 1), c(1, 2, 2), c(2, 1, 1))
# 
# # runs each (4) starts 1 iteration, and then runs the best 5 iterations
# control$maxiter <- 5
# # runs each (4) starts 1 iteration, and then runs the best 5 iterations
# control$maxiters <- c(1, 1, 1, 1, 5)
# # runs each (4) starts 5 iterations, and then runs the best 5 iterations
# control$maxiters <- c(5, 5, 5, 5, 5)
# 
# e <- RunCoxRegression_Omnibus(df, time1, time2, event,
#   names, term_n, tform, keep_constant,
#   a_n, modelform,
#   control = control
# )

