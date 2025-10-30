## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)
library(parallel)

## -----------------------------------------------------------------------------
names <- c("a", "b", "c", "d")
term_n <- c(0, 1, 1, 2)
tform <- c("loglin", "lin", "lin", "plin")
modelform <- "M"

a_n <- c(0.1, 0.1, 0.1, 0.1)

model <- outcome ~ loglinear(a, 0) + linear(b, c, 1) + plinear(d, 2) + multiplicative()

## -----------------------------------------------------------------------------
df <- data.table(
  "UserID" = c(112, 114, 213, 214, 115, 116, 117),
  "Starting_Age" = c(18, 20, 18, 19, 21, 20, 18),
  "Ending_Age" = c(30, 45, 57, 47, 36, 60, 55),
  "Cancer_Status" = c(0, 0, 1, 0, 1, 0, 0),
  "a" = c(0, 1, 1, 0, 1, 0, 1),
  "b" = c(1, 1.1, 2.1, 2, 0.1, 1, 0.2),
  "c" = c(10, 11, 10, 11, 12, 9, 11),
  "d" = c(0, 0, 0, 1, 1, 1, 1)
)
# For the interval case
time1 <- "Starting_Age"
time2 <- "Ending_Age"
event <- "Cancer_Status"

# Supposing we had left truncated data the following would change
time1 <- "Starting_Age"
time2 <- "%trunc%"

# and with right truncated data the following is used
time1 <- "%trunc%"
time2 <- "Ending_Age"

# setting back to normal
time1 <- "Starting_Age"
time2 <- "Ending_Age"

## -----------------------------------------------------------------------------
df$Person_Years <- df$Ending_Age - df$Starting_Age
pyr <- "Person_Years"
event <- "Cancer_Status"

## -----------------------------------------------------------------------------
# For the interval case
time1 <- "Starting_Age"
time2 <- "Ending_Age"
event <- "Cancer_Status"
RHS <- Cox(Starting_Age, Ending_Age, Cancer_Status) ~ risk_factors

# Supposing we had left truncated data the following would change
time1 <- "Starting_Age"
time2 <- "%trunc%"
RHS <- Cox(tstart = Starting_Age, event = Cancer_Status) ~ risk_factors

# and with right truncated data the following is used
time1 <- "%trunc%"
time2 <- "Ending_Age"
RHS <- Cox(Ending_Age, Cancer_Status) ~ risk_factors

## -----------------------------------------------------------------------------
keep_constant <- c(0, 0, 0, 0)

control <- list(
  "ncores" = 1, "lr" = 0.75, "maxiter" = 100, "halfmax" = 5, "epsilon" = 1e-3,
  "dbeta_max" = 0.5, "deriv_epsilon" = 1e-3, "step_max" = 1.0,
  "thres_step_max" = 100.0, "verbose" = 2, "ties" = "breslow", "double_step" = 1
)

## -----------------------------------------------------------------------------
# assuming the table of covariates is stored in a data.table "df"

model_cox <- Cox(Starting_Age, Ending_Age, Cancer_Status) ~ loglinear(a, 0) + linear(b, c, 1) + plinear(d, 2) + multiplicative()

e <- CoxRun(model_cox, df, a_n = a_n, control = control)
print(e)

# or a Poisson model regression
model_pois <- Poisson(Person_Years, Cancer_Status)  ~ loglinear(a, 0) + linear(b, c, 1) + plinear(d, 2) + multiplicative()
e <- PoisRun(model_pois, df, a_n = a_n, control = control)
print(e)

