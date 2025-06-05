## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)
library(survival)

## ----eval=TRUE----------------------------------------------------------------
data(reliability, package = "survival")

df <- capacitor
df$voltage <- (df$voltage - 200) / 150
df$temperature <- (df$temperature - 170) / 10
df$time <- (df$time - 216) / (1105 - 216)

t0 <- "%trunc%"
t1 <- "time"
event <- "status"

names <- c("temperature", "voltage")
tform <- c("loglin", "loglin")
control <- list("Ncores" = 1, "maxiter" = 100, "verbose" = 2)

a_n <- c(0.01, 0.01)
term_n <- c(0, 0)
keep_constant <- c(0, 0)
modelform <- "M"

e1 <- RunCoxRegression(
  df, t0, t1, event, names, term_n, tform, keep_constant,
  a_n, modelform,
  control = control
)
Interpret_Output(e1, 5)

names <- c("temperature", "voltage")
tform <- c("plin", "loglin")
a_n <- c(0.01, 0.01)

e2 <- RunCoxRegression(
  df, t0, t1, event, names, term_n, tform, keep_constant,
  a_n, modelform,
  control = control
)
Interpret_Output(e2, 5)

## ----eval=TRUE----------------------------------------------------------------
names <- c("temperature", "voltage")
tform <- c("loglin", "loglin")
ci_1 <- c(
  e1$beta_0[1] - 1.96 * e1$Standard_Deviation[1],
  e1$beta_0[1] + 1.96 * e1$Standard_Deviation[1]
)
ci_2 <- c(
  e1$beta_0[2] - 1.96 * e1$Standard_Deviation[2],
  e1$beta_0[2] + 1.96 * e1$Standard_Deviation[2]
)

a_n <- c(0.7599511, 1.9884051)
term_n <- c(0, 0)
keep_constant <- c(0, 0)
modelform <- "M"

model_control <- list(
  "basic" = FALSE, "maxstep" = 100,
  "log_bound" = TRUE, "alpha" = 0.05,
  "para_number" = 0, "manual" = TRUE
)
e <- RunCoxRegression_Omnibus(df, t0, t1, event, names,
  term_n = term_n,
  tform = tform, keep_constant = keep_constant,
  a_n = a_n, modelform = modelform,
  control = control, model_control = model_control
)
print("|------------------- Wald Estimate -------------------|")
print(ci_1)
Interpret_Output(e, 5)

a_n <- c(0.7599511, 1.9884051)
model_control <- list(
  "basic" = FALSE, "maxstep" = 100,
  "log_bound" = TRUE, "alpha" = 0.05,
  "para_number" = 1, "manual" = TRUE
)
e <- RunCoxRegression_Omnibus(df, t0, t1, event, names,
  term_n = term_n,
  tform = tform, keep_constant = keep_constant,
  a_n = a_n, modelform = modelform,
  control = control, model_control = model_control
)
print("|------------------- Likelihood Bound Estimate -------------------|")
print(ci_2)
Interpret_Output(e, 5)

## ----eval=TRUE----------------------------------------------------------------
names <- c("temperature", "voltage")
tform <- c("plin", "loglin")
ci_1 <- c(
  e2$beta_0[1] - 1.96 * e2$Standard_Deviation[1],
  e2$beta_0[1] + 1.96 * e2$Standard_Deviation[1]
)
ci_2 <- c(
  e2$beta_0[2] - 1.96 * e2$Standard_Deviation[2],
  e2$beta_0[2] + 1.96 * e2$Standard_Deviation[2]
)

a_n <- c(1.138152, 1.988403)
term_n <- c(0, 0)
keep_constant <- c(0, 0)
modelform <- "M"

model_control <- list(
  "basic" = FALSE, "maxstep" = 100,
  "log_bound" = TRUE, "alpha" = 0.05,
  "para_number" = 0, "manual" = TRUE
)
e <- RunCoxRegression_Omnibus(df, t0, t1, event, names,
  term_n = term_n,
  tform = tform, keep_constant = keep_constant,
  a_n = a_n, modelform = modelform,
  control = control, model_control = model_control
)
print("|------------------- Wald Estimate -------------------|")
print(ci_1)
Interpret_Output(e, 5)

a_n <- c(1.138152, 1.988403)
model_control <- list(
  "basic" = FALSE, "maxstep" = 100,
  "log_bound" = TRUE, "alpha" = 0.05,
  "para_number" = 1, "manual" = TRUE
)
e <- RunCoxRegression_Omnibus(df, t0, t1, event, names,
  term_n = term_n,
  tform = tform, keep_constant = keep_constant,
  a_n = a_n, modelform = modelform,
  control = control, model_control = model_control
)
print("|------------------- Wald Estimate -------------------|")
print(ci_2)
Interpret_Output(e, 5)

## ----eval=FALSE---------------------------------------------------------------
# fname <- "base_example.csv"
# df <- fread(fname)
# 
# time1 <- "entry"
# time2 <- "exit"
# event <- "event"
# names <- c("dose0", "dose1", "dose0")
# term_n <- c(0, 0, 1)
# tform <- c("loglin", "loglin", "lin")
# keep_constant <- c(0, 0, 1)
# a_n <- c(-1.493177, 5.020007, 1.438377)
# modelform <- "M"
# #
# model_control <- list()
# control <- list(
#   "ncores" = 2, "lr" = 0.75, "maxiters" = c(100, 100), "halfmax" = 5,
#   "epsilon" = 1e-6, "deriv_epsilon" = 1e-6, "abs_max" = 1.0,
#   "dose_abs_max" = 100.0, "verbose" = 2,
#   "ties" = "breslow"
# )
# 
# v0 <- sort(c((0:50 / 50 - 1.0) * 0.8, 1:50 / 50 * 3, 1.438377, -0.5909))
# for (v in v0) {
#   a_n <- c(-1.493177, 5.020007, v)
#   e <- RunCoxRegression_Omnibus(df, time1, time2, event, names,
#     term_n = term_n,
#     tform = tform, keep_constant = keep_constant, a_n = a_n,
#     modelform = modelform,
#     control = control, model_control = model_control
#   )
#   ll <- e$LogLik
#   beta <- e$beta_0
#   print(c(ll, beta[3]))
# }

## ----fig.width=7,fig.height=4-------------------------------------------------
x <- c(-0.8, -0.784, -0.768, -0.752, -0.736, -0.72, -0.704, -0.688, -0.672, -0.656, -0.64, -0.624, -0.608, -0.592, -0.5909, -0.576, -0.56, -0.544, -0.528, -0.512, -0.496, -0.48, -0.464, -0.448, -0.432, -0.416, -0.4, -0.384, -0.368, -0.352, -0.336, -0.32, -0.304, -0.288, -0.272, -0.256, -0.24, -0.224, -0.208, -0.192, -0.176, -0.16, -0.144, -0.128, -0.112, -0.096, -0.08, -0.064, -0.048, -0.032, -0.016, 0.0, 0.06, 0.12, 0.18, 0.24, 0.3, 0.36, 0.42, 0.48, 0.54, 0.6, 0.66, 0.72, 0.78, 0.84, 0.9, 0.96, 1.02, 1.08, 1.14, 1.2, 1.26, 1.32, 1.38, 1.438377, 1.44, 1.5, 1.56, 1.62, 1.68, 1.74, 1.8, 1.86, 1.92, 1.98, 2.04, 2.1, 2.16, 2.22, 2.28, 2.34, 2.4, 2.46, 2.52, 2.58, 2.64, 2.7, 2.76, 2.82, 2.88, 2.94, 3.0)
y <- c(-18500.53, -18499.829, -18499.273, -18498.831, -18498.482, -18498.21, -18497.995, -18497.832, -18497.71, -18497.621, -18497.56, -18497.519, -18497.498, -18497.49, -18497.4904, -18497.495, -18497.51, -18497.53, -18497.558, -18497.589, -18497.624, -18497.66, -18497.7, -18497.739, -18497.779, -18497.818, -18497.86, -18497.896, -18497.933, -18497.969, -18498.003, -18498.04, -18498.067, -18498.096, -18498.124, -18498.15, -18498.17, -18498.196, -18498.216, -18498.235, -18498.252, -18498.27, -18498.281, -18498.292, -18498.303, -18498.311, -18498.32, -18498.324, -18498.329, -18498.332, -18498.334, -18498.33, -18498.33, -18498.31, -18498.27, -18498.23, -18498.18, -18498.13, -18498.07, -18498.01, -18497.96, -18497.9, -18497.84, -18497.78, -18497.73, -18497.68, -18497.63, -18497.59, -18497.56, -18497.52, -18497.49, -18497.47, -18497.45, -18497.44, -18497.43, -18497.429487, -18497.43, -18497.43, -18497.44, -18497.45, -18497.47, -18497.5, -18497.52, -18497.56, -18497.6, -18497.64, -18497.69, -18497.74, -18497.8, -18497.86, -18497.93, -18498.0, -18498.07, -18498.15, -18498.23, -18498.32, -18498.41, -18498.5, -18498.6, -18498.7, -18498.81, -18498.92, -18499.03)

df <- data.table("x" = x, "y" = y)

g <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_line(color = "black", alpha = 1, "linewidth" = 1.5) +
  ggplot2::labs(x = "Linear Parameter Value", y = "Log-Likelihood") +
  ggplot2::ggtitle("Multi-Peak Curve")
g

## ----eval=FALSE---------------------------------------------------------------
# fname <- "base_example.csv"
# df <- fread(fname)
# 
# time1 <- "entry"
# time2 <- "exit"
# event <- "event"
# names <- c("dose0", "dose1", "dose0")
# term_n <- c(0, 0, 1)
# tform <- c("loglin", "loglin", "lin")
# keep_constant <- c(0, 0, 0)
# a_n <- c(-1.493177, 5.020007, 1.438377)
# modelform <- "M"
# #
# control <- list(
#   "ncores" = 2, "lr" = 0.75, "maxiter" = 100, "halfmax" = 5,
#   "verbose" = 2
# )
# 
# model_control <- list("maxstep" = 20, "alpha" = 0.005, "para_number" = 2, "step_size" = 0.5)
# e <- CoxCurveSolver(df, time1, time2, event, names,
#   term_n = term_n, tform = tform,
#   keep_constant = keep_constant, a_n = a_n, modelform = modelform,
#   control = control, model_control = model_control
# )

