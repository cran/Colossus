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

## -----------------------------------------------------------------------------
#
data(cancer, package = "survival")
df <- veteran

# Make the same adjustments as Epicure example 6.5
karno <- df$karno
karno[93] <- 20
df$karno <- karno
df$trt <- df$trt - 1
df$trt <- as.integer(df$trt == 0)
cell_string <- df$celltype
cell <- case_when(
  cell_string == "squamous" ~ 1,
  cell_string == "smallcell" ~ 2,
  cell_string == "adeno" ~ 3,
  cell_string == "large" ~ 0
)
df$cell <- cell

df$karno50 <- df$karno - 50
# Convert the cell column into factor columns
fcols <- c("cell")
val <- factorize(df, fcols) # Colossus function
df <- val$df

t0 <- "%trunc%"
t1 <- "time"
event <- "status"

names <- c(
  "karno50", "trt"
)
tform_1 <- c(
  "loglin", "loglin"
)

term_n <- c(0, 0)
a_n <- c(0.1, 0.1)

## -----------------------------------------------------------------------------
control <- list(verbose = 2, maxiters = c(25, 25))
model_control <- list("strata" = T, "conditional_threshold" = 100)
e0 <- RunCaseControlRegression_Omnibus(
  df, t0, t1, event,
  names = names, tform = tform_1,
  strat_col = "cell", model_control = model_control,
  control = control, term_n = term_n, a_n = a_n
)

model_control <- list("strata" = T, "conditional_threshold" = 40)
e1 <- RunCaseControlRegression_Omnibus(
  df, t0, t1, event,
  names = names, tform = tform_1,
  strat_col = "cell", model_control = model_control,
  control = control, term_n = term_n, a_n = a_n
)

model_control <- list("strata" = T, "conditional_threshold" = 0)
e2 <- RunCaseControlRegression_Omnibus(
  df, t0, t1, event,
  names = names, tform = tform_1,
  strat_col = "cell", model_control = model_control,
  control = control, term_n = term_n, a_n = a_n
)

Interpret_Output(e0)
Interpret_Output(e1)
Interpret_Output(e2)

