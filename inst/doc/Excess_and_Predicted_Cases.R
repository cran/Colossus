## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)

## -----------------------------------------------------------------------------
names <- c("x", "D")
term_n <- c(0, 1)
tform <- c("loglin", "lin")
modelform <- "M"
fir <- 0

a_n <- c(0.1, 0.1)

## ----eval=FALSE---------------------------------------------------------------
#  names <- c("x", "D")
#  term_n <- c(0, 1)
#  tform <- c("loglin", "lin")
#  modelform <- "M"
#  fir <- 0
#  a_n <- c(0.1, 0.1)
#  keep_constant <- rep(0, length(names))
#  der_iden <- 0
#  control <- list("Ncores" = 2, "maxiters" = c(1, 1), "verbose" = F, "abs_max" = 0.1)
#  model_control <- list("strata" = FALSE, "basic" = FALSE, "single" = FALSE, "CR" = FALSE)
#  e <- RunPoissonEventAssignment(df, pyr, event, names, Term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
#  
#  e0 <- e$predict
#  e1 <- e$caused
#  
#  BK <- e0[, 1]
#  EX <- e0[, 2]
#  Total <- e0[, 3]

