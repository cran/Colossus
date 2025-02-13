## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)

## -----------------------------------------------------------------------------
term_n <- c(0, 0, 1)
tform <- c("loglin", "loglin", "lin")
names <- c("dose0", "dose1", "dose2")
modelform <- "M"
tstart <- "t0"
tend <- "t1"
event <- "lung"

Model_Eq <- "cox(t0, t1, lung) ~ loglinear(dose0, dose1, 0) + linear(dose2, 1) + multiplicative()"
Model_Eq <- "cox(t0, t1, lung) ~ loglinear(dose0, dose1) + linear(dose2, 1)"

df <- data.table("dose0" = 1:4, "dose1" = 2:5, "dose2" = 3:6)
Convert_Model_Eq(Model_Eq, df)

