## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
Sys.setenv("OMP_THREAD_LIMIT" = 1) # Reducing core use, to avoid accidental use of too many cores
library(Colossus)
library(data.table)
library(survival)
library(dplyr)

## ----eval = TRUE--------------------------------------------------------------
#
data(cancer, package = "survival")
veteran %>% setDT()
df <- copy(veteran)
# Make the same adjustments as Epicure example 6.5
karno <- df$karno
karno[93] <- 20
df$karno <- karno
df$trt <- df$trt - 1
df$trt <- as.integer(df$trt == 0)
cell_lvl <- c("large", "squamous", "smallcell", "adeno")
df$cell <- as.integer(factor(df$celltype, level = cell_lvl)) - 1

df$karno50 <- df$karno - 50

## ----eval = TRUE--------------------------------------------------------------
model <- CaseControl_Strata(status, cell) ~ loglinear(karno50, trt)


control <- list(verbose = 2, maxiters = c(25, 25), ncores = 1)
e0 <- CaseControlRun(model, df, control = control, conditional_threshold = 100)
e1 <- CaseControlRun(model, df, control = control, conditional_threshold = 40)
e2 <- CaseControlRun(model, df, control = control, conditional_threshold = 0)


print(e0)
print(e1)
print(e2)

