## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
Sys.setenv("OMP_THREAD_LIMIT" = 1) # Reducing core use, to avoid accidental use of too many cores
library(Colossus)
library(data.table)
if (system.file(package = "survival") != "") {
  library(survival)
}
library(dplyr)

## ----eval = TRUE--------------------------------------------------------------
#
if (system.file(package = "survival") != "") {
  veteran %>% setDT()
  df <- copy(veteran)
} else {
  karno <- c(60, 70, 60, 60, 70, 20, 40, 80, 50, 70, 60, 40, 30, 80, 70, 60, 60, 40, 80, 60, 40, 60, 60, 30, 80, 30, 50, 60, 80, 40, 20, 80, 30, 75, 70, 60, 30, 60, 80, 60, 70, 50, 50, 40, 40, 20, 70, 40, 80, 80, 50, 80, 30, 80, 50, 80, 50, 70, 60, 40, 80, 80, 70, 90, 90, 80, 80, 70, 60, 90, 80, 80, 50, 50, 70, 70, 20, 60, 90, 30, 20, 70, 90, 80, 50, 70, 60, 90, 50, 30, 70, 20, 30, 60, 40, 30, 20, 60, 70, 80, 85, 70, 70, 70, 50, 30, 40, 40, 40, 99, 80, 60, 60, 60, 60, 50, 70, 10, 40, 70, 90, 80, 50, 40, 40, 60, 70, 30, 60, 30, 60, 80, 75, 60, 70, 80, 30)
  trt <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  cell <- c("squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "squamous", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "smallcell", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "adeno", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large", "large")
  status <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  df <- data.table(
    "karno" = karno,
    "trt" = trt,
    "cell" = cell,
    "status" = status
  )
}
# Make the same adjustments as Epicure example 6.5
karno <- df$karno
karno[93] <- 20
df$karno <- karno
df$trt <- df$trt - 1
df$trt <- as.integer(df$trt == 0)
cell_lvl <- c("large", "squamous", "smallcell", "adeno")
df$cell <- as.integer(factor(df$celltype, level = cell_lvl)) - 1
df$karno50 <- df$karno - 50

df$trial <- 1

## ----eval = TRUE--------------------------------------------------------------
df$cell <- factor(df$cell, levels = c(-1, 0, 1, 2, 3))

model <- Logit(trial, status) ~ loglinear(cell)
model <- Logit(status) ~ loglinear(cell)

control <- list(verbose = 0, ncores = 1)
e <- LogisticRun(model, df, control = control)
print(e)

## ----eval = TRUE--------------------------------------------------------------
a_n <- c(0.1, 0.1, 0.1, 0.1)
e <- LogisticRun(model, df, control = control, a_n = a_n, link = "odds")
print(e)

a_n <- c(-1, -1, -1, -1)
e <- LogisticRun(model, df, control = control, a_n = a_n, link = "ident")
print(e)

a_n <- c(0.1, 0.1, 0.1, 0.1)
e <- LogisticRun(model, df, control = control, a_n = a_n, link = "loglink")
print(e)

