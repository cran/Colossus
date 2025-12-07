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

## ----eval = TRUE--------------------------------------------------------------
#
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

df$trial <- 1

## ----eval = TRUE--------------------------------------------------------------
df$cell <- factor(df$cell, levels = c(-1, 0, 1, 2, 3))

model <- Logit(trial, status) ~ loglinear(cell)
model <- Logit(status) ~ loglinear(cell)

control <- list(verbose = 0, ncores = 2)
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

