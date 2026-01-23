library(Colossus)
library(data.table)
library(parallel)
library(survival)
library(dplyr)

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

print(df)

print(df[["trt"]])
df[["trt"]] <- as.numeric(as.character(df[["trt"]]))
print(df[["trt"]])
print(df)
