## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)
library(parallel)

## ----eval=TRUE----------------------------------------------------------------
apples <- c(0, 1, 2, 3, 4, 5, 6, 2, 2, 3, 4, 2, 1, 5, 6, 4, 2)
oranges <- c(1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1, 3, 2, 2, 1)
rip <- c(0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1)
hands <- c(1, 1, 2, 3, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2)
table <- data.table::data.table(
  "apples" = apples,
  "oranges" = oranges,
  "rip" = rip,
  "hands" = hands
)

## ----eval=TRUE----------------------------------------------------------------
apple_category <- "0/3/5]7"
orange_category <- list(
  lower = c(-1, 3, 6),
  upper = c(3, 6, 10),
  name = c("few", "good", "excessive")
)
categ <- list(
  "apples" = apple_category,
  "oranges" = orange_category
)

## ----eval=TRUE----------------------------------------------------------------
event <- list("rip" = "count AS dropped", "hands" = "mean")

## ----eval=TRUE----------------------------------------------------------------
Event_Count_Gen(table, categ, event)

## ----eval=TRUE----------------------------------------------------------------
a <- c(0, 1, 2, 3, 4, 5, 6, 2, 2, 3, 4, 2, 1, 5, 6, 4, 2)
b <- c(1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1, 3, 2, 2, 1)
c <- c(0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1)

d <- c(1, 1, 2, 2, 1, 1, 2, 2, 3, 3, 3, 4, 4, 2, 1, 1, 2)
e <- c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1)
f <- c(
  1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900,
  1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900
)
g <- c(4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 5, 5, 5, 4, 4, 4, 4)
h <- c(6, 4, 4, 6, 6, 6, 4, 4, 4, 6, 6, 6, 6, 4, 4, 4, 4)
i <- c(
  1901, 1902, 1903, 1904, 1905, 1906, 1907, 1903, 1904,
  1903, 1904, 1910, 1903, 1904, 1903, 1904, 1910
)
table <- data.table::data.table(
  "a" = a, "b" = b, "c" = c,
  "d" = d, "e" = e, "f" = f,
  "g" = g, "h" = h, "i" = i
)

pyr <- list(
  entry = list(year = "f", month = "e", day = "d"),
  exit = list(year = "i", month = "h", day = "g"),
  unit = "years"
)

## ----eval=TRUE----------------------------------------------------------------
categ <- list(
  "a" = "-1/3/5]7",
  "b" = list(
    lower = c(-1, 3, 6), upper = c(3, 6, 10),
    name = c("low", "medium", "high")
  ),
  "time AS time_bin" = list(
    "day" = c(1, 1, 1),
    "month" = c(1, 1, 1),
    "year" = c(1899, 1903, 1910)
  )
)

## ----eval=TRUE----------------------------------------------------------------
summary <- list("c" = "count AS cases", "b" = "weighted_mean AS b_weighted")
events <- list("c")

## ----eval=TRUE----------------------------------------------------------------
pyr <- list(
  entry = list(year = "f", month = "e", day = "d"),
  exit = list(year = "i", month = "h", day = "g"),
  unit = "years"
)
print(Event_Time_Gen(table, pyr, categ, summary, events, T))
pyr <- list(
  entry = list(year = "f", month = "e", day = "d"),
  exit = list(year = "i", month = "h", day = "g"),
  unit = "months"
)
print(Event_Time_Gen(table, pyr, categ, summary, events, T))
pyr <- list(
  entry = list(year = "f", month = "e", day = "d"),
  exit = list(year = "i", month = "h", day = "g"),
  unit = "days"
)
print(Event_Time_Gen(table, pyr, categ, summary, events, T))

