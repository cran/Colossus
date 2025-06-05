## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)

## ----fig.cap='Linear Interpolated Function'-----------------------------------
dft <- data.table("x" = c(1, 2, 3), "y" = c(2, 5, 10))
g <- ggplot2::ggplot(dft, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::geom_line(color = "black", alpha = 1) +
  ggplot2::labs(x = "age (days)", y = "Covariate Value")
x <- seq(1, 3, by = 0.1)
y <- 1 + x^2
dft <- data.table("x" = x, "y" = y)
g <- g + ggplot2::geom_line(
  data = dft, ggplot2::aes(x = .data$x, y = .data$y),
  color = "black", linetype = "dashed"
)
g

## ----fig.cap='Monotonic Step Function Applied'--------------------------------
dft <- data.table("x" = c(-1, 1, 5, 8, 13), "y" = c(0, 1, 1, 2, 3))
g <- ggplot2::ggplot(dft, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_point(color = "black")
dft <- data.table("x" = c(-1, -0.01, 0, 1, 5.99, 6, 11.99, 12, 13), "y" = c(0, 0, 1, 1, 1, 2, 2, 3, 3))
g <- g + ggplot2::geom_line(data = dft, ggplot2::aes(x = .data$x, y = .data$y), color = "black") +
  ggplot2::labs(x = "age (days)", y = "Covariate Value")
g

## ----fig.cap='Step Function Applied'------------------------------------------
dft <- data.table("x" = c(-1, 1, 5, 8, 13), "y" = c(1, 2, 2, 3, 2))
g <- ggplot2::ggplot(dft, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_point(color = "black")
dft <- data.table("x" = c(-1, -0.01, 0, 1, 5.99, 6, 11.99, 12, 13), "y" = c(1, 1, 2, 2, 2, 3, 3, 2, 2))
g <- g + ggplot2::geom_line(data = dft, ggplot2::aes(x = .data$x, y = .data$y), color = "black") +
  ggplot2::labs(x = "age (days)", y = "Covariate Value")
g

## -----------------------------------------------------------------------------
# Setting up the data for use
data(cancer, package = "survival")

df <- data.table(cancer)
df$UserID <- seq_len(nrow(df))

df$status <- df$status - 1
df$sex <- df$sex - 1

t0 <- "%trunc%"
t1 <- "time"
event <- "status"

df <- df[, c("time", "status", "sex", "UserID")]

## ----fig.cap='Linear Interpolation Example'-----------------------------------
grt_f <- function(df, time_col) {
  return((df[, "sex"] * df[, get(time_col)] / 400)[[1]])
}
func_form <- c("lin")
iscox <- TRUE
dt <- 0.01
df_new <- gen_time_dep(
  df, t0, t1, event, iscox, dt, c("sex_time"), c(),
  c(grt_f), "test_new.csv", func_form,
  nthreads = 1
)

g <- ggplot2::ggplot(df_new, ggplot2::aes(x = .data$time, y = .data$sex_time, colour = factor(.data$sex))) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(x = "Time", y = "Covariate Value")
g

## ----fig.cap='Monotonic Step Function Example'--------------------------------
func_form <- c("step?0g?200g?500g?700g?")
df_new <- gen_time_dep(
  df, t0, t1, event, iscox, dt, c("time_step"), c(),
  c(grt_f), "test_new.csv", func_form,
  nthreads = 1
)
g <- ggplot2::ggplot(df_new, ggplot2::aes(x = .data$time, y = .data$time_step)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::geom_line() +
  ggplot2::labs(x = "Time", y = "Covariate Value")
g

## ----fig.cap='Step Function Example'------------------------------------------
func_form <- c("step?0g?200g?400g?600l?700g?800b?")
df_new <- gen_time_dep(
  df, t0, t1, event, iscox, dt, c("time_step"), c(),
  c(grt_f), "test_new.csv", func_form,
  nthreads = 1
)
g <- ggplot2::ggplot(df_new, ggplot2::aes(x = .data$time, y = .data$time_step)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::geom_line() +
  ggplot2::labs(x = "Time", y = "Covariate Value")
g

