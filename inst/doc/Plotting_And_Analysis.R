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

## ----eval=TRUE----------------------------------------------------------------
data(cancer, package = "survival")
cancer %>% setDT()
df <- copy(cancer)

df$UserID <- seq_len(nrow(df))

df$status <- df$status - 1
df$sex <- df$sex - 1

control <- list(ncore = 2)
a_n <- c(0.01701289, -0.51256478)
coxres <- CoxRun(Cox(time, status) ~ loglinear(age, sex, 0), df, control = control, a_n = a_n)

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "type" = c("surv", paste(tempfile(), "run", sep = "")), "studyid" = "UserID",
  "verbose" = 2, "surv_curv" = T, "martingale" = F, "strat_haz" = F, "km" = F
)

e <- plot(coxres, df, plot_options)

norm_surv <- e[["standard"]]

g <- ggplot2::ggplot(norm_surv, ggplot2::aes(x = .data$t, y = .data$h)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::labs(x = "age", y = "Instantaneous Hazard")
g
g <- ggplot2::ggplot(norm_surv, ggplot2::aes(x = .data$t, y = .data$ch)) +
  ggplot2::geom_line(color = "black", alpha = 1) +
  ggplot2::labs(x = "age", y = "Cumulative Hazard")
g
g <- ggplot2::ggplot(norm_surv, ggplot2::aes(x = .data$t, y = .data$surv)) +
  ggplot2::geom_line(color = "black", alpha = 1) +
  ggplot2::labs(x = "age", y = "Surviving Fraction")
g
plot_options <- list(
  "type" = c("surv", paste(tempfile(), "run", sep = "")), "studyid" = "UserID",
  "verbose" = 2, "surv_curv" = F, "martingale" = F, "strat_haz" = F, "km" = T
)

e <- plot(coxres, df, plot_options)

km <- e[["kaplin-meier"]]
g <- ggplot2::ggplot(km, ggplot2::aes(x = .data$t_t, y = .data$n_t)) +
  ggplot2::geom_line(color = "black", alpha = 1) +
  ggplot2::labs(x = "age", y = "KM Survival")
g

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "type" = c("schoenfeld", paste(tempfile(), "run", sep = "")),
  "studyid" = "UserID", "verbose" = 2
)

res_all <- plot(coxres, df, plot_options)

res_age <- res_all[["age"]]

g <- ggplot2::ggplot(res_age, ggplot2::aes(x = .data$time, y = .data$y)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual (age)", sep = " ")
  )
g
g <- ggplot2::ggplot(res_age, ggplot2::aes(x = .data$time, y = .data$y_scale)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual Scaled (age)", sep = " ")
  )
g
res_sex <- res_all[["sex"]]

g <- ggplot2::ggplot(res_sex, ggplot2::aes(x = .data$time, y = .data$y)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual (sex)", sep = " ")
  )
g
g <- ggplot2::ggplot(res_sex, ggplot2::aes(x = .data$time, y = .data$y_scale)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual Scaled (sex)", sep = " ")
  )
g

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "type" = c("surv", paste(tempfile(), "run", sep = "")),
  "studyid" = "UserID", "verbose" = 2, "surv_curv" = F,
  "martingale" = T, "strat_haz" = F, "km" = F, "cov_cols" = c("age", "sex")
)
res_all <- plot(coxres, df, plot_options)

res_age <- res_all[["age"]]

g <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = res_age,
    ggplot2::aes(x = .data$cov_max, y = .data$res_sum, group = .data$event, color = .data$event)
  )
g <- g + ggplot2::labs(x = "Max Age", y = "Martingale Residuals")
g
res_sex <- res_all[["sex"]]
g <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = res_sex,
    ggplot2::aes(x = .data$cov_max, y = .data$res_sum, group = .data$event, color = .data$event)
  )
g <- g + ggplot2::labs(x = "Sex", y = "Martingale Residuals")
g
res_surv <- res_all[["survival_time"]]
g <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = res_surv,
    ggplot2::aes(x = .data$time_max, y = .data$res_sum, group = .data$event, color = .data$event)
  )
g <- g + ggplot2::labs(x = "Survival Time", y = "Martingale Residuals")
g

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "type" = c("risk", paste(tempfile(), "run", sep = "")), "studyid" = "UserID",
  "verbose" = 2
)
res_all <- plot(coxres, df, plot_options)

res_age <- res_all[["age"]]

g <- ggplot2::ggplot(res_age, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_line(color = "black") +
  ggplot2::labs(x = "Age", y = "Relative Risk")
g
res_sex <- res_all[["sex"]]
g <- ggplot2::ggplot(res_sex, ggplot2::aes(x = .data$x, y = .data$y)) +
  ggplot2::geom_point(color = "black") +
  ggplot2::labs(x = "Sex", y = "Relative Risk")
g

