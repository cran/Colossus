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
library(ggplot2)

## ----eval=TRUE----------------------------------------------------------------
data(cancer, package = "survival")
cancer %>% setDT()
df <- copy(cancer)

df$UserID <- seq_len(nrow(df))

df$status <- df$status - 1
df$sex <- df$sex - 1

control <- list(ncore = 1)
a_n <- c(0.01701289, -0.51256478)
coxres <- CoxRun(Cox(time, status) ~ loglinear(age, sex, 0),
  df,
  control = control, a_n = a_n
)

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "fname" = paste(tempfile(), "run", sep = ""), "studyid" = "UserID",
  "verbose" = 2, "surv_curv" = T, "martingale" = F, "strat_haz" = F, "km" = F
)

e <- plotSurvival(coxres, df, plot_options)

norm_surv <- e[["standard"]]

g <- ggplot(norm_surv, aes(x = .data$t, y = .data$h)) +
  geom_point(color = "black") +
  labs(x = "age", y = "Instantaneous Hazard")
g
g <- ggplot(norm_surv, aes(x = .data$t, y = .data$ch)) +
  geom_line(color = "black", alpha = 1) +
  labs(x = "age", y = "Cumulative Hazard")
g
g <- ggplot(norm_surv, aes(x = .data$t, y = .data$surv)) +
  geom_line(color = "black", alpha = 1) +
  labs(x = "age", y = "Surviving Fraction")
g
plot_options <- list(
  "fname" = paste(tempfile(), "run", sep = ""), "studyid" = "UserID",
  "verbose" = 2, "surv_curv" = F, "martingale" = F, "strat_haz" = F, "km" = T
)

e <- plotSurvival(coxres, df, plot_options)

km <- e[["kaplin-meier"]]
g <- ggplot(km, aes(x = .data$t_t, y = .data$n_t)) +
  geom_line(color = "black", alpha = 1) +
  labs(x = "age", y = "KM Survival")
g

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "fname" = paste(tempfile(), "run", sep = ""),
  "studyid" = "UserID", "verbose" = 2
)

res_all <- plotSchoenfeld(coxres, df, plot_options)

res_age <- res_all[["age"]]

g <- ggplot(res_age, aes(x = .data$time, y = .data$y)) +
  geom_point(color = "black") +
  labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual (age)", sep = " ")
  )
g
g <- ggplot(res_age, aes(x = .data$time, y = .data$y_scale)) +
  geom_point(color = "black") +
  labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual Scaled (age)", sep = " ")
  )
g
res_sex <- res_all[["sex"]]

g <- ggplot(res_sex, aes(x = .data$time, y = .data$y)) +
  geom_point(color = "black") +
  labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual (sex)", sep = " ")
  )
g
g <- ggplot(res_sex, aes(x = .data$time, y = .data$y_scale)) +
  geom_point(color = "black") +
  labs(
    x = paste("Survival Time", sep = ""),
    y = paste("Schoenfeld Residual Scaled (sex)", sep = " ")
  )
g

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "fname" = paste(tempfile(), "run", sep = ""),
  "studyid" = "UserID", "verbose" = 2, "surv_curv" = F,
  "martingale" = T, "strat_haz" = F, "km" = F, "cov_cols" = c("age", "sex")
)
res_all <- plotMartingale(coxres, df, plot_options)

res_age <- res_all[["age"]]

g <- ggplot() +
  geom_point(
    data = res_age,
    aes(x = .data$cov_max, y = .data$res_sum, group = .data$event, color = .data$event)
  )
g <- g + labs(x = "Max Age", y = "Martingale Residuals")
g
res_sex <- res_all[["sex"]]
g <- ggplot() +
  geom_point(
    data = res_sex,
    aes(x = .data$cov_max, y = .data$res_sum, group = .data$event, color = .data$event)
  )
g <- g + labs(x = "Sex", y = "Martingale Residuals")
g
res_surv <- res_all[["survival_time"]]
g <- ggplot() +
  geom_point(
    data = res_surv,
    aes(x = .data$time_max, y = .data$res_sum, group = .data$event, color = .data$event)
  )
g <- g + labs(x = "Survival Time", y = "Martingale Residuals")
g

## ----eval=TRUE, fig.width=7,fig.height=4--------------------------------------
plot_options <- list(
  "fname" = paste(tempfile(), "run", sep = ""), "studyid" = "UserID",
  "verbose" = 2, "cov_cols" = c("age", "sex"), boundary = 1.96
)
res_all <- plotRisk(coxres, df, plot_options)

res_age <- res_all[["age"]]

g <- ggplot(res_age, aes(x = .data$x, y = .data$y)) +
  geom_errorbar(aes(ymin = .data$"y:lower", ymax = .data$"y:upper"), color = "black") +
  geom_point(color = "black") +
  labs(x = "Age", y = "Relative Risk")
g


res_sex <- res_all[["sex"]]
g <- ggplot(res_sex, aes(x = .data$x, y = .data$y)) +
  geom_errorbar(aes(ymin = .data$"y:lower", ymax = .data$"y:upper"), color = "black") +
  geom_point(color = "black") +
  labs(x = "Sex", y = "Relative Risk")
g

