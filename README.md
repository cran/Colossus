
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Colossus

The goal of Colossus is to provide an open-source means of performing
survival analysis on big data with complex risk formulas. Colossus is
designed to perform Cox Proportional Hazard regressions and Poisson
regressions on datasets loaded as data.tables or data.frames. The risk
models allowed are sums or products of linear, log-linear, or several
other radiation dose response formulas highlighted in the vignettes.
Additional plotting capabilities are available.

By default, a fully portable version of the code is compiled, which does
not support OpenMP on every system. Note that Colossus requires OpenMP
support to perform parallel calculations. The environment variable
“R\_COLOSSUS\_NOT\_CRAN” is checked to determine if OpenMP should be
disabled for linux compiling with clang. The number of cores is set to 1
if the environment variable is empty, the operating system is detected
as linux, and the default compiler or R compiler is clang. Colossus
testing checks for the “NOT\_CRAN” variable to determine if additional
tests should be run. Setting “NOT\_CRAN” to “false” will disable the
longer tests. Currently, OpenMP support is not configured for linux
compiling with clang.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(data.table)
library(parallel)
library(Colossus)
## basic example code reproduced from the starting-description vignette

df <- data.table(
  "UserID" = c(112, 114, 213, 214, 115, 116, 117),
  "Starting_Age" = c(18, 20, 18, 19, 21, 20, 18),
  "Ending_Age" = c(30, 45, 57, 47, 36, 60, 55),
  "Cancer_Status" = c(0, 0, 1, 0, 1, 0, 0),
  "a" = c(0, 1, 1, 0, 1, 0, 1),
  "b" = c(1, 1.1, 2.1, 2, 0.1, 1, 0.2),
  "c" = c(10, 11, 10, 11, 12, 9, 11),
  "d" = c(0, 0, 0, 1, 1, 1, 1)
)
# For the interval case
time1 <- "Starting_Age"
time2 <- "Ending_Age"
event <- "Cancer_Status"

names <- c("a", "b", "c", "d")
term_n <- c(0, 1, 1, 2)
tform <- c("loglin", "lin", "lin", "plin")
modelform <- "M"

a_n <- c(0.1, 0.1, 0.1, 0.1)

keep_constant <- c(0, 0, 0, 0)
der_iden <- 0

control <- list(
  "lr" = 0.75, "maxiter" = 100, "halfmax" = 5, "epsilon" = 1e-9,
  "deriv_epsilon" = 1e-9, "abs_max" = 1.0,
  "verbose" = 2, "ties" = "breslow"
)

e <- RunCoxRegression(df, time1, time2, event, names, term_n, tform, keep_constant, a_n, modelform, control = control)
Interpret_Output(e)
#> |-------------------------------------------------------------------|
#> Final Results
#>    Covariate Subterm Term Number Central Estimate Standard Error 2-tail p-value
#>       <char>  <char>       <int>            <num>          <num>          <num>
#> 1:         a  loglin           0         42.10452            NaN            NaN
#> 2:         b     lin           1         98.72266    3781273.501      0.9999792
#> 3:         c     lin           1         96.82311    3698137.325      0.9999791
#> 4:         d    plin           2        101.10000       2326.871      0.9653437
#> 
#> Cox Model Used
#> -2*Log-Likelihood: 1.35,  AIC: 9.35
#> Iterations run: 100
#> maximum step size: 1.00e+00, maximum first derivative: 1.92e-04
#> Analysis did not converge, check convergence criteria or run further
#> Run finished in 0.25 seconds
#> |-------------------------------------------------------------------|
```
