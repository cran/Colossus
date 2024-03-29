
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Colossus

The goal of Colossus is to provide an open-source means of performing
survival analysis on big data with complex risk formula. Colossus is
designed to perform Cox Proportional Hazard regressions and Poisson
regressions on datasets loaded as data.tables or data.frames. The risk
models allowed are sums or products of linear, log-linear, or several
other radiation dose response formula highlighted in the vignettes.
Additional plotting capabilities are available.

Please consult the GitHub for details on libraries required for your OS.
Note that Colossus requires OpenMP support to perform parallel
calculations. During the configuration stage of installation, text
starting with “CONFIG NOTE” will denote what OS/Compiler are detected
and if OpenMP support is configured. Currently OpenMP support is not
configured for linux and MacOS operating systems compiling with clang.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(data.table)
library(parallel)
library(Colossus)
## basic example code reproduced from the starting-description vignette

df <- data.table("UserID"=c(112, 114, 213, 214, 115, 116, 117),
           "Starting_Age"=c(18,  20,  18,  19,  21,  20,  18),
             "Ending_Age"=c(30,  45,  57,  47,  36,  60,  55),
          "Cancer_Status"=c(0,   0,   1,   0,   1,   0,   0),
                      "a"=c(0,   1,   1,   0,   1,   0,   1),
                      "b"=c(1,   1.1, 2.1, 2,   0.1, 1,   0.2),
                      "c"=c(10,  11,  10,  11,  12,  9,   11),
                      "d"=c(0,   0,   0,   1,   1,   1,   1))
# For the interval case
time1 <- "Starting_Age"
time2 <- "Ending_Age"
event <- "Cancer_Status"

names <- c('a','b','c','d')
Term_n <- c(0,1,1,2)
tform <- c("loglin","lin","lin","plin")
modelform <- "M"
fir <- 0

a_n <- c(0.1, 0.1, 0.1, 0.1)

keep_constant <- c(0,0,0,0)
der_iden <- 0

control=list('lr' = 0.75,'maxiter' = 100,'halfmax' = 5,'epsilon' = 1e-9,
             'dbeta_max' = 0.5,'deriv_epsilon' = 1e-9, 'abs_max'=1.0,
             'change_all'=TRUE,'dose_abs_max'=100.0,'verbose'=FALSE,
             'ties'='breslow','double_step'=1)

e <- RunCoxRegression(df, time1, time2, event, names, Term_n, tform, keep_constant, a_n, modelform, fir, der_iden, control)
print(e)
#> $LogLik
#> [1] -0.6753644
#> 
#> $First_Der
#> [1] -2.220446e-16 -7.187040e-05  7.361232e-05  1.919948e-04
#> 
#> $Second_Der
#>              [,1]         [,2]          [,3]          [,4]
#> [1,] 2.220446e-16 2.710505e-20  1.734723e-18  3.559233e-18
#> [2,] 2.710505e-20 1.742209e-08  7.238366e-07  2.311365e-07
#> [3,] 1.734723e-18 7.238366e-07 -1.501037e-06 -2.356033e-07
#> [4,] 3.559233e-18 2.311365e-07 -2.356033e-07 -3.687577e-06
#> 
#> $beta_0
#> [1]  42.97183  98.72266  96.82311 101.10000
#> 
#> $Standard_Deviation
#> [1]      NaN      NaN 240.8255 520.7003
#> 
#> $AIC
#> [1] 9.350729
#> 
#> $BIC
#> [1] 9.134369
#> 
#> $Parameter_Lists
#> $Parameter_Lists$Term_n
#> [1] 0 1 1 2
#> 
#> $Parameter_Lists$tforms
#> [1] "loglin" "lin"    "lin"    "plin"  
#> 
#> $Parameter_Lists$names
#> [1] "a" "b" "c" "d"
#> 
#> 
#> $Control_List
#> $Control_List$Iteration
#> [1] 100
#> 
#> $Control_List$`Maximum Step`
#> [1] 1
#> 
#> $Control_List$`Derivative Limiting`
#> [1] 0.0001919948
#> 
#> 
#> $Converged
#> [1] FALSE
```
