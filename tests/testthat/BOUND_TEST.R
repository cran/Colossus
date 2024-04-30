library(data.table)
library(Colossus)
library(parallel)

fname <- 'll_0.csv'
colTypes=c("double","double","double","integer","integer")
df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
time1 <- "t0"
time2 <- "t1"
event <- "lung"
names <- c("dose","fac")
Term_n <- c(0,0)
tform <- c("loglin","loglin")
keep_constant <- c(0,0)
a_n <- c(-0.09961963, -0.05697583)
modelform <- "M"
fir <- 0
der_iden <- 0
control=list("Ncores"=2,'lr' = 0.75,'maxiter' = 20,'halfmax' = 5,'epsilon' = 1e-6,'dbeta_max' = 0.5,'deriv_epsilon' = 1e-6, 'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,'verbose'=TRUE, 'ties'='breslow','double_step'=1)
model_control=list('strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'CR'=FALSE, 'Log_Bound'=TRUE)
e <- RunCoxRegression_Omnibus(df, time1, time2, event, names, Term_n=Term_n, tform=tform, keep_constant=keep_constant, a_n=a_n, modelform=modelform, fir=fir, der_iden=der_iden, control=control,Strat_Col="fac", model_control=model_control)
print(e)
