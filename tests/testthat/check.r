library(data.table)
library(parallel)
library(Colossus)

fname <- 'll_0.csv'
colTypes=c("double","double","double","integer","integer")
df <- fread(fname,nThread=min(c(detectCores(),2)),data.table=TRUE,header=TRUE,colClasses=colTypes,verbose=FALSE,fill=TRUE)
time1 <- "t0"
time2 <- "t1"
event <- "lung"
names <- c("dose")
Term_n <- c(0)
tform <- c("loglin")
keep_constant <- c(0)
a_n <- c(0.0)
modelform <- "M"
fir <- 0
der_iden <- 0
verbose <- FALSE

control=list("Ncores"=15,'lr' = 0.75,'maxiters' = c(-1,-1),'halfmax' = -1,'epsilon' = 1e-6,'dbeta_max' = 0.5,'deriv_epsilon' = 1e-6, 'abs_max'=1.0,'change_all'=TRUE,'dose_abs_max'=100.0,'verbose'=FALSE, 'ties'='breslow','double_step'=1)
model_control=list('strata'=FALSE, 'basic'=FALSE, 'single'=FALSE, 'null'=FALSE)
e0 <- RunCoxRegression_Omnibus(df, time1, time2, event, names, Term_n=Term_n, tform=c("loglin"), keep_constant=keep_constant, a_n=a_n, modelform="M", fir=0, der_iden=der_iden, control=control,Strat_Col="fac", model_control=model_control)
for (j in c(TRUE,FALSE)){
    for (k in c(TRUE,FALSE)){
        for (l in c(TRUE,FALSE)){
            model_control=list('strata'=FALSE, 'basic'=j, 'single'=k, 'null'=l)
            if (verbose){print(model_control)}
            a_n <- c(0.0)
            e1 <- RunCoxRegression_Omnibus(df, time1, time2, event, names, Term_n=Term_n, tform=c("loglin"), keep_constant=keep_constant, a_n=a_n, modelform="M", fir=0, der_iden=der_iden, control=control,Strat_Col="fac", model_control=model_control)
            print(paste(e0$LogLik,e1$LogLik,sep=" "))
            if (verbose){print("---------------")}
        }
    }
}
