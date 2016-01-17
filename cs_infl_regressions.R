library(car)
library(lmtest)
library(MASS)
library(robustbase)
library(sandwich)
library(stats)
library(plyr)
library(dplyr)
library(leaps)
library(relaimpo)
library(psych)

##################################################################
### Cross section inflation regressions
##################################################################

load("ctr_dat_0708.Rda")
load("ctr_dat_1011.Rda")

## 2007-8 inflation regressions
data = ctr.dat.0708
infl.reg.0708 = list()
infl.fstat = list()
infl.fstat.p = list()

m = lm(fpigr.0708~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, fpigr.0708<3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.0708[[1]] = m
infl.reg.0708[[2]] = rse

f = linearHypothesis(m, c("cshare=0", "lldc=0", "dtf=0", "lpi=0"), 
                     vcov = vcovHC(m, type = "HC1"))
infl.fstat[[1]] = f[2,3]
infl.fstat.p[[1]] = f[2,4]

m = lm(fpigr.0708~log(gdp)+
         I(log(gdp)^2)+
         cdep+
         cer+
         to,
       data = filter(data, fpigr.0708<3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.0708[[3]] = m
infl.reg.0708[[4]] = rse

m = lm(fpigr.0708~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, fpigr.0708<3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.0708[[5]] = m
infl.reg.0708[[6]] = rse

m = lm(rfpigr.0708~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, rfpigr.0708 < 3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.0708[[7]] = m
infl.reg.0708[[8]] = rse

f = linearHypothesis(m, c("cshare=0", "cdep", "lldc=0", "dtf=0"), 
                     vcov = vcovHC(m, type = "HC1"))
infl.fstat[[2]] = f[2,3]
infl.fstat.p[[2]] = f[2,4]

m = lm(rfpigr.0708~log(gdp)+
         I(log(gdp)^2)+
         cer+
         to +
         lpi,
       data = filter(data, rfpigr.0708 < 3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.0708[[9]] = m
infl.reg.0708[[10]] = rse

m = lm(rfpigr.0708~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, rfpigr.0708 < 3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.0708[[11]] = m
infl.reg.0708[[12]] = rse

saveRDS(infl.reg.0708, "infl_reg_0708.rds")

## 2010-11 inflation regressions
data = filter(ctr.dat.1011, iso3c != "BLR")
infl.reg.1011 = list()

m = lm(fpigr.1011~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, fpigr.1011 < 3.5*sd(fpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.1011[[1]] = m
infl.reg.1011[[2]] = rse

f = linearHypothesis(m, c("to=0", "dtf=0", "lpi=0"), 
                     vcov = vcovHC(m, type = "HC1"))
infl.fstat[[3]] = f[2,3]
infl.fstat.p[[3]] = f[2,4]

m = lm(fpigr.1011~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer +
         lldc,
       data = data)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.1011[[3]] = m
infl.reg.1011[[4]] = rse

m = lm(fpigr.1011~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, fpigr.1011 < 3.5*sd(fpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.1011[[5]] = m
infl.reg.1011[[6]] = rse

m = lm(rfpigr.1011~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, rfpigr.1011 < 3.5*sd(rfpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.1011[[7]] = m
infl.reg.1011[[8]] = rse

f = linearHypothesis(m, c("to=0", "dtf=0", "lpi=0"), 
                     vcov = vcovHC(m, type = "HC1"))
infl.fstat[[4]] = f[2,3]
infl.fstat.p[[4]] = f[2,4]

m = lm(rfpigr.1011~log(gdp)+
          I(log(gdp)^2)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer +
         lldc,
        data = filter(data, rfpigr.1011 < 3.5*sd(rfpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.1011[[9]] = m
infl.reg.1011[[10]] = rse

m = lm(rfpigr.1011~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, rfpigr.1011 < 3.5*sd(rfpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

infl.reg.1011[[11]] = m
infl.reg.1011[[12]] = rse

saveRDS(infl.reg.1011, "infl_reg_1011.rds")
saveRDS(infl.fstat, "infl_fstat.rds")
saveRDS(infl.fstat.p, "infl_fstat_p.rds")
