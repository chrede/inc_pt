
library(car)
library(dplyr)
library(sandwich)
library(lmtest)

##################################################################
### Cross section price transmission regressions
##################################################################

load("ctr_dat_0708.Rda")
load("coef_ecm_fpi.Rda")
load("coef_ecm_rfpi.Rda")
load("coef_bew_fpi_l1.Rda")

data = merge(select(ctr.dat.0708, -(2:4)), select(coef.ecm.fpi, iso3c, lrm, lrm.se))
# data = merge(select(ctr.dat.0708, -(2:4)), coef.bew.fpi.l1)
# data = rename(data, lrm.fpi=V1, lrm.fpi.se=V2)
data = dplyr::rename(data, lrm.fpi=lrm, lrm.fpi.se=lrm.se)
data = merge(data, select(coef.ecm.rfpi, iso3c, lrm, lrm.se))
data = dplyr::rename(data, lrm.rfpi=lrm, lrm.rfpi.se=lrm.se)

## 1 lag LR multiplier regressions
pt.reg.l1 = list()
pt.fstat = list()
pt.fstat.p = list()

m = lm(lrm.fpi~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.fpi<3.5*sd(lrm.fpi)),
       weights = 1/lrm.fpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l1[[1]] = m
pt.reg.l1[[2]] = rse

f = linearHypothesis(m, c("lldc=0", "to=0", "dtf=0", "lpi=0"), 
                     vcov = vcovHC(m, type = "HC1"))
pt.fstat[[1]] = f[2,3]
pt.fstat.p[[1]] = f[2,4]


m = lm(lrm.fpi~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer,
       data = filter(data, lrm.fpi<3.5*sd(lrm.fpi)),
       weights = 1/lrm.fpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l1[[3]] = m
pt.reg.l1[[4]] = rse

m = lm(lrm.fpi~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.fpi<3.5*sd(lrm.fpi)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l1[[5]] = m
pt.reg.l1[[6]] = rse

m = lm(lrm.rfpi~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.rfpi < 3.5*sd(lrm.rfpi)),
       weights = 1/lrm.rfpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l1[[7]] = m
pt.reg.l1[[8]] = rse

f = linearHypothesis(m, c("cshare=0", "cdep=0", "cer=0", "lldc=0", "to=0", "dtf=0", "lpi=0"), 
                     vcov = vcovHC(m, type = "HC1"))
pt.fstat[[2]] = f[2,3]
pt.fstat.p[[2]] = f[2,4]

m = lm(lrm.rfpi~log(gdp)+
         I(log(gdp)^2),
       data = filter(data, lrm.rfpi < 3.5*sd(lrm.rfpi)),
       weights = 1/lrm.rfpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l1[[9]] = m
pt.reg.l1[[10]] = rse

m = lm(lrm.rfpi~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.rfpi < 3.5*sd(lrm.rfpi)),
       weights = 1/lrm.rfpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l1[[11]] = m
pt.reg.l1[[12]] = rse

saveRDS(pt.reg.l1, "pt_reg_l1.rds")

## 6 lag LR multiplier regressions
load("coef_bew_l6.Rda")
data = merge(select(ctr.dat.0708, -(2:4)), coef.bew.l6)

pt.reg.l6 = list()

m = lm(lrm.fpi~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.fpi<3.5*sd(lrm.fpi)),
       weights = 1/lrm.fpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l6[[1]] = m
pt.reg.l6[[2]] = rse

f = linearHypothesis(m, c("lldc=0", "to=0", "dtf=0", "lpi=0"), 
                     vcov = vcovHC(m, type = "HC1"))
pt.fstat[[3]] = f[2,3]
pt.fstat.p[[3]] = f[2,4]

m = lm(lrm.fpi~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer,
       data = filter(data, lrm.fpi<3.5*sd(lrm.fpi)),
       weights = 1/lrm.fpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l6[[3]] = m
pt.reg.l6[[4]] = rse

m = lm(lrm.fpi~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.fpi<3.5*sd(lrm.fpi)),
       weights = 1/lrm.fpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l6[[5]] = m
pt.reg.l6[[6]] = rse

m = lm(lrm.rfpi~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.rfpi < 3.5*sd(lrm.rfpi)),
       weights = 1/lrm.rfpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l6[[7]] = m
pt.reg.l6[[8]] = rse

f = linearHypothesis(m, c("cshare=0", "cdep=0", "cer=0", "lldc=0", "to=0", "dtf=0", "lpi=0"), 
                     vcov = vcovHC(m, type = "HC1"))
pt.fstat[[4]] = f[2,3]
pt.fstat.p[[4]] = f[2,4]

m = lm(lrm.rfpi~log(gdp)+
         I(log(gdp)^2),
       data = filter(data, lrm.rfpi < 3.5*sd(lrm.rfpi)),
       weights = 1/lrm.rfpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l6[[9]] = m
pt.reg.l6[[10]] = rse

m = lm(lrm.rfpi~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm.rfpi < 3.5*sd(lrm.rfpi)),
       weights = 1/lrm.rfpi.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

pt.reg.l6[[11]] = m
pt.reg.l6[[12]] = rse

saveRDS(pt.reg.l6, "pt_reg_l6.rds")
saveRDS(pt.fstat, "pt_fstat.rds")
saveRDS(pt.fstat.p, "pt_fstat_p.rds")
