library(car)
library(lmtest)
library(sandwich)
library(dplyr)
library(relaimpo)

##################################################################
### Cross section inflation regressions
##################################################################

load("ctr_dat_0708.Rda")
load("ctr_dat_1011.Rda")

## FPI inflation regressions
fpi.reg.m = list()
fpi.reg.se = list()

# 2007-08
data = ctr.dat.0708

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

fpi.reg.m[[1]] = m
fpi.reg.se[[1]] = rse

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

fpi.reg.m[[2]] = m
fpi.reg.se[[2]] = rse

# 2010-11
data = filter(ctr.dat.1011, iso3c != "BLR")

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

fpi.reg.m[[3]] = m
fpi.reg.se[[3]] = rse

# calc.relimp(m,type=c("lmg","last","first","pratt"), rela=TRUE)

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

fpi.reg.m[[4]] = m
fpi.reg.se[[4]] = rse

saveRDS(fpi.reg.m, "fpi_reg_m.rds")
saveRDS(fpi.reg.se, "fpi_reg_se.rds")

## Real FPI growth regressions
rfpi.reg.m = list()
rfpi.reg.se = list()

# 2007-08
data = ctr.dat.0708

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

rfpi.reg.m[[1]] = m
rfpi.reg.se[[1]] = rse

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

rfpi.reg.m[[2]] = m
rfpi.reg.se[[2]] = rse

# 2010-11
data = filter(ctr.dat.1011, iso3c != "BLR")

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

rfpi.reg.m[[3]] = m
rfpi.reg.se[[3]] = rse

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

rfpi.reg.m[[4]] = m
rfpi.reg.se[[4]] = rse

saveRDS(rfpi.reg.m, "rfpi_reg_m.rds")
saveRDS(rfpi.reg.se, "rfpi_reg_se.rds")

## 2007-08 regressions including intervention dummies
int.reg.m = list()
int.reg.se = list()
data = ctr.dat.0708

m = lm(fpigr.0708~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi+
         export+
         price+
         stocks+
         tariff+
         tax,
       data = filter(data, fpigr.0708 < 3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

int.reg.m[[1]] = m
int.reg.se[[1]] = rse

m = lm(fpigr.0708~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi+
         export+
         price+
         stocks+
         tariff+
         tax,
       data = filter(data, fpigr.0708 < 3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

int.reg.m[[2]] = m
int.reg.se[[2]] = rse

m = lm(rfpigr.0708~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi+
         export+
         price+
         stocks+
         tariff+
         tax,
       data = filter(data, rfpigr.0708 < 3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

int.reg.m[[3]] = m
int.reg.se[[3]] = rse

m = lm(rfpigr.0708~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer +
         lldc+
         to+
         dtf+
         lpi+
         export+
         price+
         stocks+
         tariff+
         tax,
       data = filter(data, rfpigr.0708 < 3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

int.reg.m[[4]] = m
int.reg.se[[4]] = rse

saveRDS(int.reg.m, "int_reg_m.rds")
saveRDS(int.reg.se, "int_reg_se.rds")
