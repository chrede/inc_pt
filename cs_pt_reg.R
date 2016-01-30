
library(car)
library(dplyr)
library(sandwich)
library(lmtest)

##################################################################
### Cross section price transmission regressions
##################################################################

load("ctr_dat_0708.Rda")
load("coef_bew_fpi_l1.Rda")
load("coef_bew_rfpi_l1.Rda")
load("coef_bew_fpi_l12.Rda")
load("coef_bew_rfpi_l12.Rda")

## FPI LR multiplier regressions
fpi.pt.reg.m = list()
fpi.pt.reg.se = list()
fpi.bptest.stat = list()
fpi.bptest.p = list()

# 1 lag LRM
data = merge(ctr.dat.0708, coef.bew.fpi.l1)
data = rename(data, lrm1 = lrm, lrm1.se = lrm.se)

# OLS
m = lm(lrm1~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1<3.5*sd(lrm1)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[1]] = m
fpi.pt.reg.se[[1]] = rse

fpi.bptest.stat[[1]] = bptest(m)[["statistic"]]
fpi.bptest.p[[1]] = bptest(m)[["p.value"]]

m = lm(lrm1~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1<3.5*sd(lrm1)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[2]] = m
fpi.pt.reg.se[[2]] = rse

fpi.bptest.stat[[2]] = bptest(m)[["statistic"]]
fpi.bptest.p[[2]] = bptest(m)[["p.value"]]

# WLS
m = lm(lrm1~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1<3.5*sd(lrm1)),
       weights = 1/lrm1.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[3]] = m
fpi.pt.reg.se[[3]] = rse

fpi.bptest.stat[[3]] = bptest(m)[["statistic"]]
fpi.bptest.p[[3]] = bptest(m)[["p.value"]]

m = lm(lrm1~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1<3.5*sd(lrm1)),
       weights = 1/lrm1.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[4]] = m
fpi.pt.reg.se[[4]] = rse

fpi.bptest.stat[[4]] = bptest(m)[["statistic"]]
fpi.bptest.p[[4]] = bptest(m)[["p.value"]]

saveRDS(fpi.pt.reg.m, "fpi_pt_reg_m.rds")
saveRDS(fpi.pt.reg.se, "fpi_pt_reg_se.rds")
saveRDS(fpi.bptest.stat, "fpi_bptest_stat.rds")
saveRDS(fpi.bptest.p, "fpi_bptest_p.rds")

# # 12 lags
# data = merge(ctr.dat.0708, coef.bew.fpi.l12)
# data = rename(data, lrm12 = lrm, lrm12.se = lrm.se)
# 
# m = lm(lrm12~log(gdp)+
#          I(log(gdp)^2)+
#          cshare+
#          cdep+
#          cer+
#          lldc+
#          to+
#          dtf+
#          lpi,
#        data = filter(data, lrm12<3.5*sd(lrm12)),
#        weights = 1/lrm12.se)
# cov = vcovHC(m, type = "HC1")
# rse = sqrt(diag(cov))
# 
# fpi.pt.reg.m[[3]] = m
# fpi.pt.reg.se[[3]] = rse
# 
# m = lm(lrm12~log(cons)+
#          I(log(cons)^2)+
#          cshare+
#          cdep+
#          cer+
#          lldc+
#          to+
#          dtf+
#          lpi,
#        data = filter(data, lrm12<3.5*sd(lrm12)),
#        weights = 1/lrm12.se)
# cov = vcovHC(m, type = "HC1")
# rse = sqrt(diag(cov))
# 
# fpi.pt.reg.m[[4]] = m
# fpi.pt.reg.se[[4]] = rse

## real FPI LR multiplier regressions
rfpi.pt.reg.m = list()
rfpi.pt.reg.se = list()

# 1 lag
data = merge(ctr.dat.0708, coef.bew.rfpi.l1)
data = rename(data, lrm1 = lrm, lrm1.se = lrm.se)

m = lm(lrm1~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1 < 3.5*sd(lrm1)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

rfpi.pt.reg.m[[1]] = m
rfpi.pt.reg.se[[1]] = rse

m = lm(lrm1~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1 < 3.5*sd(lrm1)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

rfpi.pt.reg.m[[2]] = m
rfpi.pt.reg.se[[2]] = rse

m = lm(lrm1~log(gdp)+
         I(log(gdp)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1 < 3.5*sd(lrm1)),
       weights = 1/lrm1.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

rfpi.pt.reg.m[[3]] = m
rfpi.pt.reg.se[[3]] = rse

m = lm(lrm1~log(cons)+
         I(log(cons)^2)+
         cshare+
         cdep+
         cer+
         lldc+
         to+
         dtf+
         lpi,
       data = filter(data, lrm1 < 3.5*sd(lrm1)),
       weights = 1/lrm1.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

rfpi.pt.reg.m[[4]] = m
rfpi.pt.reg.se[[4]] = rse

saveRDS(rfpi.pt.reg.m, "rfpi_pt_reg_m.rds")
saveRDS(rfpi.pt.reg.se, "rfpi_pt_reg_se.rds")

# # 12 lag
# data = merge(ctr.dat.0708, coef.bew.rfpi.l12)
# data = rename(data, lrm12 = lrm, lrm12.se = lrm.se)
# 
# m = lm(lrm12~log(gdp)+
#          I(log(gdp)^2)+
#          cshare+
#          cdep+
#          cer+
#          lldc+
#          to+
#          dtf+
#          lpi,
#        data = filter(data, lrm12 < 3.5*sd(lrm12)),
#        weights = 1/lrm12.se)
# cov = vcovHC(m, type = "HC1")
# rse = sqrt(diag(cov))
# 
# rfpi.pt.reg.m[[3]] = m
# rfpi.pt.reg.se[[3]] = rse
# 
# m = lm(lrm12~log(cons)+
#          I(log(cons)^2)+
#          cshare+
#          cdep+
#          cer+
#          lldc+
#          to+
#          dtf+
#          lpi,
#        data = filter(data, lrm12 < 3.5*sd(lrm12)),
#        weights = 1/lrm12.se)
# cov = vcovHC(m, type = "HC1")
# rse = sqrt(diag(cov))
# 
# rfpi.pt.reg.m[[4]] = m
# rfpi.pt.reg.se[[4]] = rse
