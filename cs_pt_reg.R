
library(car)
library(dplyr)
library(sandwich)
library(lmtest)
library(MASS)

##################################################################
### Cross section price transmission regressions
##################################################################

load("infl_ctr_dat_0708.Rda")
load("infl_ctr_dat_1011.Rda")
load("coef_bew_fpi_l1.Rda")
load("coef_bew_rfpi_l1.Rda")
load("imp_se_fpi_l1_12.Rda")
load("imp_se_rfpi_l1_12.Rda")
load("coef_bew_fpi_l6.Rda")
load("coef_bew_rfpi_l6.Rda")

## FPI LR multiplier regressions
fpi.pt.reg.m = list()
fpi.pt.reg.se = list()
fpi.bptest.stat = list()
fpi.bptest.p = list()

# data = merge(ctr.dat.0708, coef.bew.fpi.l1)
data = merge(ctr.dat.0708, coef.bew.fpi.l6)
data = dplyr::rename(data, lrm1 = lrm, lrm1.se = lrm.se)
data1 = na.omit(dplyr::select(data, lrm1, lrm1.se,
                              gdp.const.int, cshare, cir, cer, 
                              lldc, ste, to, dtf, lpi.infra))

# OLS
m = lm(scale(lrm1)~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1<3.5*sd(lrm1)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[1]] = m
fpi.pt.reg.se[[1]] = rse

fpi.bptest.stat[[1]] = bptest(m)[["statistic"]]
fpi.bptest.p[[1]] = bptest(m)[["p.value"]]

m = lm(scale(lrm1)~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1<3.5*sd(lrm1)))

step = stepAIC(m, direction="both")
cov = vcovHC(step, type = "HC1")
rse = sqrt(diag(cov))
fpi.pt.reg.m[[2]] = step
fpi.pt.reg.se[[2]] = rse

fpi.bptest.stat[[2]] = bptest(step)[["statistic"]]
fpi.bptest.p[[2]] = bptest(step)[["p.value"]]

# WLS
m = lm(scale(lrm1)~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1<3.5*sd(lrm1)),
       weights = 1/lrm1.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[3]] = m
fpi.pt.reg.se[[3]] = rse

fpi.bptest.stat[[3]] = bptest(m)[["statistic"]]
fpi.bptest.p[[3]] = bptest(m)[["p.value"]]

m = lm(scale(lrm1)~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1<3.5*sd(lrm1)),
       weights = 1/lrm1.se)

step = stepAIC(m, direction="both")
cov = vcovHC(step, type = "HC1")
rse = sqrt(diag(cov))
fpi.pt.reg.m[[4]] = step
fpi.pt.reg.se[[4]] = rse

fpi.bptest.stat[[4]] = bptest(step)[["statistic"]]
fpi.bptest.p[[4]] = bptest(step)[["p.value"]]

saveRDS(fpi.pt.reg.m, "fpi_pt_reg_m.rds")
saveRDS(fpi.pt.reg.se, "fpi_pt_reg_se.rds")
saveRDS(fpi.bptest.stat, "fpi_bptest_stat.rds")
saveRDS(fpi.bptest.p, "fpi_bptest_p.rds")

## real FPI LR multiplier regressions
rfpi.pt.reg.m = list()
rfpi.pt.reg.se = list()

# data = merge(ctr.dat.0708, coef.bew.rfpi.l1)
data = merge(ctr.dat.0708, coef.bew.rfpi.l6)
data = dplyr::rename(data, lrm1 = lrm, lrm1.se = lrm.se)
data1 = na.omit(dplyr::select(data, lrm1, lrm1.se,
                              gdp.const.int, cshare, cir, cer, 
                              lldc, ste, to, dtf, lpi.infra))

m = lm(lrm1~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1 < 3.5*sd(lrm1)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

rfpi.pt.reg.m[[1]] = m
rfpi.pt.reg.se[[1]] = rse

m = lm(lrm1~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1<3.5*sd(lrm1)))

step = stepAIC(m, direction="both")
cov = vcovHC(step, type = "HC1")
rse = sqrt(diag(cov))
rfpi.pt.reg.m[[2]] = step
rfpi.pt.reg.se[[2]] = rse

m = lm(lrm1~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1 < 3.5*sd(lrm1)),
       weights = 1/lrm1.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

rfpi.pt.reg.m[[3]] = m
rfpi.pt.reg.se[[3]] = rse

m = lm(lrm1~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, lrm1 < 3.5*sd(lrm1)),
       weights = 1/lrm1.se)

step = stepAIC(m, direction="both")
cov = vcovHC(step, type = "HC1")
rse = sqrt(diag(cov))
rfpi.pt.reg.m[[4]] = step
rfpi.pt.reg.se[[4]] = rse

saveRDS(rfpi.pt.reg.m, "rfpi_pt_reg_m.rds")
saveRDS(rfpi.pt.reg.se, "rfpi_pt_reg_se.rds")


## FPI impact multiplier regressions
fpi.pt.reg.m = list()
fpi.pt.reg.se = list()

dat = dplyr::select(imp.rfpi.l.1_12, iso3c, 1)
dat = dplyr::rename(dat, imp = V1)
data = merge(ctr.dat.1011, dat)

dat = dplyr::select(imp.se.rfpi.l.1_12, iso3c, 1)
dat = dplyr::rename(dat, imp.se = V1)
data = merge(data, dat)

data1 = na.omit(dplyr::select(data, imp, imp.se,
                              gdp.const.int, cshare, cir, cer, 
                              lldc, ste, to, dtf, lpi.infra))

# OLS
m = lm(imp~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         cshare+
         cir+
         cer +
         lldc+
         ste +
         to+
         dtf+
         lpi.infra,
       data = filter(data, imp<3.5*sd(imp)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[1]] = m
fpi.pt.reg.se[[1]] = rse

m = lm(imp~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         cshare+
         cir+
         cer +
         lldc+
         ste +
         to+
         dtf+
         lpi.infra,
       data = filter(data1, imp<3.5*sd(imp)))

step = stepAIC(m, direction="both")
cov = vcovHC(step, type = "HC1")
rse = sqrt(diag(cov))
fpi.pt.reg.m[[2]] = step
fpi.pt.reg.se[[2]] = rse

# WLS
m = lm(imp~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         cshare+
         cir+
         cer +
         lldc+
         ste +
         to+
         dtf+
         lpi.infra,
       data = filter(data, imp<3.5*sd(imp)),
       weights = 1/imp.se)
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))

fpi.pt.reg.m[[3]] = m
fpi.pt.reg.se[[3]] = rse

m = lm(imp~log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         cshare+
         cir+
         cer +
         lldc+
         ste +
         to+
         dtf+
         lpi.infra,
       data = filter(data1, imp<3.5*sd(imp)),
       weights = 1/imp.se)

step = stepAIC(m, direction="both")
cov = vcovHC(step, type = "HC1")
rse = sqrt(diag(cov))
fpi.pt.reg.m[[4]] = step
fpi.pt.reg.se[[4]] = rse

saveRDS(fpi.pt.reg.m, "rfpi_pt_imp_reg_m.rds")
saveRDS(fpi.pt.reg.se, "rfpi_pt_imp_reg_se.rds")

# End of script
