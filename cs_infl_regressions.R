library(car)
library(lmtest)
library(sandwich)
library(dplyr)
library(relaimpo)
library(MASS)

##################################################################
### Cross section inflation regressions
##################################################################

str = "C:/Users/nqw235/Google Drive/Research/pov_price_transmission/"
setwd(str)
load("infl_ctr_dat_0708.Rda")
load("infl_ctr_dat_1011.Rda")

boxplot(ctr.dat.0708$fpigr.0708)
boxplot(ctr.dat.0708$fpigr.0708[ctr.dat.0708$fpigr.0708<3.5*sd(ctr.dat.0708$fpigr.0708)])

boxplot(ctr.dat.0708$rfpigr.0708)
boxplot(ctr.dat.0708$rfpigr.0708[ctr.dat.0708$rfpigr.0708<3.5*sd(ctr.dat.0708$rfpigr.0708)])

boxplot(ctr.dat.1011$fpigr.1011)
boxplot(ctr.dat.1011$fpigr.1011[ctr.dat.1011$fpigr.1011<3.5*sd(ctr.dat.1011$fpigr.1011)])

boxplot(ctr.dat.1011$rfpigr.1011)
boxplot(ctr.dat.1011$rfpigr.1011[ctr.dat.1011$rfpigr.1011<3.5*sd(ctr.dat.1011$rfpigr.1011)])

## Food crisis regressions
# 2007-08
data = ctr.dat.0708
data1 = na.omit(dplyr::select(data, fpigr.0708, rfpigr.0708, ergr.0708,
                              gdp.const.int, cshare, cir, cer, 
                              lldc, ste, to, dtf, lpi.infra))

fc.reg0708.m = list()
fc.reg0708.se = list()

#m = lars(x = scale(data$fpigr.0708), y = scale(data$cpigr.0708), type = "lasso")

m = lm(scale(fpigr.0708)~scale(ergr.0708) +
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, fpigr.0708<3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg0708.m[[1]] = m
fc.reg0708.se[[1]] = rse

m = lm(scale(fpigr.0708)~
         log(gdp.const.int)+
         I(log(gdp.const.int)^2),
       data = filter(data, fpigr.0708<3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg0708.m[[2]] = m
fc.reg0708.se[[2]] = rse

# m = lm(scale(fpigr.0708)~scale(ergr.0708)+
#          log(gdp.const.int)+
#          I(log(gdp.const.int)^2)+
#          scale(cshare)+
#          scale(cir)+
#          scale(cer) +
#          lldc+
#          ste +
#          scale(to)+
#          scale(dtf)+
#          scale(lpi.infra),
#        data = filter(data1, fpigr.0708<3.5*sd(fpigr.0708)))
# 
# step = stepAIC(m, direction="both")
# cov = vcovHC(step, type = "HC1")
# rse = sqrt(diag(cov))
# fc.reg0708.m[[2]] = step
# fc.reg0708.se[[2]] = rse

m = lm(scale(rfpigr.0708)~scale(ergr.0708)+
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, rfpigr.0708<3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg0708.m[[3]] = m
fc.reg0708.se[[3]] = rse

m = lm(scale(rfpigr.0708)~
         log(gdp.const.int)+
         I(log(gdp.const.int)^2),
       data = filter(data, rfpigr.0708<3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg0708.m[[4]] = m
fc.reg0708.se[[4]] = rse

# m = lm(scale(rfpigr.0708)~ scale(ergr.0708)+
#          log(gdp.const.int)+
#          I(log(gdp.const.int)^2)+
#          scale(cshare)+
#          scale(cir)+
#          scale(cer) +
#          lldc+
#          ste +
#          scale(to)+
#          scale(dtf)+
#          scale(lpi.infra),
#        data = filter(data1, rfpigr.0708<3.5*sd(rfpigr.0708)))
# 
# step = stepAIC(m, direction="both")
# cov = vcovHC(step, type = "HC1")
# rse = sqrt(diag(cov))
# fc.reg0708.m[[4]] = step
# fc.reg0708.se[[4]] = rse

saveRDS(fc.reg0708.m, "fc_reg0708_m.rds")
saveRDS(fc.reg0708.se, "fc_reg0708_se.rds")

# 2010-11
data = filter(ctr.dat.1011, iso3c != "BLR")
data1 = na.omit(dplyr::select(data, fpigr.1011, rfpigr.1011, ergr.1011,
                              gdp.const.int, cshare, cir, cer, lldc, ste, to, dtf, lpi.infra))
fc.reg1011.m = list()
fc.reg1011.se = list()

m = lm(scale(fpigr.1011)~scale(ergr.1011)+
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, fpigr.1011<3.5*sd(fpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg1011.m[[1]] = m
fc.reg1011.se[[1]] = rse

m = lm(scale(fpigr.1011)~
         log(gdp.const.int)+
         I(log(gdp.const.int)^2),
       data = filter(data, fpigr.1011<3.5*sd(fpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg1011.m[[2]] = m
fc.reg1011.se[[2]] = rse

# m = lm(scale(fpigr.1011)~scale(ergr.1011)+
#          log(gdp.const.int)+
#          I(log(gdp.const.int)^2)+
#          scale(cshare) +
#          scale(cir)+
#          scale(cer) +
#          lldc+
#          ste +
#          scale(to)+
#          scale(dtf)+
#          scale(lpi.infra),
#        data = filter(data1, fpigr.1011<3.5*sd(fpigr.1011)))
# 
# step = stepAIC(m, direction="both")
# cov = vcovHC(step, type = "HC1")
# rse = sqrt(diag(cov))
# fc.reg1011.m[[2]] = step
# fc.reg1011.se[[2]] = rse

m = lm(scale(rfpigr.1011)~scale(ergr.1011)+
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra),
       data = filter(data1, rfpigr.1011<3.5*sd(rfpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg1011.m[[3]] = m
fc.reg1011.se[[3]] = rse

m = lm(scale(rfpigr.1011)~
         log(gdp.const.int)+
         I(log(gdp.const.int)^2),
       data = filter(data, rfpigr.1011<3.5*sd(rfpigr.1011)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
fc.reg1011.m[[4]] = m
fc.reg1011.se[[4]] = rse

# m = lm(scale(rfpigr.1011)~scale(ergr.1011) +
#          log(gdp.const.int)+
#          I(log(gdp.const.int)^2)+
#          scale(cshare)+
#          scale(cir)+
#          scale(cer) +
#          lldc+
#          ste +
#          scale(to)+
#          scale(dtf)+
#          scale(lpi.infra),
#        data = filter(data1, rfpigr.1011<3.5*sd(rfpigr.1011)))
# 
# step = stepAIC(m, direction="both")
# cov = vcovHC(step, type = "HC1")
# rse = sqrt(diag(cov))
# fc.reg1011.m[[4]] = step
# fc.reg1011.se[[4]] = rse

saveRDS(fc.reg1011.m, "fc_reg1011_m.rds")
saveRDS(fc.reg1011.se, "fc_reg1011_se.rds")

## 2007-08 regressions including intervention dummies
int.reg.m = list()
int.reg.se = list()

data = ctr.dat.0708
data1 = na.omit(dplyr::select(data, fpigr.0708, rfpigr.0708, ergr.0708,
                              gdp.const.int, cshare, cir, cer, lldc, ste, to, dtf, lpi.infra,
                              export, price, stocks,  tariff, tax))

m = lm(scale(fpigr.0708)~scale(ergr.0708)+
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra)+
         export+
         price+
         stocks+
         tariff+
         tax,
       data = filter(data1, fpigr.0708 < 3.5*sd(fpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
int.reg.m[[1]] = m
int.reg.se[[1]] = rse

m = lm(scale(fpigr.0708)~
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
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

# m = lm(scale(fpigr.0708)~scale(ergr.0708)+
#          log(gdp.const.int)+
#          I(log(gdp.const.int)^2)+
#          scale(cshare)+
#          scale(cir)+
#          scale(cer) +
#          lldc+
#          ste +
#          scale(to)+
#          scale(dtf)+
#          scale(lpi.infra)+
#          export+
#          price+
#          stocks+
#          tariff+
#          tax,
#        data = filter(data1, fpigr.0708 < 3.5*sd(fpigr.0708)))
# 
# step = stepAIC(m, direction="both")
# cov = vcovHC(step, type = "HC1")
# rse = sqrt(diag(cov))
# int.reg.m[[2]] = step
# int.reg.se[[2]] = rse

m = lm(scale(rfpigr.0708)~scale(ergr.0708)+
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
         scale(cshare)+
         scale(cir)+
         scale(cer) +
         lldc+
         ste +
         scale(to)+
         scale(dtf)+
         scale(lpi.infra)+
         export+
         price+
         stocks+
         tariff+
         tax,
       data = filter(data1, rfpigr.0708 < 3.5*sd(rfpigr.0708)))
cov = vcovHC(m, type = "HC1")
rse = sqrt(diag(cov))
int.reg.m[[3]] = m
int.reg.se[[3]] = rse

m = lm(scale(rfpigr.0708)~
         log(gdp.const.int)+
         I(log(gdp.const.int)^2)+
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
         
# m = lm(scale(rfpigr.0708)~scale(ergr.0708)+
#          log(gdp.const.int)+
#          I(log(gdp.const.int)^2)+
#          scale(cshare)+
#          scale(cir)+
#          scale(cer) +
#          lldc+
#          ste +
#          scale(to)+
#          scale(dtf)+
#          scale(lpi.infra)+
#          export+
#          price+
#          stocks+
#          tariff+
#          tax,
#        data = filter(data1, rfpigr.0708 < 3.5*sd(rfpigr.0708)))
# 
# step = stepAIC(m, direction="both")
# cov = vcovHC(step, type = "HC1")
# rse = sqrt(diag(cov))
# int.reg.m[[4]] = step
# int.reg.se[[4]] = rse

saveRDS(int.reg.m, "int_reg_m.rds")
saveRDS(int.reg.se, "int_reg_se.rds")

# end of script
