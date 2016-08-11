
library(dplyr)
library(sandwich)
library(lmtest)
library(plm)

##################################################################
### Panel price transmission regressions
##################################################################

str = "C:/Users/nqw235/Google Drive/Research/pov_price_transmission/"
setwd(str)
load("infl_ctr_dat_0708.Rda")
load("ts_dat.Rda")
df = merge(ts.dat, ctr.dat.0708, by = "iso3c")
data = plm.data(df, index = c("iso3c", "ff"))
df1 = dplyr::select(df, -fao.fpi)
df1 = dplyr::rename(df1, fao.fpi = r.fao.fpi)
data1 = plm.data(df1, index = c("iso3c", "ff"))

panel.reg.m = list()
panel.reg.se = list()
p = 6

#####################################################
## Fixed effects (FE) estimator

# FPI regressions
m = plm(diff(log(adj.fpi)) ~ 
          lag(diff(log(adj.fpi)),1) +
          lag(diff(log(adj.fpi)),2) +
          lag(diff(log(adj.fpi)),3) +
          lag(diff(log(adj.fpi)),4) +
          lag(diff(log(adj.fpi)),5) +
          lag(diff(log(adj.fpi)),6) +
          lag(diff(log(fao.fpi)), 0) +
          lag(diff(log(fao.fpi)), 1) +
          lag(diff(log(er)), 0) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(gdp.const.int)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(gdp.const.int)^2)) +
          I(lag(diff(log(fao.fpi)),1)*cshare) +
          I(lag(diff(log(fao.fpi)),1)*cir) +
          I(lag(diff(log(fao.fpi)),1)*cer) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*ste) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi.infra) ,
        data = data, 
        effect = "individual",
        model = "within")

cov = vcovHC(m, method="arellano", type="HC1")
rse = sqrt(diag(cov))
panel.reg.m[[1]] = m
panel.reg.se[[1]] = rse

m = plm(diff(log(adj.fpi)) ~ 
          lag(diff(log(adj.fpi)),1) +
          lag(diff(log(adj.fpi)),2) +
          lag(diff(log(adj.fpi)),3) +
          lag(diff(log(adj.fpi)),4) +
          lag(diff(log(adj.fpi)),5) +
          lag(diff(log(adj.fpi)),6) +
          lag(diff(log(fao.fpi)), 0) +
          lag(diff(log(fao.fpi)), 1) +
          lag(diff(log(er)), 0) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(gdp.const.int)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(gdp.const.int)^2)),
        data = data, 
        effect = "individual",
        model = "within")

cov = vcovHC(m, method="arellano", type="HC1")
rse = sqrt(diag(cov))
panel.reg.m[[2]] = m
panel.reg.se[[2]] = rse

# m = plm(diff(diff(log(adj.fpi))) ~ 
#           lag(diff(diff(log(adj.fpi))),1:p) +
#           lag(diff(diff(log(fao.fpi))), 0:1) +
#           lag(diff(diff(log(er))), 0:1) +
#           I(lag(diff(diff(log(fao.fpi))),0)*log(gdp.const.int)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*I(log(gdp.const.int)^2)) + 
#           I(lag(diff(diff(log(fao.fpi))),0)*cshare) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cir) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cer) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lldc) +
#           I(lag(diff(diff(log(fao.fpi))),0)*ste) +
#           I(lag(diff(diff(log(fao.fpi))),0)*to) +
#           I(lag(diff(diff(log(fao.fpi))),0)*dtf) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lpi.infra) -1 |
#           lag(diff(diff(log(fao.fpi))), 0:1) +
#           lag(diff(diff(log(er))), 0:1) +
#           lag(diff(diff(log(adj.fpi))),2:(p+1)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*log(gdp.const.int)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*I(log(gdp.const.int)^2)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cshare) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cir) +          
#           I(lag(diff(diff(log(fao.fpi))),0)*cer) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lldc) +
#           I(lag(diff(diff(log(fao.fpi))),0)*ste) +
#           I(lag(diff(diff(log(fao.fpi))),0)*to) +
#           I(lag(diff(diff(log(fao.fpi))),0)*dtf) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lpi.infra),
#         data = data, 
#         effect = "individual",
#         model = "pooling")
# 
# panel.reg.m[[2]] = m

# real FPI regressions
m = plm(diff(log(rfpi)) ~           
          lag(diff(log(rfpi)),1) +
          lag(diff(log(rfpi)),2) +
          lag(diff(log(rfpi)),3) +
          lag(diff(log(rfpi)),4) +
          lag(diff(log(rfpi)),5) +
          lag(diff(log(rfpi)),6) +
          lag(diff(log(fao.fpi)), 0) +
          lag(diff(log(fao.fpi)), 1) +
          lag(diff(log(er)), 0) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(gdp.const.int)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(gdp.const.int)^2)) +
          I(lag(diff(log(fao.fpi)),1)*cshare) +
          I(lag(diff(log(fao.fpi)),1)*cir) +
          I(lag(diff(log(fao.fpi)),1)*cer) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*ste) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi.infra),
        data = data1, 
        effect = "individual",
        model = "within")

cov = vcovHC(m, method="arellano", type="HC1")
rse = sqrt(diag(cov))
panel.reg.m[[3]] = m
panel.reg.se[[3]] = rse

m = plm(diff(log(rfpi)) ~           
          lag(diff(log(rfpi)),1) +
          lag(diff(log(rfpi)),2) +
          lag(diff(log(rfpi)),3) +
          lag(diff(log(rfpi)),4) +
          lag(diff(log(rfpi)),5) +
          lag(diff(log(rfpi)),6) +
          lag(diff(log(fao.fpi)), 0) +
          lag(diff(log(fao.fpi)), 1) +
          lag(diff(log(er)), 0) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(gdp.const.int)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(gdp.const.int)^2)),
        data = data1, 
        effect = "individual",
        model = "within")

cov = vcovHC(m, method="arellano", type="HC1")
rse = sqrt(diag(cov))
panel.reg.m[[4]] = m
panel.reg.se[[4]] = rse

# m = plm(diff(diff(log(rfpi))) ~ 
#           lag(diff(diff(log(rfpi))),1:p) +
#           lag(diff(diff(log(fao.fpi))), 0:1) +
#           lag(diff(diff(log(er))), 0:1) +
#           I(lag(diff(diff(log(fao.fpi))),0)*log(gdp.const.int)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*I(log(gdp.const.int)^2)) + 
#           I(lag(diff(diff(log(fao.fpi))),0)*cshare) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cir) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cer) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lldc) +
#           I(lag(diff(diff(log(fao.fpi))),0)*ste) +
#           I(lag(diff(diff(log(fao.fpi))),0)*to) +
#           I(lag(diff(diff(log(fao.fpi))),0)*dtf) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lpi.infra) -1 |
#           lag(diff(diff(log(fao.fpi))), 0:1) +
#           lag(diff(diff(log(er))), 0:1) +
#           lag(diff(diff(log(rfpi))),2:(p+1)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*log(gdp.const.int)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*I(log(gdp.const.int)^2)) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cshare) +
#           I(lag(diff(diff(log(fao.fpi))),0)*cir) +          
#           I(lag(diff(diff(log(fao.fpi))),0)*cer) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lldc) +
#           I(lag(diff(diff(log(fao.fpi))),0)*ste) +
#           I(lag(diff(diff(log(fao.fpi))),0)*to) +
#           I(lag(diff(diff(log(fao.fpi))),0)*dtf) +
#           I(lag(diff(diff(log(fao.fpi))),0)*lpi.infra),
#         data = data1, 
#         effect = "individual",
#         model = "pooling")
# 
# panel.reg.m[[4]] = m

saveRDS(panel.reg.m, "panel_reg_m.rds")
saveRDS(panel.reg.se, "panel_reg_se.rds")

# End of script
