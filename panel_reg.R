
library(dplyr)
library(sandwich)
library(lmtest)
library(plm)
library(foreign)

##################################################################
### Cross section price transmission regressions
##################################################################

load("ctr_dat_0708.Rda")
load("ts_dat.Rda")
df = merge(ts.dat, ctr.dat.0708, by = "iso3c")
df = df[!duplicated(df), ]
data = plm.data(df, index = c("iso3c", "tt"))
df1 = select(df, -fao.fpi)
df1 = rename(df1, fao.fpi = r.fao.fpi)
data1 = plm.data(df1, index = c("iso3c", "tt"))
# write.dta(data, "panel.dta")

panel.reg = list()

m = plm(diff(log(adj.fpi)) ~ lag(diff(log(adj.fpi)),1) +
          diff(log(fao.fpi)) +
          lag(diff(log(fao.fpi)), 1) +
          diff(log(er)) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(gdp)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(gdp)^2)) +
          I(lag(diff(log(fao.fpi)),1)*cshare) +
          I(lag(diff(log(fao.fpi)),1)*cdep) +
          I(lag(diff(log(fao.fpi)),1)*I(cshare*cdep)) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi),
        data = data, model = "within")

cov = vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
rse = sqrt(diag(cov))

panel.reg[[1]] = m
panel.reg[[2]] = rse

m = plm(diff(log(adj.fpi)) ~ lag(diff(log(adj.fpi)),1) +
          diff(log(fao.fpi)) +
          lag(diff(log(fao.fpi)), 1) +
          diff(log(er)) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(cons)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),1)*nir) +
          I(lag(diff(log(fao.fpi)),1)*I(nir^2)) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi),
        data = data, model = "within")

cov = vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
rse = sqrt(diag(cov))

panel.reg[[3]] = m
panel.reg[[4]] = rse

m = plm(diff(log(adj.fpi)) ~ lag(diff(log(adj.fpi)),1) +
          diff(log(fao.fpi)) +
          lag(diff(log(fao.fpi)), 1) +
          diff(log(er)) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(cons)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),1)*cshare) +
          I(lag(diff(log(fao.fpi)),1)*cdep) +
          I(lag(diff(log(fao.fpi)),1)*I(cshare*cdep)) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi),
        data = data, model = "within")

cov = vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
rse = sqrt(diag(cov))

panel.reg[[5]] = m
panel.reg[[6]] = rse

m = plm(diff(log(rfpi)) ~ lag(diff(log(rfpi)),1) +
          diff(log(fao.fpi)) +
          lag(diff(log(fao.fpi)), 1) +
          diff(log(er)) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(gdp)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(gdp)^2)) +
          I(lag(diff(log(fao.fpi)),1)*cshare) +
          I(lag(diff(log(fao.fpi)),1)*cdep) +
          I(lag(diff(log(fao.fpi)),1)*I(cshare*cdep)) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi),
        data = data1, model = "within")

cov = vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
rse = sqrt(diag(cov))

panel.reg[[7]] = m
panel.reg[[8]] = rse

m = plm(diff(log(rfpi)) ~ lag(diff(log(rfpi)),1) +
          diff(log(fao.fpi)) +
          lag(diff(log(fao.fpi)), 1) +
          diff(log(er)) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(cons)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),1)*nir) +
          I(lag(diff(log(fao.fpi)),1)*I(nir^2)) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi),
        data = data1, model = "within")

cov = vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
rse = sqrt(diag(cov))

panel.reg[[9]] = m
panel.reg[[10]] = rse

m = plm(diff(log(rfpi)) ~ lag(diff(log(rfpi)),1) +
          diff(log(fao.fpi)) +
          lag(diff(log(fao.fpi)), 1) +
          diff(log(er)) +
          lag(diff(log(er)), 1) +
          I(lag(diff(log(fao.fpi)),1)*log(cons)) +
          I(lag(diff(log(fao.fpi)),1)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),1)*cshare) +
          I(lag(diff(log(fao.fpi)),1)*cdep) +
          I(lag(diff(log(fao.fpi)),1)*I(cshare*cdep)) +
          I(lag(diff(log(fao.fpi)),1)*lldc) +
          I(lag(diff(log(fao.fpi)),1)*to) +
          I(lag(diff(log(fao.fpi)),1)*dtf) +
          I(lag(diff(log(fao.fpi)),1)*lpi),
        data = data1, model = "within")

cov = vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
rse = sqrt(diag(cov))

panel.reg[[11]] = m
panel.reg[[12]] = rse

saveRDS(panel.reg, "panel_reg.rds")
