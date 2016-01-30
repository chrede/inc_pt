
library(dplyr)
library(sandwich)
library(lmtest)
library(plm)
library(foreign)

##################################################################
### Panel price transmission regressions
##################################################################

load("ctr_dat_0708.Rda")
load("ts_dat.Rda")
df = merge(ts.dat, ctr.dat.0708, by = "iso3c")
df = df[!duplicated(df), ]
data = plm.data(df, index = c("iso3c", "tt"))
df1 = select(df, -fao.fpi)
df1 = rename(df1, fao.fpi = r.fao.fpi)
data1 = plm.data(df1, index = c("iso3c", "tt"))

panel.reg.m = list()
p = 6

#####################################################
## Fixed effects (FE) estimator

# FPI regressions
m = plm(diff(log(adj.fpi)) ~ lag(diff(log(adj.fpi)),1:p) +
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(diff(log(fao.fpi)),0)*log(gdp)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(gdp)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi) |
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          lag(diff(log(adj.fpi)),1:p) +
          I(lag(diff(log(fao.fpi)),0)*log(gdp)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(gdp)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi),
        data = data, 
        effect = "individual",
        model = "within",
        inst.method = "baltagi")

panel.reg.m[[1]] = m

m = plm(diff(log(adj.fpi)) ~ lag(diff(log(adj.fpi)),1:p) +
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(diff(log(fao.fpi)),0)*log(cons)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi) |
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          lag(diff(log(adj.fpi)),1:p) +
          I(lag(diff(log(fao.fpi)),0)*log(cons)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi),
        data = data, 
        effect = "individual",
        inst.method = "baltagi")

panel.reg.m[[2]] = m

# real FPI regressions
m = plm(diff(log(rfpi)) ~ lag(diff(log(rfpi)),1:p) +
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(diff(log(fao.fpi)),0)*log(gdp)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(gdp)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi) |
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          lag(diff(log(adj.fpi)),1:p) +
          I(lag(diff(log(fao.fpi)),0)*log(gdp)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(gdp)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi),
        data = data1, 
        effect = "individual",
        inst.method = "baltagi")

panel.reg.m[[3]] = m

m = plm(diff(log(rfpi)) ~ lag(diff(log(rfpi)),1:p) +
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(diff(log(fao.fpi)),0)*log(cons)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi) |
          lag(diff(log(fao.fpi)), 0:1) +
          lag(diff(log(er)), 0:1) +
          lag(diff(log(adj.fpi)),1:p) +
          I(lag(diff(log(fao.fpi)),0)*log(cons)) +
          I(lag(diff(log(fao.fpi)),0)*I(log(cons)^2)) +
          I(lag(diff(log(fao.fpi)),0)*cshare) +
          I(lag(diff(log(fao.fpi)),0)*cdep) +
          I(lag(diff(log(fao.fpi)),0)*lldc) +
          I(lag(diff(log(fao.fpi)),0)*to) +
          I(lag(diff(log(fao.fpi)),0)*dtf) +
          I(lag(diff(log(fao.fpi)),0)*lpi),
        data = data1, 
        effect = "individual",
        inst.method = "baltagi")

panel.reg.m[[4]] = m

saveRDS(panel.reg.m, "panel_reg_m.rds")

#####################################################
## Anderson–Hsiao (AH) estimator

# FPI regressions
m = plm(log(adj.fpi) ~ lag(log(adj.fpi),1:p) +
          lag(log(fao.fpi), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(log(fao.fpi),0)*log(gdp)) +
          I(lag(log(fao.fpi),0)*I(log(gdp)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi) |
          lag(log(fao.fpi), 0:1) +
          lag(log(er), 0:1) +
          lag(log(adj.fpi),(p+1):(p+6)) +
          I(lag(log(fao.fpi),0)*log(gdp)) +
          I(lag(log(fao.fpi),0)*I(log(gdp)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi),
        data = data1, 
        model = "fd",
        effect = "individual",
        inst.method = "baltagi")
panel.reg.m[[1]] = m

m = plm(log(adj.fpi) ~ lag(log(adj.fpi),1:p) +
          lag(log(fao.fpi), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(log(fao.fpi),0)*log(cons)) +
          I(lag(log(fao.fpi),0)*I(log(cons)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi) |
          lag(log(fao.fpi), 0:1) +
          lag(log(er), 0:1) +
          lag(log(adj.fpi),(p+1):(p+6)) +
          I(lag(log(fao.fpi),0)*log(cons)) +
          I(lag(log(fao.fpi),0)*I(log(cons)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi),
        data = data1, 
        model = "fd",
        effect = "individual",
        inst.method = "baltagi")
panel.reg.m[[2]] = m

# real FPI regressions
m = plm(log(rfpi) ~ lag(log(rfpi),1:p) +
          lag(log(fao.fpi), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(log(fao.fpi),0)*log(gdp)) +
          I(lag(log(fao.fpi),0)*I(log(gdp)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi) |
          lag(log(fao.fpi), 0:1) +
          lag(log(er), 0:1) +
          lag(log(adj.fpi),(p+1):(p+6)) +
          I(lag(log(fao.fpi),0)*log(gdp)) +
          I(lag(log(fao.fpi),0)*I(log(gdp)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi),
        data = data1, 
        model = "fd",
        effect = "individual",
        inst.method = "baltagi")
panel.reg.m[[3]] = m

m = plm(log(rfpi) ~ lag(log(rfpi),1:p) +
          lag(log(fao.fpi), 0:1) +
          lag(diff(log(er)), 0:1) +
          I(lag(log(fao.fpi),0)*log(cons)) +
          I(lag(log(fao.fpi),0)*I(log(cons)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi) |
          lag(log(fao.fpi), 0:1) +
          lag(log(er), 0:1) +
          lag(log(adj.fpi),(p+1):(p+6)) +
          I(lag(log(fao.fpi),0)*log(cons)) +
          I(lag(log(fao.fpi),0)*I(log(cons)^2)) +
          I(lag(log(fao.fpi),0)*cshare) +
          I(lag(log(fao.fpi),0)*cdep) +
          I(lag(log(fao.fpi),0)*lldc) +
          I(lag(log(fao.fpi),0)*to) +
          I(lag(log(fao.fpi),0)*dtf) +
          I(lag(log(fao.fpi),0)*lpi),
        data = data1, 
        model = "fd",
        effect = "individual",
        inst.method = "baltagi")
panel.reg.m[[4]] = m

saveRDS(panel.reg.m, "panel_reg_fd_m.rds")
