
library(dplyr)
library(urca)
library(tseries)

##################################################################
### Unit root and cointegration tests
##################################################################

load("ts_dat.Rda")
data = ts.dat
tbl = table(data$id)
data$id = as.numeric(data$id)

## FPI inflation
test.mat = matrix(NA, nrow = nrow(tbl), ncol = 8)
for (i in 1: nrow(tbl)) {
  dat = dplyr::filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, fao.fpi:er)
  
  # KPSS test
  kpss.lev = kpss.test(log(dat$adj.fpi),  null = "L", lshort = FALSE)
  kpss.dif = kpss.test(diff(log(dat$adj.fpi)),  null = "L", lshort = FALSE)
  test.mat[i,1] = kpss.lev[[1]]
  test.mat[i,2] = kpss.dif[[1]]
  test.mat[i,3] = kpss.lev[[3]]
  test.mat[i,4] = kpss.dif[[3]]

  # ADF test
  adf.lev = adf.test(log(dat$adj.fpi), alternative = "s")
  adf.dif = adf.test(diff(log(dat$adj.fpi)), alternative = "s")
  test.mat[i,5] = adf.lev[[1]]
  test.mat[i,6] = adf.dif[[1]]
  test.mat[i,7] = adf.lev[[4]]
  test.mat[i,8] = adf.dif[[4]]
}
ur.test = as.data.frame(test.mat)
ur.test$iso3c = names(tbl)

## real FPI growth
test.mat = matrix(NA, nrow = nrow(tbl), ncol = 8)
for (i in 1: nrow(tbl)) {
  dat = dplyr::filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, fao.fpi:er)
  
  # KPSS test
  kpss.lev = kpss.test(log(dat$rfpi),  null = "L", lshort = FALSE)
  kpss.dif = kpss.test(diff(log(dat$rfpi)),  null = "L", lshort = FALSE)
  test.mat[i,1] = kpss.lev[[1]]
  test.mat[i,2] = kpss.dif[[1]]
  test.mat[i,3] = kpss.lev[[3]]
  test.mat[i,4] = kpss.dif[[3]]
  
  # ADF test
  adf.lev = adf.test(log(dat$rfpi), alternative = "s")
  adf.dif = adf.test(diff(log(dat$rfpi)), alternative = "s")
  test.mat[i,5] = adf.lev[[1]]
  test.mat[i,6] = adf.dif[[1]]
  test.mat[i,7] = adf.lev[[4]]
  test.mat[i,8] = adf.dif[[4]]
}
ur.test = as.data.frame(test.mat)
ur.test$iso3c = names(tbl)

## international prices
dat = dplyr::filter(data, id == 1)
adf.test(log(dat$fao.fpi), alternative = "s")
adf.test(log(dat$r.fao.fpi), alternative = "s")
adf.test(diff(log(dat$fao.fpi)), alternative = "s")
adf.test(diff(log(dat$r.fao.fpi)), alternative = "s")

kpss.test(log(dat$fao.fpi), null = "L", lshort = FALSE)
kpss.test(log(dat$r.fao.fpi), null = "L", lshort = FALSE)
kpss.test(diff(log(dat$fao.fpi)), null = "L", lshort = FALSE)
kpss.test(diff(log(dat$r.fao.fpi)), null = "L", lshort = FALSE)

## Cointegration tests
# FPI - FAO FPI - ER
test.mat = matrix(NA, nrow = nrow(tbl), ncol = 3)
for (i in 1: nrow(tbl)) {
  dat = dplyr::filter(data, id == i)
  if(is.na(dat$er)[1] == FALSE & sd(dat$er, na.rm = TRUE) != 0) {
    dat1 = dplyr::select(dat, adj.fpi, fao.fpi, er)
    test = ca.po(log(dat1), lag = "long", 
                 type = "Pz", demean = "constant")
    test.mat[i,1] = test@teststat
    test.mat[i,2] = test@cval[2]
  } else {
    dat1 = dplyr::select(dat, adj.fpi, fao.fpi)
    test = ca.po(log(dat1), lag = "long", 
                 type = "Pz", demean = "constant")
    test.mat[i,1] = test@teststat
    test.mat[i,2] = test@cval[2]
  }
  test.mat[i,3] = ifelse(test.mat[i,1]/test.mat[i,2]<1,0,1)
}
ci.test = as.data.frame(test.mat)
ci.test$iso3c = names(tbl)

# real FPI - real FAO FPI - ER
test.mat = matrix(NA, nrow = nrow(tbl), ncol = 3)
for (i in 1: nrow(tbl)) {
  dat = dplyr::filter(data, id == i)
  if(is.na(dat$er)[1] == FALSE & sd(dat$er, na.rm = TRUE) != 0) {
    dat1 = dplyr::select(dat, rfpi, r.fao.fpi, er)
    test = ca.po(log(dat1), lag = "long", 
                 type = "Pz", demean = "constant")
    test.mat[i,1] = test@teststat
    test.mat[i,2] = test@cval[2]
  } else {
    dat1 = dplyr::select(dat, rfpi, r.fao.fpi)
    test = ca.po(log(dat1), lag = "long", 
                 type = "Pz", demean = "constant")
    test.mat[i,1] = test@teststat
    test.mat[i,2] = test@cval[2]
  }
  test.mat[i,3] = ifelse(test.mat[i,1]/test.mat[i,2]<1,0,1)
}
ci.test = as.data.frame(test.mat)
ci.test$iso3c = names(tbl)