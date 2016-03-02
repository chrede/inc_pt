
library(dplyr)
library(dyn)
library(dynlm)
library(lmtest)

##################################################################
### Time series regressions
##################################################################

load("ts_dat.Rda")
data = ts.dat
tbl = table(data$id)
data$id = as.numeric(data$id)

##################################################################

## Finding the optimal lag lengths. 
## Model: ADL

# 1) adj.fpi
p = 12; q=12
aic.mat = array(NA, dim = c(nrow(tbl), p, q))
aic.min.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, fao.fpi:er, POILAPSP)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:j) +
                    d(log(fao.fpi)) +
                    L(d(log(fao.fpi)),1:k) + 
                    d(log(er)) +
                    L(d(log(er)),1:k) +
                    d(log(POILAPSP)) +
                    L(d(log(POILAPSP)),1:k), 
                  data = ts.dat)
        aic.mat[i,j,k] = extractAIC(m)[2]
      }
    }
  } else {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:j) + 
                    d(log(fao.fpi)) +
                    L(d(log(fao.fpi)),1:k) +
                    d(log(POILAPSP)) +
                    L(d(log(POILAPSP)),1:k), 
                  data = ts.dat)
        aic.mat[i,j,k] = extractAIC(m)[2]
      }
    }
  }
  aic.min.mat[i,] = which(aic.mat[i,,] == min(aic.mat[i,,]), arr.ind = TRUE)
}
aic.min.fpi = as.data.frame(aic.min.mat)
aic.min.fpi$iso3c = names(tbl)

# 2) rfpi
p = 12; q=12
aic.mat = array(NA, dim = c(nrow(tbl), p, q))
aic.min.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, fao.fpi:er, POILAPSP)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:j) +
                    d(log(r.fao.fpi)) +
                    L(d(log(r.fao.fpi)),1:k) + 
                    d(log(er)) +
                    L(d(log(er)),1:k) +
                    d(log(POILAPSP)) +
                    L(d(log(POILAPSP)),1:k), 
                  data = ts.dat)
        aic.mat[i,j,k] = extractAIC(m)[2]
      }
    }
  } else {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:j) + 
                    d(log(r.fao.fpi)) +
                    L(d(log(r.fao.fpi)),1:k) +
                    d(log(POILAPSP)) +
                    L(d(log(POILAPSP)),1:k), 
                  data = ts.dat)
        aic.mat[i,j,k] = extractAIC(m)[2]
      }
    }
  }
  aic.min.mat[i,] = which(aic.mat[i,,] == min(aic.mat[i,,]), arr.ind = TRUE)
}
aic.min.rfpi = as.data.frame(aic.min.mat)
aic.min.rfpi$iso3c = names(tbl)

median(aic.min.fpi[,1])
median(aic.min.fpi[,2])
median(aic.min.rfpi[,1])
median(aic.min.rfpi[,2])

##################################################################

### Estimating the models (Bewley transformation).
## Common lag length: median optimal lag lengths. (=1)

data = ts.dat
tbl = table(data$id)
data$id = as.numeric(data$id)

# 1) adj.fpi
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, fao.fpi:er, POILAPSP)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE && sd(dat$er, na.rm = TRUE) != 0 ) {
    
    m = dynlm(d(log(adj.fpi))~ d(d(log(adj.fpi))) +
                d(log(fao.fpi)) +
                d(d(log(fao.fpi))) + 
                d(log(er)) +
                d(d(log(er)))  + 
                d(log(POILAPSP)) +
                d(d(log(POILAPSP))) |
                L(d(log(adj.fpi)),1) +
                d(log(fao.fpi)) +
                d(log(er)) +
                d(log(POILAPSP)) +
                L(d(log(fao.fpi)),1) +
                L(d(log(er)),1) +
                L(d(log(POILAPSP)),1), 
              data = ts.dat)
    
  } else {
    
    m = dynlm(d(log(adj.fpi))~ d(d(log(adj.fpi))) +
                d(log(fao.fpi)) +
                d(d(log(fao.fpi)))  + 
                d(log(POILAPSP)) +
                d(d(log(POILAPSP))) |
                L(d(log(adj.fpi)),1) +
                d(log(fao.fpi))  +
                d(log(POILAPSP)) +
                L(d(log(fao.fpi)),1) +
                L(d(log(POILAPSP)),1), 
              data = ts.dat)
  }
  # Recording the long run multiplier
  coef.mat[i,1] = m$coefficients[3]
  coef.mat[i,2] = sqrt(diag(vcov(m)))[3]
}
coef.bew.fpi.l1 = as.data.frame(coef.mat)
coef.bew.fpi.l1$iso3c = names(tbl)
coef.bew.fpi.l1 = dplyr::rename(coef.bew.fpi.l1, lrm = V1, lrm.se = V2)
save(coef.bew.fpi.l1, file = "coef_bew_fpi_l1.Rda")

# 2) rfpi
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, r.fao.fpi:er, POILAPSP)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE && sd(dat$er, na.rm = TRUE) != 0 ) {
    
    m = dynlm(d(log(rfpi))~ d(d(log(rfpi))) +
                d(log(r.fao.fpi)) +
                d(d(log(r.fao.fpi))) + 
                d(log(er)) +
                d(d(log(er))) |
                L(d(log(rfpi)),1) +
                d(log(r.fao.fpi)) +
                d(log(er)) +
                L(d(log(r.fao.fpi)),1) +
                L(d(log(er)),1), 
              data = ts.dat)
    
  } else {
    
    m = dynlm(d(log(rfpi))~ d(d(log(rfpi))) +
                d(log(r.fao.fpi)) +
                d(d(log(r.fao.fpi))) |
                L(d(log(rfpi)),1) +
                d(log(r.fao.fpi)) +
                L(d(log(r.fao.fpi)),1), 
              data = ts.dat)
  }
  # Recording the long run multiplier
  coef.mat[i,1] = m$coefficients[3]
  coef.mat[i,2] = sqrt(diag(vcov(m)))[3]
}
coef.bew.rfpi.l1 = as.data.frame(coef.mat)
coef.bew.rfpi.l1$iso3c = names(tbl)
coef.bew.rfpi.l1 = rename(coef.bew.rfpi.l1, lrm = V1, lrm.se = V2)
save(coef.bew.rfpi.l1, file = "coef_bew_rfpi_l1.Rda")

## Common lag length = 6
# 1) adj.fpi
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)
k=6

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE && sd(dat$er, na.rm = TRUE) != 0 ) {
    
    m = dynlm(d(log(adj.fpi))~ d(log(fao.fpi)) +
                L(d(d(log(adj.fpi))), 0:(k-1)) +
                L(d(d(log(fao.fpi))), 0:(k-1)) +
                d(log(er)) +
                L(d(d(log(er))), 0:(k-1)) |
                L(d(log(adj.fpi)),1:k) +
                d(log(fao.fpi)) +
                L(d(d(log(fao.fpi))), 0:(k-1)) +
                d(log(er)) +
                L(d(d(log(er))), 0:(k-1)), 
              data = ts.dat)
    
  } else {
    
    m = dynlm(d(log(adj.fpi))~ d(log(fao.fpi)) +
                L(d(d(log(adj.fpi))), 0:(k-1)) +
                L(d(d(log(fao.fpi))), 0:(k-1)) |
                L(d(log(adj.fpi)),1:k) +
                d(log(fao.fpi)) +
                L(d(d(log(fao.fpi))), 0:(k-1)), 
              data = ts.dat)
  }
  # Recording the long run multiplier
  coef.mat[i,1] = m$coefficients[2]
  coef.mat[i,2] = sqrt(diag(vcov(m)))[2]
}
coef.bew.fpi.l6 = as.data.frame(coef.mat)
coef.bew.fpi.l6$iso3c = names(tbl)
coef.bew.fpi.l6 = rename(coef.bew.fpi.l6, lrm = V1, lrm.se = V2)
save(coef.bew.fpi.l6, file = "coef_bew_fpi_l6.Rda")

# 2) rfpi
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = dplyr::select(dat, cpi.brk:rfpi, r.fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE && sd(dat$er, na.rm = TRUE) != 0 ) {
    
    m = dynlm(d(log(rfpi))~ d(log(r.fao.fpi)) +
                L(d(d(log(rfpi))), 0:(k-1)) +
                L(d(d(log(r.fao.fpi))), 0:(k-1)) +
                d(log(er)) +
                L(d(d(log(er))), 0:(k-1)) |
                L(d(log(rfpi)),1:k) +
                d(log(r.fao.fpi)) +
                L(d(d(log(r.fao.fpi))), 0:(k-1)) +
                d(log(er)) +
                L(d(d(log(er))), 0:(k-1)), 
              data = ts.dat)

  } else {
    
    m = dynlm(d(log(rfpi))~ d(log(r.fao.fpi)) +
                L(d(d(log(rfpi))), 0:(k-1)) +
                L(d(d(log(r.fao.fpi))), 0:(k-1)) |
                L(d(log(rfpi)),1:k) +
                d(log(r.fao.fpi)) +
                L(d(d(log(r.fao.fpi))), 0:(k-1)), 
              data = ts.dat)
  }
  # Recording the long run multiplier
  coef.mat[i,1] = m$coefficients[2]
  coef.mat[i,2] = sqrt(diag(vcov(m)))[2]
}
coef.bew.rfpi.l6 = as.data.frame(coef.mat)
coef.bew.rfpi.l6$iso3c = names(tbl)
coef.bew.rfpi.l6 = rename(coef.bew.rfpi.l6, lrm = V1, lrm.se = V2)
save(coef.bew.rfpi.l6, file = "coef_bew_rfpi_l6.Rda")

##################################################################

### Comparison of results based on 1 and 12 lags (ADL model).

# 1) adj.fpi
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 12)
dwtest.mat = matrix(NA, nrow = nrow(tbl), ncol = 12)
imp.coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 12)
imp.se.mat = matrix(NA, nrow = nrow(tbl), ncol = 12)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE & sd(dat$er, na.rm = TRUE) != 0) {
    for (j in 1:12) {
      m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:j) +
                  d(log(fao.fpi)) +
                  L(d(log(fao.fpi)),1:j) + 
                  d(log(er)) +
                  L(d(log(er)),1:j), 
                data = ts.dat)
      coef.mat[i,j] = (sum(coef(m)[(j+2):(2+2*j)]))/(1-sum(coef(m)[2:(j+1)]))
      dwtest.mat[i,j] = dwtest(m)[["p.value"]]
      imp.coef.mat[i,j] = coef(m)[j+2]
      imp.se.mat[i,j] = sqrt(diag(vcov(m)))[j+2]
    }
  } else {
    for (j in 1:12) {
      m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:j) + 
                  d(log(fao.fpi)) +
                  L(d(log(fao.fpi)),1:j), 
                data = ts.dat)
      coef.mat[i,j] = (sum(coef(m)[(j+2):(2+2*j)]))/(1-sum(coef(m)[2:(j+1)]))
      dwtest.mat[i,j] = dwtest(m)[["p.value"]]
      imp.coef.mat[i,j] = coef(m)[j+2]
      imp.se.mat[i,j] = sqrt(diag(vcov(m)))[j+2]
    }
  }
}
lrm.fpi.l.1_12 = as.data.frame(coef.mat)
lrm.fpi.l.1_12$iso3c = names(tbl)
dwtest.fpi.l.1_12 = as.data.frame(dwtest.mat)
dwtest.fpi.l.1_12$iso3c = names(tbl)
save(lrm.fpi.l.1_12, file = "lrm_fpi_l1_12.Rda")

imp.fpi.l.1_12 = as.data.frame(imp.coef.mat)
imp.fpi.l.1_12$iso3c = names(tbl)
save(imp.fpi.l.1_12, file = "imp_fpi_l1_12.Rda")

imp.se.fpi.l.1_12 = as.data.frame(imp.se.mat)
imp.se.fpi.l.1_12$iso3c = names(tbl)
save(imp.se.fpi.l.1_12, file = "imp_se_fpi_l1_12.Rda")

# 2) rfpi
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 12)
imp.coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 12)
imp.se.mat = matrix(NA, nrow = nrow(tbl), ncol = 12)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, r.fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    for (j in 1:12) {
      m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:j) +
                  d(log(r.fao.fpi)) +
                  L(d(log(r.fao.fpi)),1:j) + 
                  d(log(er)) +
                  L(d(log(er)),1:j), 
                data = ts.dat)
      coef.mat[i,j] = (sum(coef(m)[(j+2):(2+2*j)]))/(1-sum(coef(m)[2:(j+1)]))
      imp.coef.mat[i,j] = coef(m)[j+2]
      imp.se.mat[i,j] = sqrt(diag(vcov(m)))[j+2]
    }
  } else {
    for (j in 1:12) {
      m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:j) + 
                  d(log(r.fao.fpi)) +
                  L(d(log(r.fao.fpi)),1:j), 
                data = ts.dat)
      coef.mat[i,j] = (sum(coef(m)[(j+2):(2+2*j)]))/(1-sum(coef(m)[2:(j+1)]))
      imp.coef.mat[i,j] = coef(m)[j+2]
      imp.se.mat[i,j] = sqrt(diag(vcov(m)))[j+2]
    }
  }
}
lrm.rfpi.l.1_12 = as.data.frame(coef.mat)
lrm.rfpi.l.1_12$iso3c = names(tbl)
save(lrm.rfpi.l.1_12, file = "lrm_rfpi_l1_12.Rda")

imp.rfpi.l.1_12 = as.data.frame(imp.coef.mat)
imp.rfpi.l.1_12$iso3c = names(tbl)
save(imp.rfpi.l.1_12, file = "imp_rfpi_l1_12.Rda")
imp.se.rfpi.l.1_12 = as.data.frame(imp.se.mat)
imp.se.rfpi.l.1_12$iso3c = names(tbl)
save(imp.se.rfpi.l.1_12, file = "imp_se_rfpi_l1_12.Rda")

# End of script
