
library(dplyr)
library(dyn)
library(dynlm)

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
p = 6; q=6
aic.mat = array(NA, dim = c(nrow(tbl), p, q))
aic.min.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:j) +
                    d(log(fao.fpi)) +
                    L(d(log(fao.fpi)),1:k) + 
                    d(log(er)) +
                    L(d(log(er)),1:k), 
                  data = ts.dat)
        aic.mat[i,j,k] = extractAIC(m)[2]
      }
    }
  } else {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:j) + 
                    d(log(fao.fpi)) +
                    L(d(log(fao.fpi)),1:k), 
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
p = 6; q=6
aic.mat = array(NA, dim = c(nrow(tbl), p, q))
aic.min.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:j) +
                    d(log(r.fao.fpi)) +
                    L(d(log(r.fao.fpi)),1:k) + 
                    d(log(er)) +
                    L(d(log(er)),1:k), 
                  data = ts.dat)
        aic.mat[i,j,k] = extractAIC(m)[2]
      }
    }
  } else {
    for (j in 1:p) {
      for (k in 1:q) {
        m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:j) + 
                    d(log(r.fao.fpi)) +
                    L(d(log(r.fao.fpi)),1:k), 
                  data = ts.dat)
        aic.mat[i,j,k] = extractAIC(m)[2]
      }
    }
  }
  aic.min.mat[i,] = which(aic.mat[i,,] == min(aic.mat[i,,]), arr.ind = TRUE)
}
aic.min.rfpi = as.data.frame(aic.min.mat)
aic.min.rfpi$iso3c = names(tbl)

##################################################################

## Estimating the models. 
## Common lag length: median optimal lag lengths.

## Model: ADL
# 1) adj.fpi
p1 = median(aic.min.fpi[,1])
q1 = median(aic.min.fpi[,2])
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 5)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:p1) +
                d(log(fao.fpi)) +
                L(d(log(fao.fpi)),1:q1) + 
                d(log(er)) +
                L(d(log(er)),1:q1), 
              data = ts.dat)
  } else {
    m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:p1) + 
                d(log(fao.fpi)) +
                L(d(log(fao.fpi)),1:q1), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,1] = m$coefficients["(Intercept)"]
  coef.mat[i,2] = m$coefficients["d(log(fao.fpi))"]
  coef.mat[i,3] = m$coefficients["L(d(log(fao.fpi)), 1:q1)"]
  coef.mat[i,4] = m$coefficients["L(d(log(adj.fpi)), 1:p1)"]
  coef.mat[i,5] = sum(m$coefficients[3:4])/(1-m$coefficients[2])
}
coef.adl = as.data.frame(coef.mat)
coef.adl$iso3c = names(tbl)
colnames(coef.adl)[5] = "lrm"
mean(coef.adl[, "lrm"])

colnames(coef.adl) = c("intercept", "b0", "b1", "a1", "lrm")

##################################################################

## Model: Bewley transformation (1 lag)
# 1) adj.fpi
p1 = 1
q1 = 1
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    
    m.aux = dynlm(d(d(log(adj.fpi))) ~ L(d(log(adj.fpi)),1) +
                    d(log(fao.fpi)) +
                    d(log(er)) +
                    L(d(log(fao.fpi)),1) +
                    L(d(log(er)),1),
                  data = ts.dat)
    
    y.f = fitted(m.aux)
    
    m = dynlm(d(log(adj.fpi))~ y.f +
                d(log(fao.fpi)) +
                d(d(log(fao.fpi))) + 
                d(log(er)) +
                d(d(log(er))), 
              data = ts.dat)
  } else {
    
    m.aux = dynlm(d(d(log(adj.fpi))) ~ L(d(log(adj.fpi)),1) +
                    d(log(fao.fpi)) +
                    L(d(log(fao.fpi)),1),
                  data = ts.dat)
    
    y.f = fitted(m.aux)
    
    m = dynlm(d(log(adj.fpi))~ y.f +
                d(log(fao.fpi)) +
                d(d(log(fao.fpi))), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,1] = m$coefficients[3]
  coef.mat[i,2] = sqrt(diag(vcov(m)))[3]
}
coef.bew.fpi.l1 = as.data.frame(coef.mat)
coef.bew.fpi.l1$iso3c = names(tbl)
save(coef.bew.fpi.l1, file = "coef_bew_fpi_l1.Rda")

##################################################################

## Model: ECM
# 1) adj.fpi
p1 = 1 #median(aic.min.fpi[,1])
q1 = 1 #median(aic.min.fpi[,2])
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 5)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    m = dynlm(I(d(log(adj.fpi))-L(d(log(adj.fpi)),1))~ L(d(log(adj.fpi)),1:p1) +
                I(d(log(fao.fpi))-L(d(log(fao.fpi)),1)) +
                L(d(log(fao.fpi)),1:q1) + 
                I(d(log(er))-L(d(log(er)),1)) +
                L(d(log(er)),1:q1), 
              data = ts.dat)
  } else {
    m = dynlm(I(d(log(adj.fpi))-L(d(log(adj.fpi)),1))~ L(d(log(adj.fpi)),1:p1) +
                I(d(log(fao.fpi))-L(d(log(fao.fpi)),1)) +
                L(d(log(fao.fpi)),1:q1), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,1] = m$coefficients["L(d(log(fao.fpi)), 1:q1)"]
  coef.mat[i,2] = m$coefficients["L(d(log(adj.fpi)), 1:p1)"]
  coef.mat[i,3] = m$coefficients[4]/(-m$coefficients[2])
  coef.mat[i,4] = sqrt((1/m$coefficients[2]^2)*vcov(m)[4,4] +
    (m$coefficients[4]^2/m$coefficients[2]^4)*vcov(m)[2,2] -
    2*(m$coefficients[4]/m$coefficients[2]^3)*vcov(m)[4,2])
  coef.mat[i,5] = coef.mat[i,3]/coef.mat[i,4]
}
coef.ecm.fpi = as.data.frame(coef.mat)
coef.ecm.fpi$iso3c = names(tbl)
colnames(coef.ecm.fpi) = c("b1star", "a1star", "lrm", "lrm.se", "lrm.t", "iso3c")

# 2) rfpi
p1 = median(aic.min.rfpi[,1])
q1 = median(aic.min.rfpi[,2])
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 5)

for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, r.fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    m = dynlm(I(d(log(rfpi))-L(d(log(rfpi)),1))~ L(d(log(rfpi)),1:p1) +
                I(d(log(r.fao.fpi))-L(d(log(r.fao.fpi)),1)) +
                L(d(log(r.fao.fpi)),1:q1) + 
                I(d(log(er))-L(d(log(er)),1)) +
                L(d(log(er)),1:q1), 
              data = ts.dat)
  } else {
    m = dynlm(I(d(log(rfpi))-L(d(log(rfpi)),1))~ L(d(log(rfpi)),1:p1) +
                I(d(log(r.fao.fpi))-L(d(log(r.fao.fpi)),1)) +
                L(d(log(r.fao.fpi)),1:q1), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,1] = m$coefficients["L(d(log(r.fao.fpi)), 1:q1)"]
  coef.mat[i,2] = m$coefficients["L(d(log(rfpi)), 1:p1)"]
  coef.mat[i,3] = m$coefficients[4]/(-m$coefficients[2])
  coef.mat[i,4] = sqrt((1/m$coefficients[2]^2)*vcov(m)[4,4] +
                         (m$coefficients[4]^2/m$coefficients[2]^4)*vcov(m)[2,2] -
                         2*(m$coefficients[4]/m$coefficients[2]^3)*vcov(m)[4,2])
  coef.mat[i,5] = coef.mat[i,3]/coef.mat[i,4]
}
coef.ecm.rfpi = as.data.frame(coef.mat)
coef.ecm.rfpi$iso3c = names(tbl)
colnames(coef.ecm.rfpi) = c("b1star", "a1star", "lrm", "lrm.se", "lrm.t", "iso3c")

## save results
save(coef.ecm.fpi, file = "coef_ecm_fpi.l1.Rda")
save(coef.ecm.rfpi, file = "coef_ecm_rfpi.l1.Rda")

#############################

## Common lag length: max lag = 6. (ADL model)
p1 = 6
q1 = 6
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 2)

# 1) adj.fpi
for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:p1) +
                d(log(fao.fpi)) +
                L(d(log(fao.fpi)),1:q1) + 
                d(log(er)) +
                L(d(log(er)),1:q1), 
              data = ts.dat)
  } else {
    m = dynlm(d(log(adj.fpi))~ L(d(log(adj.fpi)),1:p1) + 
                d(log(fao.fpi)) +
                L(d(log(fao.fpi)),1:q1), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,1] = sum(m$coefficients[8:14])/(1-sum(m$coefficients[2:7]))
}

# 2) rfpi
for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:p1) +
                d(log(r.fao.fpi)) +
                L(d(log(r.fao.fpi)),1:q1) + 
                d(log(er)) +
                L(d(log(er)),1:q1), 
              data = ts.dat)
  } else {
    m = dynlm(d(log(rfpi))~ L(d(log(rfpi)),1:p1) + 
                d(log(r.fao.fpi)) +
                L(d(log(r.fao.fpi)),1:q1), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,2] = sum(m$coefficients[8:14])/(1-sum(m$coefficients[2:7]))
}

coef.adl.l6 = as.data.frame(coef.mat)
coef.adl.l6$iso3c = names(tbl)
coef.adl.l6 = rename(coef.adl.l6, lrm.fpi.l6=V1, lrm.rfpi.l6=V2)

## save results
save(coef.adl.l6, file = "coef_adl_l6.Rda")

#############################

## Common lag length: max lag = 6. (Bewley transformation)
p1 = 6
q1 = 6
coef.mat = matrix(NA, nrow = nrow(tbl), ncol = 4)

# 1) adj.fpi
for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
   
    m.aux = dynlm(d(d(log(adj.fpi))) ~ L(d(log(adj.fpi)),1:p1) +
                    d(log(fao.fpi)) +
                    d(log(er)) +
                    L(d(log(fao.fpi)),1:q1) +
                    L(d(log(er)),1:q1),
                  data = ts.dat)
    
    y.f = fitted(m.aux)
    
    m = dynlm(d(log(adj.fpi))~ y.f +
                d(log(fao.fpi)) +
                d(d(log(fao.fpi))) + 
                d(log(er)) +
                d(d(log(er))) +
                L(d(d(log(adj.fpi))), 1:(p1-1)) +
                L(d(d(log(fao.fpi))), 1:(q1-1)) +
                L(d(d(log(er))), 1:(q1-1)), 
              data = ts.dat)
  } else {
    
    m.aux = dynlm(d(d(log(adj.fpi))) ~ L(d(log(adj.fpi)),1:p1) +
                    d(log(fao.fpi)) +
                    L(d(log(fao.fpi)),1:q1),
                  data = ts.dat)
    
    y.f = fitted(m.aux)
    
    m = dynlm(d(log(adj.fpi))~ y.f +
                d(log(fao.fpi)) +
                d(d(log(fao.fpi))) + 
                L(d(d(log(adj.fpi))), 1:(p1-1)) +
                L(d(d(log(fao.fpi))), 1:(q1-1)), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,1] = m$coefficients[3]
  coef.mat[i,2] = sqrt(diag(vcov(m)))[3]
}

# 2) rfpi
for (i in 1:max(data$id)) {
  dat = filter(data, id==i)
  dat = select(dat, cpi.brk:rfpi, fao.fpi:er)
  ts.dat = ts(dat, start=c(2005, 1), end=c(2014, 12), frequency=12)
  
  if(is.na(dat$er)[1] == FALSE) {
    
    m.aux = dynlm(d(d(log(rfpi))) ~ L(d(log(rfpi)),1:p1) +
                    d(log(r.fao.fpi)) +
                    d(log(er)) +
                    L(d(log(r.fao.fpi)),1:q1) +
                    L(d(log(er)),1:q1),
                  data = ts.dat)
    
    y.f = fitted(m.aux)
    
    m = dynlm(d(log(rfpi))~ y.f +
                d(log(r.fao.fpi)) +
                d(d(log(r.fao.fpi))) + 
                d(log(er)) +
                d(d(log(er))) +
                L(d(d(log(rfpi))), 1:(p1-1)) +
                L(d(d(log(r.fao.fpi))), 1:(q1-1)) +
                L(d(d(log(er))), 1:(q1-1)), 
              data = ts.dat)
  } else {
    
    m.aux = dynlm(d(d(log(rfpi))) ~ L(d(log(rfpi)),1:p1) +
                    d(log(r.fao.fpi)) +
                    L(d(log(r.fao.fpi)),1:q1),
                  data = ts.dat)
    
    y.f = fitted(m.aux)
    
    m = dynlm(d(log(rfpi))~ y.f +
                d(log(r.fao.fpi)) +
                d(d(log(r.fao.fpi))) + 
                L(d(d(log(rfpi))), 1:(p1-1)) +
                L(d(d(log(r.fao.fpi))), 1:(q1-1)), 
              data = ts.dat)
  }
  # calculating the long run multiplier
  coef.mat[i,3] = m$coefficients[3]
  coef.mat[i,4] = sqrt(diag(vcov(m)))[3]
}

coef.bew.l6 = as.data.frame(coef.mat)
coef.bew.l6$iso3c = names(tbl)
coef.bew.l6 = rename(coef.bew.l6, lrm.fpi = V1, lrm.fpi.se = V2, 
                     lrm.rfpi = V3, lrm.rfpi.se = V4)
save(coef.bew.l6, file = "coef_bew_l6.Rda")
