#####################################################################
#### Creating the domestic price dataset 
#####################################################################

library(foreign)
library(dplyr)
library(reshape)
library(zoo)
library(data.table)
library(ggplot2)
library(lattice)
library(countrycode)

### Raw data:
# laborstaB9.csv
# laborstaBA.csv

#####################################################################
#### Reading and merging the raw price data

str = "C:/Users/nqw235/Google Drev/Research/pov_price_transmission/"

### The ILO data
temp = paste(str, "laborstaB9.csv", sep = "")
B9 = read.csv(temp, sep = ";")
temp = paste(str, "laborstaBA.csv", sep = "")
BA = read.csv(temp, sep = ";")

## Reshape to long format
B9$id = paste0(B9$CODE.COUNTRY, B9$CODE.AREA)
B9[B9$id == "LKLK1" & B9$YEAR == 2002, ]
B9 = B9[-2659, ] # coding mistake
B9l = reshape(B9, varying=c(9:21), direction="long", v.names="cpi", 
              idvar=c("id", "YEAR"),
              sep="", timevar = "month")
B9l = B9l[order(B9l$id, B9l$YEAR, B9l$month), ]
B9l$SRC_ID = NULL
B9l = rename(B9l, c("NOTES" = "cpi.notes"))

BA$id = paste0(BA$CODE.COUNTRY, BA$CODE.AREA)
BA[BA$id == "LKLK1" & BA$YEAR == 2002, ]
BA = BA[-2629, ] # coding mistake
BAl = reshape(BA, varying=c(9:21), direction="long", v.names="fpi", 
              idvar=c("id", "YEAR"),
              sep="", timevar = "month")
BAl = BAl[order(BAl$id, BAl$YEAR, BAl$month), ]
BAl$SRC_ID = NULL
BAl = rename(BAl, c("NOTES" = "fpi.notes"))

## Merge CPI and FPI datasets
ilo = merge(B9l, BAl, all=TRUE)
ilo = ilo[order(ilo$id, ilo$YEAR, ilo$month), ]
ilo = rename(ilo, c("YEAR" = "year", "COUNTRY"="country", 
                    "CODE.COUNTRY"="c.code", "CODE.AREA"="a.code"))

ilo = ilo[ilo$month != 13, ]
ilo$tt = as.yearmon(paste(ilo$year, ilo$month, sep = "-"))

rm(B9, BA, B9l, BAl)

#####################################################################
#### Choosing the set of domestic price series

## Identifying countries with multiple price series and select single series
ilo$id = factor(ilo$id)
str(ilo$id)
summary(tabulate(ilo$id))
table(ilo$id)
table(table(ilo$id))
ilo$c.code = factor(ilo$c.code)
str(ilo$c.code)
table(table(ilo$c.code))
tbl = table(ilo$c.code)
# subset of countries with > 1 price serie
dat = droplevels(ilo[ilo$c.code %in% names(tbl)[tbl > 180],,drop=FALSE])
table(dat$c.code)
length(table(dat$c.code))

# visual inspection of the series
# c = "MM"
# datsub = ilo[ilo$c.code == c, ]
# dat.m = melt(datsub,measure.vars=c('cpi','fpi'))
# ggplot(dat.m) +
#   geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
#   facet_grid(~variable)

# drop/keep (ir)relevant series
ilo = ilo[ilo$id != "BRBR1", ]
ilo = ilo[ilo$c.code != "BS", ]
ilo = ilo[ilo$c.code != "CL", ]
ilo = ilo[ilo$c.code != "CV", ]
ilo = ilo[ilo$id != "ETET1", ]
ilo = ilo[!(ilo$id %in% c("ININ4", "ININ6", "ININ7")), ]
ilo = ilo[ilo$c.code != "KE", ]
ilo = ilo[ilo$c.code != "LB", ]
ilo = ilo[!(ilo$id %in% c("MGMG1", "MGMG2")), ]
ilo = ilo[ilo$c.code != "MN", ]
ilo = ilo[ilo$id != "MZMZ1", ]
ilo = ilo[ilo$id != "NINI1", ]
ilo = ilo[ilo$id != "OMOM1", ]
ilo = ilo[ilo$id != "PAPA1", ]
ilo = ilo[ilo$id != "RW", ]
ilo = ilo[ilo$id != "SASA2", ]
ilo = ilo[ilo$id != "SLSL1", ]
ilo = ilo[ilo$id != "SN", ]
ilo = ilo[ilo$id != "SYSY1", ]
ilo = ilo[ilo$c.code != "VE", ]

# Additional countries with multiple series
ilo = ilo[ilo$id != "ZMZM2", ]
ilo = ilo[ilo$c.code != "KN", ]
ilo = ilo[ilo$c.code != "SZ", ]
ilo = ilo[ilo$id != "MMMM1", ]

ilo$id = factor(ilo$id)
str(ilo$id)
ilo$c.code = factor(ilo$c.code)
str(ilo$c.code)
rm(dat, dat.m, datsub)
ilo$id = NULL
ilo = rename(ilo, c("c.code" = "id"))

## Selecting the gross sample of countries
# a) 2007-08 data
dat = filter(ilo, year >= 2007 & year < 2009)
dat$id = factor(dat$id)
str(dat$id)
# require comple series
dat = dat %>% dplyr::filter(ave(!is.na(cpi), id, FUN = all))
dat = dat %>% dplyr::filter(ave(!is.na(fpi), id, FUN = all))
dat$id = factor(dat$id)
str(dat$id)
ilo_0708 = dat
rm(dat)
# b) 2010-11 data
dat = filter(ilo, year >= 2010 & year <= 2011)
dat$id = factor(dat$id)
str(dat$id)
# require comple series
dat = dat %>% dplyr::filter(ave(!is.na(cpi), id, FUN = all))
dat = dat %>% dplyr::filter(ave(!is.na(fpi), id, FUN = all))
dat$id = factor(dat$id)
str(dat$id)
ilo_1011 = dat
rm(dat)
# c) 2005-15 data
dat = filter(ilo, year >= 2005)
dat$id = factor(dat$id)
str(dat$id)
# require comple series
dat = dat %>% dplyr::filter(ave(!is.na(cpi), id, FUN = all))
dat = dat %>% dplyr::filter(ave(!is.na(fpi), id, FUN = all))
dat$id = factor(dat$id)
str(dat$id)
ilo_0515 = dat
rm(dat)

#####################################################################
#### Identifying and correcting for breaks, coding errors etc.

# a) 2007-08 data
DT = data.table(ilo_0708)
DT[, temp1 := 0]
setkey(DT, id)
DT[DT[, .I[1], by = key(DT)]$V1, temp1 := 2] 
DT[DT[, .I[.N], by = key(DT)]$V1, temp1 := 1]
DF = data.frame(DT)
DF = within(DF, lag.cpi.notes <- c(NA, head(as.character(cpi.notes), -1)))
DF = within(DF, lag.cpi <- c(NA, head((cpi), -1)))
DF = within(DF, lag.fpi.notes <- c(NA, head(as.character(fpi.notes), -1)))
DF = within(DF, lag.fpi <- c(NA, head((fpi), -1)))

DF$cpi.brk = factor(ifelse(as.character(DF$cpi.notes)!=as.character(DF$lag.cpi.notes) & 
                             is.na(DF$cpi.notes) == FALSE &
                             is.na(DF$lag.cpi.notes) == FALSE &
                             DF$temp1 !=2 &
                             abs(DF$cpi/DF$lag.cpi-1) > 0.1, 
                           1,0))
DF$fpi.brk = factor(ifelse(as.character(DF$fpi.notes)!=as.character(DF$lag.fpi.notes) & 
                             is.na(DF$fpi.notes) == FALSE &
                             is.na(DF$lag.fpi.notes) == FALSE &
                             DF$temp1 !=2 &
                             abs(DF$fpi/DF$lag.fpi-1) > 0.1, 
                           1,0))

summary(DF$cpi.brk)
summary(DF$fpi.brk)
DF$adj.cpi = ilo_0708$cpi
DF$adj.fpi = ilo_0708$fpi

# fix coding errors
DF[which(DF$id == "MD" & DF$year == 2007 & DF$month == 1), "cpi"] = 1195.14931/10

# fix structural breaks and rebase
for (c in levels(DF$id)) 
{
  temp = filter(DF, id == c)
  temp$d.cpi = c(NA, diff(temp$cpi))
  temp$x = ifelse(temp$cpi.brk == 1, temp$d.cpi, 0)
  temp = within(temp, cs.cpi <- cumsum(x))
  temp$adj.cpi = temp$cpi-temp$cs.cpi
  
  temp$d.fpi = c(NA, diff(temp$fpi))
  temp$y = ifelse(temp$fpi.brk == 1, temp$d.fpi, 0)
  temp = within(temp, cs.fpi <- cumsum(y))
  temp$adj.fpi = temp$fpi-temp$cs.fpi
  
  # choose base period
  norm = filter(temp, year == 2007 & month == 1)
  cpi.avg = mean(norm$adj.cpi)
  temp$adj.cpi = temp$adj.cpi/cpi.avg*100
  fpi.avg = mean(norm$adj.fpi)
  temp$adj.fpi = temp$adj.fpi/fpi.avg*100
  
  DF[which(DF$id == c), c("adj.cpi", "adj.fpi")] = temp[, c("adj.cpi", "adj.fpi")]
}

# visual inspection of the series with structural breaks
# View(DF[which((DF$cpi.brk) == 1),])
# View(DF[which((DF$fpi.brk) == 1),])
c = "PF"
datsub = DF[DF$id == c, ]
dat.m = melt(datsub,measure.vars=c('cpi','fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

dat.m = melt(datsub,measure.vars=c('adj.cpi','adj.fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

## finishing touches
ilo_0708 = select(DF, country:tt, cpi.brk, fpi.brk, adj.cpi, adj.fpi)
rm(dat.m, datsub, DF, DT, temp, norm, cpi.avg, fpi.avg, tbl, c)
ilo_0708 = mutate(ilo_0708, rfpi = adj.fpi/adj.cpi*100)

# visual inspection of the series
xyplot(adj.cpi + adj.fpi + rfpi ~ tt | id,
       data = ilo_0708[which(as.numeric(ilo_0708$id)>=5 & as.numeric(ilo_0708$id)<7), ],
       type = c("l", "g"),
       col = c("black", "red", "blue"))

# standardising country names
ilo_0708$iso3c = factor(countrycode(ilo_0708$id, "iso2c", "iso3c", warn = TRUE))
ilo_0708 = ilo_0708[ilo_0708$id != "T1", ]
ilo_0708 = ilo_0708[ilo_0708$id != "KS", ]
str(ilo_0708$iso3c)
ilo_0708$id = factor(ilo_0708$id)
str(ilo_0708$id)

#####################################################################

# b) 2010-11 data
DT = data.table(ilo_1011)
DT[, temp1 := 0]
setkey(DT, id)
DT[DT[, .I[1], by = key(DT)]$V1, temp1 := 2] 
DT[DT[, .I[.N], by = key(DT)]$V1, temp1 := 1]
DF = data.frame(DT)
DF = within(DF, lag.cpi.notes <- c(NA, head(as.character(cpi.notes), -1)))
DF = within(DF, lag.cpi <- c(NA, head((cpi), -1)))
DF = within(DF, lag.fpi.notes <- c(NA, head(as.character(fpi.notes), -1)))
DF = within(DF, lag.fpi <- c(NA, head((fpi), -1)))

DF$cpi.brk = factor(ifelse(as.character(DF$cpi.notes)!=as.character(DF$lag.cpi.notes) & 
                             is.na(DF$cpi.notes) == FALSE &
                             is.na(DF$lag.cpi.notes) == FALSE &
                             DF$temp1 !=2 &
                             abs(DF$cpi/DF$lag.cpi-1) > 0.1, 
                           1,0))
DF$fpi.brk = factor(ifelse(as.character(DF$fpi.notes)!=as.character(DF$lag.fpi.notes) & 
                             is.na(DF$fpi.notes) == FALSE &
                             is.na(DF$lag.fpi.notes) == FALSE &
                             DF$temp1 !=2 &
                             abs(DF$fpi/DF$lag.fpi-1) > 0.1, 
                           1,0))

summary(DF$cpi.brk)
summary(DF$fpi.brk)
DF$adj.cpi = ilo_1011$cpi
DF$adj.fpi = ilo_1011$fpi

# fix structural breaks and rebase
for (c in levels(DF$id)) 
{
  temp = filter(DF, id == c)
  temp$d.cpi = c(NA, diff(temp$cpi))
  temp$x = ifelse(temp$cpi.brk == 1, temp$d.cpi, 0)
  temp = within(temp, cs.cpi <- cumsum(x))
  temp$adj.cpi = temp$cpi-temp$cs.cpi
  
  temp$d.fpi = c(NA, diff(temp$fpi))
  temp$y = ifelse(temp$fpi.brk == 1, temp$d.fpi, 0)
  temp = within(temp, cs.fpi <- cumsum(y))
  temp$adj.fpi = temp$fpi-temp$cs.fpi
  
  # choose base period
  norm = filter(temp, year == 2010 & month == 1)
  cpi.avg = mean(norm$adj.cpi)
  temp$adj.cpi = temp$adj.cpi/cpi.avg*100
  fpi.avg = mean(norm$adj.fpi)
  temp$adj.fpi = temp$adj.fpi/fpi.avg*100
  
  DF[which(DF$id == c), c("adj.cpi", "adj.fpi")] = temp[, c("adj.cpi", "adj.fpi")]
}

# visual inspection of the series with structural breaks
# View(DF[which((DF$cpi.brk) == 1),])
# View(DF[which((DF$fpi.brk) == 1),])
c = "PH"
datsub = DF[DF$id == c, ]
dat.m = melt(datsub,measure.vars=c('cpi','fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

dat.m = melt(datsub,measure.vars=c('adj.cpi','adj.fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

## finishing touches
ilo_1011 = select(DF, country:tt, cpi.brk, fpi.brk, adj.cpi, adj.fpi)
rm(dat.m, datsub, DF, DT, temp, norm, cpi.avg, fpi.avg, tbl, c)
ilo_1011 = mutate(ilo_1011, rfpi = adj.fpi/adj.cpi*100)

# visual inspection of the series
xyplot(adj.cpi + adj.fpi + rfpi ~ tt | id,
       data = ilo_1011[which(as.numeric(ilo_1011$id)>=5 & as.numeric(ilo_1011$id)<7), ],
       type = c("l", "g"),
       col = c("black", "red", "blue"))

# standardising country names
ilo_1011$iso3c = factor(countrycode(ilo_1011$id, "iso2c", "iso3c", warn = TRUE))
ilo_1011 = ilo_1011[ilo_1011$id != "T1", ]
ilo_1011 = ilo_1011[ilo_1011$id != "KS", ]
str(ilo_1011$iso3c)
ilo_1011$id = factor(ilo_1011$id)
str(ilo_1011$id)

#####################################################################

# c) 2005-15 data
DT = data.table(ilo_0515)
DT[, temp1 := 0]
setkey(DT, id)
DT[DT[, .I[1], by = key(DT)]$V1, temp1 := 2] 
DT[DT[, .I[.N], by = key(DT)]$V1, temp1 := 1]
DF = data.frame(DT)
DF = within(DF, lag.cpi.notes <- c(NA, head(as.character(cpi.notes), -1)))
DF = within(DF, lag.cpi <- c(NA, head((cpi), -1)))
DF = within(DF, lag.fpi.notes <- c(NA, head(as.character(fpi.notes), -1)))
DF = within(DF, lag.fpi <- c(NA, head((fpi), -1)))

DF$cpi.brk = factor(ifelse(as.character(DF$cpi.notes)!=as.character(DF$lag.cpi.notes) & 
                             is.na(DF$cpi.notes) == FALSE &
                             is.na(DF$lag.cpi.notes) == FALSE &
                             DF$temp1 !=2 &
                             abs(DF$cpi/DF$lag.cpi-1) > 0.1, 
                           1,0))
DF$fpi.brk = factor(ifelse(as.character(DF$fpi.notes)!=as.character(DF$lag.fpi.notes) & 
                             is.na(DF$fpi.notes) == FALSE &
                             is.na(DF$lag.fpi.notes) == FALSE &
                             DF$temp1 !=2 &
                             abs(DF$fpi/DF$lag.fpi-1) > 0.1, 
                           1,0))

summary(DF$cpi.brk)
summary(DF$fpi.brk)
DF$adj.cpi = ilo_0515$cpi
DF$adj.fpi = ilo_0515$fpi

# fix structural breaks and rebase
for (c in levels(DF$id)) 
{
  temp = filter(DF, id == c)
  temp$d.cpi = c(NA, diff(temp$cpi))
  temp$x = ifelse(temp$cpi.brk == 1, temp$d.cpi, 0)
  temp = within(temp, cs.cpi <- cumsum(x))
  temp$adj.cpi = temp$cpi-temp$cs.cpi
  
  temp$d.fpi = c(NA, diff(temp$fpi))
  temp$y = ifelse(temp$fpi.brk == 1, temp$d.fpi, 0)
  temp = within(temp, cs.fpi <- cumsum(y))
  temp$adj.fpi = temp$fpi-temp$cs.fpi
  
  # choose base period
  norm = filter(temp, year == 2007 & month == 1)
  cpi.avg = mean(norm$adj.cpi)
  temp$adj.cpi = temp$adj.cpi/cpi.avg*100
  fpi.avg = mean(norm$adj.fpi)
  temp$adj.fpi = temp$adj.fpi/fpi.avg*100
  
  DF[which(DF$id == c), c("adj.cpi", "adj.fpi")] = temp[, c("adj.cpi", "adj.fpi")]
}

# visual inspection of the series with structural breaks
# View(DF[which((DF$cpi.brk) == 1),])
# View(DF[which((DF$fpi.brk) == 1),])
c = "LS"
datsub = DF[DF$id == c, ]
dat.m = melt(datsub,measure.vars=c('cpi','fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

dat.m = melt(datsub,measure.vars=c('adj.cpi','adj.fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

## finishing touches
ilo_0515 = select(DF, country:tt, cpi.brk, fpi.brk, adj.cpi, adj.fpi)
rm(dat.m, datsub, DF, DT, temp, norm, cpi.avg, fpi.avg, c)
ilo_0515 = mutate(ilo_0515, rfpi = adj.fpi/adj.cpi*100)

# visual inspection of the series
xyplot(adj.cpi + adj.fpi + rfpi ~ tt | id,
       data = ilo_0515[which(as.numeric(ilo_0515$id)>=5 & as.numeric(ilo_0515$id)<7), ],
       type = c("l", "g"),
       col = c("black", "red", "blue"))

# standardising country names
ilo_0515$iso3c = factor(countrycode(ilo_0515$id, "iso2c", "iso3c", warn = TRUE))
ilo_0515 = ilo_0515[ilo_0515$id != "T1", ]
ilo_0515 = ilo_0515[ilo_0515$id != "KS", ]
str(ilo_0515$iso3c)
ilo_0515$id = factor(ilo_0515$id)
str(ilo_0515$id)

#####################################################################
### Creating the inflation datasets
load("ts_dat.Rda")

# a) 2007-08 data
ilo_07 = select(filter(ilo_0708, year == 2007, month == 1), iso3c, adj.cpi, adj.fpi, rfpi)
ilo_07 = rename(ilo_07, c("adj.cpi" = "cpi07", "adj.fpi" = "fpi07", "rfpi" = "rfpi07"))
ilo_08 = select(filter(ilo_0708, year == 2008, month == 6), iso3c, adj.cpi, adj.fpi, rfpi)
ilo_08 = rename(ilo_08, c("adj.cpi" = "cpi08", "adj.fpi" = "fpi08", "rfpi" = "rfpi08"))
infl_0708 = merge(ilo_07, ilo_08)
infl_0708 = mutate(infl_0708, cpigr.0708 = ((cpi08/cpi07)^(12/18)-1)*100, 
                   fpigr.0708 = ((fpi08/fpi07)^(12/18)-1)*100,
                   rfpigr.0708 = ((rfpi08/rfpi07)^(12/18)-1)*100)

er_07 = select(filter(ts.dat, ff == 2007.000), iso3c, er)
er_07 = rename(er_07, c("er" = "er07"))
er_08 = select(filter(ts.dat, ff == (2008+5/12) ), iso3c, er)
er_08 = rename(er_08, c("er" = "er08"))
er_0708 = merge(er_07, er_08)
er_0708 = mutate(er_0708, ergr.0708 = ((er08/er07)^(12/18)-1)*100)
er_0708[is.na(er_0708)] = 0

# b) 2010-11 data
ilo_10 = select(filter(ilo_1011, year == 2010, month == 1), iso3c, adj.cpi, adj.fpi, rfpi)
ilo_10 = rename(ilo_10, c("adj.cpi" = "cpi10", "adj.fpi" = "fpi10", "rfpi" = "rfpi10"))
ilo_11 = select(filter(ilo_1011, year == 2010, month == 12), iso3c, adj.cpi, adj.fpi, rfpi)
ilo_11 = rename(ilo_11, c("adj.cpi" = "cpi11", "adj.fpi" = "fpi11", "rfpi" = "rfpi11"))
infl_1011 = merge(ilo_10, ilo_11)
infl_1011 = mutate(infl_1011, cpigr.1011 = ((cpi11/cpi10)^(12/12)-1)*100, 
                   fpigr.1011 = ((fpi11/fpi10)^(12/12)-1)*100,
                   rfpigr.1011 = ((rfpi11/rfpi10)^(12/12)-1)*100)

er_10 = select(filter(ts.dat, ff == 2010.000), iso3c, er)
er_10 = rename(er_10, c("er" = "er10"))
er_11 = select(filter(ts.dat, ff == (2010+11/12) ), iso3c, er)
er_11 = rename(er_11, c("er" = "er11"))
er_1011 = merge(er_10, er_11)
er_1011 = mutate(er_1011, ergr.1011 = ((er11/er10)^(12/12)-1)*100)
er_1011[is.na(er_1011)] = 0


infl_0708 = select(infl_0708, iso3c, cpigr.0708:rfpigr.0708)
infl_0708 = merge(infl_0708, select(er_0708, ergr.0708, iso3c), all.x=TRUE)

infl_1011 = select(infl_1011, iso3c, cpigr.1011:rfpigr.1011)
infl_1011 = merge(infl_1011, select(er_1011, ergr.1011, iso3c), all.x=TRUE)

## Creating a long dataset for figures
infl = merge(infl_0708, infl_1011)
infl.l = reshape(infl,varying=2:7, direction="long", idvar="iso3c", sep=".", timevar="period")
infl.l1 = melt(infl.l, id = c("iso3c", "period"))

# save(infl, file = "infl_0708_1011.Rda")
# save(infl.l, file = "infl_l.Rda")
# save(infl.l1, file = "infl_l1.Rda")
# save(infl_0708, file = "infl_0708.Rda")
# save(infl_1011, file = "infl_1011.Rda")
# save(ilo_0515, file = "ilo_0515.Rda")

rm(ilo_07, ilo_08, ilo_10, ilo_11)

# End of script