library(plyr)
library(dplyr)
library(zoo)
library(plm)
library(foreign)
library(reshape)
library(reshape2)
library(ggplot2)
library(data.table)
library(lattice)
library(readstata13)
library(countrycode)

### Data files used
## Domestic price indexes and exchange rates
# laborstaB9.csv
# laborstaBA.csv
# er.csv
## Covariates
# landlocked.csv
# trade_openness.csv
# ste.csv
# dtf.csv
# lpi.csv
# psd_grain.csv
# gdp_05.cs
# cons_05.csv
# fao_fpi.csv

#####################################################################

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
c = "MM"
datsub = ilo[ilo$c.code == c, ]
dat.m = melt(datsub,measure.vars=c('cpi','fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

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
x = 180 -(3*12)
x/12 # number of years required
dat = droplevels(ilo[ilo$id %in% names(tbl)[tbl >= x],,drop=FALSE])
dat$id = factor(dat$id)
str(dat$id)
# require comple series
dat = dat %>% dplyr::filter(ave(!is.na(cpi), id, FUN = all))
dat = dat %>% dplyr::filter(ave(!is.na(fpi), id, FUN = all))
dat$id = factor(dat$id)
str(dat$id)
ilo = dat
rm(dat)

## Identifying and correcting for technical breaks, coding errors etc.
DT = data.table(ilo)
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

# View(DF[which((DF$cpi.brk) == 1),])
# View(DF[which((DF$fpi.brk) == 1),])

DF$adj.cpi = ilo$cpi
DF$adj.fpi = ilo$fpi

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
  
  # rebasing all series to avg. 2005
  temp05 = filter(temp, year == 2005)
  cpi.avg = mean(temp05$adj.cpi)
  temp$adj.cpi = temp$adj.cpi/cpi.avg*100
  fpi.avg = mean(temp05$adj.fpi)
  temp$adj.fpi = temp$adj.fpi/fpi.avg*100
  
  DF[which(DF$id == c), c("adj.cpi", "adj.fpi")] = temp[, c("adj.cpi", "adj.fpi")]
}

# visual inspection of the series with structural breaks
c = "AM"
datsub = DF[DF$id == c, ]
dat.m = melt(datsub,measure.vars=c('cpi','fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

dat.m = melt(datsub,measure.vars=c('adj.cpi','adj.fpi'))
ggplot(dat.m) +
  geom_line(aes(x=as.numeric(tt),y=value,group=id,color=id)) +
  facet_grid(~variable)

#View(DF[which((DF$id) == c),])

## finishing touches
ilo = select(DF, country:tt, cpi.brk, fpi.brk, adj.cpi, adj.fpi)
rm(dat.m, datsub, DF, DT, temp, temp05, cpi.avg, fpi.avg, tbl, x, c)

ilo = mutate(ilo, rfpi = adj.fpi/adj.cpi*100)

# visual inspection of the series
xyplot(adj.cpi + adj.fpi + rfpi ~ tt | id,
       data = ilo[which(as.numeric(ilo$id)>=5 & as.numeric(ilo$id)<7), ],
       type = c("l", "g"),
       col = c("black", "red", "blue"))

# standardising country names
ilo$iso3c = factor(countrycode(ilo$id, "iso2c", "iso3c", warn = TRUE))
ilo = ilo[ilo$id != "T1", ]
str(ilo$iso3c)
ilo$id = factor(ilo$id)
str(ilo$id)

#####################################################################

### Covariates
## List of land locked developing countries. Source: UNCTAD
temp = paste(str, "landlocked.csv", sep = "")
lldc = read.csv(temp, sep = ";", header = TRUE)
lldc$iso3c = factor(countrycode(lldc$country,  "country.name", "iso3c", warn = TRUE))
lldc$lldc = 1 # land locked dummy
lldc = select(lldc, lldc, iso3c)

## Trade openness (avg. 2006-10).
## Source: World Bank Development Indicators
temp = paste(str, "trade_openness.csv", sep = "")
to = read.csv(temp, sep = ";", header = TRUE)
to = rename(to, c("Country.Code" = "iso3c", "avg_06.10" = "to"))
to = select(to, iso3c, to)

## List of countries with grain state trading enterprises (STEs). 
## Source: Greb et al. (2012)
temp = paste(str, "ste.csv", sep = "")
ste = read.csv(temp, sep = ";", header = TRUE)
ste$iso3c = factor(countrycode(ste$country,  "country.name", "iso3c", warn = TRUE))
ste$ste = 1 # ste dummy
ste = select(ste, iso3c, ste)

## Ease of trade index. Distance to frontier (DTF). 
## Source: World Bank Doing Business survey (2014)
temp = paste(str, "dtf.csv", sep = "")
dtf = read.csv(temp, sep = ";", header = TRUE)
dtf$iso3c = factor(countrycode(dtf$country,  "country.name", "iso3c", warn = TRUE))
dtf = select(dtf, iso3c, dtf)

## Logistics performance index (LPI). 
## Source: World Bank (Global rankings 2007)
temp = paste(str, "lpi.csv", sep = "")
lpi = read.csv(temp, sep = ";", header = TRUE)
lpi$iso3c = factor(countrycode(lpi$country,  "country.name", "iso3c", warn = TRUE))
lpi$country = NULL

## Net cereal import ratio.
## Source: USDA, PSD (2009-11 avg.)
temp = paste(str, "psd_grain.csv", sep = "")
psd.grain = read.csv(temp, sep = ";", header = TRUE)
temp = paste(str, "psd_countries.csv", sep = "")
psd.countries = read.csv(temp, sep = ";", header = TRUE)
psd.grain = merge(psd.grain, psd.countries, by = "cc")
psd.grain$iso3c = factor(countrycode(psd.grain$country,  "country.name", "iso3c", warn = TRUE))
psd.grain = mutate(psd.grain, nir = (export-import)/cons)
nir = select(psd.grain, iso3c, nir)

## Income and consumption (current int$, PPP)
## Source: World Bank, ICP
temp = paste(str, "gdp_05.csv", sep = "")
gdp = read.csv(temp, sep = ";", header = TRUE)
temp = paste(str, "cons_05.csv", sep = "")
cons = read.csv(temp, sep = ";", header = TRUE)

## Share of dietary energy supply derived from cereals, roots and tubers (%) 
## (3-year average, 2004-06). Source FAOSTAT
temp = paste(str, "cereals_roots_tubers_share.csv", sep = "")
cshare = read.csv(temp, sep = ";", header = TRUE)
cshare$iso3c = factor(countrycode(cshare$AreaName,  "country.name", "iso3c", warn = TRUE))
cshare = select(cshare, iso3c, Value)
cshare = rename(cshare, c("Value" = "cshare"))

## Cereal import dependency ratio (%) (3-year average, 2004-06)
## Source FAOSTAT
temp = paste(str, "cereal_dep_ratio_2005.csv", sep = "")
cdep = read.csv(temp, sep = ";", header = TRUE)
cdep$iso3c = factor(countrycode(cdep$AreaName,  "country.name", "iso3c", warn = TRUE))
cdep = select(cdep, iso3c, Value)
cdep = rename(cdep, c("Value" = "cdep"))

## Int. food prices (FAO FPI)
## Source: FAO
temp = paste(str, "fao_fpi.csv", sep = "")
fao.fpi = read.csv(temp, sep = ";", header = TRUE)

## Exchange rates (dom. currency/US$)
## Source: IMF, IFS
temp = paste(str, "er.csv", sep = "")
er = read.csv(temp, sep = ";", header = TRUE)

rm(psd.grain, psd.countries)

#####################################################################

### Create datasets for regressions
## Food crisis domestic inflation
data_07 = select(filter(ilo, year == 2007, month == 1), iso3c, adj.cpi, adj.fpi, rfpi)
data_07 = rename(data_07, c("adj.cpi" = "cpi07", "adj.fpi" = "fpi07", "rfpi" = "rfpi07"))
data_08 = select(filter(ilo, year == 2008, month == 6), iso3c, adj.cpi, adj.fpi, rfpi)
data_08 = rename(data_08, c("adj.cpi" = "cpi08", "adj.fpi" = "fpi08", "rfpi" = "rfpi08"))
data_0708 = merge(data_07, data_08)
data_0708 = mutate(data_0708, cpigr.0708 = ((cpi08/cpi07)^(12/18)-1)*100, 
                   fpigr.0708 = ((fpi08/fpi07)^(12/18)-1)*100,
                   rfpigr.0708 = ((rfpi08/rfpi07)^(12/18)-1)*100)

data_10 = select(filter(ilo, year == 2010, month == 1), iso3c, adj.cpi, adj.fpi, rfpi)
data_10 = rename(data_10, c("adj.cpi" = "cpi10", "adj.fpi" = "fpi10", "rfpi" = "rfpi10"))
data_11 = select(filter(ilo, year == 2010, month == 12), iso3c, adj.cpi, adj.fpi, rfpi)
data_11 = rename(data_11, c("adj.cpi" = "cpi11", "adj.fpi" = "fpi11", "rfpi" = "rfpi11"))
data_1011 = merge(data_10, data_11)
data_1011 = mutate(data_1011, cpigr.1011 = ((cpi11/cpi10)^(12/12)-1)*100, 
                   fpigr.1011 = ((fpi11/fpi10)^(12/12)-1)*100,
                   rfpigr.1011 = ((rfpi11/rfpi10)^(12/12)-1)*100)

infl = merge(data_0708, data_1011)
infl = select(infl, iso3c, cpigr.0708:rfpigr.0708, cpigr.1011:rfpigr.1011)

infl.l = reshape(infl,varying=2:7, direction="long", idvar="iso3c", sep=".", timevar="period")
infl.l1 = melt(infl.l, id = c("iso3c", "period"))

rm(data_07, data_08, data_0708, data_10, data_11, data_1011)

## Adding the country characteristics variables
ctr.dat = merge(infl, lldc, all.x = TRUE)
ctr.dat[, "lldc"][is.na(ctr.dat[, "lldc"])] = 0
ctr.dat = merge(ctr.dat, to, all.x = TRUE)
ctr.dat = merge(ctr.dat, ste, all.x = TRUE)
ctr.dat[, "ste"][is.na(ctr.dat[, "ste"])] = 0
ctr.dat = merge(ctr.dat, dtf, all.x = TRUE)
ctr.dat = merge(ctr.dat, lpi, all.x = TRUE)
ctr.dat = merge(ctr.dat, nir, all.x = TRUE)
ctr.dat = merge(ctr.dat, gdp, all.x = TRUE)
ctr.dat = merge(ctr.dat, cons, all.x = TRUE)
ctr.dat = merge(ctr.dat, cshare, all.x = TRUE)
ctr.dat = merge(ctr.dat, cdep, all.x = TRUE)

m = lm(fpigr.1011~log(gdp)+I(log(gdp)^2)+cshare+cdep+I(cshare*cdep), data = ctr.dat)
summary(m)
