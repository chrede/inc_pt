#####################################################################
#### Creating the time series dataset
#####################################################################

library(dplyr)
library(zoo)
library(foreign)
library(countrycode)
library(reshape)
library(reshape2)
library(stringr)

### Raw Data
# er.csv
# fao_fpi.csv

str = "C:/Users/nqw235/Google Drive/Research/pov_price_transmission/"

#####################################################################

## Int. food prices (FAO FPI)
## Source: FAO
temp = paste(str, "fao_fpi.csv", sep = "")
fao.fpi = read.csv(temp, sep = ";", header = TRUE)
fao.fpi$tt = seq(as.Date("2000/1/1"), by = "month", length.out = nrow(fao.fpi))
fao.fpi$tt = as.yearmon(fao.fpi$tt)
fao.fpi$date = NULL

## Int. oil prices (POILAPSP)
## Source: IMF
temp = paste(str, "imf_oil.csv", sep = "")
imf.oil = read.csv(temp, sep = ";", header = TRUE)
imf.oil$tt = seq(as.Date("1980/1/1"), by = "month", length.out = nrow(imf.oil))
imf.oil$tt = as.yearmon(imf.oil$tt)
imf.oil$date = NULL

## Exchange rates (dom. currency/US$)
## Source: IMF, IFS
temp = paste(str, "er.csv", sep = "")
er = read.csv(temp, sep = ";", header = TRUE)
eu = select(er, Euro.Area)
eu$tt = seq(as.Date("2000/1/1"), by = "month", length.out = nrow(eu))
eu$tt = as.yearmon(eu$tt)

er.nam = data.frame(names(er)[-1])
colnames(er.nam)[1] = "country"
er.nam$iso3c = factor(countrycode(er.nam$country,  "country.name", "iso3c", warn = TRUE))
er.nam$x = paste0("ctr_", er.nam$iso3c)

for (i in 1: nrow(er.nam)) {
  colnames(er)[i+1] = er.nam$x[i]
}

er.l = melt(er, id=1)
er.l$iso3c = str_sub(er.l$variable, start = -3,-1)
er.l = er.l[er.l$iso3c != "_NA", ]
er.l$variable = NULL
er.l = rename(er.l, c("value" = "er"))
er.l$month = str_sub(er.l$date, start = 1,3)

er.l = data.frame(lapply(er.l, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))
er.l$m1 = str_sub(er.l$month, start = 1,1)
er.l$m2 = str_sub(er.l$month, start = 2,3)

er.l1 = er.l

er.l1 = data.frame(lapply(er.l1, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

er.l$month = paste0(er.l$m1, er.l1$m2)
er.l$year = str_sub(er.l$date, start = -2,-1)
er.l$year = paste0("20", er.l$year)
er.l$mn = match(er.l$month,month.abb)
er.l$tt = as.yearmon(paste(er.l$year, er.l$mn, sep = "-"))
er = select(er.l, iso3c, tt, er)

#####################################################################
### Merging the series

load("ilo_0515.Rda")
ts.dat = select(ilo_0515, iso3c, tt, cpi.brk:rfpi)

## Add international food prices
xx = ts.dat
yy = fao.fpi
xx$ff = as.numeric(zoo(xx$tt))
xx$tt = NULL
yy$ff = as.numeric(zoo(yy$tt))
yy$tt = NULL
xxx = inner_join(xx, yy)
xxx = xxx[order(xxx$iso3c, xxx$ff), ]
ts.dat = xxx

## Add international oil prices
xx = ts.dat
yy =imf.oil
# xx$ff = as.numeric(zoo(xx$tt))
# xx$tt = NULL
yy$ff = as.numeric(zoo(yy$tt))
yy$tt = NULL
xxx = inner_join(xx, yy)
xxx = xxx[order(xxx$iso3c, xxx$ff), ]

## Add exchange rate data
zz = er
zz$ff = as.numeric(zoo(zz$tt))
zz$tt = NULL
xxx = left_join(xxx, zz)

# Add second country code
xxx$id = factor(xxx$iso3c)
levels(xxx$id)
str(xxx$id)
xxx$id1 = factor(countrycode(xxx$id, "iso3c", "iso2c", warn = TRUE))
levels(xxx$id1)
str(xxx$id1)

# Dollar countries or countries with no official ER (e.g. Cuba, Taiwan)
xxx$dollar = 0
for (i in c("BM", "CU", "EC", "PR", "TW")) {
  xxx[which(xxx$id1 == i), "dollar"] = 1
}

# Euro countries
yy = filter(eu, as.numeric(tt)>=2005)
yy$ff = as.numeric(zoo(yy$tt))
yy$tt = NULL
xxx$euro = 0

for (i in c("AD", "AT", "BE", "FI", "FR", "GF", "NC", "PF","GP", "GR", "DE", "IE", "IT", 
            "LU", "NL", "PT", "ES", "RE")) {
  temp = xxx[which(xxx$id1 == i),]
  temp = inner_join(temp, yy)
  temp$er = temp$Euro.Area
  temp$Euro.Area = NULL
  temp$euro = 1
  xxx[which(xxx$id1 == i), ] = temp
}

# Cyprus
xxx[which(xxx$id1 == "CY" & xxx$ff>=2008), "er"] = yy[which(yy$ff>=2008), "Euro.Area"]
xxx[which(xxx$id1 == "CY" & xxx$ff>=2008), "euro"] = 1
# Estonia
xxx[which(xxx$id1 == "EE" & xxx$ff>=2011), "er"] = yy[which(yy$ff>=2011), "Euro.Area"]
xxx[which(xxx$id1 == "EE" & xxx$ff>=2011), "euro"] = 1
# Greece
# xxx[which(xxx$id1 == "GR" & xxx$ff>=2001), "er"] = yy[which(yy$ff>=2001), "Euro.Area"]
# xxx[which(xxx$id1 == "GR" & xxx$ff>=2001), "euro"] = 1
# Latvia
xxx[which(xxx$id1 == "LV" & xxx$ff>=2014), "er"] = yy[which(yy$ff>=2014), "Euro.Area"]
xxx[which(xxx$id1 == "LV" & xxx$ff>=2014), "euro"] = 1
# Malta
xxx[which(xxx$id1 == "MT" & xxx$ff>=2008), "er"] = yy[which(yy$ff>=2008), "Euro.Area"]
xxx[which(xxx$id1 == "MT" & xxx$ff>=2008), "euro"] = 1
# Slovakia
xxx[which(xxx$id1 == "SK" & xxx$ff>=2009), "er"] = yy[which(yy$ff>=2009), "Euro.Area"]
xxx[which(xxx$id1 == "SK" & xxx$ff>=2009), "euro"] = 1
# Slovenia
xxx[which(xxx$id1 == "SI" & xxx$ff>=2007), "er"] = yy[which(yy$ff>=2007), "Euro.Area"]
xxx[which(xxx$id1 == "SI" & xxx$ff>=2007), "euro"] = 1

## Other countries without their own currencies 
# Palestine
xxx[which(xxx$id1 == "PS"), "er"] = xxx[which(xxx$id1 == "IL"), "er"]
# Isle of Man
xxx[which(xxx$id1 == "IM"), "er"] = xxx[which(xxx$id1 == "GB"), "er"]

# View(filter(xxx, dollar == 1))

xxx = xxx[order(xxx$iso3c, xxx$ff), ]
ts.dat = ts.dat[order(ts.dat$iso3c, ts.dat$tt), ]
xxx$tt = ts.dat$tt
xxx$er = as.numeric(as.character(xxx$er))
# View(filter(xxx, is.na(er)))
ts.dat = xxx
rm(xx, xxx, yy, zz)

save(ts.dat, file = "ts_dat.Rda")

# End of script