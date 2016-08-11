#####################################################################
#### Creating cross section dataset
#####################################################################

library(dplyr)
library(zoo)
library(foreign)
library(reshape)
library(reshape2)
library(countrycode)
library(data.table)
library(tempdisagg)

### Raw Data
# landlocked.csv
# trade_openness.csv
# ste.csv
# dtf.csv
# lpi.csv
# cereal_production.csv
# cereal_supply.csv
# cereal_import.csv
# cereal_export.csv
# gdp_05.cs
# cons_05.csv

str = "C:/Users/nqw235/Google Drive/Research/pov_price_transmission/"

# FAO country list
temp = paste(str, "fao_country_codes_list.csv", sep = "")
fao.countries = read.csv(temp, sep = ";", header = TRUE)
fao.list = dplyr::select(fao.countries, ISO3, FAOSTAT)
fao.list = rename(fao.list, c("ISO3" = "iso3c", "FAOSTAT" = "AreaCode"))

#####################################################################
#### Reading the covariates data

## Per capita GNI 
## Source: World Bank, ICP
# GNI per capita, PPP (constant 2011 international $)
temp = paste(str, "income.csv", sep = "")
inc = read.csv(temp, sep = ";", header = TRUE)
inc = dplyr::select(inc, 1, 4:12)
names(inc) = c("year", "iso3c", "gni.const.int", "gni.cur.int", "gni.const.us", "hh.exp.const.us", 
               "gni.cur.us.atlas", "gdp.const.us", "gdp.const.int", "gdp.cur.int")
for (i in 3:10) {
  inc[,i] = as.numeric(as.character(inc[,i]))
}

## Trade openness (2004-14 data).
## Source: World Bank Development Indicators
temp = paste(str, "trade_openness.csv", sep = "")
to = read.csv(temp, sep = ";", header = TRUE)
to = dplyr::select(to, 1, 4:5)
names(to) = c("year", "iso3c", "to")
to[,3] = as.numeric(as.character(to[,3]))

## Ease of trade indexes 
## Source: World Bank
# Distance to frontier (DTF)
temp = paste(str, "ease_of_trade_indexes.csv", sep = "")
eot = read.csv(temp, sep = ";", header = TRUE)
eot = dplyr::select(eot, 1, 4:7)
names(eot) = c("year", "iso3c", "dtf", "lpi", "lpi.infra")
for (i in 3:5) {
  eot[,i] = as.numeric(as.character(eot[,i]))
}

## Share of dietary energy supply derived from cereals, roots and tubers (%) 
## Source FAOSTAT
temp = paste(str, "cereals_roots_tubers_share.csv", sep = "")
cshare = read.csv(temp, sep = ";", header = TRUE)
cshare = merge(cshare, fao.list, by = "AreaCode")
cshare = dplyr::select(cshare,Value, Year, iso3c)
cshare = rename(cshare, c("Year"="year", "Value"="cshare"))
cshare[duplicated(cshare[c("iso3c", "year")]),]

## Cereal im/export ratio
## Source: FAOSTAT
# Production
temp = paste(str, "cereal_production.csv", sep = "")
cprod = read.csv(temp, sep = ";", header = TRUE)
cprod = merge(cprod, fao.list, by = "AreaCode")
# View(cprod[is.na(cprod$iso3c), ])
cprod = dplyr::select(cprod,Value, Year, iso3c)
cprod = rename(cprod, c("Year"="year", "Value"="cprod"))
cprod[duplicated(cprod[c("iso3c", "year")]),]

# export
temp = paste(str, "cereal_export.csv", sep = "")
cexp = read.csv(temp, sep = ";", header = TRUE)
cexp = merge(cexp, fao.list, by = "AreaCode")
# View(cexp[is.na(cexp$iso3c), ])
cexp = dplyr::select(cexp,Value, Year, iso3c)
cexp = rename(cexp, c("Year"="year", "Value"="cexp"))
cexp[duplicated(cexp[c("iso3c", "year")]),]

# import
temp = paste(str, "cereal_import.csv", sep = "")
cimp = read.csv(temp, sep = ";", header = TRUE)
cimp = merge(cimp, fao.list, by = "AreaCode")
# View(cimp[is.na(cimp$iso3c), ])
cimp = dplyr::select(cimp,Value, Year, iso3c)
cimp = rename(cimp, c("Year"="year", "Value"="cimp"))
cimp[duplicated(cimp[c("iso3c", "year")]),]

# domestic supply
temp = paste(str, "cereal_supply.csv", sep = "")
csup = read.csv(temp, sep = ";", header = TRUE)
csup = merge(csup, fao.list, by = "AreaCode")
# View(csup[is.na(csup$iso3c), ])
csup = dplyr::select(csup,Value, Year, iso3c)
csup = rename(csup, c("Year"="year", "Value"="csup"))
csup[duplicated(csup[c("iso3c", "year")]),]

# Cereal export ratio
cer = merge(cprod, cexp)
cer = mutate(cer, cer = (cexp/cprod)*100)
cer = dplyr::select(cer, iso3c, year, cer)

# Cereal import ratio
cir = merge(csup, cimp)
cir = mutate(cir, cir = (cimp/csup)*100)
cir = dplyr::select(cir, iso3c, year, cir)

## List of landlocked developing countries. Source: UNCTAD
temp = paste(str, "landlocked.csv", sep = "")
lldc = read.csv(temp, sep = ";", header = TRUE)
lldc$iso3c = factor(countrycode(lldc$country,  "country.name", "iso3c", warn = TRUE))
lldc$lldc = 1 # land locked dummy
lldc = dplyr::select(lldc, lldc, iso3c)
lldc[duplicated(lldc[c("iso3c")]),]

## List of country interventions during the food crisis
## Source Demeke et al. 2009
temp = paste(str, "Demeke_list.csv", sep = "")
interventions = read.csv(temp, sep = ";", header = TRUE)
stocks = data.frame(country = interventions[,1])
stocks$iso3c = factor(countrycode(stocks$country,  "country.name", "iso3c", warn = TRUE))
stocks = na.omit(stocks)
stocks$stocks = 1 # stock release dummy
stocks = dplyr::select(stocks, stocks, iso3c)

tax = data.frame(country = interventions[,2])
tax$iso3c = factor(countrycode(tax$country,  "country.name", "iso3c", warn = TRUE))
tax = na.omit(tax)
tax$tax = 1 # tax reduction dummy
tax = dplyr::select(tax, tax, iso3c)

price = data.frame(country = interventions[,3])
price$iso3c = factor(countrycode(price$country,  "country.name", "iso3c", warn = TRUE))
price = na.omit(price)
price$price = 1 # price control dummy
price = dplyr::select(price, price, iso3c)

tariff = data.frame(country = interventions[,4])
tariff$iso3c = factor(countrycode(tariff$country,  "country.name", "iso3c", warn = TRUE))
tariff = na.omit(tariff)
tariff$tariff = 1 # tariff reduction dummy
tariff = dplyr::select(tariff, tariff, iso3c)

export = data.frame(country = interventions[,5])
export$iso3c = factor(countrycode(export$country,  "country.name", "iso3c", warn = TRUE))
export = na.omit(export)
export$export = 1 # export ban dummy
export = dplyr::select(export, export, iso3c)

## List of countries with grain state trading enterprises (STEs). 
## Source: Greb et al. (2012)
temp = paste(str, "ste.csv", sep = "")
ste = read.csv(temp, sep = ";", header = TRUE)
ste$iso3c = factor(countrycode(ste$country,  "country.name", "iso3c", warn = TRUE))
ste$ste = 1 # ste dummy
ste = dplyr::select(ste, iso3c, ste)
ste[duplicated(ste[c("iso3c")]),]

rm(cexp, cimp, cprod, csup, fao.list, fao.countries, interventions, temp)

#####################################################################
#### Merging the data

## 2007-08 inflation dataset
load("infl_0708.Rda")
inc06 = dplyr::select(filter(inc, year == 2006), -year)
ctr.dat.0708 = merge(infl_0708, inc06, all.x = TRUE)
to06 = dplyr::select(filter(to, year == 2006), -year)
ctr.dat.0708 = merge(ctr.dat.0708, to06, all.x = TRUE)
lpi07 = dplyr::select(filter(eot, year == 2007), 2, 4:5)
ctr.dat.0708 = merge(ctr.dat.0708, lpi07, all.x = TRUE)
dtf09 = dplyr::select(filter(eot, year == 2009), 2:3)
ctr.dat.0708 = merge(ctr.dat.0708, dtf09, all.x = TRUE)
cshare06 = dplyr::select(filter(cshare, year == "2004-2006"), 1,3)
ctr.dat.0708 = merge(ctr.dat.0708, cshare06, all.x = TRUE)
cer06 = dplyr::select(filter(cer, year == 2006), 1,3)
ctr.dat.0708 = merge(ctr.dat.0708, cer06, all.x = TRUE)
cir06 = dplyr::select(filter(cir, year == 2006), 1,3)
ctr.dat.0708 = merge(ctr.dat.0708, cir06, all.x = TRUE)
ctr.dat.0708 = merge(ctr.dat.0708, lldc, all.x = TRUE)
ctr.dat.0708[, "lldc"][is.na(ctr.dat.0708[, "lldc"])] = 0
ctr.dat.0708 = merge(ctr.dat.0708, ste, all.x = TRUE)
ctr.dat.0708[, "ste"][is.na(ctr.dat.0708[, "ste"])] = 0
ctr.dat.0708 = merge(ctr.dat.0708, export, all.x = TRUE)
ctr.dat.0708[, "export"][is.na(ctr.dat.0708[, "export"])] = 0
ctr.dat.0708 = merge(ctr.dat.0708, price, all.x = TRUE)
ctr.dat.0708[, "price"][is.na(ctr.dat.0708[, "price"])] = 0
ctr.dat.0708 = merge(ctr.dat.0708, stocks, all.x = TRUE)
ctr.dat.0708[, "stocks"][is.na(ctr.dat.0708[, "stocks"])] = 0
ctr.dat.0708 = merge(ctr.dat.0708, tariff, all.x = TRUE)
ctr.dat.0708[, "tariff"][is.na(ctr.dat.0708[, "tariff"])] = 0
ctr.dat.0708 = merge(ctr.dat.0708, tax, all.x = TRUE)
ctr.dat.0708[, "tax"][is.na(ctr.dat.0708[, "tax"])] = 0

# save(ctr.dat.0708, file = "infl_ctr_dat_0708.Rda")

## 2010-11 inflation dataset
load("infl_1011.Rda")
inc09 = dplyr::select(filter(inc, year == 2009), -year)
ctr.dat.1011 = merge(infl_1011, inc09, all.x = TRUE)
to09 = dplyr::select(filter(to, year == 2009), -year)
ctr.dat.1011 = merge(ctr.dat.1011, to09, all.x = TRUE)
lpi10 = dplyr::select(filter(eot, year == 2010), 2, 4:5)
ctr.dat.1011 = merge(ctr.dat.1011, lpi10, all.x = TRUE)
dtf09 = dplyr::select(filter(eot, year == 2009), 2:3)
ctr.dat.1011 = merge(ctr.dat.1011, dtf09, all.x = TRUE)
cshare09 = dplyr::select(filter(cshare, year == "2007-2009"), 1,3)
ctr.dat.1011 = merge(ctr.dat.1011, cshare09, all.x = TRUE)
cer09 = dplyr::select(filter(cer, year == 2009), 1,3)
ctr.dat.1011 = merge(ctr.dat.1011, cer09, all.x = TRUE)
cir09 = dplyr::select(filter(cir, year == 2009), 1,3)
ctr.dat.1011 = merge(ctr.dat.1011, cir09, all.x = TRUE)
ctr.dat.1011 = merge(ctr.dat.1011, lldc, all.x = TRUE)
ctr.dat.1011[, "lldc"][is.na(ctr.dat.1011[, "lldc"])] = 0
ctr.dat.1011 = merge(ctr.dat.1011, ste, all.x = TRUE)
ctr.dat.1011[, "ste"][is.na(ctr.dat.1011[, "ste"])] = 0

# save(ctr.dat.1011, file = "infl_ctr_dat_1011.Rda")

# End of script