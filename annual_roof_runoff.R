# upscale measured roof runoff in BaSaR to annual total through an event-based
# linear regression

#'Y:/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_Regen'
#'c:/kwb/BaSaR/_Daten/RAW/_Regen'

rain <- readRain(rawdir = 'Y:/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_Regen',
                 dbName = 'rainDB.txt')

temperature <- readTemp(rawdir = 'Y:/AUFTRAEGE/_Auftraege_laufend/UFOPLAN-BaSaR/Data-Work packages/AP3 - Monitoring/_Daten/RAW/_KlimaDWD/Temperature',
                        dbName = 'tempData.txt')

roofrunoff_bbw <- readRoofDB(subfolder = 'data_prelim_sources',
                             dbName = 'Genommene_Proben_Dach_BaSaR.xlsx',
                             site = 'BBW')

roofrunoff_bbr <- readRoofDB(subfolder = 'data_prelim_sources',
                             dbName = 'Genommene_Proben_Dach_BaSaR.xlsx',
                             site = 'BBR')

roofmod_bbw <- fitlmroof(roofrunoffdata = roofrunoff_bbw,
                         raindata = rain,
                         temperaturedata = temperature)

roofmod_bbr <- fitlmroof(roofrunoffdata = roofrunoff_bbr,
                         raindata = rain,
                         temperaturedata = temperature)

summary(roofmod_bbw$model)
summary(roofmod_bbr$model)

par(mfcol = c(1, 2), mar = c(4, 4, 2, 2))
plot(roofmod_bbw$obsTable$specQ_lm2, 
     roofmod_bbw$obsTable$specQ_lm2_pred, 
     xlim = c(0, 30), ylim = c(0, 30),
     xlab = 'Observed roof runoff [l/m2]',
     ylab = 'Predicted roof runoff [l/m2]',
     main = 'Bitumen roof')
abline(a=0, b=1)
plot(roofmod_bbr$obsTable$specQ_lm2, 
     roofmod_bbr$obsTable$specQ_lm2_pred, 
     xlim = c(0, 30), ylim = c(0, 30),
     xlab = 'Observed roof runoff [l/m2]',
     ylab = 'Predicted roof runoff [l/m2]',
     main = 'Green roof')
abline(a=0, b=1)


rain.events <- makePredictorsRoof(
  subfolder = 'data_prelim_sources',
  rainfile = 'produkt_rr_stunde_19950901_20191231_00433.txt',
  airtempfile = 'produkt_tu_stunde_19510101_20191231_00433.txt')


# make predictions for both roof types
rain.events$roofrunoff_bitumen <- predict(
  object = roofmod_bbw$model,
  newdata = data.frame(Regenhöhe_mm = rain.events$rainfall,
                       TmaxBefore = rain.events$airtempmax))

rain.events$roofrunoff_green <- predict(
  object = roofmod_bbr$model,
  newdata = data.frame(Regenhöhe_mm = rain.events$rainfall,
                       TmaxBefore = rain.events$airtempmax))

facaderunoff <- read.table(
  'data_prelim_sources/output_annualrunoffside.txt',
  sep = ';',
  header = TRUE)

annual <- cbind(aggregate(x = rain.events$rainfall,
                          by = list(rain.events$year),
                          FUN = sum),
                aggregate(x = rain.events$roofrunoff_bitumen,
                          by = list(rain.events$year),
                          FUN = sum)$x,
                aggregate(x = rain.events$roofrunoff_green,
                          by = list(rain.events$year),
                          FUN = sum)$x)

yearsfacade <- c(1995, 1996, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 
                 2007, 2008, 2013, 2015, 2016, 2017, 2018, 2019)
annual2 <- annual[annual$Group.1 %in% yearsfacade, ]

annual2 <- cbind(annual2, apply(X = facaderunoff[, 2:ncol(facaderunoff)],
                                MARGIN = 1,
                                FUN = sum))

colnames(annual2) <- c('year', 'rain', 'roofrunoff_bitumen', 
                       'roofrunoff_green', 'facaderunoff_all_sides')



barplot(t(as.matrix(annual2[, 2:ncol(annual2)])), 
        names.arg = annual2$year,
        beside = TRUE, 
        legend.text = c('rain', 'runoff_bitumen', 'runoff_green', 'runoff_facade_all'),
        args.legend = list(x = 42, y = 900, ncol = 2, cex = 0.8),
        col = c('blue', 'grey', 'forestgreen', 'darkorange'),
        main = 'Annual runoff [l/m²]',
        las = 2)




# functions ----------------------------------------------------------------------

# read BaSaR rainfall data base
readRain <- function(rawdir, dbName){
  
  rain <- read.table(file.path(rawdir, dbName), 
                     sep=";", 
                     header=TRUE, 
                     encoding="UTF-8",
                     colClasses=c("character", rep("numeric", times=7)))
  
  rain$dateTime <- as.POSIXct(rain$dateTime,
                              format="%Y-%m-%d %H:%M",
                              tz="Etc/GMT-1")
  return(rain)
}

# read BaSaR temperature data base
readTemp <- function(rawdir, dbName){
  
  tempdData <- read.table(file.path(rawdir, dbName), 
                          header=TRUE,
                          dec=",", sep =";",
                          colClasses=c("character", rep("numeric", times=6)))
  
  tempdData$dateTime <- as.POSIXct(tempdData$dateTime, 
                                   format="%Y-%m-%d %H:%M", 
                                   tz="Etc/GMT-1")
  
  return(tempdData)
}

# read BaSaR roof runoff
readRoofDB <- function(subfolder, dbName, site){
  
  xls <- readxl::read_excel(file.path(subfolder, dbName), 
                            sheet=site, 
                            col_types="text", 
                            skip=2, 
                            na=c("na", "nb"))
  
  xls$tBegRain <- as.POSIXct(xls$tBegRain, 
                             format="%d.%m.%Y %H:%M", 
                             tz="Etc/GMT-1")
  xls$tEndRain <- as.POSIXct(xls$tEndRain, 
                             format="%d.%m.%Y %H:%M", 
                             tz="Etc/GMT-1")
  xls$Regenhöhe_mm <- as.numeric(xls$Regenhöhe_mm)
  xls$Anzahl_Ereignisse <- as.numeric(xls$Anzahl_Ereignisse)
  xls$Abflussvol_l <- as.numeric(xls$Abflussvol_l)
  xls$Abflussvol_aus_Regression <- as.logical(xls$Abflussvol_aus_Regression)
  xls2 <- xls[(!is.na(xls$Abflussvol_aus_Regression) & 
                 !xls$Abflussvol_aus_Regression), 
              c('tBegRain', 'tEndRain',
                'Regenhöhe_mm', 'Regenschreiber', 'Anzahl_Ereignisse',
                'Abflussvol_l')]
  
  xls2$site <- site
  
  xls2$specQ_lm2 <- xls2$Abflussvol_l/ifelse(site == 'BBW', 183.57, 194)
  
  return(xls2)
}

# fit linear regression
fitlmroof <- function(roofrunoffdata, raindata, temperaturedata){
  
  # for site BBW, adjust rainfall depth for event on 07.01.2019 07:05, since it was sampled before 
  # it ended and was entered as such in Genommene_Proben_Dach. For the regression, it's better to
  # use this than the interrupted event
  site <- unique(roofrunoffdata$site)
  if(site == "BBW"){
    tt   <- as.POSIXct("07.01.2019 07:05", 
                       format="%d.%m.%Y %H:%M", 
                       tz="Etc/GMT-1")
    roofrunoffdata$Regenhöhe_mm[roofrunoffdata$tBegRain==tt] <- 29.3
    roofrunoffdata$Abflussvol_l[roofrunoffdata$tBegRain==tt] <- 4284
  }
  
  # add new column for holding temperature during pBefore
  roofrunoffdata$TmaxBefore <- rep(NA_real_, times=nrow(roofrunoffdata))
  
  # compute mean temperature between end of last rainfall event and beginning
  # of current event
  for(i in seq_len(nrow(roofrunoffdata))){
    
    # select rain data for this event's best rain gauge
    rainSel <- raindata[, c('dateTime', roofrunoffdata$Regenschreiber[i])]
    
    # find time period between beginning of current rainfall event and end of last
    tBeg <- roofrunoffdata$tBegRain[i]
    rainBefore <- rainSel[rainSel$dateTime < tBeg, ]
    indexRain <- which(rainBefore[[2]]>0)
    rainBefore <- rainBefore[indexRain, ] 
    tEndPrev <- max(rainBefore$dateTime)
    
    # select temperature data for current site and time period between 
    # tEndPrev and tBeg
    tempSel <- temperaturedata[, c('dateTime',
                                   ifelse(site=="BBR", 
                                          "temperature_BBR", 
                                          "temperature_BBW"))]
    tempSel <- tempSel[tempSel$dateTime >= tEndPrev & 
                         tempSel$dateTime < tBeg, ][[2]]
    
    # make statistics
    x <- max(tempSel, na.rm = TRUE)
    roofrunoffdata$TmaxBefore[i] <- ifelse(is.infinite(x), NA, x)
  }
  
  # build regression model
  mod <- lm(formula = specQ_lm2 ~ Regenhöhe_mm + TmaxBefore, 
            data=roofrunoffdata,
            na.action = 'na.exclude')
  
  # make predictions
  roofrunoffdata$specQ_lm2_pred <- predict(
    object = mod,
    newdata = data.frame(Regenhöhe_mm = roofrunoffdata$Regenhöhe_mm,
                         TmaxBefore = roofrunoffdata$TmaxBefore))
  
  # return output
  return(list(model=mod, 
              obsTable=roofrunoffdata))
}

# build data.frame with events from long weather series
makePredictorsRoof <- function(subfolder, 
                               rainfile,
                               airtempfile){
  
  # load raw weather data
  dat <- lapply(X = file.path(subfolder, c(rainfile, airtempfile)),
                FUN = read.table,
                header = TRUE,
                sep = ';',
                colClasses = 'character')
  
  # select columns and adjust column names
  trimData <- function(dat, keepcols, mynames){
    trimmeddat <- dat[, keepcols]   
    colnames(trimmeddat) <- mynames
    return(trimmeddat)
  }
  
  dat[[1]] <- trimData(dat = dat[[1]],
                       keepcols = c(2, 4),
                       mynames = c('dateTime', 'rainfall'))
  dat[[2]] <- trimData(dat = dat[[2]],
                       keepcols = c(2, 4),
                       mynames = c('dateTime', 'airtemp'))
  
  # lengthen data tables prior to stacking one on top of the other
  rain <- tidyr::pivot_longer(data = dat[[1]],
                              cols = 'rainfall',
                              names_to = 'variable',
                              values_to = 'values')
  airtemp <- tidyr::pivot_longer(data = dat[[2]],
                                 cols = 'airtemp',
                                 names_to = 'variable',
                                 values_to = 'values')
  
  # stack individual variables on top of one another
  dat <- rbind(rain, airtemp)
  
  # format date and numbers
  dat$dateTime <- as.POSIXct(dat$dateTime,
                             format = '%Y%m%d%H',
                             tz = 'UTC')
  dat$values[grep(pattern = '-999', x = dat$values)] <- NA
  dat$values <- as.numeric(dat$values)
  
  # seprate variables
  dat <- as.data.frame(dat)
  rain <- dat[dat$variable == 'rainfall', ]
  airtemp <- dat[dat$variable == 'airtemp', ]
  
  # get storms
  rain.events <- kwb.event::getEvents(rainData = rain,
                                      seriesName = 'values', 
                                      signalWidth = 3600)
  
  # compute each storm's rainfall depth and temperature info
  # (the warnings issued here come from short storms lasting < 1h,
  # for which there is only one data point and no stand.dev. can be
  # computed)
  x <- lapply(X = seq_along(rain.events$iBeg),
              FUN = function(i){
                
                tBeg <- rain.events$tBeg[i]
                tEnd <- rain.events$tEnd[i]
                
                filterstorm <- function(data, tBeg, tEnd){
                  return(data[data$dateTime >= tBeg &
                                data$dateTime <= tEnd, ])
                }
                
                rainsel <- filterstorm(rain, tBeg, tEnd)
                airtempsel <- filterstorm(airtemp, tBeg, tEnd)
                
                stormrain <- sum(rainsel$values, na.rm = TRUE)
                stormairtempmean <- mean(airtempsel$values, na.rm = TRUE)
                stormairtempmax <- max(airtempsel$values, na.rm = TRUE)
                
                return(c(stormrain,
                         stormairtempmean,
                         stormairtempmax))
              })
  
  x <- data.frame(do.call(rbind, x))
  
  colnames(x) <- c('rainfall',
                   'airtempmean',
                   'airtempmax')
  
  # add x to events table
  rain.events <- cbind(rain.events, x) 
  
  # add year column
  rain.events$year <- as.numeric(format(rain.events$tBeg,
                                        format = '%Y'))
  
  # find years with data gaps in rainfall, temperature or wind
  findYearsNA <- function(variable, data){
    return(unique(data[is.na(data[[variable]]), 'year']))
  }
  
  yearsNA <- unique(unlist(lapply(X = c('rainfall', 
                                        'airtempmean'), 
                                  FUN = findYearsNA,
                                  data = rain.events)))
  
  # find years with no gaps
  yearsAll <- unique(rain.events$year)
  yearsNoNA <- yearsAll[!(yearsAll %in% yearsNA)]
  
  # keep only years with no gaps
  rain.events <- rain.events[rain.events$year %in% yearsNoNA, ]
  
  # keep only storms with Tmean and Tmax > 0 to avoid non-liquid precipitation.
  # dwd has a rain type indicator in the data (column 'WRTR') but 79% of values
  # are NA, so it's not usable. see here:
  # ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/historical/DESCRIPTION_obsgermany_climate_hourly_precipitation_historical_en.pdf
  rain.events <- rain.events[rain.events$airtempmean > 0 &
                               rain.events$airtempmax > 0, ]
  
  return(rain.events)
}


