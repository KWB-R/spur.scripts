

rain <- readRain(rawdir = 'c:/kwb/BaSaR/_Daten/RAW/_Regen',
                     dbName = 'rainDB.txt')
temperature <- readTemp(rawdir = 'c:/kwb/BaSaR/_Daten/RAW/_KlimaDWD/Temperature',
                     dbName = 'tempData.txt')

roofrunoff_bbw <- readRoofDB(subfolder = 'data_prelim_sources',
                              dbName = 'Genommene_Proben_Dach_BaSaR.xlsx',
                              site = 'BBW')

roofrunoff_bbr <- readRoofDB(subfolder = 'data_prelim_sources',
                             dbName = 'Genommene_Proben_Dach_BaSaR.xlsx',
                             site = 'BBR')




# for site BBW, adjust rainfall depth for event on 07.01.2019 07:05, since it was sampled before 
# it ended and was entered as such in Genommene_Proben_Dach. For the regression, it's better to
# use this than the interrupted event
if(site=="BBW"){
  tt   <- as.POSIXct("07.01.2019 07:05", format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
  29.3 -> xls2$Regenhöhe_mm[xls2$tBegRain==tt]
  4284 -> xls2$Abflussvol_l[xls2$tBegRain==tt]
}

# add new column for holding temperature during pBefore
xls2$tempBefore <- rep(NA_real_, times=nrow(xls2))

# compute mean temperature between end of last rainfall event and beginning
# of current event
for(i in 1:nrow(xls2)){
  
  # select rain gauge for the site
  rainSel <- dplyr::select(rainData, dateTime, xls2$Regenschreiber[i])  
  
  # find time period between beginning of current rainfall event and end of last
  tBeg       <- xls2$tBegRain[i]
  rainBefore <- dplyr::filter(rainSel, dateTime < tBeg)
  indexRain  <- which(rainBefore[[2]]>0)
  rainBefore <- rainBefore[indexRain, ] 
  tEndPrev   <- max(rainBefore$dateTime)
  
  # select temperature data for current site and time period between tEndPrev and tBeg
  tempSel <- dplyr::select(tempData, dateTime, ifelse(site=="BBR", "temperature_BBR", "temperature_BBW"))
  tempSel <- dplyr::filter(tempSel, dateTime>=tEndPrev & dateTime < tBeg)
  tempSel <- tempSel[[2]]
  
  # make statistics
  xls2$tempBefore[i] <- mean(tempSel, na.rm=TRUE)
}


# build regression model
mod <- lm(Abflussvol_l ~ Regenhöhe_mm + tempBefore, data=xls2)

summary(mod)

output <- list(model=mod, 
               obsTable=tbl_df(data.frame(tBegRain=xls2$tBegRain,
                                          tEndRain=xls2$tEndRain,
                                          Regenhöhe_mm=xls2$Regenhöhe_mm,
                                          Abflussvol_l=xls2$Abflussvol_l,
                                          tempBefore=xls2$tempBefore)))

# funtions ----------------------------------------------------------------------

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

# read roof runoff
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
  xls2 <- dplyr::filter(xls, !is.na(Abflussvol_aus_Regression) & !Abflussvol_aus_Regression)
  
  return(xls2)
}
