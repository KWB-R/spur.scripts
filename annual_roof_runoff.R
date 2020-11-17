rainData <- readRain()
tempData <- readTemp()

site <- 'BBW'
setwd("c:/kwb/BaSaR/")
xlsfile <- paste0(getwd(), "/Genommene_Proben_Dach_BaSaR.xlsx")

xls  <- read_excel(xlsfile, sheet=site, col_types="text", skip=2, na=c("na", "nb"))

xls$tBegRain          <- as.POSIXct(xls$tBegRain, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
xls$tEndRain          <- as.POSIXct(xls$tEndRain, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
xls$Regenhöhe_mm      <- as.numeric(xls$Regenhöhe_mm)
xls$Anzahl_Ereignisse <- as.numeric(xls$Anzahl_Ereignisse)
xls$tBegHydraul       <- as.POSIXct(xls$tBegHydraul, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
xls$tEndHydraul       <- as.POSIXct(xls$tEndHydraul, format="%d.%m.%Y %H:%M", tz="Etc/GMT-1")
xls$Abflussvol_l      <- as.numeric(xls$Abflussvol_l)
xls$Abflussvol_aus_Regression <- as.logical(xls$Abflussvol_aus_Regression)

xls2 <- dplyr::filter(xls, !is.na(Abflussvol_aus_Regression) & !Abflussvol_aus_Regression)

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
