# explore specific runoff from BaSaR facades

# get data
basar_bbr <- getFacadeRunoffBaSaR(
  dbName = 'Genommene_Proben_Fassaden_BaSaR.xlsx',
  dbTable = 'BBR',
  dateTimeFormat = '%d.%m.%Y %H:%M',
  tz = 'Etc/GMT-1',
  facadeOrientations = c(O = 94, S = 175, W = 268, N = 357))

basar_bbw <- getFacadeRunoffBaSaR(
  dbName = 'Genommene_Proben_Fassaden_BaSaR.xlsx',
  dbTable = 'BBW',
  dateTimeFormat = '%d.%m.%Y %H:%M',
  tz = 'Etc/GMT-1',
  facadeOrientations = c(O = 78, S = 170, W = 261, N = 351))

colnames(basar_bbr) <- colnames(basar_bbw)

basar <- rbind(basar_bbr, basar_bbw)

# fit model on training set (uses caret)
yexp <- 0.5
mod <- fitlm(data = basar, trainperc = 0.7, yexp = yexp)

# assess model
summary(mod) # general summary
car::vif(mod) # check for multicollinearity

# plot residuals and leverage
plotResLev(model = mod)

# test predictions on test set (the model predicts y^yexp, so we have to transform
# y back)
ypredraw <- predict(object = mod, 
                    newdata = data.frame(
                      test[, c('rainfall',
                               'angleAttack', 
                               'windvel')]))
ypred <- ypredraw^(1/yexp)

# compute residual standard error
computeRSE(yobs = test$specRunoff, 
           ypred = ypred,
           model = mod)

# plot 
par(mfcol = c(1, 1), mar=c(4, 4, 1, 1))
plot(test$specRunoff, ypred, asp=1, 
     xlab = 'observed', ylab = 'predicted')
abline(a = 0, b = 1, col = 'red')


# make predictors; weather data from station Tempelhof
rain.events <- makePredictors(
  windfile = 'produkt_ff_stunde_19740101_20191231_00433.txt',
  rainfile = 'produkt_rr_stunde_19950901_20191231_00433.txt',
  airtempfile = 'produkt_tu_stunde_19510101_20191231_00433.txt')

# predict facade runoff for hypothetical (perfectly aligned) facades of
# all four cardinal directions (S, O, W, N)
rain.events <- cbind(rain.events, 
                     as.data.frame(
                       lapply(X = c('S', 'O', 'N', 'W'),
                              FUN = makePredSide,
                              data = rain.events,
                              yexp = 0.5),
                       col.names = c('runoffS', 'runoffO', 
                                     'runoffN', 'runoffW')))

# sum runoff from all sides
rain.events$totalFacadeRunoff <- apply(
  X = rain.events[, c('runoffS', 'runoffO', 'runoffN', 'runoffW')],
  MARGIN = 1 ,
  FUN = sum)


# annual totals
annualFacadeRunoff <- aggregate(
  x = rain.events$totalFacadeRunoff, 
  by = list(rain.events$year), 
  FUN = sum)

# write outputs: 
# rain.events: individual storms with their facade runoff on all 4 sides and their sum
# annualFacadeRunoff: annual totals (total runoff of four facade sides for each year)
write.table(x = rain.events, 
            file = 'output_rain.events.txt', 
            quote = FALSE, 
            sep = ';', 
            row.names = FALSE)

write.table(x = annualFacadeRunoff, 
            file = 'output_annualFacadeRunoff.txt', 
            quote = FALSE, 
            sep = ';', 
            row.names = FALSE)


# functions ---------------------------------------------------------------------------------
angleAttack <- function(facadeOrientation, windDir){
  
  dalpha <- abs(windDir - facadeOrientation)
  
  # two cases = dalpha < 180 of dalpha > 180
  if(dalpha < 180){
    
    aa <- ifelse(dalpha>=90, 0, abs(90-dalpha))
    
  } else {
    
    # for dalpha > 180, subtract 360 to bring alpha to first quadrant of cartesian plane
    dalpha2 <- abs(dalpha - 360)
    aa <- ifelse(dalpha2 >= 90, 0, abs(90-dalpha2))
  }
  
  return(aa)
}

getFacadeRunoffBaSaR <- function(dbName, dbTable,
                                 dateTimeFormat, tz,
                                 facadeOrientations){
  
  library(readxl)
  library(dplyr)
  
  # read BaSaR facade runoff table
  db <- readxl::read_excel(path = dbName,
                           sheet = dbTable,
                           col_types = "text", 
                           skip = 2, 
                           na = c("na"), 
                           trim_ws = TRUE)
  
  # add site name
  db$site <- tolower(dbTable)
  
  # select columns
  index <- grepl(pattern = paste('tBegRain', 'tEndRain','Regenhöhe_mm',
                                 'Anzahl_Ereignisse', 'analysierte_Flasche',
                                 'temperatur', 'Wind', 'Volumen',
                                 'FlächeRinne', 'site',
                                 sep = '|',
                                 collapse = ''),
                 x = colnames(db))
  db <- db[, index]
  
  # format numerical columns
  index <- grepl(pattern = paste('Regenhöhe_mm', 'Anzahl_Ereignisse',
                                 'temperatur', 'Wind', 
                                 'FlächeRinne',
                                 sep = '|',
                                 collapse = ''),
                 x = colnames(db))
  
  db[, index] <- apply(X = db[, index], 
                       MARGIN = c(1, 2), 
                       FUN = as.numeric)
  
  # lengthen data
  db <- db %>% 
    tidyr::pivot_longer(cols = dplyr::contains(c('Volumen')),
                        names_to = 'side',
                        values_to = 'runoff') %>%
    dplyr::mutate(runoff = as.character(runoff),
                  side = substr(side, 9, 9)) %>%
    dplyr::rename(rainfall = Regenhöhe_mm ,
                  windvel = Wind_v_mean_m_s,
                  windvelsd = Wind_v_sd,
                  winddir = Windrichtung_mean_Grad,
                  winddirsd = Windrichtung_sd)
  
  # remove rows where bottles overflowed
  index <- which(sapply(strsplit(x = db$runoff, 
                                 split = '>'), 
                        length) > 1)
  db$runoff[index] <- NA
  db$runoff <- as.numeric(db$runoff)
  
  # format date columns
  tCols <- grep(pattern='tBeg|tEnd', x = colnames(db))
  for(col in tCols){ 
    db[[col]] <- as.POSIXct(db[[col]],
                            format = dateTimeFormat, 
                            tz = tz)
  }
  
  # compute specific runoff [l/m2] using corresponding areas:
  
  col <- sapply(X = db$side,  # get column for corresponding area
                FUN = function(x){
                  grep(pattern = paste0('FlächeRinne', x), 
                       x = colnames(db))
                })
  
  db$specRunoff <- unlist(mapply(FUN = function(i, c){ # loop over rows
    db$runoff[i] / db[[i, c]] / 1000
  }, 1:nrow(db), col))
  
  db$facadeOrientation <- sapply(
    X = db$side, 
    FUN = function(x){
      facadeOrientations[names(facadeOrientations) == x]
    })
  
  db$angleAttack <- mapply(FUN = angleAttack, 
                           db$facadeOrientation,
                           db$winddir)
  
  return(db)
}

fitlm <- function(data, trainperc, yexp){
  dataNoNA <- data[!is.na(data$specRunoff), ]
  
  trainSamples <- caret::createDataPartition(
    y = dataNoNA$specRunoff, 
    p = trainperc, 
    times = 1, 
    list = FALSE)[, 1]
  
  train <<- dataNoNA[trainSamples, ]
  test <<- dataNoNA[-trainSamples, ]
  
  mod <- lm(
    data = train, 
    formula = I(specRunoff^yexp) ~ rainfall  + angleAttack + windvel +
      rainfall:angleAttack:windvel, 
    weights = 1/winddirsd)
  
  return(mod)
}

plotResLev <- function(model){
  par(mfcol = c(1, 2), mar = c(4, 4, 1.5, 2))
  plot(predict(mod), residuals(mod), 
       main = 'fitted vs. res.',
       xlab = 'fitted vals.', 
       ylab = 'residuals')
  plot(hatvalues(mod), rstudent(mod), 
       main = 'leverage vs. stud. res.',
       xlab = 'leverage',
       ylab = 'studentized residuals')
}

computeRSE <- function(yobs, ypred, model){
  p <- length(model$coefficients) - 1 # no. predictors
  n <- length(yobs) # no. obs.
  rse <- sqrt(sum((yobs - ypred)^2)/(n-p-1))
  return(rse)
}

makePredictors <- function(windfile,
                           rainfile,
                           airtempfile){
  
  # load data
  dat <- lapply(X = c(windfile, rainfile, airtempfile),
                FUN = read.table,
                header = TRUE,
                sep = ';',
                colClasses = 'character')
  
  # select cols and adjust colnames
  trimData <- function(dat, keepcols, mynames){
    trimmeddat <- dat[, keepcols]   
    colnames(trimmeddat) <- mynames
    return(trimmeddat)
  }
  
  dat[[1]] <- trimData(dat = dat[[1]], 
                       keepcols = c(2, 4, 5), 
                       mynames = c('dateTime', 'windvel', 'winddir'))
  dat[[2]] <- trimData(dat = dat[[2]],
                       keepcols = c(2, 4),
                       mynames = c('dateTime', 'rainfall'))
  dat[[3]] <- trimData(dat = dat[[3]],
                       keepcols = c(2, 4),
                       mynames = c('dateTime', 'airtemp'))
  
  # lengthen data tables prior to stacking one on top of the other
  wind <- tidyr::pivot_longer(dat[[1]], 
                              cols = c('windvel', 'winddir'),
                              names_to = 'variable',
                              values_to = 'values')
  rain <- tidyr::pivot_longer(data = dat[[2]],
                              cols = 'rainfall',
                              names_to = 'variable',
                              values_to = 'values')
  airtemp <- tidyr::pivot_longer(data = dat[[3]],
                                 cols = 'airtemp',
                                 names_to = 'variable',
                                 values_to = 'values')
  
  # stack individual variables on top of one another
  dat <- rbind(wind, rain, airtemp)
  
  # format date and numbers
  dat$dateTime <- as.POSIXct(dat$dateTime,
                             format = '%Y%m%d%H',
                             tz = 'UTC')
  dat$values[grep(pattern = '-999', x = dat$values)] <- NA
  dat$values <- as.numeric(dat$values)
  
  # seprate variables
  dat <- as.data.frame(dat)
  winddir <- dat[dat$variable == 'winddir', ]
  windvel <- dat[dat$variable == 'windvel', ]
  rain <- dat[dat$variable == 'rainfall', ]
  airtemp <- dat[dat$variable == 'airtemp', ]
  
  # get storms
  rain.events <- kwb.event::getEvents(rainData = rain,
                                      seriesName = 'values', 
                                      signalWidth = 3600)
  
  # compute each storm's rainfall depth, wind and temperature info
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
                winddirsel <- filterstorm(winddir, tBeg, tEnd)
                windvelsel <- filterstorm(windvel, tBeg, tEnd)
                
                stormrain <- sum(rainsel$values, na.rm = TRUE)
                stormairtempmean <- mean(airtempsel$values, na.rm = TRUE)
                stormairtempmax <- max(airtempsel$values, na.rm = TRUE)
                stormwindvelmean <- mean(windvelsel$values, na.rm = TRUE)
                stormwindvelsd <- sd(windvelsel$values, na.rm = TRUE)
                stormwinddirmean <- mean(winddirsel$values, na.rm = TRUE)
                stormwinddirsd <- sd(winddirsel$values, na.rm = TRUE)
                
                return(c(stormrain,
                         stormairtempmean,
                         stormairtempmax,
                         stormwindvelmean,
                         stormwindvelsd,
                         stormwinddirmean,
                         stormwinddirsd))
              })
  
  x <- data.frame(do.call(rbind, x))
  
  colnames(x) <- c('rainfall',
                   'airtempmean',
                   'airtempmax',
                   'windvelmean',
                   'windvelsd',
                   'winddirmean',
                   'winddirsd')
  
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
                                        'airtempmean', 
                                        'windvelmean', 
                                        'winddirmean'), 
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
  
  
  # make angle of attack for hypothetical facades (perfectly aligned with
  # cardinal directions)
  x <- sapply(X = c(N = 0, O = 90, S = 180, W = 270),
         FUN = function(i){
           sapply(
             X = seq_along(rain.events$winddirmean),
             FUN = function(j){
               angleAttack(facadeOrientation = i,
                           windDir = rain.events$winddirmean[j])
             })}, USE.NAMES = TRUE)
  
  colnames(x) <- c('angleAttackN', 'angleAttackO', 
                   'angleAttackS', 'angleAttackW') 
         
  rain.events <- cbind(rain.events, x)
  

  return(rain.events)
}

makePredSide <- function(side, data, yexp){
  
  XpredSide <- data.frame(
    rainfall = data$rainfall,
    angleAttack = data[, paste0('angleAttack', side)],
    windvel = data$windvelmean)
  
  ypredraw <- predict(object = mod, 
                      newdata = XpredSide)
  
  ypred <- ypredraw^(1/yexp)
  
  return(ypred)
}






# plot annual totals
annrain <- aggregate(x = rain$values,
                     by = list(lubridate::year(rain$dateTime)),
                     FUN = sum,
                     na.rm = TRUE)
xy <- annualFacadeRunoff$Group.1
annrain <- annrain[annrain$Group.1 %in% xy, ]

barplot(annrain$x, names.arg = x$Group.1, las=2, main = 'annual rainfall [mm/year]')

# annual totals per side
annrainside <- as.data.frame(
  lapply(X = c('runoffS', 'runoffO', 'runoffW', 'runoffN'),
         FUN = function(x){
           aggregate(
             x = rain.events[[x]], 
             by = list(rain.events$year), 
             FUN = sum)
         }))[, c(1, 2, 4, 6, 8)]
colnames(annrainside) <- c('year', 'runoffS', 'runoffO','runoffW', 'runoffN')


par(mar=c(3, 3, 2, 1))
barplot(t(as.matrix(annrainside[, 2:ncol(annrainside)])), 
        names.arg = annrainside$year, 
        las = 2,
        main = 'Total facade runoff [l/m2/year]',
        col = c('red', 'blue', 'forestgreen', 'white'))
legend(x = 0, y = 40, 
       legend = c('runoffS', 'runoffO','runoffW', 'runoffN'),
       fill = c('red', 'blue', 'forestgreen', 'white'),
       cex = 0.8)


barplot(t(as.matrix(annrainside[, 2:ncol(annrainside)])), 
        names.arg = annrainside$year, 
        las = 2,
        main = 'Total facade runoff [l/m2/year]',
        legend.text = T)
