# explore specific runoff from BaSaR facades
#rawdir = 'Y:/SUW_Department/Projects/SuR/Data-Work_packages/AP1_Modellierung/dataBaSaR/'

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

# fit model
basarNoNA <- basar[!is.na(basar$specRunoff), ]

trainSamples <- caret::createDataPartition(
  y = basarNoNA$specRunoff, 
  p = 0.7, 
  times = 1, 
  list = FALSE)[, 1]

basar_train <- basarNoNA[trainSamples, ]
basar_test <- basarNoNA[-trainSamples, ]

mod <- lm(
  data = basar_train, 
  formula = specRunoff^(1/2) ~ rainfall  + angleAttack + windvel +
    rainfall:angleAttack:windvel, 
  weights = 1/winddirsd)

summary(mod)

plot(mod)

# assess model predictions
ypred <- predict(
  mod, 
  newdata = data.frame(
    basar_test[c('rainfall',
                 'angleAttack', 
                 'windvel')]))


par(mfcol = c(1, 2), mar=c(3, 3, 1, 1))
plot(fitted(mod), resid(mod))
plot(basar_test$specRunoff, ypred^2)
abline(a = 0, b = 1, col = 'red')




# functions ---------------------------------------------------------------------------------
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
  
  # compute angle of attack of wind
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
  
  db$facadeOrientation <- sapply(X = db$side, 
                                 FUN = function(x){
                                   facadeOrientations[names(facadeOrientations) == x]
                                 })
  
  db$angleAttack <- mapply(FUN = angleAttack, 
                           db$facadeOrientation,
                           db$winddir)
  
  return(db)
}

windfile <- 'produkt_ff_stunde_19740101_20191231_00433.txt'
rainfile <- 'produkt_rr_stunde_19950901_20191231_00433.txt'


makeRunoffPredictors <- functoin(windfile,
                                 rainfile){
  
  dat <- lapply(X = c(windfile, rainfile),
                FUN = read.table,
                header = TRUE,
                sep = ';',
                colClasses = 'character',
                na.strings = '-999')

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
                   mynames = c('dateTime', 'rain'))
  
  wind <- tidyr::pivot_longer(dat[[1]], 
                              cols = c('windvel', 'winddir'),
                              names_to = 'variable',
                              values_to = 'values')
  rain <- tidyr::pivot_longer(data = dat[[2]],
                              cols = c('rain'),
                              names_to = 'variable',
                              values_to = 'values')

  dat <- rbind(wind, rain)    
  
  dat$dateTime <- as.POSIXct(dat$dateTime,
                             format = '%Y%m%d%H',
                             tz = 'Europe/Berlin')

  dat$values <- as.numeric(dat$values)
    
  winddir <- dat[dat$variable == 'winddir', ]
  windvel <- dat[dat$variable == 'windvel', ]
  rain <- dat[dat$variable == 'rain', ]
  
  # get storms
  rain.events <- kwb.event::getEvents(rainData = rain,
                                      seriesName = 'values')
  
  # build predictors
  
  
}


