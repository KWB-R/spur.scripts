# explore mean emitted concentrations from BaSaR and SpuR facades

# grab data from both projects
basar_bbr_facade <- getMonitoringTable(
  subfolder = 'data_facade_vol_c_annual',
  dbName = 'BBRf_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbr',
  source = 'facade',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

basar_bbw_facade <- getMonitoringTable(
  subfolder = 'data_facade_vol_c_annual',
  dbName = 'BBWf_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbw',
  source = 'facade',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

basar_bbr_roof <- getMonitoringTable(
  subfolder = 'data_facade_vol_c_annual',
  dbName = 'BBRr_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbr',
  source = 'roof',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

basar_bbw_roof <- getMonitoringTable(
  subfolder = 'data_facade_vol_c_annual',
  dbName = 'BBWr_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbw',
  source = 'roof',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

spur_pkw <- getMonitoringTable(
  subfolder = 'data_facade_vol_c_annual',
  dbName = 'Felddatenbank_SpuR_Pankow.xlsx',
  dbTable = 'Felddatenbank',
  format = 'xls',
  project = 'spur',
  detLimOperator = 0.5,
  site = 'pkw',
  source = 'facade+roof',
  dateTimeFormat = '%d.%m.%Y %H:%M', 
  tz = 'Etc/GMT-1')


basar_bbr_kanal <- getMonitoringTable(
  subfolder = 'data_facade_vol_c_annual',
  dbTable = NA,
  dbName = 'BBRs_20200518_conc.txt',
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbr',
  source = 'storm_sewer',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')


basar_bbw_kanal <- getMonitoringTable(
  subfolder = 'data_facade_vol_c_annual',
  dbTable = NA,
  dbName = 'BBWs_20200518_conc.txt',
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbw',
  source = 'storm_sewer',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')


# make tables for target substances
Zn <- rbind(makeSubstanceTable(
  dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
              'basar_bbr_roof', 'basar_bbw_roof',
              'basar_bbr_kanal','basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Zn_total'),
  makeSubstanceTable(
    dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
                'basar_bbr_roof', 'basar_bbw_roof',
                'basar_bbr_kanal','basar_bbw_kanal',
                'spur_pkw'),
    substance = 'Zn'))

Cu <- rbind(makeSubstanceTable(
  dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
              'basar_bbr_roof', 'basar_bbw_roof',
              'basar_bbr_kanal','basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Cu_total'),
  makeSubstanceTable(
    dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
                'basar_bbr_roof', 'basar_bbw_roof',
                'basar_bbr_kanal','basar_bbw_kanal',
                'spur_pkw'),
    substance = 'Cu'))

Diuron <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
              'basar_bbr_roof', 'basar_bbw_roof',
              'basar_bbr_kanal','basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Diuron')

Terbutryn <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
              'basar_bbr_roof', 'basar_bbw_roof',
              'basar_bbr_kanal','basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Terbutryn')

Terbutryn_desethyl <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
              'basar_bbr_roof', 'basar_bbw_roof',
              'basar_bbr_kanal','basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Terbutryn_desethyl')

Terbutryn_2_hydroxy <- rbind(
  makeSubstanceTable(
    dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
                'basar_bbr_roof', 'basar_bbw_roof',
                'basar_bbr_kanal','basar_bbw_kanal',
                'spur_pkw'),
    substance = 'Terbutryn.2.hydroxy'),
  makeSubstanceTable(
    dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
                'basar_bbr_roof', 'basar_bbw_roof',
                'basar_bbr_kanal','basar_bbw_kanal',
                'spur_pkw'),
    substance = 'Terbutryn-2-hydroxy'))

Mecoprop <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
              'basar_bbr_roof', 'basar_bbw_roof',
              'basar_bbr_kanal','basar_bbw_kanal',
              'spur_pkw'), 
  substance = 'Mecoprop')

Benzothiazol <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade', 'basar_bbw_facade', 
              'basar_bbr_roof', 'basar_bbw_roof',
              'basar_bbr_kanal','basar_bbw_kanal',
              'spur_pkw'), 
  substance = 'Benzothiazol')


# statistics
summary(Terbutryn$concentration)
summary(Diuron$concentration)
summary(Mecoprop$concentration)

# plots
plotSubstance(Zn)
plotSubstance(Cu)
plotSubstance(Diuron)
plotSubstance(Terbutryn)
plotSubstance(Terbutryn_desethyl) 
plotSubstance(Terbutryn_2_hydroxy)
plotSubstance(Mecoprop)

ALT <- read.table('data/Konz_ALT.csv', header = TRUE, sep = ';')
NEU <- read.table('data/Konz_NEU.csv', header = TRUE, sep = ';')
EFH <- read.table('data/Konz_NEU.csv', header = TRUE, sep = ';')
GEW <- read.table('data/Konz_NEU.csv', header = TRUE, sep = ';')



boxplot(concentration ~ source, data= Diuron, main = 'Diuron', xlab = NA , ylab = 'concentration [mg/L]')
abline(h = (1000*c(ALT$Konz_Diuron, NEU$Konz_Diuron, EFH$Konz_Diuron, GEW$Konz_Diuron)), col= c('red', 'green','blue','orange' ), lwd = 1, lty = 2)
legend(x= 3.4 , y=2900, legend = c('ALT', 'NEU', 'EFH', 'GEW'), col = c('red','green','blue','orange'), lty=2)

boxplot(concentration ~ source, data= Mecoprop, main = 'Mecoprop', xlab = NA , ylab = 'concentration [mg/L]')
abline(h = (1000*c(ALT$Konz_Mecoprop, NEU$Konz_Mecoprop, EFH$Konz_Mecoprop, GEW$Konz_Mecoprop)), col= c('red', 'green','blue','orange' ), lwd = 1, lty = 2)
legend(x= 3.4 , y= 440, legend = c('ALT', 'NEU', 'EFH', 'GEW'), col = c('red','green','blue','orange'), lty=2)

boxplot(concentration ~ source, data= Terbutryn, main = 'Terbutryn', xlab = NA , ylab = 'concentration [mg/L]')
abline(h = (1000*c(ALT$Konz_Terbutryn, NEU$Konz_Terbutryn, EFH$Konz_Terbutryn, GEW$Konz_Terbutryn)), col= c('red', 'green','blue','orange' ), lwd = 1, lty = 2)
legend(x= 3.4 , y=83, legend = c('ALT', 'NEU', 'EFH', 'GEW'), col = c('red','green','blue','orange'), lty=2)

boxplot(concentration ~ source, data = Benzothiazol, main = 'Benzothiazol', xlab = NA , ylab = 'concentration [mg/L]')
abline(h = (1000*c(ALT$Konz_Benzothiazol, NEU$Konz_Benzothiazol, EFH$Konz_Benzothiazol, GEW$Konz_Benzothiazol)), col= c('red', 'green','blue','orange' ), lwd = 1, lty = 2)
legend(x= 1.22 , y=22, legend = c('ALT', 'NEU', 'EFH', 'GEW'), col = c('red','green','blue','orange'), lty=2)

boxplot(concentration ~ source, data = Zn, main = 'Zn', xlab = NA , ylab = 'concentration [mg/L]')
abline(h = (1000*c(ALT$Konz_Zn, NEU$Konz_Zn, EFH$Konz_Zn, GEW$Konz_Zn)), col= c('red', 'green','blue','orange' ),lwd = 1, lty = 2)
legend(x= 3.4 , y=13000, legend = c('ALT', 'NEU', 'EFH', 'GEW'), col = c('red','green','blue','orange'), lty=2)

boxplot(concentration ~ source, data = Cu, main = 'Cu', xlab = NA , ylab = 'concentration [mg/L]')
abline(h = (1000*c(ALT$Konz_Cu, NEU$Konz_Cu, EFH$Konz_Cu, GEW$Konz_Cu)), col= c('red', 'green','blue','orange' ), lwd = 1, lty = 2)
legend(x= 3.4 , y=790, legend = c('ALT', 'NEU', 'EFH', 'GEW'), col = c('red','green','blue','orange'), lty=2)



# functions ---------------------------------------------------------------------------------
getMonitoringTable <- function(subfolder,
                               dbName, dbTable, 
                               format, project, site, source,
                               detLimOperator, dateTimeFormat, 
                               tz){
  
  library(dplyr)
  
  if(format == 'txt'){
    
    db <- read.table(file.path(subfolder, dbName), 
                     header = TRUE, 
                     sep = ";", 
                     colClasses = 'character')
    
  } else if (format == 'xls'){
    db <- as.data.frame(readxl::read_excel(file.path(subfolder, dbName), 
                                           sheet = dbTable, 
                                           col_types = "text", 
                                           skip = 3, 
                                           na = c("na", "nb","NA")),
                        stringsAsFactors = FALSE)
  } else {
    stop('wrong file format, either "xls" or "txt"')
  }
  
  db$site <- site
  db$project <- project
  db$source <- source
  
  # grab targeted substances and accompanying event data
  index <- grepl(pattern = paste('tBegRain', 'tEndRain','Regenhöhe_mm',
                                 'Anzahl_Ereignisse',
                                 'Lufttemperatur', 'Wind',
                                 'Zn', 'Cu', 'Diuron', 'Isoproturon', 
                                 'Mecoprop', 'Terbutryn',  'Terbumeton', 
                                 'Benzothiazol', 'OIT', 'site', 'project',
                                 'source',
                                 sep = '|',
                                 collapse = ''),
                 x = colnames(db))
  db <- db[, index]
  
  # lengthen data
  db <- db %>% 
    tidyr::pivot_longer(cols = dplyr::contains(c('Zn', 'Cu', 'Diuron', 'Isoproturon', 
                                                 'Mecoprop', 'Terbutryn',  'Terbumeton', 
                                                 'Benzothiazol', 'OIT')),
                        names_to = 'substance',
                        values_to = 'concentration') %>%
    dplyr::mutate(concentration = as.character(concentration),
                  Regenhöhe_mm = as.numeric(Regenhöhe_mm))
  
  # remove '<' and apply detLimOperator
  detLimManage <- function(x, detLimOperator){
    rowsDetLim <- which(sapply(X = strsplit(x = x, 
                                            split = '<'), 
                               FUN = length) > 1)
    x <- as.numeric(gsub(pattern = '<', 
                         replacement = '', 
                         x = gsub(pattern = ',', 
                                  replacement = '.', 
                                  x = x)))
    x[rowsDetLim] <- x[rowsDetLim]*detLimOperator
    return (x)
  }
  
  db$concentration <- detLimManage(x = db$concentration, 
                                   detLimOperator = detLimOperator)
  
  # in spur data, get rid of filter outlflow concentrations, and remove '_zu' in substance names
  if(project == 'spur'){
    index <- !grepl(pattern = '_ab$',
                    x = db$substance)
    db <- db[index, ]
    
    db$substance <- gsub(pattern = '_zu$', replacement = '', x = db$substance)
  }
  
  # format dateTime columns
  tCols <- grep(pattern='tBeg|tEnd', x = colnames(db))
  for(col in tCols){
    db[[col]] <- as.POSIXct(db[[col]], format = dateTimeFormat, tz = tz)
  }
  
  return(db)
}

makeSubstanceTable <- function(dbNames, substance){
  
  output <- data.frame()
  
  colnam <- c("tBegRain", "tEndRain", "Regenhöhe_mm", "Anzahl_Ereignisse",
              "site", "project", "source", "substance", "concentration")
  
  for(dbi in dbNames){
    dat <- get(dbi)
    dat <- dat[dat$substance == substance,
               grepl(pattern = paste('tBegRain', 'tEndRain', 'Regenhöhe',
                                     'Anzahl_Ereig', 'site', 'project', 'source',
                                     'substance', 'concentration',
                                     sep = '|', collapse = ''), 
                     x = colnames(dat))]
    colnames(dat) <- colnam
    dat$stormDuration <- (as.numeric(dat$tEndRain) - as.numeric(dat$tBegRain))/
      as.numeric(dat$Anzahl_Ereignisse)/3600
    dat$rainMeanIntensity <- dat$Regenhöhe_mm/dat$stormDuration
    
    output <- rbind(output, dat)
  }
  
  return(output)
}

plotSubstance <- function(substance){
  
  library(colorspace)
  
  nsites <- length(unique(substance$site))
  name <- substitute(substance)
  
  par(mfcol=c(1, 3))
  boxplot(substance$concentration, las = 2); title(main = name)
  plot(concentration ~ Regenhöhe_mm, data = substance,
       col = rainbow_hcl(nsites),
       pch = 20, cex = 2.5, main = name)
  plot(concentration ~ rainMeanIntensity, data = substance,
       col = rainbow_hcl(nsites),
       pch = 20, cex = 2.5, main = name)
  legend('topright', legend = unique(substance$site), pch = 20, cex=1.5,
         col = rainbow_hcl(nsites))
}
