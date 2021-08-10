# explore mean emitted concentrations from BaSaR and SpuR facades

# grab data from both projects BaSaR and SpuR
basar_bbr_facade <- getMonitoringTable(
  subfolder = 'data_prelim_sources',
  dbName = 'bbr_facade_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbr',
  source = 'facade',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

basar_bbw_facade <- getMonitoringTable(
  subfolder = 'data_prelim_sources',
  dbName = 'bbw_facade_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbw',
  source = 'facade',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

basar_bbr_roof <- getMonitoringTable(
  subfolder = 'data_prelim_sources',
  dbName = 'bbr_roof_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbr',
  source = 'roof',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

basar_bbw_roof <- getMonitoringTable(
  subfolder = 'data_prelim_sources',
  dbName = 'bbw_roof_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbw',
  source = 'roof',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

spur_pkw <- getMonitoringTable(
  subfolder = 'data_prelim_sources',
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
  subfolder = 'data_prelim_sources',
  dbTable = NA,
  dbName = 'bbr_sewer_20200518_conc.txt',
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbr',
  source = 'storm_sewer',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')


basar_bbw_kanal <- getMonitoringTable(
  subfolder = 'data_prelim_sources',
  dbTable = NA,
  dbName = 'bbw_sewer_20200518_conc.txt',
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbw',
  source = 'storm_sewer',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')


# make tables for target substances
Zn <- rbind(makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade', 
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_kanal',
              'basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Zn_total'),
  makeSubstanceTable(
    dbNames = c('basar_bbr_facade',
                'basar_bbw_facade',
                'basar_bbr_roof',
                'basar_bbw_roof',
                'basar_bbr_kanal',
                'basar_bbw_kanal',
                'spur_pkw'),
    substance = 'Zn'))

Cu <- rbind(makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade', 
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_kanal',
              'basar_bbw_kanal',
              'spur_pkw'),
  
  substance = 'Cu_total'),
  makeSubstanceTable(
    dbNames = c('basar_bbr_facade',
                'basar_bbw_facade', 
                'basar_bbr_roof',
                'basar_bbw_roof',
                'basar_bbr_kanal',
                'basar_bbw_kanal',
                'spur_pkw'),
    substance = 'Cu'))

Diuron <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade',
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_kanal',
              'basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Diuron')

# remove BBR 
Diuron_without_bbr <- Diuron[Diuron$site != 'bbr', ]

Terbutryn <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade',
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_kanal',
              'basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Terbutryn')

Terbutryn_desethyl <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade',
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_kanal',
              'basar_bbw_kanal',
              'spur_pkw'),
  substance = 'Terbutryn_desethyl')


Mecoprop <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade', 
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_kanal',
              'basar_bbw_kanal',
              'spur_pkw'), 
  substance = 'Mecoprop')

# remove BBR (green roof)
Mecoprop_without_bbr <- Mecoprop[Mecoprop$site != 'bbr', ]

Benzothiazol <- makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade', 
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_kanal',
              'basar_bbw_kanal',
              'spur_pkw'), 
  substance = 'Benzothiazol')


# statistics
summary(Zn$concentration)
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
plotSubstance(Benzothiazol)

c_ALT <- read.table('data/Konz_ALT.csv', header = TRUE, sep = ';')
c_NEU <- read.table('data/Konz_NEU.csv', header = TRUE, sep = ';')
c_EFH <- read.table('data/Konz_GEW.csv', header = TRUE, sep = ';')
c_GEW <- read.table('data/Konz_EFH.csv', header = TRUE, sep = ';')

# bring plots in the right order
#substances<- c(Mecoprop$source, Diuron$source, Terbutryn$source, Benzothiazol$source, Zn$source, Cu$source)
#for (i in 1:6){
#substances[i]<- factor(substances[i], levels = c("facade", "roof", "sewer", "facade & roof"))  
#}


#Mecoprop
Mecoprop_without_bbr$plotOrder <- plyr::mapvalues(Mecoprop_without_bbr$source,
                                                     from = c('facade', 'roof', 'facade+roof', 'storm_sewer'), 
                                                     to = c(1, 2, 4, 3))
boxplot(concentration ~ plotOrder, 
        data = Mecoprop_without_bbr, 
        main = 'Mecoprop', 
        xlab = NA , ylab = 'concentration [μg/L]', 
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, 500))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Mecoprop[1], 
                   NEU$Konz_Mecoprop[1], 
                   EFH$Konz_Mecoprop[1], 
                   GEW$Konz_Mecoprop[1])), 
       col = c('red', 'green','blue','orange' ), lwd = 1, lty = 2)
legend(x= 0.5 , y = 500,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'),lty = 2)

#Mecoprop zoomed in
Mecoprop_without_bbr$plotOrder <- plyr::mapvalues(Mecoprop_without_bbr$source,
                                                  from = c('facade', 'roof', 'facade+roof', 'storm_sewer'), 
                                                  to = c(1, 2, 4, 3))
boxplot(concentration ~ plotOrder, 
        data = Mecoprop_without_bbr, 
        main = 'Mecoprop', 
        xlab = NA , ylab = 'concentration [μg/L]', 
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, 60))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Mecoprop[1], 
                   NEU$Konz_Mecoprop[1], 
                   EFH$Konz_Mecoprop[1], 
                   GEW$Konz_Mecoprop[1])), 
       col = c('red', 'green','blue','orange' ), lwd = 1, lty = 2)
legend(x= 0.5 , y = 60,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'),lty = 2)


#Diuron
Diuron_without_bbr$plotOrder <- plyr::mapvalues(Diuron_without_bbr$source,
                                                   from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                                   to = c(1, 2, 4, 3))

boxplot(concentration ~ plotOrder,
        data = Diuron_without_bbr,
        main = 'Diuron',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, 1800))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Diuron[6],
                   NEU$Konz_Diuron[6],
                   EFH$Konz_Diuron[6],
                   GEW$Konz_Diuron[6])),
       col = c('red', 'green','blue','orange' ),lwd = 1, lty = 2)

legend(x= 3.5 , y = 1800,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty = 2)

#Diuron zoomed in
Diuron_without_bbr$plotOrder <- plyr::mapvalues(Diuron_without_bbr$source,
                                                from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                                to = c(1, 2, 4, 3))

boxplot(concentration ~ plotOrder,
        data = Diuron_without_bbr,
        main = 'Diuron',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, 400))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Diuron[6],
                   NEU$Konz_Diuron[6],
                   EFH$Konz_Diuron[6],
                   GEW$Konz_Diuron[6])),
       col = c('red', 'green','blue','orange' ),lwd = 1, lty = 2)

legend(x= 0.5 , y = 400,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty = 2)

#Terbutryn
Terbutryn$plotOrder <- plyr::mapvalues(Terbutryn$source,
                                               from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                               to = c(1, 2, 4, 3))

boxplot(concentration ~ plotOrder,
        data= Terbutryn,
        main = 'Terbutryn',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, 900))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Terbutryn[6],
                   NEU$Konz_Terbutryn[6],
                   EFH$Konz_Terbutryn[6],
                   GEW$Konz_Terbutryn[6])), 
       col = c('red', 'green','blue','orange' ), lwd = 1, lty = 2)

legend(x = 3.5 , y = 900,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty = 2)

#Terbutryn zoomed in
Terbutryn$plotOrder <- plyr::mapvalues(Terbutryn$source,
                                       from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                       to = c(1, 2, 4, 3))

boxplot(concentration ~ plotOrder,
        data= Terbutryn,
        main = 'Terbutryn',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, 35))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Terbutryn[6],
                   NEU$Konz_Terbutryn[6],
                   EFH$Konz_Terbutryn[6],
                   GEW$Konz_Terbutryn[6])), 
       col = c('red', 'green','blue','orange' ), lwd = 1, lty = 2)

legend(x = 3.5 , y = 35,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty = 2)


#Benzothiazol
Benzothiazol$plotOrder <- plyr::mapvalues(Benzothiazol$source,
                                       from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                       to = c(1, 2, 4, 3))


boxplot(concentration ~ plotOrder,
        data = Benzothiazol,
        main = 'Benzothiazol',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, 25))

axis(side = 1, 
     at = 1:1, 
     labels = c('SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Benzothiazol[1],
                   NEU$Konz_Benzothiazol[1],
                   EFH$Konz_Benzothiazol[1],
                   GEW$Konz_Benzothiazol[1])),
       col = c('red', 'green','blue','orange' ), lwd = 1, lty = 2)

legend(x = 0.5 , y = 25.00,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty=2)

#Zinc
Zn$plotOrder <- plyr::mapvalues(Zn$source,
                                       from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                       to = c(1, 2, 4, 3))
boxplot(concentration ~ plotOrder,
        data = Zn,
        main = 'Zinc',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0,2800))
        
axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Zn[1],
                   NEU$Konz_Zn[1],
                   EFH$Konz_Zn[1],
                   GEW$Konz_Zn[1])),
       col= c('red', 'green','blue','orange' ),lwd = 1, lty = 2)

legend(x= 0.5 , y=2800,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty=2)

#Zinc zoomed in
Zn$plotOrder <- plyr::mapvalues(Zn$source,
                                from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                to = c(1, 2, 4, 3))
boxplot(concentration ~ plotOrder,
        data = Zn,
        main = 'Zinc',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0,1000))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Zn[1],
                   NEU$Konz_Zn[1],
                   EFH$Konz_Zn[1],
                   GEW$Konz_Zn[1])),
       col= c('red', 'green','blue','orange' ),lwd = 1, lty = 2)

legend(x= 3.85 , y=1000,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty=2)

#Copper
Cu$plotOrder <- plyr::mapvalues(Cu$source,
                                from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                to = c(1, 2, 4, 3))

boxplot(concentration ~ plotOrder,
        data = Cu,
        main = 'Copper',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0,800))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Cu[1],
                   NEU$Konz_Cu[1],
                   EFH$Konz_Cu[1],
                   GEW$Konz_Cu[1])),
       col= c('red', 'green','blue','orange' ), lwd = 1, lty = 2)

legend(x= 0.5 , y=800,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty=2)


#Copper zoomed in
Cu$plotOrder <- plyr::mapvalues(Cu$source,
                                from = c('facade','roof', 'facade+roof', 'storm_sewer'), 
                                to = c(1, 2, 4, 3))

boxplot(concentration ~ plotOrder,
        data = Cu,
        main = 'Copper',
        xlab = NA , ylab = 'concentration [μg/L]',
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0,60))

axis(side = 1, 
     at = 1:4, 
     labels = c('BaSaR\nfacade',
                'BaSaR\nroof\n1 year old', 
                'BaSaR\nstorm sewer', 
                'SpuR\nfacade+roof\nnew'),
     padj = 0.75)

abline(h = (1000*c(ALT$Konz_Cu[1],
                   NEU$Konz_Cu[1],
                   EFH$Konz_Cu[1],
                   GEW$Konz_Cu[1])),
       col= c('red', 'green','blue','orange' ), lwd = 1, lty = 2)

legend(x= 0.5 , y=60,
       legend = c('ALT', 'NEU', 'EFH', 'GEW'),
       col = c('red','green','blue','orange'), lty=2)


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
