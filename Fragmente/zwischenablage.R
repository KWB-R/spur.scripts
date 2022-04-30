#functions
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

#----------------------------------------------------------------------------------------------------




# explore mean emitted concentrations from BaSaR and SpuR facades

# grab data from both projects BaSaR and SpuR

sites <- c('bbr', 'bbw')
sources<- c('facade', 'roof', 'sewer')


for(place in sites){
  for (my_source in sources) {
    
  assign( paste0('basar_', place, '_', my_source),getMonitoringTable(
    subfolder = 'data_prelim_sources',
    dbName = paste0(place,'_',my_source,'_20200518_conc.txt'),
    dbTable = NA,
    format = 'txt',
    project = 'basar',
    detLimOperator = 0.5,
    site = place,
    source = my_source,
    dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
    tz = 'Etc/GMT-1'))
 }  
}

substances<- c('Diuron', 'Mecoprop', 'Zn')

for(substance in substances){
# make tables for target substances
  
  assign(substance,rbind(makeSubstanceTable(
  dbNames = c('basar_bbr_facade',
              'basar_bbw_facade', 
              'basar_bbr_roof',
              'basar_bbw_roof',
              'basar_bbr_sewer',
              'basar_bbw_sewer'),
  substance = substance)))
}

# remove BBR
Diuron <- Diuron[Diuron$site != 'bbr', ]
# remove BBR (green roof)
Mecoprop <- Mecoprop[Mecoprop$site != 'bbr', ]

# plots
plotSubstance(Zn)
plotSubstance(Diuron)
plotSubstance(Mecoprop)


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


Mecoprop$plotOrder <- plyr::mapvalues(Mecoprop$source,
                                                  from = c('facade', 'roof', 'sewer'), 
                                                  to = c(1, 2, 3))
boxplot(concentration ~ plotOrder, 
        data = Mecoprop, 
        main = 'Mecoprop', 
        xlab = NA , ylab = 'concentration [μg/L]', 
        outline= FALSE,
        xaxt = 'n',
        ylim = c(0, max(Mecoprop$concentration)))

axis(side = 1, 
     at = 1:4, 
     labels = c('Fassade',
                'Dach', 
                'Kanal'),
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

