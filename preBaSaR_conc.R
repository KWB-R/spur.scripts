# explore mean emitted concentrations from BaSaR and SpuR facades

# grab data from both projects
basar_bbr <- getMonitoringTable(
  rawdir = 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/dataBaSaR/',
  dbName = 'BBRf_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbr',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

basar_bbw <- getMonitoringTable(
  rawdir = 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/dataBaSaR/',
  dbName = 'BBWf_20200518_conc.txt',
  dbTable = NA,
  format = 'txt',
  project = 'basar',
  detLimOperator = 0.5,
  site = 'bbw',
  dateTimeFormat = '%Y-%m-%d %H:%M:%S', 
  tz = 'Etc/GMT-1')

spur_pkw <- getMonitoringTable(
  rawdir = 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP2_Regenfilter/Monitoring/_Daten/',
  dbName = 'Felddatenbank_SpuR_Pankow.xlsx',
  dbTable = 'Felddatenbank',
  format = 'xls',
  project = 'spur',
  detLimOperator = 0.5,
  site = 'pkw',
  dateTimeFormat = '%d.%m.%Y %H:%M', 
  tz = 'Etc/GMT-1')

# make tables for target substances based on these data
Zn <- rbind(makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                               substance = 'Zn_total'),
            makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                               substance = 'Zn')) 
Cu <- rbind(makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                               substance = 'Cu_total'),
            makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                               substance = 'Cu')) 
Diuron <- makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                             substance = 'Diuron')
Diuron <- makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                             substance = 'Diuron')
Terbutryn <- makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                                substance = 'Terbutryn')
Terbutryn_desethyl <- makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                                substance = 'Terbutryn_desethyl')
Terbutryn_2_hydroxy <- rbind(makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                                                substance = 'Terbutryn.2.hydroxy'),
                             makeSubstanceTable(dbNames = c('basar_bbr', 'basar_bbw', 'spur_pkw'),
                                                substance = 'Terbutryn-2-hydroxy'))

# keep low-concentration facades from basar (closer to real situation with old facades)
# benzothiazol spur, mecroprop basar


# plot
plotSubstance(Zn)
plotSubstance(Cu)
plotSubstance(Diuron)
plotSubstance(Terbutryn)
plotSubstance(Terbutryn_desethyl) 
plotSubstance(Terbutryn_2_hydroxy)

#'Zn', 'Cu', 'Diuron', 'Isoproturon', 
# 'Terbutryn',  'Terbumeton', 
#'Benzothiazol', 'OIT'
                                 
                                 
# functions ---------------------------------------------------------------------------------
getMonitoringTable <- function(rawdir, dbName, dbTable, 
                               format, project, site, 
                               detLimOperator, dateTimeFormat, 
                               tz){

  library(dplyr)
  
  setwd(rawdir)
  
  if(format == 'txt'){
    
    db <- read.table(dbName, header = TRUE, sep = ";", colClasses = 'character')
    
  } else if (format == 'xls'){
    db <- as.data.frame(readxl::read_excel(dbName, 
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
  
  # grab targeted substances and accompanying event data
  index <- grepl(pattern = paste('tBegRain', 'tEndRain','Regenhöhe_mm',
                                 'Anzahl_Ereignisse',
                                 'Lufttemperatur', 'Wind',
                                 'Zn', 'Cu', 'Diuron', 'Isoproturon', 
                                 'Mecoprop', 'Terbutryn',  'Terbumeton', 
                                 'Benzothiazol', 'OIT', 'site', 'project' ,
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
              "Wind_v_mean_m_s", "Wind_v_sd_m_s", "Windricht_grad_mean", 
              "Windricht_grad_se", "site", "project", "substance", "concentration")
  
  for(dbi in dbNames){
    dat <- get(dbi)
    dat <- dat[dat$substance == substance,
               grepl(pattern = paste('tBegRain', 'tEndRain', 'Regenhöhe',
                                     'Anzahl_Ereig', 'Wind', 'site', 'project',
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
