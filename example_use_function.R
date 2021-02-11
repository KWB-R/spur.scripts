
source(file = "SpuR_functions_klaas.R")

substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
data.dir <- "data/"

x <- SpuR_statusquo_model(substances = substances, data.dir = data.dir)
