simulated_loads<-read.csv('data/simulated_loads.csv')

substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')

boxplot(simulated_loads, log = 'y',  ylab= 'loads [μg]', outline=FALSE, cex.axis= 1.1, cex.lab= 1.3, col = 3)

max(simulated_loads[,index_substance])
min(simulated_loads[,index_substance])

for (substance in substances) {
  index_substance<- which(substances==substance)
  boxplot(simulated_loads[,index_substance], 
          main = substance,  
          ylab = 'loads [μg]',
          
          outline = FALSE, 
          cex.axis = 1.1, 
          cex.lab = 1.3, 
          col = 3)
  
}
