filter_efficiencies <- data.frame(matrix( data= NA, nrow=1, ncol = 6))
colnames(filter_efficiencies) <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
filter_efficiencies[1,] <- c(0.97, 0.85, 0.93, 0.94, 0.93, 0.89)

          
write.csv(filter_efficiencies, 'data/filter_efficiencies.csv', row.names = FALSE)
