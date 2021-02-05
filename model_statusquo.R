OgRe_types <- c("ALT", "EFH", "GEW", "NEU")
substances <- c('Diuron', 'Mecoprop', 'Terbutryn', 'Benzothiazol', 'Zn', 'Cu')
sources <- c("Bitumendach", "Ziegeldach", "Dach_weitere", "Strasse", "Hof", "Putzfassade")


for (substance in substances) {
  
  #substanz auswählen
  
  
  for (OgRe_typ in OgRe_types) {
    
  #Zeilen auswählen die OgRe_typ entsprechen
    
    
    for (my_source in sources) {

      c_current <- eval(parse(text = paste0("c_", OgRe_typ)))
      
      col_Konz <- which(names(c_current) == paste0("Konz_", substance))
      row_Konz <- which(c_current$Source == my_source)
      
      concentration <- c_current[row_Konz, col_Konz]
      
      row_runoff <- which(BTF_input$OgRe_Type == OgRe_typ)
      
      index_source <- which(sources$KLAR == my_source)
      col_runoff <- which(names(BTF_input) == paste0("runoff_", sources))
      
      col_output <- which(names(substance_output) == paste0("load_", sources))
      
      substance_output[row_runoff, col_output] <- concentration * BTF_input[row_runoff, col_runoff]
      
      #Quelle auswählen (im entsprechenden c_ File (1 Zelle) und Abflussfile (1 Spalte))
      #multiplizieren
      
      
    }
  }
  
  #Ergebnis einm neuen data.frame zuweisen
  assign(paste0(substance, "_output"), substance_output)
  
} 