# Henter data fra Analysedatabasen i schemaname.tablename
getData <- function(schemaname, tablename){
  
  library(dplyr)
  library(stringr)
  library(dbplyr)
  library(odbc)
  
  # Opretter forbindelse til analyse database
  con <- dbConnect(odbc::odbc(), 
                   Driver = "SQL Server", 
                   Server = "RGHPRODAPP007\\RGHPRODAPP007",
                   Database = "Analyse",
                   encoding = "latin1")
  
  # EXPORTER DATA FOR ALLE SPECIALER OG GEM I TO TABELLER MINDST OG UNDER 10op
  PDATA <- tbl(con, in_schema(schemaname,tablename)) %>% collect()
  
  # Afbryd forbindelsen
  dbDisconnect(con)
  remove(con)
  
  return(PDATA)
  
}



# Gemmer data i Analysedatabasen i schemaname.tablename
saveData <- function(data,schemaname, tablename, overwrite = FALSE){
  
  library(dplyr)
  library(stringr)
  library(DBI)
  library(dbplyr)
  library(odbc)

  # Opretter forbindelse til analyse database
  con <- dbConnect(odbc::odbc(), 
                   Driver = "SQL Server", 
                   Server = "RGHPRODAPP007\\RGHPRODAPP007",
                   Database = "Analyse",
                   encoding = "latin1")
  
  # Tjek om tabellen allerede findes
  if (dbExistsTable(con, Id(schema = schemaname, table = tablename))) {
    
    # Hvis tabellen skal overskrives
    if (overwrite) {
      
      backuptablename = paste0(tablename,"_BACKUP")
      
      print("gemmer backup")
      dbWriteTable(con, Id(schema = schemaname, table = backuptablename), data)
      
      print("sletter tabel")
      dbRemoveTable(con, Id(schema = schemaname, table = tablename))
      
      print("gemmer tabel")
      dbWriteTable(con, Id(schema = schemaname, table = tablename), data)
      
      print("sletter backup")
      dbRemoveTable(con, Id(schema = schemaname, table = backuptablename))
      
      remove(backuptablename)
      
    } else { print("Tabel findes allerede. Vaelg overwrite = TRUE hvis den skal overskrives.") }
    
  } else {
    print("gemmer ny tabel")
    dbWriteTable(con, Id(schema = schemaname, table = tablename), data)
  }
  
  # Afbryd forbindelsen
  dbDisconnect(con)
  remove(con)
}

# Generere anonymt id og returnere noegleliste.
generereId<-function(idListe){
  
  # Tjekker om idListen er en dataframe
  if("data.frame" %in% class(idListe)){
    
    # Tjekker om dataframe kun indeholder en kolonne som den skal
    if(ncol(idListe)==1){
      
      # Aendrer navnet paa den ene kolonne, saa det passer med koden 
      idListeMdl<-idListe
      colnames(idListeMdl)<-"idOrg"
      
    }else{
      
      stop("Der findes flere kolonner i idListen. Input maa kun indeholde en variabel.")
      
    }
    
  }else{
    
    # Danner et datasaet med inputlisten som en character variabel med kun unikke vaerdier
    idListeMdl<-data.frame("idOrg"=as.character(unique(idListe)), stringsAsFactors = FALSE)
    
  }
  
  # Nummererer hvert raekke/vaerdi i inputslisten
  mdl<-as.numeric(as.factor(as.character(idListeMdl$idOrg)))
  
  # Blander numrene x antal gange
  for(i in 1:sample(1:100,1)){
    mdl<-sample(mdl,length(mdl))
  }
  
  # Saetter de nye id'er i datasaettet
  idListeMdl$id<-paste0(sample(rep(LETTERS,length(mdl)),length(mdl)),mdl)
  
  if(length(unique(idListeMdl$idOrg))!=length(unique(idListeMdl$id))){
    print("Noget er gaaet galt!")
    stop("Fejl!")
  }
  
  return(idListeMdl)
  
}