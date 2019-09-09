setwd("Kode")
source("DagkirurgiFunktioner.R")


sti = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Input\\Liste over SKS koder.csv"

DATA <- udvalgteKoder(sti)
DATA <- DATA %>% select(SKS,Beskrivelse,Speciale,Gruppe,Operation,Kategori,operationsID,patientID,kontaktID,samletktkID,proceduredato,antalSKS,antalP,antalPfuld,antalOP,SKSkombi,primSKS,primBeskrivelse,primSpeciale,sghafd,Sygehus,Afdeling,Afsnit,indmaade,patienttype,medindlSK,inddatoSK,uddatoSK,liggetid,FORliggetid,adiag,dia01,sex,alderAar,sammesghafd,antalsghafd)

DATA <- DATA %>% filter(antalOP == 1, indmaade == 2, liggetid <= 10)

DATA <- DATA %>% mutate(RegistreretPatienttype = ifelse(patienttype == 0,"Indlagt (reg)","Ambulant (reg)"), BeregnetPatienttype = ifelse(liggetid > 0,"Indlagt (beregnet)","Ambulant (beregnet)"))
DATA1 <- DATA %>% mutate(RegistreretPatienttype = ifelse(patienttype == 0,"Indlagt (reg)","Ambulant (reg)"), BeregnetPatienttype = ifelse(liggetid > 0 | FORliggetid > 0,"Indlagt (beregnet)","Ambulant (beregnet)"))



write.csv2(x = DATA, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\ResultaterORG10.csv")
write.csv2(x = DATA1, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\ResultaterORG101.csv")


kodeopslag = read.table(sti, header = TRUE, sep = ";")
kodeopslag <- kodeopslag %>% mutate_all(as.character)

NYDATA <- PDATA %>% filter(SKS %in% kodeopslag$SKS)

# Tilfoej gruppe-, operations- og kategoribeskrivelser
NYDATA <- merge(x = NYDATA, y = kodeopslag, x.all = TRUE)

NYDATA <- NYDATA %>% select(SKS,Beskrivelse,Speciale,Gruppe,Operation,Kategori,operationsID,patientID,kontaktID,samletktkID,proceduredato,antalSKS,antalP,antalPfuld,antalOP,SKSkombi,primSKS,primBeskrivelse,primSpeciale,sghafd,Sygehus,Afdeling,Afsnit,indmaade,patienttype,medindlSK,inddatoSK,uddatoSK,liggetid,FORliggetid,adiag,dia01,sex,alderAar,sammesghafd,antalsghafd)

NYDATA <- NYDATA %>% filter(antalOP == 1, indmaade == 2, liggetid <= 10)

NYDATA <- NYDATA %>% mutate(RegistreretPatienttype = ifelse(patienttype == 0,"Indlagt (reg)","Ambulant (reg)"), BeregnetPatienttype = ifelse(liggetid > 0,"Indlagt (beregnet)","Ambulant (beregnet)"))
NYDATA1 <- NYDATA %>% mutate(RegistreretPatienttype = ifelse(patienttype == 0,"Indlagt (reg)","Ambulant (reg)"), BeregnetPatienttype = ifelse(liggetid > 0 | FORliggetid > 0,"Indlagt (beregnet)","Ambulant (beregnet)"))


write.csv2(x = NYDATA, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\ResultaterORG10_NYMODEL.csv")
write.csv2(x = NYDATA1, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\ResultaterORG10_NYMODEL1.csv")




udvalgteKoder <- function(sti){
  
  # Indlaes udvalgte sks koder
  kodeopslag = read.table(sti, header = TRUE, sep = ";")
  kodeopslag <- kodeopslag %>% mutate_all(as.character)
  
  # Indlaes datatabel med alle OP procedurer
  
  library(dplyr)
  library(dbplyr)
  library(odbc)
  
  # Opretter forbindelse til analyse database
  con <- dbConnect(odbc::odbc(), 
                   Driver = "SQL Server", 
                   Server = "RGHPRODAPP007\\RGHPRODAPP007",
                   Database = "Analyse",
                   encoding = "latin1")
  
  PROCEDUREDATA <- tbl(con, in_schema("Dagkirurgi","ALLE_OP_PROCEDURER")) %>% collect()
  
  dbDisconnect(con)
  
  # Fjern procedurer som ikke er primaer, hvis dette er angivet
  PROCEDUREDATA <- PROCEDUREDATA %>% filter(OPart == "P" | antalPfuld == 0, SKS %in% kodeopslag$SKS)
  
  # Tilfoej gruppe-, operations- og kategoribeskrivelser
  PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = kodeopslag, x.all = TRUE)
  
  # Udtraek data for operationer med flere SKS og sorter efter operationsID og SKS
  FLEREKODERDATA <- PROCEDUREDATA %>% group_by(operationsID) %>% filter(n() > 1) %>% arrange(operationsID, SKS)
  
  # I NYDATA indsaettes sammensatte dataraekker for operationerne i FLEREKODERDATA
  NYDATA <- PROCEDUREDATA[FALSE,]
  
  # Initialisering til loopet
  samletRK <- FLEREKODERDATA[1,]
  prevRK <- samletRK
  SGHVEKTOR <- c(prevRK["sghafd"])
  
  for (r in 2:nrow(FLEREKODERDATA)){
    
    # Ved samme id sammensaettes beskrivelserne, hvis indholdet er forskelligt
    if (FLEREKODERDATA[r,c("operationsID")] == prevRK["operationsID"]){
      
      # Hvis SKS koden ikke er den samme, sammensaettes beskrivelser af SKS og dertilhoerende beskrivelser og kategorier
      if (FLEREKODERDATA[r,c("SKS")] != prevRK["SKS"]){
        samletRK["SKS"] <- paste(samletRK["SKS"], FLEREKODERDATA[r,c("SKS")], sep = "/")
        samletRK["Beskrivelse"] <- paste(samletRK["Beskrivelse"], FLEREKODERDATA[r,c("Beskrivelse")], sep = " / ")
        
        if (FLEREKODERDATA[r,c("Speciale")] != prevRK["Speciale"]){
          samletRK["Speciale"] <- paste(samletRK["Speciale"], FLEREKODERDATA[r,c("Speciale")], sep = " / ")
        }
        
        if (FLEREKODERDATA[r,c("Gruppe")] != prevRK["Gruppe"]){
          samletRK["Gruppe"] <- paste(samletRK["Gruppe"], FLEREKODERDATA[r,c("Gruppe")], sep = "/")
          samletRK["Operation"] <- paste(samletRK["Operation"], FLEREKODERDATA[r,c("Operation")], sep = " / ")
        }
        
        if (FLEREKODERDATA[r,c("Kategori")] != prevRK["Kategori"]){
          samletRK["Kategori"] <- paste(samletRK["Kategori"], FLEREKODERDATA[r,c("Kategori")], sep = " / ")
        }
      }
      
      # Hvis sghafd ikke matcher operationsID'ets forrige shgafd'er sammensaettes beskrivelser af sygehuse og afdelinger
      if (!(FLEREKODERDATA[r,c("sghafd")]) %in% SGHVEKTOR){
        samletRK["sghafd"] <- paste(samletRK["sghafd"], FLEREKODERDATA[r,c("sghafd")], sep = "/")
        samletRK["Afsnit"] <- paste(samletRK["Afsnit"], FLEREKODERDATA[r,c("Afsnit")], sep = " / ")
        
        if (FLEREKODERDATA[r,c("Sygehus")] != prevRK["Sygehus"]){
          samletRK["Sygehus"] <- paste(samletRK["Sygehus"], FLEREKODERDATA[r,c("Sygehus")], sep = " / ")
        }
        if (FLEREKODERDATA[r,c("Afdeling")] != prevRK["Afdeling"]){
          samletRK["Afdeling"] <- paste(samletRK["Afdeling"], FLEREKODERDATA[r,c("Afdeling")], sep = " / ")
        }
        SGHVEKTOR <- append(SGHVEKTOR,FLEREKODERDATA[r,c("sghafd")])
      }
      
      prevRK <- FLEREKODERDATA[r,]
      
    } else {
      
      # Ved nyt id opdateres NYDATA og samletRK, prevRK og SGHVEKTOR initialiseres
      NYDATA <- bind_rows(NYDATA, samletRK)
      samletRK <- FLEREKODERDATA[r,]
      prevRK <- samletRK
      SGHVEKTOR <- c(prevRK["sghafd"])
    }
  }

  # Sidste raekke indsaettes i NYDATA
  NYDATA <- bind_rows(NYDATA, samletRK)
  
  PROCEDUREDATA <- PROCEDUREDATA %>% filter(!(operationsID %in% NYDATA$operationsID))
  
  PROCEDUREDATA <- bind_rows(PROCEDUREDATA, NYDATA)
  
}
