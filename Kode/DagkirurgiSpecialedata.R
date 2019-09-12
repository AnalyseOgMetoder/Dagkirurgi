library(dplyr)
library(dbplyr)
library(odbc)
library(stringr)

setwd("Kode")
source("DagkirurgiFunktioner.R")

# Opretter forbindelse til analyse database
con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "RGHPRODAPP007\\RGHPRODAPP007",
                 Database = "Analyse",
                 encoding = "latin1")


# EXPORTER DATA FOR ALLE SPECIALER OG GEM I TO TABELLER MINDST OG UNDER 10op

PDATA <- tbl(con, in_schema("Dagkirurgi","ELEK_P_PROCEDURER")) %>% collect()

# Omdoeb sygehusnavne. NOH behandles separat pga ae
PDATA$Sygehus <- recode(PDATA$Sygehus,"Amager og Hvidovre Hospital" = "AHH","Bispebjerg og Frederiksberg Hospitaler" = "BFH", "Bornholms Hospital" = "BH", 
  "Herlev og Gentofte Hospital" = "HGH", "Rigshospitalet" = "RH","Steno Diabetes Center" = "STENO","Region Hovedstadens Psykiatri" = "PSYKIATRI")
PDATA[grepl("Hospitalerne i", PDATA$Sygehus), "Sygehus"] <- "NOH"

# Beregn noegletal per sks kode
PDATA <- PDATA %>% group_by(SKS) %>% mutate(SKS_N = n(), KIR = sum(KIRafd == "Kirurgisk afdeling") / SKS_N) %>% ungroup()

TOTALTABEL <- PDATA %>%group_by(SKS) %>%
  mutate(Sygehus = "Total",
         N = SKS_N,
         D = sum(liggetid == 0 & FORliggetid == 0)/N,
         ML = median(liggetid),
         TL = sum(liggetid)) %>% ungroup()

TOTAL_U10 <- TOTALTABEL %>% filter(SKS_N < 10) %>% select(SKS,Beskrivelse,Speciale,Sygehus,KIR,N,D,ML,TL) %>% distinct()
TOTALTABEL <- TOTALTABEL %>% filter(SKS_N >= 10) %>% select(SKS,Beskrivelse,Speciale,Sygehus,KIR,N,D,ML,TL) %>% distinct()

HOSPTABEL <- PDATA %>%group_by(SKS,Sygehus) %>% 
  mutate(N = n(), 
         D = sum(liggetid == 0 & FORliggetid == 0)/N,
         ML = median(liggetid),
         TL = sum(liggetid)) %>% ungroup() 

HOSP_U10 <- HOSPTABEL %>% filter(SKS_N < 10) %>% select(SKS,Beskrivelse,Speciale,Sygehus,KIR,N,D,ML,TL) %>% distinct()
HOSPTABEL <- HOSPTABEL %>% filter(SKS_N >= 10) %>% select(SKS,Beskrivelse,Speciale,Sygehus,KIR,N,D,ML,TL) %>% distinct()

SAMLET_U10 <- rbind(TOTAL_U10,HOSP_U10)
SAMLETTABEL <- rbind(TOTALTABEL,HOSPTABEL)

rm(TOTALTABEL,TOTAL_U10,HOSPTABEL,HOSP_U10)

write.csv2(x = SAMLET_U10, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\Specialeu10DATA.csv")
write.csv2(x = SAMLETTABEL, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\SpecialeDATA.csv")


# EXPORTER DATA FOR OPERATIONER MED UKENDT P

P_FRASORT <- tbl(con, in_schema("Dagkirurgi","ELEK_P_FRASORTERET")) %>% collect()
P_FRASORT <- P_FRASORT %>% filter(FrasortPGA == "Ukendt P")
P_FRASORT <- P_FRASORT %>% mutate(antalP = antalPfuld,primSKS = primSKSfuld,alleSKS = SKSkombi) %>% 
  select(Speciale,operationsID,antalP,primSKS,alleSKS,primBeskrivelse,primSpeciale,Sygehus,liggetid) %>% distinct()

P_FRASORT$Sygehus <- renameSGH(P_FRASORT$Sygehus)

ENKELTSGH <- P_FRASORT %>% group_by(Speciale,operationsID) %>% filter(n()==1) %>% ungroup()
DOBBELTSGH <- P_FRASORT %>% group_by(Speciale,operationsID) %>% filter(n()>1) %>% ungroup() %>% arrange(operationsID, Sygehus)
colorder <- names(DOBBELTSGH)

Opslag <- DOBBELTSGH %>% distinct(operationsID) %>% mutate(Sygehus = "") %>% arrange(operationsID)

# Loop gennemgaar alle procedurer i PROCflereSKS og tilfoejer sks til primaer-beskrivelserne i OPtabel hvis de er P eller antalPrim = 0
prevID <- 0
oprow <- 0
for (r in 1:nrow(DOBBELTSGH)){
  
  id <- as.character(DOBBELTSGH[r,c("operationsID")])
  sgh <- as.character(DOBBELTSGH[r,c("Sygehus")])
  
  if (id != prevID) {
    oprow <- oprow + 1
    sepSGH <- ""
  }
  
  if (as.character(Opslag[oprow,1]) != id) {break}
 
  Opslag[oprow,2] <- paste(Opslag[oprow,2],sgh,sep = sepSGH)
  
  sepSGH = " / "
  prevID <- id

}

DOBBELTSGH <- DOBBELTSGH %>% select(-c("Sygehus")) %>% distinct()
DOBBELTSGH <- merge(x = DOBBELTSGH, y = Opslag, by = "operationsID")
DOBBELTSGH <- DOBBELTSGH %>% select(colorder)

P_FRASORT <- rbind(ENKELTSGH,DOBBELTSGH)

rm(ENKELTSGH,DOBBELTSGH,Opslag)

write.csv2(x = P_FRASORT, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\SpecialeFRASORT.csv")



# OPTAELLING AF DATA FILTRERINGER FOR ALLE SPECIALER

ALLE_DATA <- tbl(con, in_schema("Dagkirurgi","ALLE_OP_PROCEDURER")) %>% collect()
ELEK_DATA <- ALLE_DATA %>% filter(indmaade == 2)
P_DATA <- tbl(con, in_schema("Dagkirurgi","ELEK_P_PROCEDURER")) %>% collect()
N_DATA <- P_DATA %>% group_by(SKS) %>% filter(n() >= 10) %>% ungroup()

specialeliste <- ALLE_DATA %>% distinct(Speciale)

N <- 4*nrow(specialeliste)+4
HOVEDTAL <- data.frame(Speciale=rep("", N),FilterNIV=rep("", N),antalSKS=rep(NA, N), antalOP=rep(NA, N),antalPROC=rep(NA, N),stringsAsFactors=FALSE)

HOVEDTAL[1,] <- c("Total","Alle",n_distinct(ALLE_DATA$SKS),n_distinct(ALLE_DATA$operationsID),nrow(ALLE_DATA))
HOVEDTAL[2,] <- c("Total","Elektive",n_distinct(ELEK_DATA$SKS),n_distinct(ELEK_DATA$operationsID),nrow(ELEK_DATA))
HOVEDTAL[3,] <- c("Total","Prim?re",n_distinct(P_DATA$SKS),n_distinct(P_DATA$operationsID),nrow(P_DATA))
HOVEDTAL[4,] <- c("Total","Mindst10",n_distinct(N_DATA$SKS),n_distinct(N_DATA$operationsID),nrow(N_DATA))

r <- 5
for (i in 1:nrow(specialeliste)){
  
  speciale <- as.character(specialeliste[i,"Speciale"])
  
  SPEC1 <- ALLE_DATA %>% filter(Speciale == speciale)
  SPEC2 <- ELEK_DATA %>% filter(Speciale == speciale)
  SPEC3 <- P_DATA %>% filter(Speciale == speciale)
  SPEC4 <- N_DATA %>% filter(Speciale == speciale)
  
  HOVEDTAL[r,] <- c(speciale,"Alle",n_distinct(SPEC1$SKS),n_distinct(SPEC1$operationsID),nrow(SPEC1))
  HOVEDTAL[r+1,] <- c(speciale,"Elektive",n_distinct(SPEC2$SKS),n_distinct(SPEC2$operationsID),nrow(SPEC2))
  HOVEDTAL[r+2,] <- c(speciale,"Prim?re",n_distinct(SPEC3$SKS),n_distinct(SPEC3$operationsID),nrow(SPEC3))
  HOVEDTAL[r+3,] <- c(speciale,"Mindst10",n_distinct(SPEC4$SKS),n_distinct(SPEC4$operationsID),nrow(SPEC4))
  r <- r + 4

}

rm(SPEC1,SPEC2,SPEC3,SPEC4,specialeliste,i,r,N)

write.csv2(x = HOVEDTAL, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\SpecialeHOVEDTAL.csv")



# HOVEDTAL TIL UKENDT P BEREGNINGER

ELEK_DATA %>% filter(antalSKS == 1 & antalPfuld == 0) %>% count()
ELEK_DATA %>% filter(antalSKS == 1 & antalPfuld == 1) %>% count()

ELEK_DATA %>% filter(antalSKS > 1 & antalPfuld == 0) %>% select(operationsID) %>% distinct() %>% count()
ELEK_DATA %>% filter(antalSKS > 1 & antalPfuld == 1) %>% select(operationsID) %>% distinct() %>% count()
ELEK_DATA %>% filter(antalSKS > 1 & antalPfuld > 1) %>% select(operationsID) %>% distinct() %>% count()

ELEK_DATA %>% filter(antalSKS == 1 & antalP == 1) %>% count()
ELEK_DATA %>% filter(antalSKS > 1 & antalP == 1) %>% select(operationsID) %>% distinct() %>% count()
ELEK_DATA %>% filter(antalSKS > 1 & antalP > 1) %>% select(operationsID) %>% distinct() %>% count()


dbDisconnect(con)