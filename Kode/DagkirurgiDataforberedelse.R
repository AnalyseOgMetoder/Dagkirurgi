# Kode til at hente og skrive tabeller i Dagkirurgi skemaet
#data <- tbl(con, in_schema("Dagkirurgi","ALLE_OP_PROCEDURER")) %>% collect()
#dbWriteTable(con, Id(schema = "Dagkirurgi", table = "ALLE_OP_PROCEDURER"), data)
#dbRemoveTable(con, Id(schema = "Dagkirurgi", table = "ALLE_OP_PROCEDURER"))

library(dplyr)
library(dbplyr)
library(odbc)
library(stringr)

setwd("Kode")
source("DagkirurgiFunktioner.R")
source("SK funktionen.R")


# Opretter forbindelse til analyse database
con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "RGHPRODAPP007\\RGHPRODAPP007",
                 Database = "Analyse",
                 encoding = "latin1")

PROCBACKUP <- tbl(con, in_schema("Dagkirurgi","ALLE_OP_PROCEDURER")) %>% collect()
ELEKBACKUP <- tbl(con, in_schema("Dagkirurgi","ELEK_P_PROCEDURER")) %>% collect()

# Hent alle operationsprocedurer
PROCEDUREDATA <- tbl(con,"V_DRG2018_DRGPROCEDURER_levDato20190327") %>% 
  filter(PROCEDUREKODE %like% "K%") %>%
  select(PROCEDUREKODE, DRGKONTAKT_ID, PROCEDURE_DATO, PRODUCERENDE_SGHAFDELING, OPERATIONSART) %>% 
  rename(SKS = PROCEDUREKODE, kontaktID = DRGKONTAKT_ID, proceduredato = PROCEDURE_DATO,
         sghafd = PRODUCERENDE_SGHAFDELING, OPart = OPERATIONSART) %>% 
  collect()

# Hent alle kontakter med mindst en operationsprocedure
KONTAKTDATA <- tbl(con,"V_DRG2018_DRGKONTAKTER_levDato20190327") %>% 
  select(Patient_RK, ID, INDMAADE, PATIENTTYPE, ADIAG, DIA01, KOEN, ALDER_AAR, ALDER_DAGE, BO_KOM, BO_REGION, AMBULANT_DATO, 
         INDDATO, INDTIME, INDTIDSPUNKT_DRGKONTAKT, UDDATO, UDTIME, UDTIDSPUNKT_DRGKONTAKT, SYGEHUS, AFDELING) %>% 
  rename(patientID = Patient_RK, kontaktID = ID, indmaade = INDMAADE, patienttype = PATIENTTYPE, adiag = ADIAG, dia01 = DIA01,
         sex = KOEN, alderAar = ALDER_AAR, alderDage = ALDER_DAGE, kommune = BO_KOM, region = BO_REGION, ambdato = AMBULANT_DATO, 
         inddatoKTK = INDDATO, indtimeKTK = INDTIME, indtidDRGKTK = INDTIDSPUNKT_DRGKONTAKT, uddatoKTK = UDDATO, udtimeKTK = UDTIME, 
         udtidDRGKTK = UDTIDSPUNKT_DRGKONTAKT, sygehusKTK = SYGEHUS, afdelingKTK = AFDELING) %>% collect()

OPkontaktliste <- PROCEDUREDATA %>% select(kontaktID) %>% distinct()
KONTAKTDATA <- KONTAKTDATA %>% filter(kontaktID %in% OPkontaktliste$kontaktID)

# Flet proceduredata med kontaktinformation
PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = KONTAKTDATA, by = "kontaktID")

# Fjern procedurer der ikke er udfoert i 2018
PROCEDUREDATA <- PROCEDUREDATA %>% filter(format(as.Date(proceduredato, origin = "1960-01-01"),"%Y") == 2018)

# Opret kolonne med operationsID = kontaktID+proceduredato saa procedurer foretaget paa samme op kan kobles
PROCEDUREDATA <- PROCEDUREDATA %>% mutate(operationsID = paste0(kontaktID,proceduredato))

# Erstat NA med "" i OPart og tilfoej kolonner med antal koder og antal P koder
PROCEDUREDATA <- PROCEDUREDATA %>% group_by(operationsID) %>% mutate(OPart = ifelse(!is.na(OPart),OPart,""), antalSKS = n(), antalPfuld = sum(OPart == "P")) %>% ungroup()

# Tael antal forskellige P sks til operationenen (eller antal tomme hvis ingen P)
antalPtabel <- PROCEDUREDATA %>% filter(OPart == "P" | antalPfuld == 0) %>% distinct(operationsID,SKS)
antalPtabel <- antalPtabel%>% group_by(operationsID) %>% mutate(antalP = n()) %>% ungroup()
antalPtabel <- antalPtabel %>% select(operationsID,antalP) %>% distinct()
PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = antalPtabel, by = "operationsID")

# Tilfoej procedure- og sygehusbeskrivelser og KIRafd
procedureopslag = read.table("L:\\LovbeskyttetMapper\\AnalyseKoder\\Opslag_proceduretabel.csv", header = TRUE, sep=";")
sygehusopslag = read.table("L:\\LovbeskyttetMapper\\AnalyseKoder\\Opslag_sygehustabel.csv", header = TRUE, sep=";")
kiropslag <- read.table("L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Input\\KirAfdListe.csv", header = TRUE, sep = ";")

procedureopslag <- procedureopslag %>% select(SKS,Beskrivelse,Speciale) %>% filter(SKS %in% PROCEDUREDATA$SKS)
sygehusopslag <- sygehusopslag %>% select(Afdelingskode,Sygehus,Afdeling,Afsnit) %>% rename(sghafd = Afdelingskode) %>% filter(sghafd %in% PROCEDUREDATA$sghafd)
kiropslag <- kiropslag %>% select(sghafd,KIRafd) %>% mutate_all(as.character)

PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = procedureopslag, by = "SKS", x.all = TRUE)
PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = sygehusopslag, by = "sghafd", x.all = TRUE)
PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = kiropslag, by = "sghafd", x.all = TRUE)

remove(OPkontaktliste,antalPtabel,procedureopslag,sygehusopslag,kiropslag,KONTAKTDATA)

# Tilfoej primaer beskrivelser - del op for en eller flere sks for at lette koerslen for komplicerede operationer
PROC1SKS <- PROCEDUREDATA %>% filter(antalSKS == 1) %>% mutate(SKSkombi = SKS, primSKS = SKS, primSKSfuld = SKS, primBeskrivelse = Beskrivelse, primSpeciale = Speciale)

PROCflereSKS <- PROCEDUREDATA %>% filter(antalSKS > 1) %>% arrange(operationsID, SKS)

# Opret opslagstabel med alle operationsid til information om SKS kombi, primaer SKS, antal SKS og antal primaer SKS
OPtabel <- PROCflereSKS %>% distinct(operationsID) %>% mutate(SKSkombi = "", primSKS = "", primSKSfuld = "", primBeskrivelse = "", primSpeciale = "") %>% arrange(operationsID)

# Loop gennemgaer alle procedurer i PROCflereSKS og tilfoejer sks til prim?r-beskrivelserne i OPtabel hvis de er P eller antalPrim = 0
prevID <- 0
oprow <- 0
for (r in 1:nrow(PROCflereSKS)){
  
  id <- as.character(PROCflereSKS[r,c("operationsID")])
  sks <- as.character(PROCflereSKS[r,c("SKS")])
  opart <- as.character(PROCflereSKS[r,c("OPart")])
  antalP <- as.numeric(PROCflereSKS[r,c("antalPfuld")])
  
  if (id != prevID) {
    oprow <- oprow + 1
    prevSKS <- ""
    prevBeskrivelse <- ""
    prevSpeciale <- ""
    sepSKS <- ""
  }
  
  if (as.character(OPtabel[oprow,1]) != id) {break}
  
  OPtabel[oprow,2] <- paste(OPtabel[oprow,2],sks,sep = sepSKS)
  
  if (opart == "P" | antalP == 0){
    
    if (OPtabel[oprow,4] == ""){ 
      sepSKS <- ""
      sepSKSspace <- ""
    }
    
    OPtabel[oprow,4] <- paste(OPtabel[oprow,4],sks,sep = sepSKS)
    
    beskrivelse <- as.character(PROCflereSKS[r,c("Beskrivelse")])
    speciale <- as.character(PROCflereSKS[r,c("Speciale")])
    
    if (sks != prevSKS) {OPtabel[oprow,3] <- paste(OPtabel[oprow,3],sks,sep = sepSKS)}
    if (beskrivelse != prevBeskrivelse) {OPtabel[oprow,5] <- paste(OPtabel[oprow,5],beskrivelse,sep = sepSKSspace)}
    if (speciale != prevSpeciale) {OPtabel[oprow,6] <- paste(OPtabel[oprow,6],speciale,sep = sepSKSspace)}
    
    prevSKS <- sks
    prevBeskrivelse <- beskrivelse
    prevSpeciale <- speciale
    
    sepSKS = "/"
    sepSKSspace = " / "
    
  }
  
  prevID <- id
  sepSKS <- "/"
}

# Flet sks beskrivelser paa PROCEDUREDATA
PROCflereSKS <- merge(x = PROCflereSKS, y = OPtabel, by = "operationsID", x.all = TRUE)
PROCflereSKS <- PROCflereSKS %>% select(names(PROC1SKS))

PROCEDUREDATA <- bind_rows(PROC1SKS,PROCflereSKS)

remove(prevID,prevSKS,prevBeskrivelse,prevSpeciale,oprow,r,id,sks,beskrivelse,speciale,opart,antalP,sepSKS,sepSKSspace,OPtabel,PROCflereSKS,PROC1SKS)


# BEREGN DATA FOR SAMLET KONTAKT SK - HUSK AT INDLAESE FUNKTIONEN

# Hent relevante kolonner for alle kontakter
KTKDATA <- dbGetQuery(con, "SELECT ID as raekkeID, Patient_RK as id, INDDATO_DATO as inddato, UDDATO_DATO as uddato,
                            AMBULANT_DATO_DATO as ambdato, SYGEHUS as sgh, PATIENTTYPE as patienttype, AKUT_ELEKTIV as ambElektiv  
                            FROM dbo.V_DRG2018_DRGKONTAKTER_levDato20190327
                            ")

# Filtrer paa aktuelle patienter
kontaktliste <- PROCEDUREDATA %>% select(patientID) %>% distinct()
KTKDATA <- KTKDATA %>% filter(id %in% kontaktliste$patientID)

# Beregn inddato, uddato og indl ud fra patienttype og ambulant dato
KTKDATA <- KTKDATA %>% mutate(startDato = as.Date(ifelse(ambElektiv == 1,ambdato,inddato),format = "%Y-%m-%d"),
                              slutDato = as.Date(ifelse(ambElektiv == 1,ambdato,uddato),format = "%Y-%m-%d"),
                              indl = ifelse(patienttype == 0,1,0))

# Sorter efter id, startDato, slutDato og sgh 
KTKDATA <- KTKDATA %>% arrange(id,startDato,slutDato,sgh)

# Beregn SK data og flet paa PROCEDUREDATA
SKdata <- skFunktion(datasaet = KTKDATA, tilladteDageMellemKontakter=NA, skiftVedSygehus = TRUE, skNavn = "")
SKdata <- SKdata %>% rename(kontaktID = raekkeID, samletktkID = SkPID, inddatoSK = SkStartDato, uddatoSK = SkSlutDato, medindlSK = SkMedIndl)

PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = SKdata, by = "kontaktID")
remove(kontaktliste,KTKDATA,SKdata)

# Gem datoer som datoer
PROCEDUREDATA <- PROCEDUREDATA %>% mutate_at(vars(inddatoSK,uddatoSK,proceduredato,ambdato,inddatoKTK,uddatoKTK),list(~as.Date(.,origin = "1960-01-01")))

# Beregn liggetider
PROCEDUREDATA <- PROCEDUREDATA %>% mutate(liggetid = as.numeric(uddatoSK - proceduredato), FORliggetid = as.numeric(proceduredato - inddatoSK))

# Beregn antal OP per samlet kontakt
PROCEDUREDATA <- PROCEDUREDATA %>% group_by(samletktkID) %>% mutate(antalOP = n_distinct(operationsID)) %>% ungroup()

# Kolonner til datavalidering: om producerende afd og ktk afd matcher, og antal forskellige producerende sghafd til samme operationsID
PROCEDUREDATA <- PROCEDUREDATA %>% mutate(sammesghafd = ifelse(paste0(sygehusKTK,afdelingKTK,sep="")==sghafd,1,0))
PROCEDUREDATA <- PROCEDUREDATA %>% group_by(operationsID) %>% mutate(antalsghafd = n_distinct(sghafd)) %>% ungroup()

Pafd <- PROCEDUREDATA %>% filter(OPart == "P" | antalPfuld == 0) %>% group_by(operationsID) %>% summarise(antalsghafdP0 = n_distinct(sghafd))
PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = Pafd, by = "operationsID")

# Juster kolonneraekkefoelge
colOrder <- c("operationsID","patientID","kontaktID","samletktkID","SKS","Beskrivelse","Speciale","OPart","proceduredato","antalSKS",
              "antalP","antalPfuld","antalOP","SKSkombi","primSKS","primSKSfuld","primBeskrivelse","primSpeciale","KIRafd",
              "sghafd","Sygehus","Afdeling","Afsnit","indmaade","patienttype","medindlSK","inddatoSK","uddatoSK","liggetid","FORliggetid",
              "adiag","dia01","sex","alderAar","alderDage","kommune","region","ambdato","inddatoKTK","indtimeKTK","indtidDRGKTK",
              "uddatoKTK","udtimeKTK","udtidDRGKTK","sygehusKTK","afdelingKTK","sammesghafd","antalsghafd","antalsghafdP0")
PROCEDUREDATA <- PROCEDUREDATA %>% select(colOrder)

# Tilfoej anonymiseret ID til patientID, kontaktID og operationsID
PROCEDUREDATA <- PROCEDUREDATA %>% rename(opIDorg = operationsID, ptIDorg = patientID, ktkIDorg = kontaktID)

# Beregn anonymiserede id
operationsIDliste <- generereId(PROCEDUREDATA$opIDorg) %>% rename(opIDorg = idOrg, operationsID = id)
patientIDliste <- generereId(PROCEDUREDATA$ptIDorg) %>% rename(ptIDorg = idOrg, patientID = id)
kontaktIDliste <- generereId(PROCEDUREDATA$ktkIDorg) %>% rename(ktkIDorg = idOrg, kontaktID = id)

# Hent id-noegler fra databasen
operationsIDliste <- tbl(con, in_schema("Dagkirurgi","OP_ID")) %>% collect()
patientIDliste <- tbl(con, in_schema("Dagkirurgi","PT_ID")) %>% collect()
kontaktIDliste <- tbl(con, in_schema("Dagkirurgi","KTK_ID")) %>% collect()

PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = operationsIDliste, by = "opIDorg", x.all = TRUE)
PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = patientIDliste, by = "ptIDorg", x.all = TRUE)
PROCEDUREDATA <- merge(x = PROCEDUREDATA, y = kontaktIDliste, by = "ktkIDorg", x.all = TRUE)


# Juster kolonneraekkefoelge og fjern de oprindelige ID kolonner
PROCEDUREDATA <- PROCEDUREDATA %>% select(colOrder)

remove(kontaktIDliste,operationsIDliste,patientIDliste)

saveData(PROCEDUREDATA,"Dagkirurgi","ALLE_OP_PROCEDURER",overwrite=TRUE)

dbDisconnect(con)



# P ELEK DATA

# Kun elektive procedurer som er entydigt primaere, eller med samme sks
PDATA <- PROCEDUREDATA %>% filter(indmaade == 2,antalP == 1,OPart == "P" | antalPfuld == 0)

DUBLETTER1 <- PDATA[duplicated(PDATA),]
PDATA <- PDATA[!duplicated(PDATA),]

# PDATA1 med entydige producerende afdelinger
PDATA1 <- PDATA %>% filter(antalsghafdP0 == 1)

# PDATA2 med forskellige producerende afdelinger (men samme sks)
PDATA2 <- PDATA %>% filter(antalsghafdP0 > 1)

# Indlaes opslagstabel med valg af sghafd
#afdopslag <- read.table("L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Input\\DobbeltSKSafdvalg.csv", header = TRUE, sep = ";")
#saveData(afdopslag,"Dagkirurgi","AFD_VALG",overwrite=TRUE)
afdopslag <- tbl(con, in_schema("Dagkirurgi","AFD_VALG")) %>% collect()

PDATA2 <- merge(x = PDATA2, y = afdopslag, by = "operationsID")

# Frasorter de oevrige procedurer
DUBLETTER2 <- PDATA2 %>% filter(sghafd != afdvalg) %>% select(-c(afdvalg))
PDATA2 <- PDATA2 %>% filter(sghafd == afdvalg) %>% select(-c(afdvalg))

#Flet data sammen og gem i databasen
DUBLETTER <- rbind(DUBLETTER1,DUBLETTER2) %>% mutate(FrasortPGA = "Dublet")
#saveData(PDATA,"Dagkirurgi","ELEK_P_FRASORTERET",overwrite=TRUE)

PDATA <- rbind(PDATA1,PDATA2)
saveData(PDATA,"Dagkirurgi","ELEK_P_PROCEDURER",overwrite=TRUE)

# Gem frasorterede procedurer i seperat tabel
UKENDTP <- PROCEDUREDATA %>% filter(indmaade == 2, antalP != 1, OPart == "P" | antalPfuld == 0) %>% mutate(FrasortPGA = "Ukendt P")
FRASORT <- rbind(DUBLETTER,UKENDTP)
saveData(FRASORT,"Dagkirurgi","ELEK_P_FRASORTERET",overwrite=TRUE)
rm(DUBLETTER1,DUBLETTER2,PDATA1,PDATA2,DUBLETTER,UKENDTP)



# Oversigt over alle producerende afdelinger og hvor mange operationer de har udfoert med 0 og med flere liggedage
dkafdopslag <- PROCEDUREDATA %>% group_by(sghafd,SKS) %>% mutate(Total = n(),under1dag = sum(liggetid == 0), mindst1dag = sum(liggetid>0),Akut = sum(indmaade == 1),Elektiv = sum(indmaade == 2)) %>% select(sghafd,Sygehus,Afdeling,Afsnit,SKS,Beskrivelse,Speciale,Total,under1dag,mindst1dag,Akut,Elektiv) %>% distinct()
write.csv2(x = dkafdopslag, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\KirurgiskeAfdelinger.csv")

# Problemdata: hvilken afdeling skal taelle?
DATA <- ELEKDATA %>% filter(antalSKSprim == 1, antalsghafdP0 > 1,OPart == "P" | antalSKSprimfuld == 0)
write.csv2(x = DATA, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Dobbeltafdelinger.csv")

# UDTRAEK TIL MARIANNE
PROCEDUREDATA <- PROCEDUREDATA %>% filter(OPart == "P" | antalP == 0)
PROCEDUREDATA <- PROCEDUREDATA[,c(1:32,45,46,48,49)]

write.csv2(x = PROCEDUREDATA, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\DataMarianne.csv")


