library(dplyr)
library(dbplyr)
library(odbc)
library(stringr)
library(tidyverse)

# Opretter forbindelse til analyse database
con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "RGHPRODAPP007\\RGHPRODAPP007",
                 Database = "Analyse",
                 encoding = "latin1")

PROCEDUREDATA <- tbl(con, in_schema("Dagkirurgi","ALLE_OP_PROCEDURER")) %>% collect()
PDATA <- tbl(con, in_schema("Dagkirurgi","ELEK_P_PROCEDURER")) %>% collect()

# Beregn dagkir andel mm.
PDATA <- PDATA %>% group_by(SKS) %>% 
  mutate(SKS_N = n(),
         SKS_KIRantal = sum(KIRafd == "Kirurgisk afdeling"),
         SKS_KIRandel = 100 * SKS_KIRantal/SKS_N,
         SKS_MedLiggetid = median(liggetid),
         SKS_TotalLiggetid = sum(liggetid),
         SKS_DagKIRantal = sum(liggetid == 0),
         SKS_DagKIRandel = 100 * SKS_DagKIRantal/SKS_N
         )

# Plot potentialer
PLOTDATA <- PDATA %>% select(SKS,SKS_N,SKS_KIRandel,SKS_MedLiggetid,SKS_DagKIRandel) %>% distinct() %>% filter(SKS_N >= 50, SKS_KIRandel > 50, SKS_MedLiggetid > 0)
ggplot(data = PLOTDATA) + geom_point(mapping = aes(x = SKS_MedLiggetid, y = SKS_DagKIRandel, size = SKS_N), position = "jitter")




# SKRIV FILER TIL KATEGORISERING

# Hvilke operationskoder er reelle operationskoder?
SKSTABEL <- PROCEDUREDATA %>% 
  group_by(SKS) %>% 
  summarize(AntalKIR = sum(KIRafd == "Kirurgisk afdeling"), AntalikkeKIR = sum(KIRafd == "Ikke kirurgisk")) %>% 
  mutate(andelKIR = 100 * AntalKIR / (AntalKIR + AntalikkeKIR)) %>% 
  arrange(desc(andelKIR),desc(AntalKIR),desc(AntalikkeKIR))

procedureopslag = read.table("L:\\LovbeskyttetMapper\\AnalyseKoder\\Opslag_proceduretabel.csv", header = TRUE, sep=";")
procedureopslag <- procedureopslag %>% select(SKS,Beskrivelse,Speciale) %>% filter(SKS %in% PROCEDUREDATA$SKS)

SKSTABEL <- merge(x = SKSTABEL, y = procedureopslag, by = "SKS", x.all = TRUE) %>% arrange(desc(andelKIR),desc(AntalKIR),desc(AntalikkeKIR))

write.csv2(x = SKSTABEL, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Operationskoder.csv")


# Hvilke afdelinger er dagkirurgiske?
AFDTABEL <- PROCEDUREDATA %>% 
  filter(KIRafd == "Kirurgisk afdeling") %>% 
  group_by(sghafd,Sygehus,Afdeling,Afsnit,SKS) %>% 
  summarize(antal = n(),median_liggetid = median(liggetid)) %>% 
  top_n(10,antal) %>% 
  arrange(sghafd,desc(antal),desc(median_liggetid))

AFDTABEL <- merge(x = AFDTABEL, y = procedureopslag, by = "SKS", x.all = TRUE) %>%
  select(sghafd,Sygehus,Afdeling,Afsnit,SKS,Beskrivelse,Speciale,antal,median_liggetid) %>% 
  arrange(sghafd,desc(antal),desc(median_liggetid))

write.csv2(x = AFDTABEL, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\DagkirAfdelinger.csv")
