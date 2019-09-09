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

PDATA <- tbl(con, in_schema("Dagkirurgi","ELEK_P_PROCEDURER")) %>% collect()

PDATA0 <- PDATA %>% filter(liggetid == 0)
OBSTABEL <- PDATA0 %>% group_by(FORliggetid) %>% summarise(antal = n())

PDATA01 <- PDATA %>% filter(liggetid == 0, antalOP == 1)
OBSTABEL1 <- PDATA01 %>% group_by(FORliggetid) %>% summarise(antal = n())

TOPSKS <- PDATA01 %>% filter(FORliggetid > 0) %>% group_by(SKS) %>% mutate(antal = n(),median = median(FORliggetid),mean = mean(FORliggetid)) 

TOPSKS <- TOPSKS %>% select(SKS,Beskrivelse,antal,median,mean) %>% distinct() %>% arrange(desc(antal))

MERGETABLE <- PDATA %>% group_by(SKS) %>% summarise(n = n())

TOPSKS <- merge(x = TOPSKS, y = MERGETABLE, by = "SKS")

TOPSKS <-  TOPSKS %>% mutate(andel = antal / n) %>% select(SKS,Beskrivelse,antal,andel,median,mean) %>% arrange(desc(antal)) %>% top_n(20,antal)

                                                                                                                