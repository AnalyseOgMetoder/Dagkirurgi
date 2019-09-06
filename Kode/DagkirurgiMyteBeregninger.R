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
