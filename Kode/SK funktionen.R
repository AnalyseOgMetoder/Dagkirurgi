skFunktion<-function(datasaet,tilladteDageMellemKontakter=NA,skiftVedSygehus=NA,skNavn=NA){
  
  # Starter med at hente oplysninger om udskrivningsdatoen paa de forgaaende kontakt.
  # Danner en binaere variabel, der fortaeller os, hvornaar en ny SK starter.
  datasaet1 <- datasaet %>%
    select(id,raekkeID,startDato,slutDato,sgh,indl)%>%#Beholder kun de variable, som er n?dvendig for beregningerne
    group_by(id) %>%
    mutate(cummaxSlutDatoMLM=as.Date(cummax(as.numeric(slutDato)),origin="1970-01-01"),
           nytSkStarterMLM=0)#F?rte r?kke for hver patient vil altid v?re starten af en ny SK.
  
  
  # Hvis der ikke er tilladt dage mellem kontakter under samme SK, saa starter der en ny SK for hver gang startdatoen for kontakten er stoerre end den maksimale forgaaende udskrivningsdato.
  if(is.na(tilladteDageMellemKontakter)){
    datasaet1 <- datasaet1 %>% 
      mutate(nytSkStarterMLM = ifelse(row_number()==1,1,
                                      ifelse(as.integer(startDato>lag(cummaxSlutDatoMLM)), 1, nytSkStarterMLM)))
  }
  
  # Hvis der er tilladt x dage mellem kontakter under samme SK, saa starter der en ny SK for hver gang startdatoen for kontakten er mere end x dage stoerre end den maksimale forgaaende udskrivningsdato.
  if(!is.na(tilladteDageMellemKontakter)){
    datasaet1 <- datasaet1 %>% 
      mutate(nytSkStarterMLM = ifelse(row_number()==1,1,
                                      ifelse(as.integer(startDato - lag(cummaxSlutDatoMLM)) > tilladteDageMellemKontakter, 1, nytSkStarterMLM)))
  }
  
  # Hvis SK skal vaere begraenset for hver hospital, saa starter et nyt SK, hver gang der sker et sygehusskift.
  if(!is.na(skiftVedSygehus)){
    datasaet1 <- datasaet1 %>% 
      mutate(nytSkStarterMLM = ifelse(row_number()==1,1,
                                      ifelse(sgh!=lag(sgh),1,nytSkStarterMLM)))
  }
  
  # Definere et SK nummer for hver SK og soerger for at datasaetframe ikke laengere er grupperet.
  datasaet1 <- datasaet1 %>%
    mutate(skNr =cumsum(nytSkStarterMLM))%>%
    group_by()%>%
    mutate(skPID=paste0(id,"_",skNr))%>%
    select(-c(cummaxSlutDatoMLM,nytSkStarterMLM))
  
  
  # Finder start og slut dato for SK
  datasaet2 <- datasaet1 %>%
    group_by(skPID) %>% 
    mutate(skStartDato=min(startDato,na.rm=TRUE),
           skSlutDato=max(slutDato,na.rm=TRUE),
           skMedIndl=max(indl))%>%
    group_by()
  
  
  # Ekstra oplysninger og rettelse af fejlregistreret akutte indlaeggelser
  datasaet3 <- datasaet2 %>%
    select(raekkeID,skPID,skStartDato,skSlutDato,skMedIndl)
  
  if(!is.na(skNavn)){
    colnames(datasaet3)[-1] <- paste0(foersteBogstavTilLille(skNavn),foersteBogstavTilStor(colnames(datasaet3)[-1]))
  }
  
  return(datasaet3)
  
}

foersteBogstavTilStor<-function(x){
  paste0(str_to_upper(substring(x,1,1)),substring(x,2,nchar(x)))
}

foersteBogstavTilLille<-function(x){
  paste0(str_to_lower(substring(x,1,1)),substring(x,2,nchar(x)))
}
