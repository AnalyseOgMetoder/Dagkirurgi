library(dplyr)
library(dbplyr)
library(odbc)
library(stringr)

setwd("Kode")
source("DagkirurgiFunktioner.R")

# Indlaes aktuelle sks koder
kodeopslag = read.table("L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Input\\Liste over SKS koder.csv", header = TRUE, sep = ";")
kodeopslag <- kodeopslag %>% mutate_all(as.character)

# Hent grundtabel og filtrer på aktuelle koder
PDATA <- getData("Dagkirurgi","ELEK_P_PROCEDURER")
DATA <- PDATA %>% filter(SKS %in% kodeopslag$SKS)

# Tilfoej gruppe-, operations- og kategoribeskrivelser
DATA <- merge(x = DATA, y = kodeopslag, x.all = TRUE)

# Tilfoej kolonne for registreret og beregnet patienttype Ambulant/Indlagt
DATA <- DATA %>% mutate(RegPatienttype = ifelse(patienttype == 0,"Indlagt (reg)","Ambulant (reg)"), BerPatienttype = ifelse(liggetid > 0 | FORliggetid > 0,"Mindst 1 overnatning","0 overnatninger"))

# Begraens tabelkolonnerne
DATA <- DATA %>% select(SKS,Beskrivelse,Speciale,Gruppe,Operation,Kategori,operationsID,patientID,kontaktID,samletktkID,proceduredato,antalSKS,antalP,antalPfuld,antalOP,SKSkombi,primSKS,primBeskrivelse,primSpeciale,sghafd,Sygehus,Afdeling,Afsnit,indmaade,RegPatienttype,BerPatienttype,medindlSK,inddatoSK,uddatoSK,liggetid,FORliggetid,sammesghafd,antalsghafd)

DATA <- DATA %>% filter(antalOP == 1, liggetid <= 10)

write.csv2(x = DATA, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\ResultaterORG10.csv")


# OPTAEL FRASORTEREDE DATA
ALLEOP <- getData("Dagkirurgi","ALLE_OP_PROCEDURER")
ALLEDATA <- ALLEOP %>% filter(SKS %in% kodeopslag$SKS)
ALLEDATA <- merge(x = ALLEDATA, y = kodeopslag, x.all = TRUE)

FRASORTFOR <- ALLEDATA %>% group_by(Operation) %>% summarise(nFOR = n_distinct(operationsID))
FRASORTP <- ALLEDATA %>% filter(antalP == 1,OPart == "P" | antalPfuld == 0) %>% group_by(Operation) %>% summarise(ptotal = n_distinct(operationsID))
FRASORTOP <- ALLEDATA %>% filter(antalOP != 1) %>% group_by(Operation) %>% summarise(OPfilter = n_distinct(operationsID))
FRASORT10D <- ALLEDATA %>% filter(liggetid > 10) %>% group_by(Operation) %>% summarise(L10filter = n_distinct(operationsID))
FRASORTAKUT <- ALLEDATA %>% filter(indmaade == 1) %>% group_by(Operation) %>% summarise(AKUTfilter = n_distinct(operationsID))
FRASORTEFTER <- DATA %>% filter(indmaade == 2,liggetid <= 10,antalOP == 1) %>% group_by(Operation) %>% summarise(nEFTER = n())

FRASORTTABEL <- merge(x = FRASORTFOR, y = FRASORTP,by = "Operation", all = TRUE) %>% mutate(Pfilter = nFOR - ptotal) %>% select(Operation,nFOR,Pfilter)
FRASORTTABEL <- merge(x = FRASORTTABEL, y = FRASORTOP,by = "Operation", all = TRUE)
FRASORTTABEL <- merge(x = FRASORTTABEL, y = FRASORT10D,by = "Operation", all = TRUE)
FRASORTTABEL <- merge(x = FRASORTTABEL, y = FRASORTAKUT,by = "Operation", all = TRUE)
FRASORTTABEL <- merge(x = FRASORTTABEL, y = FRASORTEFTER,by = "Operation", all = TRUE)
rm(FRASORTFOR,FRASORTP,FRASORTOP,FRASORT10D,FRASORTAKUT,FRASORTEFTER)

FRASORTTABEL <- FRASORTTABEL %>% replace(., is.na(.), 0)

write.csv2(x = FRASORTTABEL, file = "L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Resultater\\FrasortORG10.csv")




################# GENERERE GRAFER ############################

memory.limit()
memory.limit(size = 4000000)

### Indlaeser pakkerne ###
library(TTR)
library(zoo)
library(tidyr)
library(readxl)
library(rJava)
library(reshape2)
library(stringi)
library(tidyverse)
library(rio)
library(grid)
library(gridExtra)

dagkirurgi <- DATA

# Indlaeser datasaettet
#dagkirurgi <- read_excel("L:/LovbeskyttetMapper/Analyse - KS/Dagkirurgi_UDVALGTE.xlsx", 
#                         sheet = "DATA udvalgte SKS", col_types = c("text", 
#                                                                    "text", "numeric", "numeric", "text", 
#                                                                    "text", "text", "text", "text", "numeric", 
#                                                                    "text", "text", "text", "text", "text", 
#                                                                    "text", "numeric", "text", "text", 
#                                                                    "numeric", "numeric", "numeric", 
#                                                                    "numeric", "numeric", "numeric","text"))

# Manipulere datasaettet
dagkirurgi$operationsDatoAar<-"2018"

kblSygehusNavn<-data.frame("Sygehus"=sort(unique(dagkirurgi$Sygehus)))%>%
  mutate(sygehusNavn=ifelse(Sygehus=="Amager og Hvidovre Hospital","AHH",
                            ifelse(Sygehus=="Bispebjerg og Frederiksberg Hospitaler","BFH",
                                   ifelse(Sygehus=="Bornholms Hospital","BoH",
                                          ifelse(Sygehus=="Herlev og Gentofte Hospital","HGH",
                                                 ifelse(Sygehus=="Hospitalerne i Nordsjælland","NH",
                                                        ifelse(Sygehus=="Rigshospitalet","RH","Fejl")))))))

dagkirurgiA<-dagkirurgi%>%#4053
  left_join(kblSygehusNavn,by="Sygehus")

# Definere outputmappen
outputmappe<-"L:\\LovbeskyttetMapper\\BogA - Analyse\\Dagkirurgi\\Grafer\\"

tekstTilPdfNavn<- "- resultater med ALLO "


# Definere hvilke farver der skal bruges i graferne
voresFarver<-c("royalblue4","slategray3","dimgray","gray60","lightpink4","navajowhite4","wheat1")#"gray20"

# Funktioner
foersteBogstavTilStor<-function(x){
  paste0(str_to_upper(substring(x,1,1)),substring(x,2,nchar(x)))
}


# ANTALLET AF LIGGEDAGE FOR HVER OPERATION

# FrekvensPrGruppe er en funktion til at beregne og afbilde frekvensen.
frekvensPrGruppe<-function(datasaet,
                           opdelEfterVariablen=NA,# Hvis barplot skal stables
                           gruppereEfter,# id, hvis vi prøver at findes fx. antal ambbesøg pr pat.
                           periodeVariabel,# året for aktivitet
                           xVariablen,# Navnet på variblen der skal tælles/summeres
                           yVariablen,# Navnet på variablen der skal laves frekvenstabeller for - typisk en gruppe
                           overskrift,# Overskrift på grafen
                           navnPaaXAksen,
                           navnPaaYAksen,
                           filtrerNulVaek=FALSE,
                           retXAkseTal=FALSE,
                           startFraNul=FALSE,
                           filSti){# filen som frekvensgraferne skal udskrives til
  
  # Aabner pdf
  pdf(filSti,width = 10,height = 5,bg = rgb(226,223,204,maxColorValue=255,alpha=255))
  
  # Danner kopi af tabellen så jeg ikke skal overskrive variabelnavnet om igen til sidst.
  datasaetMdl<-datasaet%>%
    group_by()
  
  # Aendrer navnet på variablen for at bruge den til at danne graffer
  colnames(datasaetMdl)[colnames(datasaetMdl)==xVariablen]<-"xVariablen"
  colnames(datasaetMdl)[colnames(datasaetMdl)==yVariablen]<-"yVariablen"
  colnames(datasaetMdl)[colnames(datasaetMdl)==gruppereEfter]<-"gruppereEfter"
  colnames(datasaetMdl)[colnames(datasaetMdl)==periodeVariabel]<-"periodeVariabel"
  
  if(!is.na(opdelEfterVariablen)){
    colnames(datasaetMdl)[colnames(datasaetMdl)==opdelEfterVariablen]<-"opdelEfterVariablen"
  }
  
  startFra<-0
  
  # For hver unikt vaerdi i y-variablen laves frekvenstabeller
  for(a in sort(unique(datasaetMdl$yVariablen),na.last = TRUE)){
    
    # For hvert periode laves der en seperat frekvenstabel
    for(b in sort(unique(datasaetMdl$periodeVariabel))){
      
      # Afgraenser data og manipulere datasaettet, saa den staar i den rigtige format
      if(is.na(a)){
        datasaetMdl1<-datasaetMdl%>%
          filter(periodeVariabel==b)%>%
          filter(is.na(yVariablen))%>%
          group_by(gruppereEfter)%>%
          mutate(antalAfXVariablen=sum(xVariablen,na.rm=TRUE))%>%
          group_by()%>%
          distinct(gruppereEfter,.keep_all=TRUE)
        
        
      }else{
        datasaetMdl1<-datasaetMdl%>%
          filter(periodeVariabel==b)%>%
          filter(yVariablen==a)%>%
          group_by(gruppereEfter)%>%
          mutate(antalAfXVariablen=sum(xVariablen,na.rm=TRUE))%>%
          group_by()%>%
          distinct(gruppereEfter,.keep_all=TRUE)
        
      }
      
      if(nrow(datasaetMdl1)>0 & filtrerNulVaek==TRUE){
        
        datasaetMdl1<-datasaetMdl1%>%
          filter(antalAfXVariablen>0)
        
        startFra<-1
        
      }
      
      if(nrow(datasaetMdl1)>0){
        
        statOplysninger<-c(paste0("Antal = ",nrow(datasaetMdl1)),
                           paste0("Median = ",round(median(datasaetMdl1$antalAfXVariablen),1)),
                           paste0("Middelværdi = ",round(mean(datasaetMdl1$antalAfXVariablen),1)),
                           paste0("95% kvartil = ",round(quantile(datasaetMdl1$antalAfXVariablen,probs = c(0.95)),1)))
        
        
        if(!is.na(opdelEfterVariablen)){
          
          tabelMdlX<-data.frame(table(datasaetMdl1$antalAfXVariablen,datasaetMdl1$opdelEfterVariablen,useNA = "always"))%>%
            filter(Freq!=0)%>%
            mutate(Var1=as.numeric(as.character(Var1)))%>%
            arrange(Var1)%>%
            reshape(idvar = "Var1",timevar = "Var2",direction = "wide")
          
          if(retXAkseTal&is.numeric(tabelMdlX$Var1)){
            
            if(startFraNul){
              
              mindsteX<-0
              
            }else{
              
              mindsteX<-min(datasaetMdl1$antalAfXVariablen)
              
            }
            
            stoersteX<-max(datasaetMdl1$antalAfXVariablen)
            
            for(xTal in mindsteX:stoersteX){
              
              if(!(xTal %in% tabelMdlX$Var1)){
                xTalRaekke<-as.vector(c(xTal,rep(NA,ncol(tabelMdlX)-1)))
                names(xTalRaekke)<-colnames(tabelMdlX)
                tabelMdlX<-bind_rows(tabelMdlX,xTalRaekke)
              }
              
            }
            tabelMdlX<-tabelMdlX%>%
              arrange(Var1)
          }
          
          colnames(tabelMdlX)[-1]<-stri_sub(colnames(tabelMdlX)[-1],6,-1)
          
          zVariablen<-colnames(tabelMdlX)[-1]
          
          for(ii in 1:length(zVariablen)){
            
            tabelMdlXMdl<-tabelMdlX
            
            # Aendrer navnet paa variablen for at bruge den til at danne grafer
            colnames(tabelMdlXMdl)[colnames(tabelMdlXMdl)==zVariablen[ii]]<-"zVariablen"
            
            # Laver det om til vektor
            vektorGraf<-as.vector(tabelMdlXMdl$zVariablen)
            names(vektorGraf)<-as.vector(paste0(tabelMdlXMdl$Var1))
            
            # Aendrer navnet paa variablen tilbage igen
            colnames(tabelMdlXMdl)[colnames(tabelMdlXMdl)=="zVariablen"]<-zVariablen[ii]
            
            if(ii==1){
              tabelGraf<-as.table(vektorGraf)
            }else{
              tabelGraf<-as.table(rbind(tabelGraf,vektorGraf))
            }
            
          }
          rownames(tabelGraf)<-zVariablen
          
          tabelGraf[is.na(tabelGraf)]<-0
          
          plot=barplot(tabelGraf,las=2,col = voresFarver[1:length(zVariablen)],
                       ylim = c(0,max(colSums(tabelGraf))+max(colSums(tabelGraf))*0.8),main =paste0(overskrift," - ",foersteBogstavTilStor(a)," i ",b),
                       cex.names = 0.8,xaxs="r",yaxs="r",adj  = 0)
          
          title(ylab = navnPaaYAksen,font.lab=2,line = 3.2,adj=0.88)
          # axis(side = 1,at =  seq(from=0.25, to=(1.2*ncol(tabelGraf)+0.25),by=1.2) ,labels = FALSE)
          tabelGrafA<-colSums(tabelGraf)
          tabelGrafA[tabelGrafA==0]<-""
          text(plot,(colSums(tabelGraf)+max(colSums(tabelGraf))*0.05),tabelGrafA,cex = 0.7,font=1)
          title(xlab = navnPaaXAksen,line = 2.5,font.lab=2)
          legend("topleft",paste(rownames(tabelGraf),"-",rowSums(tabelGraf)),
                 fill = voresFarver[1:length(zVariablen)],bty = "n",cex = 0.8)
          legend("topright",statOplysninger,
                 cex = 0.8)
          
        }else{
          
          
          tabelMdlX<-data.frame(table(datasaetMdl1$antalAfXVariablen,useNA = "always"))%>%
            filter(Freq!=0)%>%
            mutate(Var1=as.numeric(as.character(Var1)))%>%
            arrange(Var1)
          
          tabelMdl<-data.frame("Var1"=rep(startFra:max(tabelMdlX$Var1)))%>%
            left_join(tabelMdlX,by="Var1")%>%
            mutate(Freq=ifelse(is.na(Freq),0,Freq))%>%
            arrange(Var1)
          
          # Laver det om til vektor
          vektorGraf<-as.vector(tabelMdl$Freq)
          names(vektorGraf)<-as.vector(paste0(tabelMdl$Var1))
          
          tabelGraf<-as.table(vektorGraf)
          
          plot=barplot(tabelGraf,las=2,col = voresFarver[1],
                       ylim = c(0,max(tabelGraf)+max(tabelGraf)*0.8),main =paste0(overskrift," - ",foersteBogstavTilStor(a)," i ",b),
                       cex.names = 0.8,xaxs="r",yaxs="r")
          
          title(ylab = navnPaaYAksen,font.lab=2,line = 3.2,adj = 0.8)
          # axis(side = 1,at =  seq(from=0.25, to=(1.2*nrow(tabelGraf)+0.25),by=1.2) ,labels = FALSE)
          tabelGrafA<-tabelGraf
          tabelGrafA[tabelGrafA==0]<-""
          text(plot,(tabelGraf+max(tabelGraf)*0.05),tabelGrafA,cex = 0.6,font=1)
          title(xlab = navnPaaXAksen,line = 0,font.lab=2)
          legend("topright",statOplysninger,
                 cex = 0.8)#fill = voresFarver[2],bty = "n",pch=NA,
          
        }
        
      }else{
        
      }
    }
  }
  
  dev.off()
}

frekvensPrGruppe(datasaet=dagkirurgiA,
                 gruppereEfter="operationsID",
                 opdelEfterVariablen ="sygehusNavn", 
                 periodeVariabel="operationsDatoAar",
                 xVariablen="liggetid",
                 yVariablen="Operation",
                 overskrift="Fordelingen af antal operationer mod antal overnatninger",
                 navnPaaXAksen="Overnatninger efter operationen",
                 navnPaaYAksen="Antal operationer",
                 filtrerNulVaek=FALSE,
                 retXAkseTal = TRUE,
                 startFraNul = TRUE,
                 filSti =paste0(outputmappe,"Dagkirurgi liggetid grafer ",tekstTilPdfNavn," ",format(Sys.time(),"%y%m%d %H%M"),".pdf"))



# ANDEL AF AMBULANTE BEREGNET VS REGISTRERET

procentPrGruppe<-function(datasaet,
                          periodeVariabel,# Aaret for aktivitet
                          seperatGrafForHver,
                          xVariablen,# Navnet paa variblen der skal taelles/summeres
                          yVariablen,# Navnet paa variablen der skal laves frekvenstabeller for - typisk en gruppe
                          overskrift,# Overskrift paa grafen
                          navnPaaXAksen,
                          navnPaaYAksen,
                          samletVarNavn="",
                          beregnAndel,
                          filSti){# filen som frekvensgraferne skal udskrives til
  
  # Aabner pdf
  pdf(filSti,width = 10,height = 5,bg = rgb(226,223,204,maxColorValue=255,alpha=255))
  
  # Danner kopi af tabellen saa jeg ikke skal overskrive variabelnavnet om igen til sidst.
  datasaetMdl<-datasaet%>%
    group_by()
  
  # Aendrer navnet paa variablen for at bruge den til at danne graffer
  colnames(datasaetMdl)[colnames(datasaetMdl)==xVariablen]<-"xVariablen"
  # colnames(datasaetMdl)[colnames(datasaetMdl)==yVariablen]<-"yVariablen"
  colnames(datasaetMdl)[colnames(datasaetMdl)==periodeVariabel]<-"periodeVariabel"
  colnames(datasaetMdl)[colnames(datasaetMdl)==seperatGrafForHver]<-"seperatGrafForHver"
  
  # For hver unikt vaerdi i y-variablen laves en seperat tabel
  for(a in sort(unique(datasaetMdl$seperatGrafForHver),na.last = TRUE)){
    
    # For hvert periode laves der en seperat tabel
    for(b in sort(unique(datasaetMdl$periodeVariabel))){
      
      datasaetMdl1<-datasaetMdl%>%
        filter(periodeVariabel==b)%>%
        filter(seperatGrafForHver==a)
      
      if(nrow(datasaetMdl1)>0){
        
        tabelMdl<-data.frame(table(datasaetMdl1$xVariablen,useNA = "always"))%>%
          filter(Freq!=0)
        
        colnames(tabelMdl)<-c("xVariablen","samlet")
        # 
        # colnames(tabelMdlX)[-1]<-stri_sub(colnames(tabelMdlX)[-1],6,-1)
        # 
        # zVariablen<-colnames(tabelMdlX)[-1]
        # 
        
        tabelMdlX<-tabelMdl
        
        # Beregner summen af hver yVariabel
        for(ii in 1:length(yVariablen)){
          
          # Aendrer navnet paa variablen for at bruge den til at danne grafer
          colnames(datasaetMdl1)[colnames(datasaetMdl1)==yVariablen[ii]]<-"yVariablen"
          
          
          # Beregner summen for den valgte yvariabel
          tabelMdlYvar<-aggregate(yVariablen~xVariablen,data = datasaetMdl1,FUN = sum)
          
          if(beregnAndel){
            
            tabelMdlX<-tabelMdlX%>%
              left_join(tabelMdlYvar,by="xVariablen")%>%
              mutate(yVariablen1=ifelse(is.na(yVariablen),0,yVariablen),
                     yVariablen=round((yVariablen/samlet)*100,2))%>%
              select(-yVariablen1)
            
          }else{
            
            tabelMdlX<-tabelMdlX%>%
              left_join(tabelMdlYvar,by="xVariablen")
            
          }
          
          colnames(datasaetMdl1)[colnames(datasaetMdl1)=="yVariablen"]<-yVariablen[ii]
          colnames(tabelMdlX)[colnames(tabelMdlX)=="yVariablen"]<-yVariablen[ii]
          
        }
        
        # Erstatter NA med 0
        tabelMdlX[is.na(tabelMdlX)]<-0
        
        zVariablen<-c("samlet",yVariablen)
        
        # Danner en tabel til barplot
        for(ii in 1:length(zVariablen)){
          
          tabelMdlXMdl<-tabelMdlX
          
          # Aendrer navnet paa variablen for at bruge den til at danne graffer
          colnames(tabelMdlXMdl)[colnames(tabelMdlXMdl)==zVariablen[ii]]<-"zVariablen"
          
          # Laver det om til vektor
          vektorGraf<-as.vector(tabelMdlXMdl$zVariablen)
          names(vektorGraf)<-as.vector(paste0(tabelMdlXMdl$xVariablen))
          
          # Aendrer navnet paa variablen tilbage igen
          colnames(tabelMdlXMdl)[colnames(tabelMdlXMdl)=="zVariablen"]<-zVariablen[ii]
          
          if(ii==1){
            tabelGraf<-as.table(vektorGraf)
          }else{
            tabelGraf<-as.table(rbind(tabelGraf,vektorGraf))
          }
          
        }
        rownames(tabelGraf)<-zVariablen
        
        tabelGraf[is.na(tabelGraf)]<-0
        
        tabelGrafA<-tabelGraf[-1,]
        
        if(beregnAndel){
          
          # ymax<-max(tabelGrafA)+max(tabelGrafA)*0.5#0.75
          ymax<-100
          
        }else{
          
          ymax<-max(colSums(tabelGrafA))+max(colSums(tabelGrafA))*0.8#0.75
          
        }
        par(mar=c(6,8,6,2),xpd=TRUE)
        plot=barplot(tabelGrafA,beside = TRUE,las=2,col = voresFarver[1:length(yVariablen)],
                     ylim = c(0,ymax),
                     cex.names = 0.8,xaxs="r",yaxs="r")
        title(main = paste0(overskrift,"\n",foersteBogstavTilStor(a)," i ",b), line = 3, adj = 0)
        
        title(ylab = navnPaaYAksen,font.lab=2,line = 3.2,adj = 0.8)
        # axis(side = 1,at =  seq(from=0.25, to=(1.2*ncol(tabelGraf)+0.25),by=1.2) ,labels = FALSE)
        tabelGrafB<-tabelGrafA
        # tabelGrafB[tabelGrafB==0]<-""
        # text(plot,(tabelGraf+max(tabelGraf)*0.03),tabelGrafA,cex = 0.5,font=1)
        title(xlab = navnPaaXAksen,line = 3.5,font.lab=2)
        # legend("topleft",paste(rownames(tabelGraf),"-",rowSums(tabelGraf)),
        #        fill = voresFarver[1:length(yVariablen)],bty = "n",cex = 0.8)
        # legend("topright",statOplysninger,
        #        cex = 0.8)
        if(max(tabelGrafA)<10){
          dataEtiketPla<-max(5,(max(tabelGrafA)*1.2))
        }else if(max(tabelGrafA)<50){
          dataEtiketPla<-(max(tabelGrafA)*0.1)
        }else{
          dataEtiketPla<-(max(tabelGrafA)*0.05)
        }
        
        if(beregnAndel){
          text(plot,(tabelGrafA+dataEtiketPla),paste0(tabelGrafB,"%"),cex = 0.6,font=1)
          # samletTabel<-tabelGraf[1,]
          samletTabel1<-rep("",length(tabelGraf[1,]))
          samletTabel<-as.table(rbind(paste(tabelGraf[1,],samletVarNavn),samletTabel1))
          #text(plot,ymax*0.8,samletTabel,cex = 0.8,font=1)
          #legend("topright",paste(rownames(tabelGrafA)),
          #       fill = voresFarver[1:length(yVariablen)],bty = "n",cex = 0.8)
          mtext(samletTabel,at = seq(from=0.24, to = 0.955,by=0.715/(length(samletTabel)-1)),side=3,line = -5,outer=TRUE,cex = 0.8)
          
          
          legend(length(samletTabel)*1.16,138,paste(rownames(tabelGrafA)),
                 fill = voresFarver[1:length(yVariablen)],bty = "n", cex = 0.8,horiz = TRUE)
          
        }else{
          text(plot,(tabelGrafA+dataEtiketPla),tabelGrafB,cex = 0.6,font=1)
          legend("topright",paste(rownames(tabelGrafB),"-",rowSums(tabelGrafB)),
                 fill = voresFarver[1:length(yVariablen)],bty = "n",cex = 0.8)
        }
        
      }else{
        
      }
    }
  }
  
  dev.off()
}



dagkirurgiB<-dagkirurgiA%>%
  mutate(registreret=ifelse(RegPatienttype=="Ambulant (reg)",1,0),
         beregnet=ifelse(BerPatienttype=="0 overnatninger",1,0))


procentPrGruppe(datasaet =dagkirurgiB,
                periodeVariabel = "operationsDatoAar",
                seperatGrafForHver = "Operation",
                xVariablen ="sygehusNavn",#"Sygehus"
                yVariablen = c("registreret","beregnet"),
                overskrift = "",
                navnPaaXAksen = "Sygehus",
                navnPaaYAksen = "Andel af ambulante operationer",
                beregnAndel=TRUE,
                samletVarNavn = "operationer",
                filSti = paste0(outputmappe,"Dagkirurgi andel ambulant ",tekstTilPdfNavn," ",format(Sys.time(),"%y%m%d %H%M"),".pdf"))

# SOM OVENSTÅENDE MEN MED ANTAL I STEDET FOR ANDEL
#procentPrGruppe(datasaet =dagkirurgiB,
#                periodeVariabel = "operationsDatoAar",
#                seperatGrafForHver = "Operation",
#                xVariablen ="sygehusNavn",#"Sygehus"
#                yVariablen = c("registreret","beregnet"),
#                overskrift = "Antal ambulante operationer",
#                navnPaaXAksen = "Sygehus",
#                navnPaaYAksen = "Antal ambulante operationer",
#                beregnAndel=FALSE,
#                filSti = paste0(outputmappe,"Dagkirurgi antal ambulant ",tekstTilPdfNavn," ",format(Sys.time(),"%y%m%d %H%M"),".pdf"))



# FORSKEL I ANTAL AMBULANTE REGISTRERET OG BEREGNET SAMLET FOR ALLE OPERATIONER

procentGraf2<-function(datasaet,
                       periodeVariabel,# året for aktivitet
                       #seperatGrafForHver,
                       xVariablen,# Navnet paa variblen der skal tælles/summeres
                       yVariablen,# Navnet paa variablen der skal laves frekvenstabeller for - typisk en gruppe
                       overskrift,# Overskrift paa grafen
                       navnPaaXAksen,
                       navnPaaYAksen,
                       samletVarNavn="",
                       beregnAndel,
                       filSti){# filen som frekvensgraferne skal udskrives til
  
  # Aabner pdf
  pdf(filSti,width = 10,height = 5,bg = rgb(226,223,204,maxColorValue=255,alpha=255) )
  
  # Danner kopi af tabellen saa jeg ikke skal overskrive variabelnavnet om igen til sidst.
  datasaetMdl<-datasaet%>%
    group_by()
  
  # Aendrer navnet paa variablen for at bruge den til at danne grafer
  colnames(datasaetMdl)[colnames(datasaetMdl)==xVariablen]<-"xVariablen"
  # colnames(datasaetMdl)[colnames(datasaetMdl)==yVariablen]<-"yVariablen"
  colnames(datasaetMdl)[colnames(datasaetMdl)==periodeVariabel]<-"periodeVariabel"
  # colnames(datasaetMdl)[colnames(datasaetMdl)==seperatGrafForHver]<-"seperatGrafForHver"
  
  # For hver unikt vaerdi i y-variablen laves en seperat tabel
  # for(a in sort(unique(datasaetMdl$seperatGrafForHver),na.last = TRUE)){
  
  # For hvert periode laves der en seperat tabel
  for(b in sort(unique(datasaetMdl$periodeVariabel))){
    
    datasaetMdl1<-datasaetMdl%>%
      filter(periodeVariabel==b)#%>%
    #filter(seperatGrafForHver==a)
    
    if(nrow(datasaetMdl1)>0){
      
      tabelMdl<-data.frame(table(datasaetMdl1$xVariablen,useNA = "always"))%>%
        filter(Freq!=0)
      
      colnames(tabelMdl)<-c("xVariablen","samlet")
      # 
      # colnames(tabelMdlX)[-1]<-stri_sub(colnames(tabelMdlX)[-1],6,-1)
      # 
      # zVariablen<-colnames(tabelMdlX)[-1]
      # 
      
      tabelMdlX<-tabelMdl
      
      # Beregner summen af hver yVariabel
      for(ii in 1:length(yVariablen)){
        
        # Ændrer navnet på variablen for at bruge den til at danne graffer
        colnames(datasaetMdl1)[colnames(datasaetMdl1)==yVariablen[ii]]<-"yVariablen"
        
        
        # Beregner summen for den valgte yvariabel
        tabelMdlYvar<-aggregate(yVariablen~xVariablen,data = datasaetMdl1,FUN = sum)
        
        if(beregnAndel){
          
          tabelMdlX<-tabelMdlX%>%
            left_join(tabelMdlYvar,by="xVariablen")%>%
            mutate(yVariablen1=ifelse(is.na(yVariablen),0,yVariablen),
                   yVariablen=round((yVariablen/samlet)*100,1))%>%
            select(-yVariablen1)
          
        }else{
          
          tabelMdlX<-tabelMdlX%>%
            left_join(tabelMdlYvar,by="xVariablen")
          
        }
        
        colnames(datasaetMdl1)[colnames(datasaetMdl1)=="yVariablen"]<-yVariablen[ii]
        colnames(tabelMdlX)[colnames(tabelMdlX)=="yVariablen"]<-yVariablen[ii]
        
      }
      
      # Erstatter NA med 0
      tabelMdlX[is.na(tabelMdlX)]<-0
      
      zVariablen<-c("samlet",yVariablen)
      
      # Danner en tabel til barplot
      for(ii in 1:length(zVariablen)){
        
        tabelMdlXMdl<-tabelMdlX
        
        # Aendrer navnet paa variablen for at bruge den til at danne graffer
        colnames(tabelMdlXMdl)[colnames(tabelMdlXMdl)==zVariablen[ii]]<-"zVariablen"
        
        # Laver det om til vektor
        vektorGraf<-as.vector(tabelMdlXMdl$zVariablen)
        names(vektorGraf)<-as.vector(paste0(tabelMdlXMdl$xVariablen))
        
        # Aendrer navnet paa variablen tilbage igen
        colnames(tabelMdlXMdl)[colnames(tabelMdlXMdl)=="zVariablen"]<-zVariablen[ii]
        
        if(ii==1){
          tabelGraf<-as.table(vektorGraf)
        }else{
          tabelGraf<-as.table(rbind(tabelGraf,vektorGraf))
        }
        
      }
      rownames(tabelGraf)<-zVariablen
      
      tabelGraf[is.na(tabelGraf)]<-0
      
      tabelGrafA<-tabelGraf[-1,]
      
      #if(beregnAndel){
      
      # ymax<-max(tabelGrafA)+max(tabelGrafA)*0.5#0.75
      ymax<-100
      
      #}else{
      
      #  ymax<-max(colSums(tabelGrafA))+max(colSums(tabelGrafA))*0.8#0.75
      
      #}
      
      # par("mar") -default  5.1 4.1 4.1 2.1
      par(mar=c(12,5,5,5))
      
      plot=barplot(tabelGrafA,beside = TRUE,las=2,col = voresFarver[1:length(yVariablen)],
                   ylim = c(0,ymax),#main = paste0(overskrift),#,"\n i ",b),
                   cex.names = 0.8,xaxs="r",yaxs="r")
      
      title(overskrift,adj = 0)
      title(ylab = navnPaaYAksen,font.lab=2,line = 3.2)
      # axis(side = 1,at =  seq(from=0.25, to=(1.2*ncol(tabelGraf)+0.25),by=1.2) ,labels = FALSE)
      tabelGrafB<-tabelGrafA
      # tabelGrafB[tabelGrafB==0]<-""
      # text(plot,(tabelGraf+max(tabelGraf)*0.03),tabelGrafA,cex = 0.5,font=1)
      title(xlab = navnPaaXAksen,line = 10,font.lab=2)
      # legend("topleft",paste(rownames(tabelGraf),"-",rowSums(tabelGraf)),
      #        fill = voresFarver[1:length(yVariablen)],bty = "n",cex = 0.8)
      # legend("topright",statOplysninger,
      #        cex = 0.8)
      if(max(tabelGrafA)<10){
        dataEtiketPla<-max(5,(max(tabelGrafA)*1.2))
      }else if(max(tabelGrafA)<50){
        dataEtiketPla<-(max(tabelGrafA)*0.1)
      }else{
        dataEtiketPla<-(max(tabelGrafA)*0.05)
      }
      
      if(beregnAndel){
        text(plot,(tabelGrafA+dataEtiketPla),paste0(tabelGrafB,"%"),cex = 0.6,font=1)
        # samletTabel<-tabelGraf[1,]
        #samletTabel1<-rep(length(tabelGraf[1,]))
        samletTabel<-as.table(rbind(paste(tabelGraf[1,],samletVarNavn)))#,samletTabel1))
        #text(plot,ymax*0.75,samletTabel,cex = 0.8,font=1)
        
        mtext(samletTabel,at = seq(from=0.155, to = 0.845,by=0.69/(length(samletTabel)-1)),side=3,line = -5,outer=TRUE,cex = 0.8)
        
        par(xpd=TRUE)
        legend(22.9,147,paste(rownames(tabelGrafA)),
               fill = voresFarver[1:length(yVariablen)],bty = "n", cex = 0.8,horiz = TRUE)
      }else{
        text(plot,(tabelGrafA+dataEtiketPla),tabelGrafB,cex = 0.6,font=1)
        legend("topright",paste(rownames(tabelGrafB),"-",rowSums(tabelGrafB)),
               fill = voresFarver[1:length(yVariablen)],bty = "n",cex = 0.8)
      }
      
    }else{
      
    }
  }
  # }
  
  dev.off()
}

# FOR ALLE 12 KODER
#procentGraf2(datasaet =dagkirurgiB,
#             periodeVariabel = "operationsDatoAar",
#             # seperatGrafForHver = "Beskrivelse",
#             xVariablen ="Operation",#"Sygehus"
#             yVariablen = c("registreret","beregnet"),
#             overskrift = "Andelen af ambulante operationer",
#             navnPaaXAksen = "",
#             navnPaaYAksen = "",
#             beregnAndel=TRUE,
#             samletVarNavn = "op",
#             filSti = paste0(outputmappe,"Dagkirurgi andel ambulant samlet ",tekstTilPdfNavn," ",format(Sys.time(),"%y%m%d %H%M"),".pdf"))

#UDEN ALLO
dagkirurgiC<-dagkirurgiB%>%
  filter(Operation!="Hoftealloplastik")%>%
  filter(Operation!="Knæalloplastik")

tekstTilPdfNavn2<- "- resultater uden ALLO "

procentGraf2(datasaet =dagkirurgiC,
             periodeVariabel = "operationsDatoAar",
             # seperatGrafForHver = "Beskrivelse",
             xVariablen ="Operation",#"Sygehus"
             yVariablen = c("registreret","beregnet"),
             overskrift = "Andelen af ambulante operationer",
             navnPaaXAksen = "",
             navnPaaYAksen = "",
             beregnAndel=TRUE,
             samletVarNavn = "op",
             filSti = paste0(outputmappe,"Dagkirurgi andel ambulant samlet ",tekstTilPdfNavn2," ",format(Sys.time(),"%y%m%d %H%M"),".pdf"))



# SAMLET ANTAL LIGGEDAGE

barplotEnVariabel<-function(datasaet,xVariablen,yVariablen,overskrift,navnPaaXAksen,navnPaaYAksen){
  
  # par("mar") -default  5.1 4.1 4.1 2.1
  par(mar=c(12,6,4,2))
  
  ### Barplot ###
  # Danner kopi af tabellen saa jeg ikke skal overskrive variabelnavnet om igen til sidst.
  datasaetMdl<-datasaet
  
  # Aendrer navnet paa variablen for at bruge den til at danne graffer
  colnames(datasaetMdl)[colnames(datasaetMdl)==xVariablen]<-"xVariablen"
  colnames(datasaetMdl)[colnames(datasaetMdl)==yVariablen]<-"yVariablen"
  
  # Laver det om til vektor
  vektorGraf<-as.vector(datasaetMdl$yVariablen)
  names(vektorGraf)<-as.vector(paste0(datasaetMdl$xVariablen))
  
  tabelGraf<-as.table(vektorGraf)
  
  plot=barplot(tabelGraf,las=2,col = voresFarver[1],
               ylim = c(0,max(tabelGraf)+max(tabelGraf)*0.50),#main =overskrift,
               cex.names = 0.8,xaxs="r",yaxs="r")
  
  title(ylab = navnPaaYAksen,font.lab=2,line = 3.2, adj = 0.9)
  #axis(side = 1,at =  seq(from=0.25, to=(1.2*nrow(tabelGraf)+0.25),by=1.2) ,labels = FALSE)
  tabelGrafA<-tabelGraf
  tabelGrafA[tabelGrafA==0]<-""
  text(plot,(tabelGraf+max(tabelGraf)*0.05),tabelGrafA,cex = 0.6,font=1)
  title(xlab = navnPaaXAksen,line = 10,font.lab=2)
  title(overskrift,adj = 0, line = 0)
}


# Med Allo

summeretLiggedage<-aggregate(liggetid~Operation,data=dagkirurgiA,FUN=sum)%>%
  arrange(Operation)

filSti<-paste0(outputmappe,"Samlet antal overnatninger fordelt på operationstype ",tekstTilPdfNavn," ",format(Sys.time(),"%y%m%d %H%M"),".pdf")

pdf(filSti,width = 10,height = 5,bg = rgb(226,223,204,maxColorValue=255,alpha=255))

barplotEnVariabel(datasaet = summeretLiggedage,
                  xVariablen = "Operation",
                  yVariablen = "liggetid",
                  overskrift = "Samlet antal overnatninger efter operationen fordelt på operationstype",
                  navnPaaXAksen ="",
                  navnPaaYAksen ="Overnatninger")


dev.off()


# Uden Allo

summeretLiggedage<-aggregate(liggetid~Operation,data=dagkirurgiC,FUN=sum)%>%
  arrange(Operation)

filSti<-paste0(outputmappe,"Samlet antal overnatninger fordelt på operationstype ",tekstTilPdfNavn2," ",format(Sys.time(),"%y%m%d %H%M"),".pdf")

pdf(filSti,width = 10,height = 5,bg = rgb(226,223,204,maxColorValue=255,alpha=255))

barplotEnVariabel(datasaet = summeretLiggedage,
                  xVariablen = "Operation",
                  yVariablen = "liggetid",
                  overskrift = "Samlet antal overnatninger efter operationen fordelt på operationstype",
                  navnPaaXAksen ="",
                  navnPaaYAksen ="Overnatninger")


dev.off()

