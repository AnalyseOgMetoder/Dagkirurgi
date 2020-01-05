Dagkirurgi i Region Hovedstaden
================

  - [De forskellige scripts i
    projektet](#de-forskellige-scripts-i-projektet)
  - [Datagrundlag og beregninger](#datagrundlag-og-beregninger)
      - [Beregninger af nye kolonner](#beregninger-af-nye-kolonner)
      - [Frasorteret data](#frasorteret-data)

Hvis patient og operation egner sig til dagkirurgi, har dagkirurgi en
positiv effekt på både patienttilfredsheden, patientens sundhed og
omkostningerne. På den baggrund er der i foråret 2019 påbegyndt en
analyse af nuværende og potentiel udbredelse af dagkirurgi i Region
Hovedstaden.

I denne vejledning gennemgås vigtige elementer af databehandlingen, og
en beskrivelse af de scripts der hører til projektet. Kommissorium og
løbende afrapportering kan for Region H medarbejdere findes i workzone
på sag 19031897.

## De forskellige scripts i projektet

**Dagkirurgi12koder.R**  
Dansk Selskab for Dagkirurgi lavede i januar 2019 en rundspørge blandt
alle landets hospitaler med henblik på at klarlægge andelen af
dagkirurgi for 10 udvalgte operationer, som de vidste egnede sig til
dagkirurgi. *Dagkirurgi12koder.R* henter data for Region Hovedstaden for
disse 10 udvalgte operationer samt 2 yderligere (knæ- og
hoftealloplastik).  
I scriptet beregnes antal overnatninger, som det samlede antal liggedage
på tværs af patienternes kontaktoprettelser på hospitalet. Data
opsummeres i en række grafer. OBS: Ved denne præsentation er andelen af
dagkirurgi defineret til 0 overnatninger efter operationen - i det
videre arbejde er denne definition justeret til 0 overnatninger efter OG
før operationen.

**DagkirurgiFunktioner.R**  
Indeholder hjælpefunktioner til databehandlingen i de øvrige scripts.  
*saveData(data,schemaname, tablename, overwrite = FALSE)* gemmer en ny
tabel i analysedatabasen, eller tager en backup af en gammel tabel,
overskriver tabellen med den nye og sletter backuppen.  
*generereId(idListe)* genererer nye tilfældige id til en idliste og
returnerer nøglen.  
*renameSGH(col)* omdøber fulde sygehusnavne i en kolonne til
forkortelser, fx Rigshospitalet -\> RH.

**DagkirurgiGrunddata.R**  
Dette script udvider behandlingsområdet fra 12 udvalgte koder til
samtlige udførte kirurgiske sks-koder i 2018. Der beregnes data for hver
operationsID, som er defineret ud fra procedurer udført på samme kontakt
den samme dag. Data kobles med kontaktdata, der beregnes antal
overnatninger og id anonymiseres.  
Tabellerne giver mulighed for udtræk til videre analysering af nuværende
og potentiel udbredelse af dagkirurgien i Region Hovedstaden.
Beregningerne og frasorteringskriterier er beskrevet i næste særskilte
afsnit.

**DagkirurgiKategorisering.R**  
Printer nogle tabeller til Excel, som kan bruges som oplæg til at
kategorisere reelle operationskoder og dagkirurgiske afsnit.

**DagkirurgiSpecialedata.R**  
Beregner antal udførte procedurer, andelen af procedurer udført på en
kirurgisk afdeling, andel af procedurer udført dagkirurgisk, median
antal overnatninger og total antal overnatninger for alle SKS-koder for
hvert hospital i Region Hovedstaden.  
Resultaterne printes til et Excelark som bruges som grundlag til den
videre analyse og validering hos klinikerne.

**SK funktionen.R**  
Funktion der beregner et id og egenskaber for overlappende kontakter
(Samlet Kontakt, SK). Funktionen beregnet et samlet kontakt ID,
startdato, slutdato og MedIndl som angiver om patienten har været
indlagt undervejs i de overlappende kontakter.

## Datagrundlag og beregninger

Analysen er baseret på DRG 2018 data. I **DagkirurgiDataforberedelse.R**
hentes data fra DRGPROCEDURER og DRGKONTAKTER ind, flettes sammen og
manipuleres. Den resulterende tabel er gemt som ELEK\_P\_PROCEDURER i
sektionens database RGHPRODAPP007\\RGHPRODAPP007 og danner basis for
mulige beregninger og utræk på operationsniveau, med oplysninger om
afdeling, liggetider, operationskombination osv.

### Beregninger af nye kolonner

Alle proceduredata i tabellen DRGPROCEDURER hvor PROCEDUREKODE starter
med K og PROCEDURE\_DATO har år 2018 er koblet med kontaktdata fra
tabellen DRGKONTAKTER.  
Dette giver et datagrundlag på 619.745 procedurer.

*operationsID*  
For at afdække et dagkirurgisk potentiale regnes der på operationer frem
for enkelte procedurer. Til formålet er der beregnet et fiktivt
operationsID, som kobler procedurer udført samme dag på samme kontakt
sammen. Denne beregning vil være misvisende for patienter der har
gennemgået flere operationer på samme dag. Patienterne frasorteres hvis
primærproceduren ikke er entydig, og disse patienter vil næppe egne sig
til dagkirurgi uanset.

*samletktkID*  
ID der grupperer overlappende eller sammenhængende patientID, hvis
patienten ikke har været hjemme fra hospitalet.

*antalSKS*, *antalP* og *antalPfuld*  
antalSKS angiver antal SKS knyttet til samme operationsID, antalPfuld er
procedurer hvor OPart = “P” og antalP er antallet af *forskellige*
sks-koder knyttet til det givne operationsID hvor OPart = “P” eller
antalPfuld = 0.

*antalOP*  
Antal operationer under indlæggelsen, dvs. knyttet til samletktkID.

*SKSkombi*, *primSKS* og *primSKSfuld*  
SKSkombi er alle SKS-koder knyttet til operationen, primSKS er alle
forskellige primærprocedurer og primSKSfuld er alle primærprocedurer
inkl. dubletter.

*KIRafd*  
Ikke alle procedurekoder der starter med K er reelle operationer, KIRafd
angiver om den udførende afdeling er kirurgisk. Når data eksporteres
beregnes variablen KIR, som er en indikator for hvor stor en andel af
den givne procedure der er udført på en kirurgisk afdeling. Vurderingen
af hvilke afdelinger der er kirurgiske er foretaget ud fra en manuel
gennemgang af alle afdelinger ud fra de 10 hyppigste
operationsprocedurer de hver især udfører.

*medindlSK*, *inddatoSK* og *uddatoSK*  
Variable beregnet med samlet-kontakt funktionen der beregner dato for
første og sidste dag af indlæggelsen på tværs af overlappende
kontakter. medindlSK angiver om en eller flere af de overlappende
kontakter er af typen indlagt.

*liggetid*  
Er for indlagte kontakter beregnet ud fra ind- og uddato, og ud fra
ambdato for ambulante kontakter. For patienter med flere overlappende
kontaktoprettelser er disse datoer slået sammen. Denne liggetid tager
højde for overlappende kontakteroprettelser, og er derfor mere nøjagtig
end DRG kontakternes liggetider, samt registreringen ambulant/indlagt.

*FORliggetid*  
Angiver indlæggelsestiden forudgående for operationen.

*antalsghafd* og *antalsghafdP0*  
Angiver antal forskellige producerende sygehusafdelinger knyttet til
samme operationsID for hhv. alle de forskellige sks-koder og alle de
primære/ingen-angivede procedurer.

*Anonymisering*  
Kolonnerne med operationsID, patientID og kontaktID er erstattet af nye
tilfældigt genererede id. Nøglen gemmes ikke.

### Frasorteret data

Datagrundlaget på de 619.745 procedurer er blevet tildelt et
operationsID, hvor primærproceduren identificeres og udvælges. Akutte
patienter frasorteres, da disse ikke vil være relevante for
dagkirurgiske afsnit. For at skærpe fokus yderligere, frasorteres
sks-koder med mindre end 10 udførte primærprocedurer i 2018.

Det gradvist indsnævrede datagrundlag opsummeres her:

| Filtrering           | SKS-koder | Operationer | Procedurer |
| -------------------- | --------- | ----------- | ---------- |
| Alle procedurer      | 4.450     | 451.020     | 619.745    |
| Elektive kontakter   | 4.149     | 357.764     | 503.440    |
| Kun primærprocedurer | 3.555     | 351.247     | 351.247    |
| Mindst 10 procedurer | 1.471     | 344.985     | 344.985    |

**Elektive kontakter**  
Alle kontakter hvor indmaade == 2

**Identificering af primærproceduren**  
I DRG data er operationsarten for hver procedurer angivet som “P”
(primær), “D” (deloperation) eller tom (ikke udfyldt). De fiktive
operationsID kan have tilknyttet 0, 1 eller flere “P” procedurer. For de
357.764 elektive operationer fordeler operationerne sig
således:

| Antal operationer med | 0 primære      | 1 primær        | Flere primære |
| --------------------- | -------------- | --------------- | ------------- |
| 1 procedurer          | 31.795 (8,9%)  | 227.823 (63,7%) |               |
| Flere procedurer      | *6.524 (1,8%)* | 84.302 (23,6%)  | *7.320 (2%)*  |

Er der kun udført 1 procedure er denne den primære.  
For de 13.844 øvrige operationer med 0 eller flere primære procedurer er
det ofte den samme sks-kode som er udført flere gange. Fjerner vi
dubletter, ser fordelingen således ud:

| Antal operationer med | 1 primær        | Flere mulige primære |
| --------------------- | --------------- | -------------------- |
| 1 procedurer          | 259.618 (72,6%) |                      |
| Flere procedurer      | 91.629 (25,6%)  | *6.517 (1,8%)*       |

630 dublet-operationer har ikke en entydig producerende afdeling. For
disse operationer fremgår valget af afdeling i tabellen AFD\_VALG. Alle
frasorterede dubletter er gemt i tabellen ELEK\_P\_FRASORTERET med
“Dublet” angivet i kolonnen FrasortPGA. Procedurende hørende til de
6.517 operationsID med flere mulige primærprocedurer er ligeledes gemt i
FRASORTERET tabellen med “Ukendt P” i kolonnen FrasortPGA.
