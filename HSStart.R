#################################################
##  Ctrl+A (Velge alt) og så Ctrl+R (Kjøre koden)
#################################################

rm(list=ls(all = T))

### OBS!!!
### Valg riktig sti til datasettet i CSV fil
### Bruk Forward Slash "/" og IKKE backslash "\"
setwd("~/OUS/Hjertestans/Rapport/")
libkat <- "~/OUS/Hjertestans/Rapport/"
RegData <- read.csv2("anonymt20150903v3.csv", header = TRUE, fileEncoding='latin1')

##############################################
###--Tallene/Teksten kan endres etter behov---

outfile <- "" #skrev hva filen skal heter f.eks "Alder.pdf"

minAlder <- 0   #minimum alder
maxAlder <- 150 #maximum alder

#########################
## Skal sammenlignes?
#########################

ReshID <- 100021 

Resultat <- 0

## 0 - hele landet
## 1 - egen HF mot resten av landet
## 2 - egen HF

#############################
## Valg variabel fra listen
##############################

Variabel <- "Overlevelse30d"  #skrev utvalgte variabel med " "

## Variablene å velge mellom er:-
## Gender, Alder, FirstRytme, Observert, HLRbefore, Tidsbruk, Vedvarende_ROSC, Overlever24h, Overlevelse30d

##########################
## IKKE endre noe herfra
#########################
source("HSFigAndeler.R", encoding = "UTF-8")
valgtVar <- Variabel
FigAndeler(RegData = RegData, valgtVar = valgtVar,
           libkat = libkat, ReshID = ReshID, outfile = outfile,
           minAlder = minAlder, maxAlder = maxAlder)

