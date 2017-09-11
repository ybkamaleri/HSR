## Hjertestansreg.
rm(list = ls())

source("datakilder01.R")

## Data ##
## regdata - data uten value label
## data.value - value label


library(data.table)
regdata <- setDT(regdata)
data.value <- setDT(data.value)
## rename col data.num
setnames(data.value, 1:ncol(data.value), paste(names(data.value), "v", sep = "_"))

## names(data.value)
## View(regdata)

reshID <- levels(data.value$ReshId_v)
reshID <- iconv(reshID, "latin1", "utf-8") #konvertere til utf-8
regdata[, ReshNavn := ReshId][, setattr(ReshNavn, "levels", reshID)]


##############
## Analyser
##############

## Antall per ReshID
regdata[, .N, by = list(ReshId, ReshNavn)]

## Filteret dataset
regdataink <- regdata[AnyBystander_CPR == 1 | HLRvedakuttmedisinskpersonell == 0,]

dim(regdataink)
regdataink[, .N, by = list(ReshId, ReshNavn)]

## Kjønn
regdataink[, gender := PatientGender]
regdataink$gender <- factor(regdataink$gender,
                            levels = c(1, 2),
                            labels = c("mann", "kvinne"))

regdataink[, .N, by = list(ReshNavn, gender)]
