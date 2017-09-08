## Prepare dataset

## ## Hvis rawdata skal hentes fra SPSS fil
## file.choose()
## rawdata <- "K:\\Sensitivt\\kvalitetsregistre\\2011-21096_Norsk_hjertestansregister\\Årsrapport2016\\Årsrapport2016_14082017.sav"
## kpath <- "K:\\Sensitivt\\kvalitetsregistre\\2011-21096_Norsk_hjertestansregister\\Yusman\\"

## library(foreign)
## data.value <- read.spss(rawdata, to.data.frame = TRUE, use.value.labels = TRUE)
## regdata <- read.spss(rawdata, to.data.frame = TRUE, use.value.labels = FALSE)


## Valg variable for annonymesering
avid <- grep("dselsnummer", names(regdata), value = TRUE)
regdata[, (avid) := NULL]
data.value[, grep("dselsnummer", colnames(data.value)) := NULL]

saveRDS(regdata, file = paste0(kpath, "regdata.RDS"))
saveRDS(data.value, file = paste0(kpath, "data.value.RDS"))
