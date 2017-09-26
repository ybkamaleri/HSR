## Hjertestansreg.
rm(list = ls())

source("datakilder01.R")
source("setuphrs2016.R")
## Data ##
## regdata - data uten value label
## data.value - value label

## options(encoding = "UTF-8")


library(data.table)
regdata <- setDT(regdata)
data.value <- setDT(data.value)

## ## rename col data.num
## setnames(data.value, 1:ncol(data.value), paste(names(data.value), "v", sep = "_"))


## names(data.value)
## View(regdata)

## reshID <- levels(data.value$ReshId_v)
## reshID <- iconv(reshID, "latin1", "utf-8") #konvertere til utf-8
## regdata[, ReshNavn := ReshId][, setattr(ReshNavn, "levels", reshID)]



#########################
## Felles funksjon

library('ggplot2')
library('dplyr')
library('tidyr')
library('gridExtra')
library('grid')
library('cowplot')

## Hvor skal figurene lages?
savefig <- "~/Git-work/HSR/rapport2016/fig"


##############
## Analyser
##############

## Convert to utf-8 for HF navn
regdata$ReshNavn <- iconv(regdata$ReshNavn, 'latin1', 'utf-8')


## "DatoogtidhenvendelsemottatAMK" exclude time
amk <- grep("mottatAMK$", names(regdata), value = TRUE)
regdata[, dateamk := as.IDate(format(as.POSIXct(get(amk), format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"))]

## Create AGE from "fdato" and "dateamk"
regdata[, ageamk := round(floor(difftime(dateamk, fdato, units = "days")) / 365.25)]

## Value for Kjønn
regdata[, gender := PatientGender]
regdata$gender <- factor(regdata$gender,
                         levels = c(1, 2),
                         labels = c("mann", "kvinne"))


## Antall per ReshID
regdata[, .N, by = list(ReshId, ReshNavn)]

## Filteret dataset
reg <- regdata[AnyBystander_CPR == 1 | HLRvedakuttmedisinskpersonell == 0,]
## Antall pasienter
totalN <- dim(reg)[1]

## Antall N per HF
regHF <- reg[, .N, by = list(ReshId, ReshNavn)]

## Antall per HF og kjønn
regGender <- reg[, .N, by = list(ReshNavn, gender)]


###########################################
## Pasients alder ved hendelse mottatt AMK

## hente bare dato ved hendelse
regdata[, henddato := as.Date(format(as.POSIXct(DatoogtidhenvedelsemottatAMK, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d"))]
## alder ved hendelse
regdata[, agehs := floor(difftime(henddato, fdato, units = "days")/365.25)]


#######################################
## Pasients alder

## Konvertere til numeric
nr <- "ageamk"
for (i in seq_along(nr)) {
  set(reg, i = NULL, j = nr[i], value = as.numeric(reg[[nr[i]]]))
}

## alder.kat funksjon lages i setup
reg[, ageKat := alder.kat(get(nr), 0, 100, 5)]
regAge <- reg[, .N, by = .(ageKat)]
regAge <- na.omit(regAge, cols = "ageKat") #exclude NA
ageN <- sum(regAge$N) #sum exclude NA
ageNA <- reg[is.na(fdato), .N] #sum NA fdato


## Position for text when N > 30
regAge[, pos := ifelse(N > 30, 1, 0)]
regAge[pos == 1, txtpos := N - 0.03 * max(N)] #text position inside bar
regAge[pos == 0, txtpos := N + (0.2 * max(N))] #text position outside bar

title <- paste0("Aldersfordeling", " (N=", ageN, ")")

## Figure Age categories
ageAlle <- ggplot(regAge, aes(ageKat, N)) +
  geom_bar(stat='identity', fill = col1) +
  geom_text(aes(x = ageKat, y = txtpos, label = N), size = 3) +
  ## geom_text(data = regAge[pos == 1], aes(x = ageKat, y = N - 10, label = N), size = 3) +
  ## geom_text(data = regAge[pos == 0], aes(x = ageKat, y = N + 5, label = N), size = 3) +
  labs(title = title, y = "Antall pasienter", x = "Pasientens alder") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme3


figAge1 <- ggplot_gtable(ggplot_build(ageAlle))
figAge1$layout$clip[figAge1$layout$name == 'panel'] <- 'off'
cowplot::save_plot("~/Git-work/HSR/rapport2016/fig/Alder.jpg", figAge1, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/rapport2016/fig/Alder.pdf", figAge1, base_height = 7, base_width = 7)
grid.draw(ageAlle)
dev.off()

##################
## Pyramid figure age and kjønn








## Mean age med HF
ageHF <- reg[, list(mean = mean(get(nr), na.rm = TRUE),
                    n = .N,
                    sd = sd(get(nr), na.rm = TRUE)), by = ReshNavn]
## Norge
ageN <- reg[, .( mean = mean(get(nr), na.rm = TRUE),
                ReshNavn = "Norge",
                n = .N,
                sd = sd(get(nr), na.rm = TRUE))]

## Merge both
ageAlle <- data.table::rbindlist(list(ageN, ageHF), use.names = TRUE)
## alt. to use rbind if position for colnames not at the same position

## Y text position
ageAlle[, ypos := 0.06 * max(mean)]

title <- " "

fig1 <- ggplot(ageAlle, aes(x=reorder(ReshNavn, mean), y = mean)) +
    geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(data = ageAlle[ReshNavn != "Norge"], aes(y = ypos, label = paste0(sprintf("%1.1f", mean))), size = 3.5) +
  geom_text(data = ageAlle[ReshNavn == "Norge"], aes(y = ypos, label = paste0(sprintf("%1.1f", mean))), size = 3.5, color = "white") +
  ## geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),  width = .3, color = "blue",
  ##               position = position_dodge(.9)) +
  coord_flip() +
  ##guides(fill = FALSE) +
  labs(title = title, y = "Gjennomsnitt alder") +
  scale_fill_manual(values = col2, guide = 'none') +
  scale_y_continuous(expand = c(0,0)) +
  theme2

fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste(savefig, "AlderMean.jpg", sep = "/"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste(savefig, "AlderMean.pdf", sep = "/"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()


###############################
## Kollaps hørt eller sett av
###############################

## if problem with locale when running "grep" then run this
Sys.setlocale(locale = "C")

koll <- grep("*rtellersettav$", colnames(reg), value = TRUE) #name

## rename variable to 'kollaps'
reg[, kollaps := get(koll)]

## set back locale to default
Sys.setlocale(locale = "")

## recode 'ikke valgt' and 'Ukjent' to 999
reg[list(kollaps = c(-1, 999), to = 999), on = "kollaps", kollaps := i.to]

## count for each category of kollaps
kollv <- reg[, list(n = .N),  by = kollaps]
## Get prosent
kollv[, sum := sum(n)][, pro := as.numeric(format(round(n / sum * 100), nsmall = 0))] #ingen decimal
## give value names
kollv$value <- factor(kollv$kollaps,
                      levels = c(0, 1, 99, 999),
                      labels = c("Tilstedeværende", "Akuttmedisinsk personell", "Ingen", "Ukjent"))


## ## Endre tilbake til norsk locale for å få norske bokstaver
## Sys.setlocale("LC_ALL", "nb_NO.UTF-8")
## kollv$value <- iconv(kollv$value, "utf-8", "latin1")


## Lage figur ======================

## include N in the value name
kollv[, fig:=paste0(value, " (N=", n, ")")]

title <- " "
## text position
kollv[, ypos := pro + (0.05 * max(pro))]

fig2 <- ggplot(kollv, aes(fig, pro)) +
  geom_bar(stat = 'identity', fill = col1) +
  geom_text(data = kollv, aes(y = ypos, label = pro), size = 3) +
  labs(title = title, y = "Prosent", x = "") +
  coord_flip() +
  theme2


## save file generic
fig1 <- fig2
title <- "Kollaps"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


########################################
## Kollaps hørt eller sett av per HF
########################################

## antall per kollpas category er HF
kollHF <- reg[, .N, by = list(ReshId, ReshNavn, kollaps)]

## Antall kollaps per HF
kollHF[, sum := sum(N), by = .(ReshId)]

## Prosent hver kollaps kategori
kollHF[, pros := as.numeric(format(round(N / sum * 100), nsmall = 0))]

## Hele landet
nor <- "Norge"
norge <- reg[, .N, by = .(kollaps)]
norge[, sum := sum(N)]
norge[, ReshId := 99999]
norge[, ReshNavn := (nor)]
norge[, pros := as.numeric(format(round(N / sum * 100), nsmall = 0))]

### Kombinere begge datasett
kollalle <- rbindlist(list(kollHF, norge), use.names = TRUE)

## include N in HF names
kollalle[, fig := paste0(ReshNavn, " (N=", sum, ")")]


##########################========================
## Kollaps sett av Helse personell

## keep only kollaps sett av Akuttmedisinskpersonnell = 1
kollper <- kollalle[kollaps == 1]

## ## ta bort HF med cases < 5
## bortHF <- kollper$ReshId[kollper$N >= 5] #beholder bare de over og lik 5
## kollperfig <- kollper[ReshId %in% (bortHF)]

title <- " "
ylab <- "Prosent (%)"

## text position
kollper[, ypos := pros + (0.05 * max(pros))]

## N < 5 gi prosent == 0
kollperfig <- kollper[, pros := ifelse(N < 6, 0, pros)]

fig3 <- ggplot(kollperfig, aes(x=reorder(fig, pros), y = pros)) +
  geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(data = kollperfig[N > 5], aes(y = ypos, label = pros), size = 3.5) +
  geom_text(data = kollperfig[N < 6], aes(y = 1.5, label = "n<6"), size = 3.5) +
  ## geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),  width = .3, color = "blue",
  ##               position = position_dodge(.9)) +
  coord_flip() +
  ##guides(fill = FALSE) +
  labs(title = "", y = ylab) +
  scale_fill_manual(values = col2, guide = 'none') +
  scale_y_continuous(expand = c(0,0)) +
  theme2

## ## Add notes
## library(grid)
## library(gridExtra)
## grid.newpage()
## footnote <- "HF med < 5 er eksludert"
## fig3 <- arrangeGrob(fig3, buttom = textGrob(footnote, x = 0, hjust = -0.1, vjust = 0.1, gp = gpar(fontface = "italic", fontsize = 9)))
## grid.draw(fig3)

## save file generic
fig1 <- fig3
filnavn <- "KollapsSettAvAmbulanse"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", filnavn, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", filnavn, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


##########################==============##################
### Utsteinkomparatorgruppe - Vedvarende ROSC

Sys.setlocale(locale = "C") #need to change to C lang to be able to use grep coz of text format

## grep index "Årsaktilhjertestans"
var02 <- grep("rsaktilhjertestans", names(reg), value = TRUE) #name
## rename
reg[, arsak_hs := get(var02)]

## filter variable som Ingvild laget i SPSS
var03 <- grep("^filter", names(reg), value = TRUE)
reg[, filter := get(var03)] #rename to filter

Sys.setlocale(locale = "") #set back to default


## recode to factor
setindex(reg, NULL) #unset previous index
reg[, filter := as.factor(filter)]
setindex(reg, filter)
indices(reg)

##### per HF
## ROSC == JA
rosc1 <- reg[filter == 1 & VedvarendeROSC == 0, .N, by = ReshNavn]
setkey(rosc1, ReshNavn)
## ROSC er JA og Nei - Ikke valgt og Ukjent eksludert
rosc2 <- reg[filter == 1 & VedvarendeROSC %in% c(0, 1), list(sum = .N), by = ReshNavn]
setkey(rosc2, ReshNavn)

##merge both
roscHF <- rosc1[rosc2]

## merge(rosc1, rosc2, all = TRUE) #alternative

### Norge
## ROSC == JA
rosc3 <- reg[filter == 1 & VedvarendeROSC == 0, list( N = .N,
                                                     ReshNavn = "Norge")]
setkey(rosc3, ReshNavn)
## ROSC er JA og Nei - Ikke valgt og Ukjent eksludert
rosc4 <- reg[filter == 1 & VedvarendeROSC %in% c(0, 1), list(sum = .N,
                                                             ReshNavn = "Norge")]
setkey(rosc4, ReshNavn)

roscNorge <-rosc3[rosc4]

## Row bind HF og Norge
roscAlle <- rbindlist(list(roscHF, roscNorge), use.names = TRUE)

## Prosent
roscAlle[, pros := format(round(N / sum * 100))]

## If sum <6 prosent is 0
roscAlle[, pros := ifelse(sum < 6, 0, pros)]

## Reorder prosent with highst on the top
roscAlle <- roscAlle[order(-pros)]

roscAlle[, ReshTxt := paste0(ReshNavn, " (N=", sum, ")")]
## N < 6
roscAlle[, ReshTxt := ifelse(pros != 0, ReshTxt, paste0(ReshNavn, " (N < 6)"))]
## ## Delete N if N < 6
## roscAlle[, ReshTxt := ifelse(sum < 6, paste0(ReshNavn), ReshTxt)]

#####
## Figure

roscAlle[, pros := lapply(.SD, function(x) as.integer(x)), .SDcol = "pros"]

## text position
roscAlle[, ypos := pros + (0.05 * max(pros))]

## ta bort cases < 6
roscAlle1 <- roscAlle[, pros := ifelse(N < 6, 0, pros)]
roscAlle1 <- roscAlle[, pros := ifelse(is.na(pros), 0, pros)]#when NA

fig4 <- ggplot(roscAlle1, aes(x=reorder(ReshTxt, pros), y = pros)) +
  geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(data = roscAlle1[pros != 0], aes(y = ypos, label = pros), size = 3.5) +
  geom_text(data = roscAlle1[pros == 0], aes(y = 0.05 * max(roscAlle1$pros), label = "n<6")) +
  ## geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),  width = .3, color = "blue",
  ##               position = position_dodge(.9)) +
  coord_flip() +
  ##guides(fill = FALSE) +
  labs(title = "", y = ylab) +
  scale_fill_manual(values = col2, guide = 'none') +
  scale_y_continuous(expand = c(0,0)) +
  theme2

## ## Add notes
## library(grid)
## library(gridExtra)
## grid.newpage()
## footnote <- "HF med < 5 er eksludert"
## fig3 <- arrangeGrob(fig3, buttom = textGrob(footnote, x = 0, hjust = -0.1, vjust = 0.1, gp = gpar(fontface = "italic", fontsize = 9)))
## grid.draw(fig3)

## save file generic
fig1 <- fig4
filnavn <- "ROSC"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", filnavn, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", filnavn, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Prosess control
library(qicharts2)

## exclude Norge (gjennonsnitt) og N < 6
pcROSC <- roscAlle[ReshNavn != "Norge"] #tar bort Norge
pcROSC <- pcROSC[!is.na(N)]
pcROSC$ReshNavn <- factor(pcROSC$ReshNavn, levels = pcROSC$ReshNavn[order(-pcROSC$sum)])


fig5 <- qic(x = ReshNavn, y = N,
            n = sum,
            data = pcROSC,
            chart = 'p',
            y.percent = TRUE,
            title = "",
            ylab = "",
            xlab = "",
            flip = TRUE)

## lage prosess control fig

## save file generic
fig1 <- fig5
filnavn <- "prosessControlROSC"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", filnavn, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", filnavn, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL
