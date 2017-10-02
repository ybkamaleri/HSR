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
## library('dplyr')
## library('tidyr')
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



###########################################
## Pasients alder basert på personnr og tiddato hendelse mottatt av AMK

## "DatoogtidhenvendelsemottatAMK" exclude time
amk <- grep("mottatAMK$", names(regdata), value = TRUE) #grep colnames
regdata[, dateamk := as.IDate(format(as.POSIXct(get(amk), format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"))] #henter bare dato

## Create AGE from "fdato" and "dateamk"
## 'fdato' hentes allerede fra personnr
regdata[, ageamk := age(fdato, dateamk)] #bruk function age(from,to)

## ## Another way to calculate age
## ## 1
## regdata[, ageamk := round(floor(difftime(dateamk, fdato, units = "days")) / 365.25)]

## ## hente bare dato ved hendelse
## regdata[, henddato := as.Date(format(as.POSIXct(DatoogtidhenvedelsemottatAMK, format = "%Y-%m-%d %H:%M"), "%Y-%m-%d"))]
## ## alder ved hendelse

## ## 2
## regdata[, agehs := floor(difftime(henddato, fdato, units = "days")/365.25)]

## ## 3
## regdata[, henddato1 := as.numeric(gsub("-", "", henddato))]
## regdata[, fdato1 := as.numeric(gsub("-", "", fdato))]
## regdata[, ageFrode := as.numeric((henddato1 - fdato1) / 10000)]



## Value for Kjønn
regdata[, gender := PatientGender]
regdata$gender <- factor(regdata$gender,
                         levels = c(1, 2),
                         labels = c("mann", "kvinne"))


#################################
### Antall befolkning for vekting
#################################

bfolk <- fread("~/Dropbox/OUS/HSR/Rapport2016/befolkning2016.csv", dec = ",")
regdata <- merge(regdata, bfolk, by.x = "ReshNavn", by.y = "V1")

setnames(regdata, old = "V2", new = "pop") #change var name to pop - population
regdata[, unique(pop), by = ReshNavn]

## Antall per ReshID
regdata[, .N, by = list(ReshId, ReshNavn)]

###########################
## Filteret data REG
###########################

reg <- regdata[AnyBystander_CPR == 1 | HLRvedakuttmedisinskpersonell == 0,]
## Antall pasienter
totalN <- dim(reg)[1]

#############################
#### Background analysis ####
#############################

## Antall N per HF og total populasjon
regHF <- reg[, list(n = .N,
                    pop = unique(pop)), #number of population
             by = list(ReshId, ReshNavn)]

## Antall per HF og kjønn
regGender <- reg[, .N, by = list(ReshNavn, gender)]


####################################
## person-year before cardiac arrest
## ========================
## Calculate number of days from 01.01.2017 to hendelse date
## regdata[, intid := as.numeric(difftime(as.IDate("2016-01-01"), dateamk, units="days"))]

reg[, indyear := as.numeric(age_calc(as.IDate("2016-01-01"), dateamk, units = "years"))]
## regdata[, inyear := intid / 365]


####== Generic data preparation stops here =====#####


#####################################################
## Incidence of cardiac arrest per 10,000 person-year - INDATA
#####################################################

## count person-year before cardiac arrest (healthy time before attack) and number of cases by HF
indata <- reg[, list(nyear = sum(indyear),
                     n = .N,
                     pop = unique(pop)
                     ),
              by = list(ReshId, ReshNavn)]

indata[, ReshId := as.numeric(ReshId)]
## how long the data has been collected i.e 0.8 = 8 month and 1.0 = 1 year
indata[,  year := ifelse(ReshId == 601047, 0.8, 1.0)]

## exposed population only
indata[, poph := pop - n]
## person-year including healthy year for cases ie. before cardiac arrest
indata[, nhealthyear := round(((pop - n) * year) + nyear, digits = 0)] #healthy year including healthy year for cases

## change to numeric
for (nn in names(indata)[3:7]) {
  set(indata, j = nn, value = as.numeric(indata[[nn]]))
}

## Tall for Norge (hele landet)
coln2 <- names(indata)[3:8]
irNorge <- indata[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), .SDcols = coln2]
irNorge[, ReshNavn := "Norge"][, ReshId := 99999]

inAlle <- rbind(indata, irNorge, fill = TRUE)


indata <- inAlle #rename to indata so I don't have to change all the code below

proph <- "nhealthyear"
indata[, cprop := n / get(proph)] #incident rate
## Cases per 10000
indata[, ir := cprop * 100000] #IR per 10000

## incidence rate with lower and upper bounds
prop <- "cprop"

## ## with continuity correction
## indata[, `:=` (lbw = ((get(prop) - 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) - ( 0.5 / get(proph))) * 100000)] #lower bound
## indata[, `:=` (ubw = ((get(prop) + 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) + ( 0.5 / get(proph))) * 100000)] #upper bound
## ## without continuity correction
## indata[, `:=` (lb = (get(prop) - 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #lower bound
## indata[, `:=` (ub = (get(prop) + 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #upper bound
## ## ## without continuity correction 99%
## ## indata[, `:=` (lb99 = (get(prop) - 2.576 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #lower bound
## ## indata[, `:=` (ub99 = (get(prop) + 2.576 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #upper bound

## incidence rate CI with poisson distribution http://epid.blogspot.no/2012/08/how-to-calculate-confidence-interval-of.html
## test the calculation here http://www.openepi.com/PersonTime1/PersonTime1.htm
indata[, irll := (cprop - 1.96 * cprop / sqrt(n)) * 100000] #lower limit
indata[, irul := (cprop + 1.96 * cprop / sqrt(n)) * 100000] #upper limit

## reduce digits showed
nrvar <- dim(indata)[2]
indatacol <- names(indata)[10:12]
for (var in indatacol) {
  set(indata, i = NULL, j = var, value = round(indata[[var]], digits = 0))
}


### Figure
maxx <- max(indata$irul, na.rm = TRUE)
inxx <- round(maxx / 5, digits = 0)
ftit <- "Forekomst av hjertestans"
fsub <- "(95% konfidensintervall)"
ytit <- "Antall per 100 000 personår"
xlabels <- seq(0, 100, 20)

figinc <- ggplot(indata, aes(reorder(ReshNavn, ir), ir)) +
  geom_errorbar(aes(ymax = irul, ymin = irll), width = 0.25, size = 0.4) +
  ##geom_point(size = 2, shape = 23, color = colb1, fill = colb1) +
  geom_label(aes(label = ir), size = 3,
             label.padding = unit(0.1, "lines"),
             ##label.r = unit(0.2, "lines"),
             label.size = 0,
             fill = "#c6dbef",
             color = "black") +
  geom_label(data = indata[indata$ReshId == 99999], aes(label = ir), size = 3,
             label.padding = unit(0.1, "lines"),
             ##label.r = unit(0.2, "lines"),
             label.size = 0,
             fill = colb2,
             color = "white",
             fontface = "bold") +
  labs(title = ftit, subtitle = fsub, y = ytit) +
  coord_flip() +
  scale_y_continuous(breaks = xlabels) +
  theme2 +
  theme(
    panel.grid.major.x = element_line(color = "grey", size = 0.1, linetype = 2),
    ##panel.grid.minor.x = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.major.y = element_line(color = "grey", size = 0.1, linetype = 1),
    axis.line.x = element_line(size = 0.3)
  )

### other option for geom_label - label.size = 0 means no line around


## save file generic
fig1 <- figinc
title <- "forekomstpersonyear100"

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

figTitle <- paste0("Aldersgrupper", " (N=", ageN, ")")

## Figure Age categories
ageAlle <- ggplot(regAge, aes(ageKat, N)) +
  geom_bar(stat='identity', fill = col1) +
  geom_text(aes(x = ageKat, y = txtpos, label = N), size = 3) +
  ## geom_text(data = regAge[pos == 1], aes(x = ageKat, y = N - 10, label = N), size = 3) +
  ## geom_text(data = regAge[pos == 0], aes(x = ageKat, y = N + 5, label = N), size = 3) +
  labs(title = figTitle, y = "Antall pasienter", x = "Pasientens alder") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme3


figAge1 <- ggplot_gtable(ggplot_build(ageAlle))
figAge1$layout$clip[figAge1$layout$name == 'panel'] <- 'off'
cowplot::save_plot("~/Git-work/HSR/rapport2016/fig/Alder.jpg", figAge1, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/rapport2016/fig/Alder.pdf", figAge1, base_height = 7, base_width = 7)
grid.draw(ageAlle)
dev.off()

################################
## Pyramid figure age and kjønn

ageGender <- reg[, .N, by = list(ageKat, gender)]
ageGender <- na.omit(ageGender, cols = "ageKat") #remove NA

ageGender[, fign := ifelse(gender == "mann", N * -1, N)] #lage negative value for menn (som skal være RHS)

figTitle <- "Antall pasienter fordelt på aldersgrupper og kjønn"
gap <- 20 #the gap for text btw the figures
rownr <- dim(ageGender)[1] / 2 #antall row for each gender
nman <- sum(ageGender$N[ageGender$gender == "mann"])
nwomen <- sum(ageGender$N[ageGender$gender == "kvinne"])

figgender <- ggplot(ageGender, aes(x = ageKat, color = gender)) +
  geom_linerange(data = ageGender[ageGender$gender == "mann", ],
                 aes(ymin = -gap, ymax = -gap + fign), size = 7) +
  geom_linerange(data = ageGender[ageGender$gender == "kvinne", ],
                 aes(ymin = gap, ymax = gap + fign), size = 7) +
  ## geom_text(data = ageGender[ageGender$gender == "mann", ],
  ##           aes(x = ageKat, y = fign + (-gap), label = N), color = "black") +
  ## geom_label(aes(x = ageKat, y = 0, label = ageKat),
  ##            inherit.aes = FALSE,
  ##            size = 4) +
  geom_text(aes(x = ageKat, y = 0, label = ageKat), inherit.aes = FALSE) +
  coord_flip() +
  scale_y_continuous(breaks = c(c(-250, -200, -150, -100, -50, 0) + -gap,
                                c(0, 50, 100, 150, 200, 250) + gap),
                     labels =  c(c("250", "200", "150", "100", "50", "0", "0", "50", "100", "150", "200", "250"))) +
  scale_color_manual(name = "", values = c(mann = colb1, kvinne = colb2)) +
  labs(title = figTitle, y = "", x = "") +
  annotate("text", x = rownr + 1, y = -gap + (-40), label = paste0("Mann (N=", nman, ")")) +
  annotate("text", x = rownr + 1, y = gap + 45, label = paste0("Kvinne (N=", nwomen, ")")) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", size = 0.5, color = "grey"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none"
  )


## save file generic
fig1 <- figgender
fileTitle <- "AgeGender"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", fileTitle, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", fileTitle, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


################################
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
  geom_text(data = ageAlle[ReshNavn != "Norge"], aes(y = ypos, label = sprintf("%1.1f", mean)), size = 3.5) +
  geom_text(data = ageAlle[ReshNavn == "Norge"], aes(y = ypos, label = sprintf("%1.1f", mean)), size = 3.5, color = "white") +
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


#############################
## Årsak til hjertestans
#############################

kol2 <- grep("rsaktilhjertestans$", colnames(reg), value = TRUE) #var "årsaktilhjertestans"

## rename to "why"
reg[, whyhs := as.integer(get(kol2))]

## recode 1=kardial og 2=annen  - ref https://stackoverflow.com/questions/44590935/recode-a-variable-using-data-table
reg[, whyhs := sapply(whyhs, function(x){
  ifelse(x != 0, 2, 1)

})]

## count reason for cardiac arrest pr HF
whyhs <- reg[, .N, by = .(ReshNavn, whyhs)]
## procent
whyhs[, total := sum(N), by = .(ReshNavn)]
whyhs[, pros := round((N / total) * 100, digits = 0)]

## hele landet
whynor <- reg[, .N, by = .(whyhs)][, ReshNavn := "Norge"]
whynor[, total := sum(N)][, pros := round((N / total) * 100, digits = 0)]

## Combine Norge and HF
whyAll <- rbindlist(list(whyhs, whynor), use.names = TRUE)

## include N in ReshNavn
whyAll[, fig := paste0(ReshNavn, " (N=", total, ")")]


## Figure for Antatt kardial = 1
whyfig <- whyAll[whyhs == 1, ]

## ## ta bort HF med cases < 5
## bortHF <- kollper$ReshId[kollper$N >= 5] #beholder bare de over og lik 5
## kollperfig <- kollper[ReshId %in% (bortHF)]

figtitle <- ""
ylab <- "Prosent (%)"

## text position
whyfig[, ypos := pros - (0.05 * max(pros))]


figwhy <- ggplot(whyfig, aes(x=reorder(fig, pros), y = pros)) +
  geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(aes(y = ypos, label = pros), size = 3.5) +
  geom_text(data = whyfig[whyfig$ReshNavn == "Norge"], aes(y = ypos, label = pros), size = 3.5, color = "white") +
  coord_flip() +
  ##guides(fill = FALSE) +
  labs(title = figtitle, y = ylab) +
  scale_fill_manual(values = col2, guide = 'none') +
  scale_y_continuous(expand = c(0,0)) +
  theme2


## save file generic
fig1 <- figwhy
filnavn <- "anttattKardialTilHertestans"

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
## norge[, pros := as.numeric(format(round(N / sum * 100), nsmall = 0))]
norge[, pros := as.numeric(round(N / sum * 100))]

### Kombinere begge datasett
kollalle <- rbindlist(list(kollHF, norge), use.names = TRUE)

## include N in HF names
kollalle[, fig := paste0(ReshNavn, " (N=", sum, ")")]


##########################========================
## Figure for Kollaps sett av Helse personell

## keep only kollaps sett av Akuttmedisinskpersonnell = 1
kollper <- kollalle[kollaps == 1]

## ## ta bort HF med cases < 5
## bortHF <- kollper$ReshId[kollper$N >= 5] #beholder bare de over og lik 5
## kollperfig <- kollper[ReshId %in% (bortHF)]

## title two lines
## figtitle <- paste("Kollaps hørt eller sett", "av ambulansepersonell", sep = "\n")
figtitle <- ""
ylab <- "Prosent (%)"

## text position
kollper[, ypos := pros - (0.05 * max(pros))]


fig3 <- ggplot(kollper, aes(x=reorder(fig, pros), y = pros)) +
  geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(aes(y = ypos, label = pros), size = 3.5) +
  geom_text(data = kollper[kollper$ReshId == 99999], aes(y = ypos, label = pros), size = 3.5, color = "white") +
  coord_flip() +
  ##guides(fill = FALSE) +
  labs(title = figtitle, y = ylab) +
  scale_fill_manual(values = col2, guide = 'none') +
  scale_y_continuous(expand = c(0,0)) +
  theme2



###### If annonimizing to be done
## ## N < 5 gi prosent == 0
## kollperfig <- kollper[, pros := ifelse(N < 6, 0, pros)]

## fig3 <- ggplot(kollperfig, aes(x=reorder(fig, pros), y = pros)) +
##   geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
##   geom_text(data = kollperfig[N > 5], aes(y = ypos, label = pros), size = 3.5) +
##   geom_text(data = kollperfig[N < 6], aes(y = 1.5, label = "n<6"), size = 3.5) +
##   ## geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),  width = .3, color = "blue",
##   ##               position = position_dodge(.9)) +
##   coord_flip() +
##   ##guides(fill = FALSE) +
##   labs(title = "", y = ylab) +
##   scale_fill_manual(values = col2, guide = 'none') +
##   scale_y_continuous(expand = c(0,0)) +
##   theme2

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

## ## ta bort cases < 6
## roscAlle1 <- roscAlle[, pros := ifelse(N < 6, 0, pros)]
## roscAlle1 <- roscAlle[, pros := ifelse(is.na(pros), 0, pros)]#when NA

fig4 <- ggplot(roscAlle, aes(x=reorder(ReshTxt, pros), y = pros)) +
  geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(data = roscAlle[pros != 0], aes(y = ypos, label = pros), size = 3.5) +
  geom_text(data = roscAlle[pros == 0], aes(y = 0.05 * max(roscAlle$pros), label = "n<6")) +
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
filnavn <- "UtsteinROSC"

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
filnavn <- "UtsteinROSC_pc"

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


###############################################
## FILTER 2 - HLR ved akuttmedisinskpersonnell - fil: filter2
###############################################

reg[, fil2 := as.numeric(reg$HLRvedakuttmedisinskpersonell)] #create new col for filter variable

setindex(reg, NULL) #remove
setindex(reg, fil2)

## setkey(reg, fil2)
## if setkey() is used then "on" isn't necessary for sorting temporarily. ie. setkey and on do the same thing
filter2 <- reg[.(0), on = "fil2"] #select only "JA"


#########################
### Vedvarende ROSC - fil: roscAlle
#########################
filter2[, rosc := ifelse(VedvarendeROSC == 0, 1, 2)] #recode JA to 1 else to 2

## Antall ROSC av de 2 kategorier
## but this doesn't include 0 when the sum is 0 f.eg Finnmarkssykehus
TESTroscHF <- filter2[, list(n = .N), by = list(rosc, ReshNavn, ReshId)]

## To include 0 if the sum is 0
roscHF <- setkey(filter2, rosc, ReshNavn)[CJ(unique(rosc), unique(ReshNavn)),
                                          list(n = .N), by = .EACHI]

## Sum ROSC per HF
roscHF[, tot := sum(n), by = .(ReshNavn)]

######### Not use cox the solution was found by using CJ and .EACHI above #############
## ### Easy solution but have to find out better way to test that every HF should have rosc 1 and 2
## ### if not then n = 0
## fin0 <- roscHF[ReshId == 601047]
## fin0[, rosc := 1][, n := 0] # values for column not specified will be copied

## ## include HF with 0 n
## roscHF <- rbindlist(list(fin0, roscHF), use.names = TRUE)


## percentage for rosc - ja and nei
roscHF[, pros := round((n / tot) * 100, digits = 1)]
## proportion for rosc - ja vs nei
roscHF[, del := n / tot] #proportion for calculating prop CI

roscHF[, ll := round((del - 1.96 * sqrt((del *( 1 - del)) / tot)) * 100, digits = 1)]
roscHF[, ul := round((del + 1.96 * sqrt((del *( 1 - del)) / tot)) * 100, digits = 1)]

### Norge
roscNo <- filter2[, list(n = .N), by = list(rosc)]
roscNo[, ReshNavn := "Norge"][, tot := sum(n)]
roscNo[, pros := round((n / tot) * 100, digits = 1)][, del := n / tot]

roscNo[, ll := round((del - 1.96 * sqrt((del *( 1 - del)) / tot)) * 100, digits = 1)]
roscNo[, ul := round((del + 1.96 * sqrt((del *( 1 - del)) / tot)) * 100, digits = 1)]


### Join the two HF and Norge

roscAll <- rbindlist(list(roscHF, roscNo), use.names = TRUE, fill = TRUE) #fill use NA when missing col


## include total number of patients in HF names
roscAll[, ReshNr := paste0(ReshNavn, " (N=", tot, ")")]

## ## When there is no cases of interest, this should be included as 0 case and 0 procent
## ## columns to change value when pros is 0
## colrs <- c("n", "tot", "pros", "del", "ll", "ul")

## roscAll[pros == 100, (colrs) := 0] #replace all in colrs to 0
## roscAll[pros == 0, rosc := 1] #replace rosc to rosc JA ie. 1

## keep only rosc == 1 (JA)
roscfig <- roscAll[rosc == 1]


### Figure
ftit <- paste0("Andel pasienter med vedvarende", "\n", "egensirkulasjon (ROSC)")
fsub <- "(95% konfidensintervall)"
ytit <- "Andel pasienter med ROSC (%)"
xlabels <- seq(0, 100, 10)

figrosc <- ggplot(roscfig, aes(reorder(ReshNr, pros), pros)) +
  geom_errorbar(aes(ymax = ul, ymin = ll), width = 0.25, size = 0.4) +
  ##geom_point(size = 2, shape = 23, color = colb1, fill = colb1) +
  geom_label(aes(label = pros), size = 3,
             label.padding = unit(0.1, "lines"),
             ##label.r = unit(0.2, "lines"),
             label.size = 0,
             fill = "#c6dbef",
             color = "black") +
  geom_label(data = roscfig[roscfig$ReshNavn == "Norge"], aes(label = pros), size = 3,
             label.padding = unit(0.1, "lines"),
             ##label.r = unit(0.2, "lines"),
             label.size = 0,
             fill = colb2,
             color = "white",
             fontface = "bold") +
  labs(title = ftit, subtitle = fsub, y = ytit) +
  coord_flip() +
  scale_y_continuous(breaks = xlabels) +
  theme2 +
  theme(
    panel.grid.major.x = element_line(color = "grey", size = 0.1, linetype = 2),
    ##panel.grid.minor.x = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.major.y = element_line(color = "grey", size = 0.1, linetype = 1),
    axis.line.x = element_line(size = 0.3)
  )

### other option for geom_label - label.size = 0 means no line around


## save file generic
fig1 <- figrosc
title <- "vedvarendeROSC"

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


######################
## Geom point and text
figroscpoint <- ggplot(roscfig, aes(reorder(ReshNr, pros), pros)) +
  geom_errorbar(aes(ymax = ul, ymin = ll), width = 0.25, size = 0.4) +
  geom_point(size = 2.5, shape = 21, color = colb1, fill = colb1) +
  geom_point(data = roscfig[roscfig$ReshNavn == "Norge"], size = 2.5, shape = 21, color = colb2, fill = colb2) +
  geom_text(data = roscfig[roscfig$ReshNavn != "Norge"], aes(label = pros), size = 3,
            vjust = -0.7, nudge_y = 0) +
  geom_text(data = roscfig[roscfig$ReshNavn == "Norge"], aes(label = pros), size = 3,
            vjust = -0.7, nudge_y = 0) +
  labs(title = ftit, subtitle = fsub, y = ytit) +
  coord_flip() +
  scale_y_continuous(breaks = xlabels) +
  theme2 +
  theme(
    panel.grid.major.x = element_line(color = "grey", size = 0.1, linetype = 2),
    ##panel.grid.minor.x = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.major.y = element_line(color = "grey", size = 0.1, linetype = 1),
    axis.line.x = element_line(size = 0.3)
  )


## save file generic
fig1 <- figroscpoint
title <- "vedvarendeROSCpoint"

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


#####################
## SPC ROSC

npro2 <- roscNo$n[roscNo$rosc == 1] / sum(roscNo$n)

## keep only relevant variables
roscspc <- roscHF[, c(1:5)]
roscspc[, p := npro2] #proportion defective for hele landet
roscspc[, pros := (n / tot) * 100]

## ## use function npro2 - for programming
## roscspc[, slll := npro2 - 3 * (sqrt(npro2 * (1 - npro2)) / n)]

roscspc[, sd := sqrt(p * (1 - p) / n)]
roscspc[, ll := p - 3 * sd]
roscspc[, ul := p + 3 * sd]
roscspc[, llp := round(ll * 100, digits = 2)]
roscspc[, ulp := round(ul * 100, digits = 2)]

## if ll less than 0 then ll should be 0
roscspc[ll < 0 | is.infinite(ll), ll := 0]
## if ub infinite then should be 100
roscspc[is.infinite(ul), ul := max(ul)]


roscspc[, sll := round((p - 3 * (sqrt(p * (1 - p)) / n)) * 100, digits = 2)]
roscspc[, sul := round((p + 3 * (sqrt(p * (1 - p)) / n)) * 100, digits = 2)]
roscspc[, snitpro := round(p * 100, digits = 2)]


## ## When there is no cases of interest, this should be included as 0 case and 0 procent
## ## columns to change value when pros is 0
## ncol <- dim(roscspc)[2]
## col0 <- names(roscspc)[6:ncol]

## roscspc[pros == 100, (col0) := 0] #replace all in colrs to 0
## roscspc[pros == 0, rosc := 1] #replace rosc to rosc JA ie. 1
## roscspc[pros == 0, n := 0] #if procent is 0 then n is 0

## include total number of patients in HF names
roscspc[, ReshNr := paste0(ReshNavn, " (N=", tot, ")")]

## keep only rosc == 1
roscspcfig <- roscspc[rosc == 1]

## order ReshNr by n
roscspcfig <- roscspcfig[, ReshNr := factor(ReshNr, levels = ReshNr[order(-n)])]
## maxrow <- dim(roscspcfig)[1]
## roscspcfig[, nr := 1:maxrow]
spctest <- ggplot(roscspcfig, aes(ReshNr, pros))
spctest + geom_ribbon(aes(ymin = llp, ymax = ulp), fill = "grey", color = "grey")+
  geom_point() +
  coord_flip()


library(qicharts2)
qic(x = ReshNr, y = n,
    n = tot,
    data = roscspcfig,
    chart = 'p',
    y.percent = TRUE,
    title = "",
    ylab = "",
    xlab = "",
    flip = TRUE)


########################
## Incidence for ROSC
########################

## Calculate number of cases per HF and include 0 when there is no cases
## To include 0 if the sum is 0
roscsum <- setkey(filter2, rosc, ReshNavn)[CJ(unique(rosc), unique(ReshNavn)),
                                           list(n = .N), by = .EACHI]

roscsum[, tot := sum(n), by = list(ReshNavn)]
roscJa <- roscsum[rosc == 1] # select only ROSC == 1 (JA)

## Caluclate person-year per HF
roscyear <- filter2[, list(nyear = sum(indyear), #sum person-year for healthy year
                           pop = unique(pop) #sum population. Use 'unique' to just get one per each HF
                           ),
                   by = list(ReshNavn, ReshId)]

## Join number of cases and person-year
indrosc <- merge(roscJa, roscyear, by = "ReshNavn")

indrosc[, ReshId := as.numeric(ReshId)]
## how long the data has been collected i.e 0.8 = 8 month and 1.0 = 1 year
indrosc[,  year := ifelse(ReshId == 601047, 0.8, 1.0)]

## total number of exposed population only (minus number of cases)
indrosc[, poph := pop - n]
## person-year including healthy year for cases ie. before cardiac arrest
indrosc[, nhealthyear := round(((pop - n) * year) + nyear, digits = 0)] #healthy year including healthy year for cases

## ## change to numeric
## for (nn in names(indrosc)[3:7]) {
##   set(indrosc, j = nn, value = as.numeric(indrosc[[nn]]))
## }


## Tall for Norge (hele landet)
coln3 <- c("n", "tot", "nyear", "pop", "year", "poph", "nhealthyear")
iroscNorge <- indrosc[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), .SDcols = coln3]
iroscNorge[, ReshNavn := "Norge"][, ReshId := 99999]

inrosccAlle <- rbind(indrosc, iroscNorge, fill = TRUE)


indrosc <- inrosccAlle #rename to indrosc so I don't have to change all the code below

proph <- "nhealthyear"
indrosc[, cprop := n / get(proph)] #incident rate
## Cases per 10000
indrosc[, ir := cprop * 100000] #IR per 100000

## incidence rate with lower and upper bounds
prop <- "cprop"

## ## with continuity correction
## indrosc[, `:=` (lbw = ((get(prop) - 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) - ( 0.5 / get(proph))) * 100000)] #lower bound
## indrosc[, `:=` (ubw = ((get(prop) + 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) + ( 0.5 / get(proph))) * 100000)] #upper bound
## ## without continuity correction
## indrosc[, `:=` (lb = (get(prop) - 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #lower bound
## indrosc[, `:=` (ub = (get(prop) + 1.96 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #upper bound
## ## ## without continuity correction 99%
## ## indrosc[, `:=` (lb99 = (get(prop) - 2.576 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #lower bound
## ## indrosc[, `:=` (ub99 = (get(prop) + 2.576 * sqrt(get(prop) * (1 - get(prop))) / get(proph)) * 100000)] #upper bound

## incidence rate CI with poisson distribution http://epid.blogspot.no/2012/08/how-to-calculate-confidence-interval-of.html
## test the calculation here http://www.openepi.com/PersonTime1/PersonTime1.htm
indrosc[, irll := (cprop - 1.96 * cprop / sqrt(n)) * 100000] #lower limit
indrosc[, irul := (cprop + 1.96 * cprop / sqrt(n)) * 100000] #upper limit

## reduce digits showed
## get the colnames for the last 5 columns
colnm <- tail(names(indrosc), n = 5)

## nrvar <- dim(indrosc)[2]
## indrosccol <- names(indrosc)[10:12]
for (var in colnm) {
  set(indrosc, i = NULL, j = var, value = round(indrosc[[var]], digits = 0))
}


### Figure
maxx <- max(indrosc$irul, na.rm = TRUE)
ftit <- "Forekomst av vedvarende egensirkulasjon"
fsub <- "(per HF, 95% konfidensintervall)"
ytit <- "Antall per 100 000 personår"
xlabels <- seq(0, maxx, 3)

figinrosc <- ggplot(indrosc, aes(reorder(ReshNavn, ir), ir)) +
  geom_errorbar(aes(ymax = irul, ymin = irll), width = 0.25, size = 0.4) +
  ##geom_point(size = 2, shape = 23, color = colb1, fill = colb1) +
  geom_label(aes(label = ir), size = 3,
             label.padding = unit(0.1, "lines"),
             ##label.r = unit(0.2, "lines"),
             label.size = 0,
             fill = "#c6dbef",
             color = "black") +
  geom_label(data = indrosc[indrosc$ReshId == 99999], aes(label = ir), size = 3,
             label.padding = unit(0.1, "lines"),
             ##label.r = unit(0.2, "lines"),
             label.size = 0,
             fill = colb2,
             color = "white",
             fontface = "bold") +
  labs(title = ftit, subtitle = fsub, y = ytit) +
  coord_flip() +
  scale_y_continuous(breaks = xlabels) +
  theme2 +
  theme(
    panel.grid.major.x = element_line(color = "grey", size = 0.1, linetype = 2),
    ##panel.grid.minor.x = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.major.y = element_line(color = "grey", size = 0.1, linetype = 1),
    axis.line.x = element_line(size = 0.3)
  )

### other option for geom_label - label.size = 0 means no line around


## save file generic
fig1 <- figinrosc
title <- "forekomstROSCpersonyear"

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






################################################
## Incidence based on Exel file
## (THIS CALCULATION IS NOT USED IN THE REPORT)
################################################

data2 <- fread("~/Dropbox/OUS/HSR/Rapport2016/forekomst2016.csv", dec = ",") #replace "," used in the dataset with "."

## give coloumn names
setnames(data2, c("hf", "pop", "hendelse", "year", "personyr", "per100tusen", "kollasp_tilstd", "in_koltilstd", "koll_medper", "in_kollper"))


###
## Function calculate CI prop

## lower bound
proplb <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out$lb
}

## upper bound
propub <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out$ub
}


### Convert to numeric
colnum = c("personyr", "per100tusen")

for (i in seq_along(colnum)) {
  set(data2, i = NULL, j = colnum[i], value = as.numeric(data2[[colnum[i]]]))
}

data2[, prop := hendelse / personyr]
data2[, prop104 := prop * 10000]

data2[, `:=` (lb = ((prop - 1.96 * sqrt(prop * (1 - prop)) / personyr) - ( 0.5 / personyr)) * 10000)]
data2[, `:=` (ub = ((prop + 1.96 * sqrt(prop * (1 - prop)) / personyr) + ( 0.5 / personyr)) * 10000)]
