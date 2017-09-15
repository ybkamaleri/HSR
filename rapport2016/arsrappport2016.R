## Hjertestansreg.
rm(list = ls())

source("datakilder01.R")

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

reshID <- levels(data.value$ReshId_v)
reshID <- iconv(reshID, "latin1", "utf-8") #konvertere til utf-8
regdata[, ReshNavn := ReshId][, setattr(ReshNavn, "levels", reshID)]



#########################
## Felles funksjon

library('ggplot2')
library('dplyr')
library('tidyr')
library('gridExtra')
library('grid')
library('cowplot')

## Theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"))

## Theme uten title axis-y
theme2 <- theme_bw() +
  theme(
    axis.text = element_text(size = 10), #text for y and x axis
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(size = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12),
    plot.margin = unit(c(0, 2, 1,1), 'cm'),
    plot.title = element_text(size = 14),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())


## Theme med title axis-y
theme3 <- theme_bw() +
  theme(
    panel.grid.major.x = element_line(colour = "grey", size = 0.4),
    panel.grid.minor.x = element_line(color = "grey", size = 0.2, linetype = 2),
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 14),
    plot.margin = unit(c(0, 2, 1,1), 'cm'),
    panel.border = element_blank(),
    axis.text = element_text(size = 10),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0.5),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12))

## Farge
col1 <- "#6DAED6" # #28f
cols <- c("#c6dbef", "#6baed6","#4292c6", "#2171b5", "#084594", "#000059")
col2 <- c("#6baed6","#0845ff")
col4 <- c("#c6dbef", "#6baed6", "#2171b5", "#084594")

## Telling HF
telling <- function(data, by1 , by2) {
  library(dplyr)

  dataN <- data %>%
    group_by_(by1) %>%
    tally %>%
    mutate(pct = n/sum(n) * 100) %>%
    mutate(HF = "Norge")

  dataHF <- data %>%
    group_by_(by2, by1) %>%
    tally %>%
    mutate(pct = n/sum(n) * 100)

  dataAll <- full_join(dataN, dataHF)

  return(dataAll)
}

## Telling RHF
tellingRHF <- function(data, by1 , by2) {
  library(dplyr)

  dataN <- data %>%
    group_by_(by1) %>%
    tally %>%
    mutate(pct = n/sum(n) * 100) %>%
    mutate(RHF1 = "Norge")

  dataRHF <- data %>%
    group_by_(by2, by1) %>%
    tally %>%
    mutate(pct = n/sum(n) * 100)

  dataAll <- full_join(dataN, dataRHF)

  return(dataAll)
}

## Hvor skal figurene lages?
savefig <- "~/Git-work/HSR/rapport2016/fig"


##############
## Analyser
##############

## Antall per ReshID
regdata[, .N, by = list(ReshId, ReshNavn)]

## Filteret dataset
reg <- regdata[AnyBystander_CPR == 1 | HLRvedakuttmedisinskpersonell == 0,]
## Antall pasienter
totalN <- dim(reg)[1]

## Antall N per HF
regHF <- reg[, .N, by = list(ReshId, ReshNavn)]

## Kjønn
reg[, gender := PatientGender]
reg$gender <- factor(reg$gender,
                     levels = c(1, 2),
                            labels = c("mann", "kvinne"))

## Antall per HF og kjønn
regGender <- reg[, .N, by = list(ReshNavn, gender)]



#######################################
## Pasients alder

## Konvertere til numeric
nr <- c("PatientAge")
for (i in seq_along(nr)) {
  set(reg, i = NULL, j = nr[i], value = as.numeric(reg[[nr[i]]]))
}


##  Alder del i kategorier
alder.kat <- function(x, lower, upper, by,
                      sep = "-") {
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, "+", sep = ""))
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      include.lowest = TRUE, right = FALSE, labels = labs)
}

reg[, ageKat := alder.kat(PatientAge, 0, 100, 5)]
regAge <- reg[, .N, by = .(ageKat)]


## Position for text when N > 40
regAge[, pos := ifelse(N > 40, 1, 0)]

title <- " "

## Figure Age categories
  ageAlle <- ggplot(regAge, aes(ageKat, N)) +
    geom_bar(stat='identity', fill = col1) +
    geom_text(data = regAge[pos == 1], aes(x = ageKat, y = N - 10, label = N), size = 3) +
    geom_text(data = regAge[pos == 0], aes(x = ageKat, y = N + 5, label = N), size = 3) +
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



## Mean age med HF
ageHF <- reg[, list(mean = mean(PatientAge),
                    n = .N,
                    sd = sd(PatientAge)), by = ReshNavn]
## Norge
ageN <- reg[, .( mean = mean(PatientAge),
                ReshNavn = "Norge",
                n = .N,
                sd = sd(PatientAge))]

## Merge both
ageAlle <- data.table::rbindlist(list(ageN, ageHF), use.names = TRUE)
## alt. to use rbind if position for colnames not at the same position

title <- " "

fig1 <- ggplot(ageAlle, aes(x=reorder(ReshNavn, mean), y = mean)) +
    geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
    geom_text(aes(y = 5, label = paste0(sprintf("%1.1f", mean))), size = 3.5) +
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


############################## =================================
## Kollaps hørt eller sett av

## if problem with locale when running "grep" then run this
Sys.setlocale(locale = "C")

koll <- grep("*rtellersettav$", colnames(reg), value = TRUE)
## rename variable to 'kollaps'
reg[, kollaps := get(koll)]

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


## Endre tilbake til norsk locale for å få norske bokstaver
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")
kollv$value <- iconv(kollv$value, "utf-8", "latin1")

## include N in the value name
kollv[, fig:=paste0(value, " (N=", n, ")")]

title <- " "

fig2 <- ggplot(kollv, aes(fig, pro)) +
    geom_bar(stat = 'identity', fill = col1) +
    geom_text(data = kollv, aes(y = pro + 2, label = pro), size = 3) +
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


#################### ===================================
## Kollaps hørt eller sett av per HF

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
kollalle[, fig := paste0(ReshNavn, " (N=", N, ")")]


##########################
## Kollaps sett av Helse personell

## keep only kollaps sett av Akuttmedisinskpersonnell = 1
kollper <- kollalle[kollaps == 1]

## ta bort HF med cases < 5
bortHF <- kollper$ReshId[kollper$N >= 5] #beholder bare de over og lik 5
kollperfig <- kollper[ReshId %in% (bortHF)]

title <- " "
ylab <- "Prosent (%)"


fig3 <- ggplot(kollperfig, aes(x=reorder(fig, pros), y = pros)) +
  geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(aes(y = pros + 1, label =  paste0(pros, "%")), size = 3.5) +
  ## geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),  width = .3, color = "blue",
  ##               position = position_dodge(.9)) +
  coord_flip() +
  ##guides(fill = FALSE) +
  labs(title = title, y = ylab, caption = "HF med pasienter < 6 er eksludert") +
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
title <- "settAmbulanse"

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


##########################==============##################
### Utsteinkomparatorgruppe - Vedvarende ROSC

Sys.setlocale(locale = "C") #need to change to C lang to be able to use grep coz of text format
var02 <- grep("rsaktilhjertestans", colnames(reg)) #get index

## Bruk filter som Ingvild har gjort i SPSS
fil02 <- grep("^filter_", colnames(reg), value = TRUE)
reg[, filrosc := as.factor(get(fil02))]

Sys.setlocale(locale = "") #set back to default
