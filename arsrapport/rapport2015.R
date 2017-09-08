##########################
### HSR årsrapport 2015

rm(list=ls())
setwd('~/Dropbox/OUS/HSR/')
regdata <- read.csv2('HSR2015.csv', encoding = 'latin1', stringsAsFactors = FALSE)

resh1 <- read.csv2('reshkode.csv', encoding = 'UTF8', stringsAsFactors = FALSE)
resh1 <- na.omit(resh1)
colnames(resh1) <- c("HF","ReshId")
resh <- dplyr::filter(resh1, HF!="Akershus universitetssykehus")
##colnames(resh)[c(1,2)] <- c("HF","ReshId")

## Pakke nødvending
inspak <- function(pkg){
    nypkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(nypkg))
        install.packages(nypkg, dependencies = TRUE)
}

pakke <- c("dplyr", "ggplot2", "tidyr", "gridExtra", "grid", "cowplot")
inspak(pakke)

library('ggplot2')
library('dplyr')
library('tidyr')
library('gridExtra')
library('grid')
library('cowplot')

## White space
trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

regdata$ReshId <- trim(regdata$ReshId)
resh$ReshId <- trim(resh$ReshId)

## ReshID merge
## reshNot <- dplyr::filter(resh, ReshId != 601047 & ReshId !=105253)
reg1 <- merge(regdata, resh, by = "ReshId") #beholde bare regdata
reg2 <- left_join(regdata, resh)

## HF
regdata$HF[regdata$ReshId == 107857] <- "Universitetssykehuset i Nord-Norge HF"
regdata$HF[regdata$ReshId == 601047] <- "Helse Finnmark HF" #0 i 2015
regdata$HF[regdata$ReshId == 601067] <- "Nordlandssykehuset HF"
regdata$HF[regdata$ReshId == 102145] <- "Helgelandssykehuset HF"
regdata$HF[regdata$ReshId == 105851] <- "Helse Nord-Trøndelag"
regdata$HF[regdata$ReshId == 4204998] <- "Helse Sør-Trøndelag"
regdata$HF[regdata$ReshId == 706083] <- "Helse Møre og Romsdal HF"
regdata$HF[regdata$ReshId == 105253] <- "Helse Førde" #0 i 2015
regdata$HF[regdata$ReshId == 112044] <- "Helse Bergen"
regdata$HF[regdata$ReshId == 103097] <- "Helse Fonna"
regdata$HF[regdata$ReshId == 701316] <- "Helse Stavanger"
regdata$HF[regdata$ReshId == 4207374] <- "Sørlandet sykehus"
regdata$HF[regdata$ReshId == 111823] <- "Sykehuset i Vestfold"
regdata$HF[regdata$ReshId == 102030] <- "Sykehuset Telemark"
regdata$HF[regdata$ReshId == 4204082] <- "Vestre Viken HF"
regdata$HF[regdata$ReshId == 108361] <- "Sykehuset innlandet HF"
regdata$HF[regdata$ReshId == 700330] <- "Oslo universitetssykehus HF"
regdata$HF[regdata$ReshId == 4208037] <- "Sykehuset Østfold"

## RHF
regdata$RHF[regdata$ReshId == 601047] <- 1
regdata$RHF[regdata$ReshId == 107857] <- 1
regdata$RHF[regdata$ReshId == 601067] <- 1
regdata$RHF[regdata$ReshId == 102145] <- 1
regdata$RHF[regdata$ReshId == 105851] <- 2
regdata$RHF[regdata$ReshId == 4204998] <- 2
regdata$RHF[regdata$ReshId == 706083] <- 2
regdata$RHF[regdata$ReshId == 112044] <- 3
regdata$RHF[regdata$ReshId == 103097] <- 3
regdata$RHF[regdata$ReshId == 701316] <- 3
regdata$RHF[regdata$ReshId == 4207374] <- 4
regdata$RHF[regdata$ReshId == 111823] <- 4
regdata$RHF[regdata$ReshId == 102030] <- 4
regdata$RHF[regdata$ReshId == 4204082] <- 4
regdata$RHF[regdata$ReshId == 108361] <- 4
regdata$RHF[regdata$ReshId == 700330] <- 4
regdata$RHF[regdata$ReshId == 4208037] <- 4

regdata$RHF1 <- factor(regdata$RHF, levels = 1:4,
                      labels = c("Helse Nord", "Helse Midt-Norge", "Helse Vest", "Helse Sør-Øst"))


#########################
## Felles funksjon

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


theme2 <- theme_bw() +
    theme(
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        plot.margin = unit(c(0, 2, 1,1), 'cm'),
        plot.title = element_text(size = 13),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

## Farge
cols <- c("#c6dbef", "#6baed6","#4292c6", "#2171b5", "#084594", "#000059")
col2 <- c("#6baed6","#084594")
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



########################################
## Filter data for inklusjonskriterier:
########################################
## HLR utført av tilstedeværende "JA" og HLR ved akkuttmedpersonell "JA"

reg <- dplyr::filter(regdata, CprByst == 0 | CprEms == 0)



#######################################
## Pasients alder

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

reg$ageKat <- alder.kat(reg$PatientAge, 0, 100, 5)
table(reg$ageKat)

regAge <- reg %>%
    group_by(ageKat) %>%
    tally

ageAlle <- ggplot(regAge, aes(ageKat, n)) +
    geom_bar(stat='identity', fill = "#6baed6") +
labs(title = "Aldersfordeling", y = "Antall pasienter", x = "Pasientens alder") +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() + theme_bw() +
    theme(
        panel.grid.major.x = element_line(colour = "grey", size = 0.4),
        panel.grid.minor.x = element_line(color = "grey", size = 0.2, linetype = 2),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12))


figAge1 <- ggplot_gtable(ggplot_build(ageAlle))
figAge1$layout$clip[figAge1$layout$name == 'panel'] <- 'off'
cowplot::save_plot("~/Git-work/HSR/arsrapport/Alder.jpg", figAge1, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/Alder.pdf", figAge1, base_height = 7, base_width = 7)
grid.draw(ageAlle)
dev.off()


## Mean age med HF

regAgeN <- reg %>%
    summarise( meanAge = mean(PatientAge)) %>%
    mutate(HF = "Norge")

regAgeHF <- reg %>%
    group_by(HF) %>%
    summarise( meanAge = mean(PatientAge))

regAgeAlle <- dplyr::full_join(regAgeN, regAgeHF)


fig1 <- ggplot(regAgeAlle, aes(x=reorder(HF, meanAge), y = meanAge)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = meanAge + 3.5, label = paste0(sprintf("%1.1f", meanAge))), size = 3.5) +
    coord_flip() + theme_bw() +
    ##guides(fill = FALSE) +
    labs(title = "Gjennomsnitt alder", y = "Gjennomsnitt alder") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/AlderMin.jpg", fig1a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/AlderMin.pdf", fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()


###################################
## Kollaps hørt eller sett av

reg$CaObs1 <- reg$CaObs #beholder "ikke valgt"
reg$CaObs[reg$CaObs == -1] <- 999 #omkode "ikke valgt" til "Ukjent"
regCa <- reg

## Pie
label1 <- c('Tilstedeværende', 'Akuttmedisinsk personell', 'Ingen', 'Ukjent')

regd2 <- regCa %>%
    group_by(CaObs) %>%
    summarise(n = n()) %>%
    mutate(pct = n/sum(n) * 100) %>%
    mutate(var = factor(CaObs, levels = c(0, 1, 99, 999), labels = label1))

library(scales)
pie <- ggplot(regd2, aes(x = "", y = pct, fill = var)) +
    geom_bar(width = 1, stat = "identity") +
    blank_theme +
    theme(legend.position = 'top',
          legend.margin = unit(0, "cm"),
          legend.text = element_text(size = 12)
          ##legend.key = element_rect(color = "black"),
          ##plot.margin = unit(c(-0.2,0,0,0), "cm")
          ) +
    labs(title = "Kollaps hørt eller sett av") +
    ## guides(fill = guide_legend(title.position = 'top')) +
    ##scale_fill_brewer(palette =  "Blues") +
    scale_fill_manual(values = cols) +
    coord_polar("y") +
    geom_text(aes(y = pct/4 + c(0, cumsum(pct)[-length(pct)]),
                  label = percent(pct/100)), size = 5)

##### Lage figuren ####
## png("~/Git-work/HSR/arsrapport/pie.png", width = 1000, height = 1000)
grid.draw(pie)
## ggsave("~/Git-work/HSR/arsrapport/pie.pdf")
cowplot::save_plot("~/Git-work/HSR/arsrapport/pieCaObs.jpg", pie, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/pieCaObs.pdf", pie, base_height = 7, base_width = 7)
dev.off()


## Telling - Tilstedeværende
regAlle <- telling(regCa, "CaObs", "HF")


## Tilstedeværende == 0
regTil <- filter(regAlle, CaObs == 0)


##options(digits = 2)
fig2 <- ggplot(regTil, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 4.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Kollaps hørt eller sett av tilstedeværende", y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig2a <- ggplot_gtable(ggplot_build(fig2))
fig2a$layout$clip[fig2a$layout$name == 'panel'] <- 'off'
grid.draw(fig2a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figTilstede.jpg", fig2a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figTilstede.pdf", fig2a, base_height = 7, base_width = 7)
dev.off()



## AkuttmedPersonell
regPer <- filter(regAlle, CaObs == 1)

fig3 <- ggplot(regPer, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 1.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Kollaps hørt eller sett av akuttmedisinsk personell", y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2 +
    theme(axis.title.y = element_blank())


fig3a <- ggplot_gtable(ggplot_build(fig3))
fig3a$layout$clip[fig3a$layout$name == 'panel'] <- 'off'
grid.draw(fig3a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figAkkutper.jpg", fig3a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figAkkutper.pdf", fig3a, base_height = 7, base_width = 7)
dev.off()


################################################
## HLR utført av tilstedeværende (CprByst)

##Filter i 3.2 - ta bort CaObs == 1 "akkutmedpersonell"
regClean2 <- reg %>%
    filter(CaObs != 1) #akkuttmedpersonell

## Filter Akkuttmed og "ikke valgt"
regClean1 <- reg %>%
    filter(CaObs1 !=1 & CaObs1 != -1)

regCprAlle <- telling(regClean2, "CprByst", "HF")

## Only JA = 0
regCpr <- dplyr::filter(regCprAlle, CprByst == 0 )


fig4 <- ggplot(regCpr, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 5.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "HLR utført av tilstedeværende", y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig4a <- ggplot_gtable(ggplot_build(fig4))
fig4a$layout$clip[fig4a$layout$name == 'panel'] <- 'off'
grid.draw(fig4a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figHLRtilstede.jpg", fig4a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figHLRtilstede.pdf", fig4a, base_height = 7, base_width = 7)
dev.off()


########################################
## HLR ved akuttmedisinsk personell

## filter som i 3.3 kun pasienter som har HLR ved akuttmedisinsk personell dvs. Cprms = 0 "JA"
regAkk <- dplyr::filter(reg, CprEms == 0)

## ROSC - Vedvarende ROSC

regAkk1 <- telling(regAkk, "Rosc", "HF")

## Rosc = 0 "JA"
roscJa <- dplyr::filter(regAkk1, Rosc == 0 )

fig5 <- ggplot(roscJa, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 2.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Egensirkulasjon", y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig5a <- ggplot_gtable(ggplot_build(fig5))
fig5a$layout$clip[fig5a$layout$name == 'panel'] <- 'off'
grid.draw(fig5a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figRosc.jpg", fig5a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/figRosc.pdf", fig5a, base_height = 7, base_width = 7)
dev.off()


## 24 timer etter hendelse. Surv24H == 0 "i live"

reg24h <- telling(regAkk, "Surv24H", "HF")
reg24hlive <- dplyr::filter(reg24h, Surv24H == 0)


fig6 <- ggplot(reg24hlive, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 2, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "I live 24 timer etter hendelse", y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig6a <- ggplot_gtable(ggplot_build(fig6))
fig6a$layout$clip[fig6a$layout$name == 'panel'] <- 'off'
grid.draw(fig6a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/status24h.jpg", fig6a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/status24h.pdf", fig6a, base_height = 7, base_width = 7)
dev.off()


## Overlevelse 30 dager. Survival30D == 0 "i live"

reg30d <- telling(regAkk, "Survival30D", "HF")
reg30dlive <- dplyr::filter(reg30d, Survival30D == 0)


fig7 <- ggplot(reg30dlive, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 1.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Overlevelse 30 dager", y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig7a <- ggplot_gtable(ggplot_build(fig7))
fig7a$layout$clip[fig7a$layout$name == 'panel'] <- 'off'
grid.draw(fig7a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/status30d.jpg", fig7a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/status30d.pdf", fig7a, base_height = 7, base_width = 7)
dev.off()


####################################################
## For pasienter som blir innlagt i sykehus
## for "JA" i HLR ved akuttmedisinskpersonnell i.e regAkk dataset

## Pasient transortert til syk eller overlevert annen tjeneste. Transp == 0

regTran <- telling(regAkk, "Transp", "HF")
regTranJa <- dplyr::filter(regTran, Transp == 0)


fig8 <- ggplot(regTranJa, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 3.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Pasient transportert til sykehus \n eller overlevert annen tjeneste",
         y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig8a <- ggplot_gtable(ggplot_build(fig8))
fig8a$layout$clip[fig8a$layout$name == 'panel'] <- 'off'
grid.draw(fig8a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/Transp.jpg", fig8a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/Transp.pdf", fig8a, base_height = 7, base_width = 7)
dev.off()


## Transport direkte til sykehus med angio/PCI. TranspDirect == 0

regTpci <- telling(regAkk, "TranspDirect", "HF")
regTpciJa <- dplyr::filter(regTpci, TranspDirect == 0)


fig9 <- ggplot(regTpciJa, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 3.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Transportert direkte til sykehus med angio/PCI",
         y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig9a <- ggplot_gtable(ggplot_build(fig9))
fig9a$layout$clip[fig9a$layout$name == 'panel'] <- 'off'
grid.draw(fig9a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/TranspDirect.jpg", fig9a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/TranspDirect.pdf", fig9a, base_height = 7, base_width = 7)
dev.off()


## Transport direkte angio/PCI-lab. TranspDirectPci == 0

regTpci2 <- telling(regAkk, "TranspDirectPci", "HF")
regTpci2Ja <- dplyr::filter(regTpci2, TranspDirectPci == 0)


fig10 <- ggplot(regTpci2Ja, aes(x=reorder(HF, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = HF == 'Norge')) +
    geom_text(aes(y = pct + 1, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Transportert direkte til sykehusets angio/PCI-lab",
         y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig10a <- ggplot_gtable(ggplot_build(fig10))
fig10a$layout$clip[fig10a$layout$name == 'panel'] <- 'off'
grid.draw(fig10a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/TranspDirectPci.jpg", fig10a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/TranspDirectPci.pdf", fig10a, base_height = 7, base_width = 7)
dev.off()

#####################################################################
#### NY Bestilling
#####################################################################
## Transport til sykehus med angio per RHF. reg$TransferHospPci

### Annen filter for datasett
regRos <- filter(reg, Rosc == 0 & CprEms == 0)

regHospPci2 <- tellingRHF(regRos, "TransferHospPci", "RHF1")
regHospPci <- dplyr::filter(regHospPci2, TransferHospPci == 0)

fig11c <- ggplot(regHospPci, aes(x=reorder(RHF1, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = RHF1 == 'Norge')) +
    geom_text(aes(y = pct + 2.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Overført til sykehus med PCI/angio",
         y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig11d <- ggplot_gtable(ggplot_build(fig11c))
fig11d$layout$clip[fig11d$layout$name == 'panel'] <- 'off'
grid.draw(fig11d)
cowplot::save_plot("~/Git-work/HSR/arsrapport/HospPci.jpg", fig11d, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/HospPci.pdf", fig11d, base_height = 7, base_width = 7)
dev.off()


## Andel overlevelse 30 dager av Rosc == 0 by RHF

reg30R <- tellingRHF(regRos, "Survival30D", "RHF1") #datasettet er regRos (n=730)
reg30RJa <- dplyr::filter(reg30R, Survival30D == 0)


fig11 <- ggplot(reg30RJa, aes(x=reorder(RHF1, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = RHF1 == 'Norge')) +
    geom_text(aes(y = pct + 3, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Overlevelse til 30 dager av de med \n gjenopprettet egensirkulasjon >20 min",
         y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig11a <- ggplot_gtable(ggplot_build(fig11))
fig11a$layout$clip[fig11a$layout$name == 'panel'] <- 'off'
grid.draw(fig11a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/Sur30dRosc.jpg", fig11a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/Sur30dRosc.pdf", fig11a, base_height = 7, base_width = 7)
dev.off()


## Direkte til angio/PCI for de som er lagt inn med Angio. TranspDirectPci == 0 / TransferHospPci == 0

regPci <- filter(regRos, TransferHospPci == 0)
regDirPci <- tellingRHF(regPci, "TranspDirectPci" , "RHF1")
regDirPciJa <- filter(regDirPci, TranspDirectPci == 0)


fig12 <- ggplot(regDirPciJa, aes(x=reorder(RHF1, pct), y = pct)) +
    geom_bar(stat = 'identity', aes(fill = RHF1 == 'Norge')) +
    geom_text(aes(y = pct + 1.5, label = paste0(sprintf("%1.1f", pct), '%')), size = 3.5) +
    coord_flip() + theme_bw() +
    labs(title = "Direkt til angio/PCI for pasienter lagt inn \n på sykehus med Angio-mulighet",
         y = "prosent (%)") +
    scale_fill_manual(values = col2, guide = 'none') +
    scale_y_continuous(expand = c(0,0)) +
    theme2

fig12a <- ggplot_gtable(ggplot_build(fig12))
fig12a$layout$clip[fig12a$layout$name == 'panel'] <- 'off'
grid.draw(fig12a)
cowplot::save_plot("~/Git-work/HSR/arsrapport/DirPci.jpg", fig12a, base_height = 7, base_width = 7)
cowplot::save_plot("~/Git-work/HSR/arsrapport/DirPci.pdf", fig12a, base_height = 7, base_width = 7)
dev.off()



#### Filter StatusArrHosp

regAr <- filter(reg, StatusArrHosp == 0 | StatusArrHosp == 1 | StatusArrHosp == 4)
regRos2 <- filter(regAr, Rosc == 0)
