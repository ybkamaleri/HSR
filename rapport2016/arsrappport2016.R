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

dim(reg)
reg[, .N, by = list(ReshId, ReshNavn)]

## Kjønn
reg[, gender := PatientGender]
reg$gender <- factor(reg$gender,
                     levels = c(1, 2),
                            labels = c("mann", "kvinne"))

reg[, .N, by = list(ReshNavn, gender)]



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

## Figure Age categories
ageAlle <- ggplot(regAge, aes(ageKat, N)) +
  geom_bar(stat='identity', fill = col1) +
  geom_text(data = regAge[pos == 1], aes(x = ageKat, y = N - 10, label = N), size = 3) +
  geom_text(data = regAge[pos == 0], aes(x = ageKat, y = N + 5, label = N), size = 3) +
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


ageAlle <- data.table::rbindlist(list(ageN, ageHF), use.names = TRUE)
## alt. to use rbind if position for colnames not at the same position



fig1 <- ggplot(ageAlle, aes(x=reorder(ReshNavn, mean), y = mean)) +
  geom_bar(stat = 'identity', aes(fill = ReshNavn == 'Norge')) +
  geom_text(aes(y = 5, label = paste0(sprintf("%1.1f", mean))), size = 3.5) +
  ## geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),  width = .3, color = "blue",
  ##               position = position_dodge(.9)) +
  coord_flip() +
  ##guides(fill = FALSE) +
  labs(title = "Gjennomsnitt alder", y = "Gjennomsnitt alder") +
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


##############################
## Kollaps hørt eller sett av

## if problem with locale when running "grep" then run this
Sys.setlocale(locale = "C")

koll <- grep("*rtellersettav$", colnames(reg), value = TRUE)


kollv <- reg[, list(n = .N),  by = get(koll)]
kollv[, sum := sum(n)][, pro := format(round(n / sum * 100), nsmall = 0)] #ingen decimal

kollv[list(get = c(-1, 999), to = 999), on = "get", get := i.to] #recode alt annen enn 0 ,1 og 99 to 999

#### kategorier: ukjent og ikke valgt bør slå sammen

kollv$value <- factor(kollv$get,
                      levels = c(-1, 0, 1, 99, 999),
                      labels = c("Ikke valgt", "Tilstedeværende", "Akuttmedisinsk personell", "Ingen", "Ukjent"))


kollv[, sum(n), by = value]

## Endre tilbake til norsk locale
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")
kollv$value <- iconv(kollv$value, "utf-8", "latin1")

(fig2 <- ggplot(kollv, aes(value, pro)) +
   geom_bar(stat = 'identity', fill = col1) +
   ## geom_text(data = kollv, aes(x = value, label = n), size = 3) +
   labs(title = "Kollaps hørt eller sett av", y = "Prosent", x = "") +
   ## coord_flip() +
   theme_classic()
)
