###########################
##  Alder del i kategorier
###########################
alder.kat <- function(x, lower, upper, by,
                      sep = "-") {
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, "+", sep = ""))
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      include.lowest = TRUE, right = FALSE, labels = labs)
}

###########################
## plot theme
###########################
library(ggplot2)

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
