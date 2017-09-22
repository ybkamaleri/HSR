#########################
## Calculate age
#########################

age_calc <- function(dob, enddate=Sys.Date(), units='months', precise=TRUE){
  if (!inherits(dob, "Date") | !inherits(enddate, "Date")){
    stop("Both dob and enddate must be Date class objects")
  }
  if(any(enddate < dob)){
    stop("End date must be a date after date of birth")
  }
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  if(precise){
    start_is_leap <- ifelse(start$year %% 400 == 0, TRUE,
                            ifelse(start$year %% 100 == 0, FALSE,
                                   ifelse(start$year %% 4 == 0, TRUE, FALSE)))
    end_is_leap <- ifelse(end$year %% 400 == 0, TRUE,
                          ifelse(end$year %% 100 == 0, FALSE,
                                 ifelse(end$year %% 4 == 0, TRUE, FALSE)))
  }
  if(units=='days'){
    result <- difftime(end, start, units='days')
  }else if(units=='months'){
    months <- sapply(mapply(seq, as.POSIXct(start), as.POSIXct(end),
                            by='months', SIMPLIFY=FALSE),
                     length) - 1
    # length(seq(start, end, by='month')) - 1
    if(precise){
      month_length_end <- ifelse(end$mon==1 & end_is_leap, 29,
                                 ifelse(end$mon==1, 28,
                                        ifelse(end$mon %in% c(3, 5, 8, 10),
                                               30, 31)))
      month_length_prior <- ifelse((end$mon-1)==1 & start_is_leap, 29,
                                   ifelse((end$mon-1)==1, 28,
                                          ifelse((end$mon-1) %in% c(3, 5, 8, 10),
                                                 30, 31)))
      month_frac <- ifelse(end$mday > start$mday,
      (end$mday-start$mday)/month_length_end,
      ifelse(end$mday < start$mday,
      (month_length_prior - start$mday) /
        month_length_prior +
        end$mday/month_length_end, 0.0))
      result <- months + month_frac
    }else{
      result <- months
    }
  }else if(units=='years'){
    years <- sapply(mapply(seq, as.POSIXct(start), as.POSIXct(end),
                           by='years', SIMPLIFY=FALSE),
                    length) - 1
    if(precise){
      start_length <- ifelse(start_is_leap, 366, 365)
      end_length <- ifelse(end_is_leap, 366, 365)
      start_day <- ifelse(start_is_leap & start$yday >= 60,
                          start$yday - 1,
                          start$yday)
      end_day <- ifelse(end_is_leap & end$yday >=60,
                        end$yday - 1,
                        end$yday)
      year_frac <- ifelse(start_day < end_day,
      (end_day - start_day)/end_length,
      ifelse(start_day > end_day,
      (start_length-start_day) / start_length +
        end_day / end_length, 0.0))
      result <- years + year_frac
    }else{
      result <- years
    }
  }else{
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  return(result)
}


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
