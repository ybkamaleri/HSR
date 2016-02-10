######################
### LibUtvalg
######################

HSLibUtvalg <- function(RegData, minAlder, maxAlder, fargepalett='BlaaOff')
{


###Hvis "Variabel" ikke definert
    "%i%" <- intersect
    if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
    Ninn <- dim(RegData)[1]

    indVarMed <- which(RegData$Variabel !='NA') %i% which(!is.na(RegData$Variabel)) %i% which(RegData$Variabel !='NaN')

    ##Alder (minFa og maxFa)
    if ((minAlder > 0) | (maxAlder < 150)) {indAlder <- which(RegData$Alder >= minAlder & RegData$Alder <= maxAlder)
    } else {
        indAlder <- 1:Ninn}

    indMed <- indAlder %i% indVarMed

    RegData <- RegData[indMed, ]

    N <- dim(RegData)[1]


    utvalgTxt <- c(paste0("Alder fra: ", if ((minAlder > 0) | (maxAlder < 150)) {min(RegData$Alder, na.rm = T)
                                             } else {minAlder},' til ', if ((minAlder > 0) | (maxAlder < 150)) {max(RegData$Alder, na.rm = T)
                                                                        } else {maxAlder}))


    UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
    return(invisible(UtData))
}
