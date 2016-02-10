##################
### FigAndeler
#################

FigAndeler <- function(RegData, valgtVar, libkat, outfile='', minAlder=0, maxAlder=150, ReshID)
{

######################
### Andre funksjoner
######################

    source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")

### Definerer registerspesifikke variable

    RegData$reshID <- RegData$Helseforetak
    RegData$Kjønn[RegData$Kjønn==999] <- NA
    RegData$Alder[RegData$Alder==999] <- NA
    RegData$Første_rytme[RegData$Første_rytme==999] <- NA
    RegData$Observert[RegData$Observert==999] <- NA
    ## RegData$AMK_varslet[RegData$Amb_ankommet==999] <- NA
    ## RegData$Amb_ankommet[RegData$AMK_varslet==999] <- NA
    RegData$Vedvarende_ROSC[RegData$Vedvarende_ROSC==999] <- NA
    RegData$Overlever24h[RegData$Overlever24h==999] <- NA
    RegData$Overlevelse30d[RegData$Overlevelse30d==999] <- NA

### Definere text

    shtxt <- switch(as.character(Resultat),
                    '0' = 'Hele landet',
                    '1' = as.character(ReshID),
                    '2' = as.character(ReshID))

#######################
### Valge variabler
######################

    if (valgtVar == 'Gender') {
        RegData$Variabel <- RegData$Kjønn
    }

    if (valgtVar == 'FirstRytme') {
        RegData$Variabel <- RegData$Første_rytme
    }

    if (valgtVar == 'Alder') {
        RegData$Variabel <- RegData$Alder
    }

    if (valgtVar == "Observert") {
        RegData$Variabel <- RegData$Observert
    }

    if (valgtVar == "HLRbefore") {
        RegData$Variabel <- RegData$HLR_før_amb
    }

    if (valgtVar == "Tidsbruk") {
        RegData$tid1 <- as.POSIXlt(RegData$AMK_varslet, format = "%H:%M:%S")
        RegData$tid2 <- as.POSIXlt(RegData$Amb_ankommet, format = "%H:%M:%S")
        RegData$tiddiff <- with(RegData, difftime(tid2, tid1, units = c("mins")))
        RegData$tiddiff[RegData$tiddiff<0] <- NA
        RegData$Variabel <- as.numeric(RegData$tiddiff)
        ## library('lubridate')
        ## RegData$Variabel <- seconds_to_period(RegData$tiddiff)
    }

    if (valgtVar %in% c("Vedvarende_ROSC", "Overlever24h", "Overlevelse30d")) {
        RegData$Variabel <- RegData[ ,valgtVar]
    }

    
########################################################################
###Tar ut de med manglende registrering av valgt variabel og gjør utvalg
    source(paste(libkat, 'HSLibUtvalg.R', sep=''), encoding="UTF-8")

    HSUtvalg <- HSLibUtvalg(RegData=RegData, minAlder=minAlder, maxAlder=maxAlder)
    RegData <- HSUtvalg$RegData
    utvalgTxt <- HSUtvalg$utvalgTxt

###Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
###if ((sml == 0) & (egenavd == 1)) {RegData <- RegData[which(RegData$ReshId == reshID), ]}     #{indShUt <- which(RegData$ReshId != reshID)}
    if (Resultat == 2) {RegData <- RegData[which(RegData$reshID == ReshID), ]}

#############################################
###----------- Figurparametre ---------------
#############################################
    cexgr <- 1  #Kan endres for enkeltvariable
    retn <- 'V' #Vertikal som standard. 'H' angis evt. for enkeltvariable
    grtxt <- '' #Spesifiseres for hver enkelt variabel
    grtxt2 <- ''        #Spesifiseres evt. for hver enkelt variabel
    subtxt <- ''        #Benevning
    flerevar <- 0


###Hvis for få observasjoner..
###if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
    if (dim(RegData)[1] < 5 | (length(which(RegData$reshID == ReshID))<2 & Resultat == 1)) {
###-----------Figur---------------------------------------
        FigTypUt <- figtype(outfile)
        farger <- FigTypUt$farger
        plot.new()
        title(main=paste('variabel: ', valgtVar, sep=''))       #, line=-6)
        legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
        text(0.5, 0.6, 'Færre enn 2 egne registreringer eller færre 5 totalt', cex=1.2)
        if ( outfile != '') {dev.off()}
    } else {

############################################
###----------- Gjøre beregninger ---------
############################################

        medSml <- 0
        utvalg <- c('Sh', 'Rest')  #Sh vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
        Andeler <- list(Sh = 0, Rest =0)

        ##if (sml == 1) {
        ##Hvis det skal gjøres sammenligning:
        if (Resultat %in% c(1,3)) {
            indSh <-which(RegData$reshID == ReshID)
            indRest <- which(RegData$reshID != ReshID)
            RegDataLand <- RegData
            ind <- list(Sh=indSh, Rest=indRest)
            medSml <- 1
        }

        for (teller in 1:(medSml+1)) {

            ##for (teller in 1:(sml+1)) {
            ##       if (sml == 1) {
            if (medSml == 1) {
                RegData <- RegDataLand[switch(utvalg[teller], Sh = ind$Sh, Rest=ind$Rest), ]
            }

            ##Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.

            if (valgtVar == 'Gender') {
                tittel <- 'Kjønnsfordeling'
                grtxt <- c("Mann", "Kvinne")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 0:1, labels = grtxt)
                subtxt <- 'Kjønn'
                retn <- 'H'
            }

            if (valgtVar == 'FirstRytme') {
                tittel <- 'Første registerte hjerterytme etter ankomst'
                grtxt <- c('VF', 'VT', 'Asystole', 'PEA', 'Spontaneous circulation')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 0:4, labels = grtxt)
                retn <- 'H'
            }

            if (valgtVar == 'Alder') {
                tittel <- 'Aldersfordeling'
                gr <- c(0, seq(10, 90, 10), 150)
                RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = TRUE, right = FALSE)
                grtxt <- c('<10', levels(RegData$VariabelGr)[2:(length(gr)-2)], '90+')
                subtxt <- 'Aldersgrupper'
                reth <- 'H'
            }

            if (valgtVar == "Observert") {
                tittel <- "Kollaps hørt eller sett av"
                grtxt <- c("Akuttmed personell", "Tilstedeværende", "Ingen")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = c(0:1, 99), labels = grtxt)
                retn <- 'H'
            }

            if (valgtVar == "HLRbefore") {
                tittel <- "HLR utført av tilstedeværende"
                grtxt <- c("Nei", "Ja", "Ukjent", "Ikke relevant")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = c(1:2, 888, 999), labels = grtxt)
                retn <- 'H'
            }

            ## if (valgtVar == "Tidsbruk") {
            ##     tittel <- "Tid fra melding mottat til ambulanse fremme på stedet"
            ##     gr <- c(0, 10, 20, 30, 40, 50, 60, 120, 150, 180, 10000)
            ##     RegData$VariabelGr <- cut(as.numeric(RegData$Variabel), breaks = gr, include.lowest = TRUE, right = FALSE)
            ##     grtxt <- c('<10', levels(RegData$VariabelGr)[2:(length(gr)-2)], '>180')
            ##     subtxt <- 'Antall minuter'
            ##     retn <- 'H'
            ## }
            if (valgtVar == "Tidsbruk") {
                tittel <- "Tid fra melding mottat til ambulanse fremme på stedet (minuter)"
                gr <- c(0, seq(5, 60, 5), 90, 120, 180, 10000)
                RegData$VariabelGr <- cut(as.numeric(RegData$Variabel), breaks = gr, include.lowest = TRUE, right = FALSE)
                grtxt <- c('<5', levels(RegData$VariabelGr)[2:(length(gr)-2)], '>180')
                subtxt <- 'Antall minuter'
                retn <- 'H'
            }

            if (valgtVar == "Vedvarende_ROSC") {
                tittel <- "Overlevelse ROSC"
                gr <- c(1,2)
                grtxt <- c("Død", "Live")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:2, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Overlever24h") {
                tittel <- "Overlevelse 24h"
                gr <- c(1,2)
                grtxt <- c("Død", "Live")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:2, labels = grtxt)
                retn <- "H"
            }

            if (valgtVar == "Overlevelse30d") {
                tittel <- "Overlevelse 30d"
                gr <- c(1,2)
                grtxt <- c("Død", "Live")
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:2, labels = grtxt)
                retn <- "H"
            }
            

            if (teller == 1) {Andeler$Sh <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
                Nsh <- dim(RegData)[1]}

            if (teller == 2) {Andeler$Rest <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
                Nrest <- dim(RegData)[1]}

        }

#############################
        ##-----------Figur------------
        ##Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr

        ##Plottspesifikke parametre:
        FigTypUt <- figtype(outfile)
        ##Tilpasse marger for å kunne skrive utvalgsteksten
        NutvTxt <- length(utvalgTxt)
        grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Sh)), '%)', sep='')
        vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
        ##vmarg <- max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7)
        par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))   #Har alltid datoutvalg med

        farger <- FigTypUt$farger
        fargeSh <- farger[1]
        fargeRest <- farger[3]
        antGr <- length(grtxt)
        lwdRest <- 3    #tykkelse på linja som repr. landet
        cexleg <- 1     #Størrelse på legendtekst

        ##Horisontale søyler
        if (retn == 'H') {
            xmax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
            pos <- barplot(rev(as.numeric(Andeler$Sh)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel personer (%)", #main=tittel,
                           col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)   #
            mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

            if (medSml == 1) {
                points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
                legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                       border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
                       lwd=lwdRest,     lty=NA, ncol=1, cex=cexleg)
            } else {
                legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
                       border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
            }
        }


        if (retn == 'V' ) {
            ##Vertikale søyler eller linje
            ymax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
            pos <- barplot(as.numeric(Andeler$Sh), beside=TRUE, las=1, ylab="Andel personer (%)",
                           sub=subtxt,  col=fargeSh, border='white', ylim=c(0, ymax))
            mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
            mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
            if (medSml == 1) {
                points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
                legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                       border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
                       lwd=lwdRest, ncol=2, cex=cexleg)
            } else {
                legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
                       border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
            }
        }


        title(tittel, line=1, font.main=1)

        ##Tekst som angir hvilket utvalg som er gjort
        avst <- 0.8
        utvpos <- 3     #Startlinje for teksten
        mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

        par('fig'=c(0, 1, 0, 1))
        if ( outfile != '') {dev.off()}


    }
}

