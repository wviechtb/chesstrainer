.drawpiece <- function(x, y, piece) {
   if (piece != "") {
      txt <- paste0("rasterImage(img.", piece, ",", y, ",", x, ",", y+1, ",", x+1, ")")
      eval(parse(text=txt), envir=.chesstrainer)
   }
}

.drawboard <- function(pos, flip=FALSE, newplot=TRUE) {

   bg <- .get("col.bg")
   fg <- .get("col.fg")

   if (dev.cur() == 1L)
      dev.new(bg=bg)

   if (newplot) {

      par(xpd=NA, pty="s", mar=rep(5,4)+0.2, fg=.get("col.fg"), bg=.get("col.bg"))

      mat <- outer(1:8, 1:8, function(x,y) .is.even(x+y))

      image(1:8+0.5, 1:8+0.5, col=c(.get("col.square.l"), .get("col.square.d")), mat, xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)

      if (flip) {
         par(mgp=c(3,0.5,0))
         axis(side=1, 1:8+0.5, rev(LETTERS[1:8]), las=1, tick=FALSE, col.axis=fg)
         par(mgp=c(3,0.8,0))
         axis(side=2, 1:8+0.5, rev(1:8),          las=1, tick=FALSE, col.axis=fg)
      } else {
         par(mgp=c(3,0.5,0))
         axis(side=1, 1:8+0.5, LETTERS[1:8],      las=1, tick=FALSE, col.axis=fg)
         par(mgp=c(3,0.8,0))
         axis(side=2, 1:8+0.5, 1:8,               las=1, tick=FALSE, col.axis=fg)
      }

   }

   if (flip) {
      for (i in 1:8) {
         for (j in 1:8) {
            .drawpiece(i, j, pos[9-i,9-j])
         }
      }
   } else {
      for (i in 1:8) {
         for (j in 1:8) {
            .drawpiece(i, j, pos[i,j])
         }
      }
   }

}

.updateboard <- function(pos, x1, y1, x2, y2, flip, volume, verbose) {

   x1 <- unname(x1)
   y1 <- unname(y1)
   x2 <- unname(x2)
   y2 <- unname(y2)

   isrochade <- ""

   if (flip) {
      if (identical(c(x1,y1), c(9-1,9-5)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(9-1,9-7))) {
         isrochade <- "0-0"
         pos[1,6] <- "WR"
         pos[1,8] <- ""
         .drawsquare(9-1, 9-8)
         .drawsquare(9-1, 9-6)
         .drawpiece(9-1, 9-6, "WR")
      }
      if (identical(c(x1,y1), c(9-1,9-5)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(9-1,9-3))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         .drawsquare(9-1, 9-1)
         .drawsquare(9-1, 9-4)
         .drawpiece(9-1, 9-4, "WR")
      }
      if (identical(c(x1,y1), c(9-8,9-5)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(9-8,9-7))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         .drawsquare(9-8, 9-8)
         .drawsquare(9-8, 9-6)
         .drawpiece(9-8, 9-6, "BR")
      }
      if (identical(c(x1,y1), c(9-8,9-5)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(9-8,9-3))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         .drawsquare(9-8, 9-1)
         .drawsquare(9-8, 9-4)
         .drawpiece(9-8, 9-4, "BR")
      }
   } else {
      if (identical(c(x1,y1), c(1,5)) && pos[x1,y1] == "WK" && identical(c(x2,y2), c(1,7))) {
         isrochade <- "0-0"
         pos[1,6] <- "WR"
         pos[1,8] <- ""
         .drawsquare(1, 8)
         .drawsquare(1, 6)
         .drawpiece(1, 6, "WR")
      }
      if (identical(c(x1,y1), c(1,5)) && pos[x1,y1] == "WK" && identical(c(x2,y2), c(1,3))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         .drawsquare(1, 1)
         .drawsquare(1, 4)
         .drawpiece(1, 4, "WR")
      }
      if (identical(c(x1,y1), c(8,5)) && pos[x1,y1] == "BK" && identical(c(x2,y2), c(8,7))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         .drawsquare(8, 8)
         .drawsquare(8, 6)
         .drawpiece(8, 6, "BR")
      }
      if (identical(c(x1,y1), c(8,5)) && pos[x1,y1] == "BK" && identical(c(x2,y2), c(8,3))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         .drawsquare(8, 1)
         .drawsquare(8, 4)
         .drawpiece(8, 4, "BR")
      }
   }

   .drawsquare(x1, y1)
   .drawsquare(x2, y2)

   if (flip) {
      .drawpiece(x2, y2, pos[9-x1,9-y1])
      if (pos[9-x2,9-y2] != "") {
         playsound(system.file("sounds", "capture.ogg", package="chesstrainer"), volume=volume)
      } else {
         playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
      }
      iscapture <- pos[9-x2,9-y2] != ""
      piece1 <- ifelse(substr(pos[9-x1,9-y1], 2, 2) == "P", "", substr(pos[9-x1,9-y1], 2, 2))
      piece2 <- ifelse(substr(pos[9-x2,9-y2], 2, 2) == "P", "", substr(pos[9-x2,9-y2], 2, 2))
      pos[9-x2,9-y2] <- pos[9-x1,9-y1]
      pos[9-x1,9-y1] <- ""
   } else {
      .drawpiece(x2, y2, pos[x1,y1])
      if (pos[x2,y2] != "") {
         playsound(system.file("sounds", "capture.ogg", package="chesstrainer"), volume=volume)
      } else {
         playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
      }
      iscapture <- pos[x2,y2] != ""
      piece1 <- ifelse(substr(pos[x1,y1], 2, 2) == "P", "", substr(pos[x1,y1], 2, 2))
      piece2 <- ifelse(substr(pos[x2,y2], 2, 2) == "P", "", substr(pos[x2,y2], 2, 2))
      pos[x2,y2] <- pos[x1,y1]
      pos[x1,y1] <- ""
   }

   if (verbose) {
      if (flip) {
         if (identical(isrochade, "")) {
            cat("\nMove: ", piece1, letters[9-y1], 9-x1, ifelse(iscapture, "x", "-"), piece2, letters[9-y2], 9-x2, "\n\n", sep="")
         } else {
            cat("\nMove: ", isrochade, "\n\n", sep="")
         }
      } else {
         if (identical(isrochade, "")) {
            cat("\nMove: ", piece1, letters[y1], x1, ifelse(iscapture, "x", "-"), piece2, letters[y2], x2, "\n\n", sep="")
         } else {
            cat("\nMove: ", isrochade, "\n\n", sep="")
         }
      }
      printpos <- pos
      printpos[printpos == ""] <- "."
      printpos[printpos == "WP"] <- "\U000265F"
      printpos[printpos == "WR"] <- "\U000265C"
      printpos[printpos == "WN"] <- "\U000265E"
      printpos[printpos == "WB"] <- "\U000265D"
      printpos[printpos == "WK"] <- "\U000265A"
      printpos[printpos == "WQ"] <- "\U000265B"
      printpos[printpos == "BP"] <- "\U0002659"
      printpos[printpos == "BR"] <- "\U0002656"
      printpos[printpos == "BN"] <- "\U0002658"
      printpos[printpos == "BB"] <- "\U0002657"
      printpos[printpos == "BK"] <- "\U0002654"
      printpos[printpos == "BQ"] <- "\U0002655"
      if (flip) {
         print(printpos[,8:1], quote=FALSE)
      } else {
         print(printpos[8:1,], quote=FALSE)
      }
   }

   return(pos)

}

.is.even <- function(x) x %% 2 == 0

.addrect <- function(x, y, offset=0.028, col, lwd)
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd, border=col, ljoin=1)

.rmrect <- function(x, y, offset=0.028, lwd)
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), ljoin=1)

.addcircle <- function(x, y, lwd)
   symbols(y+0.5, x+0.5, circles=0.45, inches=FALSE, lwd=lwd+2, fg=.get("col.annot"), add=TRUE)

.drawsquare <- function(x, y)
   rect(y, x, y+1, x+1, col=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), border=NA)

.texttop <- function(text) {
   rect(-2, 9.2, 12, 10, col=.get("col.bg"), border=NA)
   if (!identical(text, "")) {
      text(5, 9.5, text, cex=1.4, col=.get("col.texttop"))
   }
   return(text)
}

.drawsideindicator <- function(i, flip) {

   indsize <- 0.18

   if (flip) {
      if (!.is.even(i)) {
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col="white")
      } else {
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col="black")
      }
   } else {
      if (.is.even(i)) {
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col="black")
      } else {
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col="white")
      }
   }

}

.selectplayer <- function(current, seqdir, mustselect=FALSE) {

   files <- list.files(seqdir, full.names=TRUE, pattern=".rds$")
   dat <- lapply(files, readRDS)

   players <- unique(unlist(lapply(dat, function(x) names(x$score))))
   nplayers <- length(players)

   if (nplayers == 0L) {
      while (TRUE) {
         player <- readline(prompt=.text("newplayername"))
         if (!identical(player, ""))
            break
      }
   } else {
      cat(.text("foundplayers"))
      tmp <- data.frame(players)
      names(tmp) <- .text("player")
      print(tmp)
      cat("\n")
      while (TRUE) {
         player <- readline(prompt=.text("enterplayer"))
         if (identical(player, "")) {
            if (mustselect) {
               next
            } else {
               player <- current
               break
            }
         }
         if (grepl("^[0-9]+$", player)) {
            player <- round(as.numeric(player))
            if (player >= 1L && player <= nplayers) {
               player <- players[player]
               break
            }
         }
         break
      }
   }

   return(player)

}

.removeplayer <- function(player, seqdir) {

   files <- list.files(seqdir, full.names=TRUE, pattern=".rds$")

   if (length(files) >= 1L) {
      for (i in 1:length(files)) {
         tmp <- readRDS(files[i])
         pos.score <- which(names(tmp$score) == player)
         if (length(pos.score) != 0)
            tmp$score <- tmp$score[-pos.score]
         pos.played <- which(names(tmp$played) == player)
         if (length(pos.played) != 0)
            tmp$played <- tmp$played[-pos.played]
         if (length(pos.score) != 0 || length(pos.played) != 0)
            saveRDS(tmp, file=files[i])
      }
   }

}

.printinfo <- function(mode, show, player, seqname, score, played, i, totalmoves) {

   lang <- .get("lang")
   cex  <- .get("cex.info")
   font <- .get("font.mono")
   col  <- .get("col.textbot")

   rect(-2, -2, 12, 0.6, col=.get("col.bg"), border=NA)

   if (lang == "en") {

      if (mode == "add") {
         text(0.0, 0.45, paste0("Mode: ", "Add"), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.15, paste0("Move: ", i), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.30, paste0("Show: ", ifelse(show, "Ja", "Nein")), pos=4, cex=cex, family=font, col=col)
      }

      if (mode == "play") {
         text(0.00, 0.45, paste0("Mode:   ", "Play"), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.30, paste0("Player: ", player), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.15, paste0("Name:   ", seqname), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.00, paste0("Move:   ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.15, paste0("Score:  ", score), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.00, paste0("Played: ", played), pos=4, cex=cex, family=font, col=col)
      }

   }

   if (lang == "de") {

      if (mode == "add") {
         text(0.0, 0.45, paste0("Modus:  ", "Hinzuf\U000000FCgen"), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.15, paste0("Zug:    ", i), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.30, paste0("Zeigen: ", ifelse(show, "Ja", "Nein")), pos=4, cex=cex, family=font, col=col)
      }

      if (mode == "play") {
         text(0.00, 0.45, paste0("Modus:   ", "Spielen"), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.30, paste0("Spieler: ", player), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.15, paste0("Name:    ", seqname), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.00, paste0("Zug:     ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.15, paste0("Punkte:   ", score), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.00, paste0("Gespielt: ", played), pos=4, cex=cex, family=font, col=col)
      }

   }

}

.get <- function(x)
   get(x, envir=.chesstrainer)

.printhelp <- function(...) {

   ddd <- list(...)

   if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))

   lang <- .get("lang")

   if (lang == "en") {

      cat("--------------------------------------------------------------\n")
      cat("<space> - switch mode (play/add)\n")
      cat("p       - select a player\n")
      cat("Ctrl-r  - remove a player\n")
      cat("l       - list all sequences\n")
      cat("/       - find/select sequences\n")
      cat("n       - start a new sequence\n")
      cat("e       - edit the current sequence\n")
      cat("-/+     - decrease/increase the time between moves\n")
      cat("w       - pause between completed sequences on/off\n")
      cat("m       - sound on/off\n")
      cat("[/]     - decrease/increase the sound volume\n")
      cat("u       - update/redraw the board\n")
      cat("^       - specify the exponent value\n")
      cat("i       - toggle the language (English/German)\n")
      cat("r       - toggle between random/sequential mode\n")
      cat("F1      - print help\n")
      cat("F2      - show the leaderboard and player statistics\n")
      cat("q       - quit the trainer\n")
      cat("\n")
      cat("Only in play mode:\n")
      cat("h       - get a hint (can select twice)\n")
      cat("t       - take back a score adjustment due to a wrong move\n")
      cat("o       - manually edit the score for the current sequence\n")
      cat("Ctrl-d  - delete the current sequence\n")
      cat("a       - copy the current sequence and extend with more moves\n")
      cat("\n")
      cat("Only in add mode:\n")
      cat("f       - flip the board\n")
      cat("z       - show moves on/off\n")
      cat("c       - add a comment to the current move\n")
      cat("0       - make the current position the starting position\n")
      cat("b       - take move back\n")
      cat("s       - save the sequence\n")
      cat("--------------------------------------------------------------\n")

   }

   if (lang == "de") {

      cat("--------------------------------------------------------------------\n")
      cat("<space> - Modus wechseln (spielen/hinzuf\U000000FCgen)\n")
      cat("p       - Spieler ausw\U000000E4hlen\n")
      cat("Strg-r  - Spieler l\U000000F6schen\n")
      cat("l       - alle Sequenzen auflisten\n")
      cat("/       - Sequenzen finden/ausw\U000000E4hlen\n")
      cat("n       - neue Sequenz starten\n")
      cat("e       - aktuelle Sequenz editieren\n")
      cat("-/+     - Zeit zwischen Z\U000000FCgen verringern/erh\U000000F6hen\n")
      cat("w       - Pausen zwischen abgeschlossenen Sequenzen an/aus\n")
      cat("m       - Ton an/aus\n")
      cat("[/]     - Lautst\U000000E4rke verringern/erh\U000000F6hen\n")
      cat("u       - Brett neu zeichnen\n")
      cat("^       - Exponentenwert angeben\n")
      cat("i       - wechsel die Sprache (Englisch/Deutsch)\n")
      cat("r       - wechsel zwischen Zufalls/sequenziellen Modus\n")
      cat("F1      - Hilfe drucken\n")
      cat("F2      - Rangliste und Spielerstatistiken anzeigen\n")
      cat("q       - Trainer beenden\n")
      cat("\n")
      cat("Nur im Spiel Modus:\n")
      cat("h       - einen Tipp bekommen (kann zweimal gew\U000000E4hlt werden)\n")
      cat("t       - Punktekorrektur aufgrund eines falschen Zuges zur\U000000FCcknehmen\n")
      cat("o       - Punktewert f\U000000FCr die aktuelle Sequenz manuell eingeben\n")
      cat("Strg-d  - aktuelle Sequenz l\U000000F6schen\n")
      cat("a       - aktuelle Sequenz kopieren und mit weiteren Z\U000000FCgen erweitern\n")
      cat("\n")
      cat("Nur im Hinzuf\U000000FCgen Modus:\n")
      cat("f       - Brett umdrehen\n")
      cat("z       - Z\U000000FCge zeigen an/aus\n")
      cat("c       - Kommentar zum aktuellen Zug hinzuf\U000000FCgen\n")
      cat("0       - aktuelle Position zur Ausgangsposition machen\n")
      cat("b       - Zug zur\U000000FCck nehmen\n")
      cat("s       - Sequenz abspeichern\n")
      cat("--------------------------------------------------------------------\n")

   }

   if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))

}
