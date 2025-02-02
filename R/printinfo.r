.printinfo <- function(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode) {

   lang <- .get("lang")
   cex  <- .get("cex.bot")
   font <- .get("font.mono")
   col  <- .get("col.bot")

   rect(-2, -2, 12, 0.6, col=.get("col.bg"), border=NA)

   if (lang == "en") {

      if (mode == "add") {
         text(0.0, 0.45, paste0("Mode: ", "Add"), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.15, paste0("Move: ", i), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.30, paste0("Show: ", ifelse(show, "Ja", "Nein")), pos=4, cex=cex, family=font, col=col)
      }

      selmode <- switch(selmode,
                        score_random  = "score, at random",
                        score_highest = "score, highest next",
                        played_random = "play frequency, at random",
                        played_lowest = "play frequency, lowest next",
                        days_random   = "date, at random",
                        days_oldest   = "date, oldest next",
                        sequential    = "sequential")

      if (mode == "play") {
         text(0.00, 0.45, paste0("Mode:   ", "Play (selection: ", selmode, ")"), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.30, paste0("Name:   ", "(", seqnum, ") ", seqname), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.15, paste0("Player: ", player), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.00, paste0("Move:   ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.15, paste0("Played: ", played), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.00, paste0("Score:  ", score), pos=4, cex=cex, family=font, col=col)
      }

   }

   if (lang == "de") {

      if (mode == "add") {
         text(0.0, 0.45, paste0("Modus:  ", "Hinzuf\U000000FCgen"), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.15, paste0("Zug:    ", i), pos=4, cex=cex, family=font, col=col)
         text(0.0, 0.30, paste0("Zeigen: ", ifelse(show, "Ja", "Nein")), pos=4, cex=cex, family=font, col=col)
      }

      selmode <- switch(selmode,
                        score_random  = "Punktewert, zuf\U000000E4llig",
                        score_highest = "Punktewert, h\U000000F6chster",
                        played_random = "Spielh\U000000E4ufigkeit, zuf\U000000E4llig",
                        played_lowest = "Spielh\U000000E4ufigkeit, niedrigste",
                        days_random   = "Datum, zuf\U000000E4llig",
                        days_oldest   = "Datum, \U000000E4ltestes",
                        sequential    = "sequenziell")

      if (mode == "play") {
         text(0.00, 0.45, paste0("Modus:   ", "Spielen (Selektion: ", selmode, ")"), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.30, paste0("Name:    ", "(", seqnum, ") ", seqname), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.15, paste0("Spieler: ", player), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.00, paste0("Zug:     ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.15, paste0("Gespielt: ", played), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.00, paste0("Punkte:   ", score), pos=4, cex=cex, family=font, col=col)
      }

   }

}
