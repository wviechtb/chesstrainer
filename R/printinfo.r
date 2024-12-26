.printinfo <- function(mode, show, player, seqname, score, played, i, totalmoves, random) {

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

      if (mode == "play") {
         text(0.00, 0.45, paste0("Mode:   ", "Play (", ifelse(random, "random", "sequential"), " mode)"), pos=4, cex=cex, family=font, col=col)
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
         text(0.00, 0.45, paste0("Modus:   ", "Spielen (", ifelse(random, "Zufallsmodus", "sequentieller Modus"), ")"), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.30, paste0("Spieler: ", player), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.15, paste0("Name:    ", seqname), pos=4, cex=cex, family=font, col=col)
         text(0.00, 0.00, paste0("Zug:     ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.15, paste0("Punkte:   ", score), pos=4, cex=cex, family=font, col=col)
         text(9.00, 0.00, paste0("Gespielt: ", played), pos=4, cex=cex, family=font, col=col)
      }

   }

}
