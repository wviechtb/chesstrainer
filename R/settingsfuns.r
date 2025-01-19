.colorsettings <- function(cols.all, pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, selmode) {

   cat(.text("currentsettings"))

   tab <- data.frame(col=cols.all, val=unname(sapply(cols.all, function(x) .get(x))))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode)
   .addrect(4, 5, col=.get("col.hint"), lwd=lwd)
   .addrect(4, 3, col=.get("col.wrong"), lwd=lwd)
   .addrect(4, 4, col=.get("col.rect"), lwd=lwd)
   .drawsquare(0, 4, col=.get("col.square.be"))
   .drawsquare(0, 5, col=.get("col.square.be"))
   .addrect(0, 4, offset=0.028, .get("col.bg"), lwd+2)
   .addrect(0, 5, offset=0.028, .get("col.bg"), lwd+2)
   .addcircle(4, 6, lwd=lwd)
   .drawsideindicator("w", flip)
   .drawsideindicator("b", flip, clear=FALSE)
   .draweval(0, flip)

   while (TRUE) {
      resp <- readline(prompt=.text("colwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[0-9]+$", resp)) {
         colno <- round(as.numeric(resp))
         if (colno < 1 || colno > nrow(tab))
            next
         col <- readline(prompt=.text("colval", tab[colno,2]))
         if (identical(col, ""))
            next
         assign(tab[colno,1], col, envir=.chesstrainer)
         tab[colno,2] <- col
         .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode)
         .addrect(4, 5, col=.get("col.hint"), lwd=lwd)
         .addrect(4, 3, col=.get("col.wrong"), lwd=lwd)
         .addrect(4, 4, col=.get("col.rect"), lwd=lwd)
         .drawsquare(0, 4, col=.get("col.square.be"))
         .drawsquare(0, 5, col=.get("col.square.be"))
         .addrect(0, 4, offset=0.028, .get("col.bg"), lwd+2)
         .addrect(0, 5, offset=0.028, .get("col.bg"), lwd+2)
         .addcircle(4, 6, lwd=lwd)
         .drawsideindicator("w", flip)
         .drawsideindicator("b", flip, clear=FALSE)
         .draweval(0, flip)
      }
   }

   invisible()

}

.cexsettings <- function(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, selmode) {

   cat(.text("currentsettings"))

   tab <- data.frame(cex = c("cex.top", "cex.bot", "cex.eval"),
                     val = c(.get("cex.top"), .get("cex.bot"), .get("cex.eval")))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode)
   .draweval(-0.2, flip)

   while (TRUE) {
      resp <- readline(prompt=.text("cexwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[0-9]+$", resp)) {
         cexno <- round(as.numeric(resp))
         if (cexno < 1 || cexno > nrow(tab))
            next
         cex <- readline(prompt=.text("cexval", tab[cexno,2]))
         if (identical(cex, ""))
            next
         cex <- as.numeric(cex)
         cex[cex < 0.1] <- 0.1
         assign(tab[cexno,1], cex, envir=.chesstrainer)
         tab[cexno,2] <- cex
         .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode)
         .draweval(-0.2, flip)
      }
   }

   invisible()

}

.miscsettings <- function(multiplier, adjustwrong, adjusthint, evalsteps) {

   cat(.text("currentsettings"))

   tab <- data.frame(setting = c("multiplier", "adjustwrong", "adjusthint", "evalsteps"),
                     val     = c(multiplier, adjustwrong, adjusthint, evalsteps))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   while (TRUE) {
      resp <- readline(prompt=.text("settingwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[0-9]+$", resp)) {
         setno <- round(as.numeric(resp))
         if (setno < 1 || setno > nrow(tab))
            next
         val <- readline(prompt=.text("settingval", tab[setno,2]))
         if (identical(val, ""))
            next
         val <- as.numeric(val)
         if (setno == 1) {
            val[val < 0] <- 0
            val[val > 1] <- 1
         }
         if (setno == 2 || setno == 3)
            val[val < 0] <- 0
         if (setno == 4)
            val[val < 2] <- 2
         tab[setno,2] <- val
      }
   }

   out <- as.list(tab[[2]])
   names(out) <- tab[[1]]

   return(out)

}

.selmodesetting <- function(selmode, lwd) {

   lang <- .get("lang")

   if (lang == "en") {

      txt <- c(
      "Choose a selection mode:",
      "",
      "1 - based on the score, at random",
      "2 - based on the score, highest score",
      "3 - based on the play frequency, at random",
      "4 - based on the play frequency, lowest frequency",
      "5 - based on the date, at random",
      "6 - based on the date, oldest date",
      "7 - sequential")

   }

   if (lang == "de") {

      txt <- c(
      "Selektionsmodus w\U000000E4hlen:",
      "",
      "1 - basierend auf dem Punktewert, zuf\U000000E4llig",
      "2 - basierend auf dem Punktewert, h\U000000F6chster Punktewert",
      "3 - basierend auf der Spielh\U000000E4ufigkeit, zuf\U000000E4llig",
      "4 - basierend auf der Spielh\U000000E4ufigkeit, niedrigste H\U000000E4ufigkeit",
      "5 - basierend auf dem Datum, zuf\U000000E4llig",
      "6 - basierend auf dem Datum, \U000000E4ltestes Datum",
      "7 - sequenziell")

   }

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
   maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
   cex <- (8.5 - 1.5) / max(maxsw, maxsh) * 0.95

   selmodes <- c("score_random", "score_highest", "played_random", "played_lowest", "days_random", "days_oldest", "sequential")

   text(1+0.5, seq(6.5, 3.5, length.out=length(txt)), txt, pos=4, cex=cex,
        family=.get("font.mono"), font=ifelse(c("","",selmodes)==selmode, 2, 1), col=.get("col.help"))

   wait <- TRUE

   while (wait) {

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=function(key) return(key))

      if (identical(resp, "\r") || identical(resp, "q") || identical(resp, "\033"))
         wait <- FALSE

      for (j in 1:length(selmodes)) {
         if (identical(resp, as.character(j))) {
            selmode <- selmodes[j]
            wait <- FALSE
         }
      }

   }

   return(selmode)

}