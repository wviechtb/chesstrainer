.selmode <- function(selmode, lwd) {

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
      "7 - based on the RMSSD, at random",
      "8 - based on the RMSSD, highest value",
      "9 - sequential")

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
      "7 - basierend auf der RMSSD, zuf\U000000E4llig",
      "8 - basierend auf der RMSSD, h\U000000F6chster Wert",
      "9 - sequenziell")

   }

   rect(1.2, 1.2, 8.8, 8.8, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(strwidth(txt, family=.get("font.mono")))
   maxsh <- strheight("A", family=.get("font.mono")) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.9)

   selmodes <- c("score_random", "score_highest", "played_random", "played_lowest", "days_random", "days_oldest", "rmssd_random", "rmssd_highest", "sequential")

   oldmode <- which(selmode == selmodes)

   ypos <- seq(7.5, 3.5, length.out=length(txt))

   text(1+0.5, ypos, txt, pos=4, cex=cex,
        family=.get("font.mono"), font=ifelse(c("","",selmodes)==selmode, 2, 1), col=.get("col.help"))

   ypos.modes <- ypos[-c(1:2)]
   dist <- (ypos.modes[1] - ypos.modes[2]) / 2

   while (TRUE) {

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(click)) {

         x <- grconvertX(click[[1]], from="ndc", to="user")
         y <- grconvertX(click[[2]], from="ndc", to="user")

         if (x >= 1.5 && x <= 8) {
            click <- which(y < ypos.modes + dist & y > ypos.modes - dist)
            if (length(click) == 1L) {
               selmode <- selmodes[click]
               break
            }
         }

      } else {

         if (identical(click, "\r") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-["))
            break

         if (is.element(click, 1:length(selmodes))) {
            click <- as.numeric(click)
            selmode <- selmodes[click]
            break
         }

      }

   }

   if (selmodes[oldmode] != selmode) {
      rect(1.5, ypos.modes[oldmode]-dist, 8, ypos.modes[oldmode]+dist, col=.get("col.bg"), border=NA)
      text(1+0.5, ypos.modes[oldmode], txt[oldmode+2], pos=4, cex=cex, family=.get("font.mono"), col=.get("col.help"))
      text(1+0.5, ypos.modes[click], txt[click+2], pos=4, cex=cex, family=.get("font.mono"), font=2, col=.get("col.help"))
      Sys.sleep(1)
   }

   #.erase(1, 1, 9, 9)

   return(selmode)

}

.loadselmode <- function(seqdir, seqdirpos, selmode, texttop=FALSE) {

   if (file.exists(file.path(seqdir[seqdirpos], ".selmode"))) {
      tmp <- try(read.table(file.path(seqdir[seqdirpos], ".selmode"), header=FALSE), silent=TRUE) # if .selmode is empty, get an error
      if (inherits(tmp, "try-error")) {
         write.table(data.frame(selmode), file=file.path(seqdir[seqdirpos], ".selmode"), col.names=FALSE, row.names=FALSE, quote=FALSE)
      } else {
         selmode <- tmp[[1]]
      }
   } else {
      write.table(data.frame(selmode), file=file.path(seqdir[seqdirpos], ".selmode"), col.names=FALSE, row.names=FALSE, quote=FALSE)
   }

   # .sequential file in sequence directory overrides selmode

   if (file.exists(file.path(seqdir[seqdirpos], ".sequential")) && selmode != "sequential") {
      if (texttop)
         .texttop(.text("sequential"), sleep=1.5)
      selmode <- "sequential"
   }

   return(selmode)

}
