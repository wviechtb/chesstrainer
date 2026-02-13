.colorsettings <- function(cols.all, pos, flip, mode, show, showcomp, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove) {

   cat(.text("currentsettings"))

   tab <- data.frame(col=cols.all, val=unname(sapply(cols.all, function(x) .get(x))))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   coords <- .get("coords")
   timed <- .get("timed")
   zenmode <- .get("zenmode")
   assign("coords", TRUE, envir=.chesstrainer)
   assign("timed", FALSE, envir=.chesstrainer)
   assign("zenmode", FALSE, envir=.chesstrainer)

   .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
   .addrect(4, 5, col=.get("col.hint"))
   .addrect(4, 3, col=.get("col.wrong"))
   .addrect(4, 4, col=.get("col.rect"))
   .drawsquare(0, 4, flip, col=.get("col.square.be"))
   .drawsquare(0, 5, flip, col=.get("col.square.be"))
   .addrect(0, 4, .get("col.bg"), lwdadj=2)
   .addrect(0, 5, .get("col.bg"), lwdadj=2)
   .drawcircle(4, 6)
   .drawarrow(3, 7, 6, 7)
   .drawarrow(3, 8, 6, 8, col=adjustcolor(.get("col.best"), alpha.f=0.5))
   .drawsideindicator("w", flip=flip)
   .drawsideindicator("b", flip=flip, clear=FALSE)
   .draweval(0.2, flip=flip)
   .drawtimer(settings=TRUE)

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
         .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
         .addrect(4, 5, col=.get("col.hint"))
         .addrect(4, 3, col=.get("col.wrong"))
         .addrect(4, 4, col=.get("col.rect"))
         .drawsquare(0, 4, flip, col=.get("col.square.be"))
         .drawsquare(0, 5, flip, col=.get("col.square.be"))
         .addrect(0, 4, .get("col.bg"), lwdadj=2)
         .addrect(0, 5, .get("col.bg"), lwdadj=2)
         .drawcircle(4, 6)
         .drawarrow(3, 7, 6, 7)
         .drawarrow(3, 8, 6, 8, col=adjustcolor(.get("col.best"), alpha.f=0.5))
         .drawsideindicator("w", flip=flip)
         .drawsideindicator("b", flip=flip, clear=FALSE)
         .draweval(0.2, flip=flip)
         .drawtimer(settings=TRUE)
      }
   }

   assign("coords", coords, envir=.chesstrainer)
   assign("timed", timed, envir=.chesstrainer)
   assign("zenmode", zenmode, envir=.chesstrainer)

   return()

}

.cexsettings <- function(pos, flip, mode, show, showcomp, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove) {

   cat(.text("currentsettings"))

   tab <- data.frame(cex = c("cex.top", "cex.bot", "cex.eval", "cex.coords", "cex.matdiff", "cex.plots", "cex.glyphs"),
                     val = c(.get("cex.top"), .get("cex.bot"), .get("cex.eval"), .get("cex.coords"), .get("cex.matdiff"), .get("cex.plots"), .get("cex.glyphs")))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   coords <- .get("coords")
   timed <- .get("timed")
   zenmode <- .get("zenmode")
   assign("coords", TRUE, envir=.chesstrainer)
   assign("timed", FALSE, envir=.chesstrainer)
   assign("zenmode", FALSE, envir=.chesstrainer)

   .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
   .draweval(0.2, flip=flip)

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
         .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
         .draweval(0.2, flip=flip)
      }
   }

   assign("coords", coords, envir=.chesstrainer)
   assign("timed", timed, envir=.chesstrainer)
   assign("zenmode", zenmode, envir=.chesstrainer)

   return()

}

.miscsettings <- function(multiplier, adjustwrong, adjusthint, evalsteps, timepermove, idletime, mintime) {

   cat(.text("currentsettings"))

   tab <- data.frame(setting = c("multiplier", "adjustwrong", "adjusthint", "evalsteps", "timepermove", "idletime", "mintime"),
                     val     = c(multiplier, adjustwrong, adjusthint, evalsteps, timepermove, idletime, mintime))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   while (TRUE) {
      resp <- readline(prompt=.text("settingwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[1-7]+$", resp)) {
         setno <- round(as.numeric(resp))
         if (setno < 1 || setno > nrow(tab))
            next
         val <- readline(prompt=.text("settingval", tab[setno,2]))
         if (identical(val, ""))
            next
         val <- as.numeric(val)
         if (is.na(val))
            next
         if (setno == 1) {
            val[val < 0] <- 0
            val[val > 1] <- 1
         }
         if (setno %in% c(2,3,5))
            val[val < 0] <- 0
         if (setno == 4) {
            val[val < 2] <- 2
            val <- round(val)
         }
         if (setno %in% c(6,7)) {
            val[val < 1] <- 1
            val <- round(val)
         }
         tab[setno,2] <- val
      }
   }

   out <- as.list(tab[[2]])
   names(out) <- tab[[1]]

   return(out)

}

.showsettings <- function(tab) {

   #.clearsideindicator()
   #.drawtimer(clear=TRUE)

   lwd <- tab$lwd

   lang <- .get("lang")
   tab$lang <- lang

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   seqdir <- tab$seqdir
   sfpath <- tab$sfpath

   tab.save <- tab

   if (!is.na(tab$sflim)) {
      if (tab$sflim <= 20) {
         tab$sflim <- paste(tab$sflim, "(level)")
      } else {
         tab$sflim <- paste(tab$sflim, "(Elo)")
      }
   }

   #tab$eval <- paste0(names(tab$eval[tab$eval]), collapse=", ")
   tab$eval <- paste0(tab$eval, collapse="/")
   if (lang == "en") {
      tab$eval <- gsub("TRUE", "Yes", tab$eval, fixed=TRUE)
      tab$eval <- gsub("FALSE", "No", tab$eval, fixed=TRUE)
   }
   if (lang == "de") {
      tab$eval <- gsub("TRUE", "Ja",    tab$eval, fixed=TRUE)
      tab$eval <- gsub("FALSE", "Nein", tab$eval, fixed=TRUE)
   }

   tab$mar  <- paste0(tab$mar,  collapse="/")
   tab$mar2 <- paste0(tab$mar2, collapse="/")

   tab <- as.data.frame(tab)

   tab <- t(tab)
   tab <- cbind(tab, .text("explsettings"))
   colnames(tab) <- c("", "")

   tab <- tab[rownames(tab) != "seqdir",]
   tab <- tab[rownames(tab) != "sfpath",]

   txt <- capture.output(print(tab, quote=FALSE, print.gap=3))
   txt <- txt[-1]

   maxchars <- max(nchar(txt))

   #lines <- paste0(rep("-", maxchars), collapse="")
   #txt <- c(lines, txt[-1], lines)

   if (nchar(seqdir) > maxchars-3)
      seqdir <- paste0("...", substr(seqdir, nchar(seqdir)-maxchars-3+1, nchar(seqdir)))
   if (nchar(sfpath) > maxchars-3)
      sfpath <- paste0("...", substr(sfpath, nchar(sfpath)-maxchars-3+1, nchar(sfpath)))

   txt <- gsub("<NA>", "NA  ", txt, fixed=TRUE)
   sfpos <- grep("difffun", txt, fixed=TRUE)
   txt <- c(.text("generalsettings"), txt[1:(sfpos-1)], "", .text("seqdirsettings"), seqdir, "", .text("sfsettings"), txt[sfpos:length(txt)], "", .text("sfpathsettings"), sfpath)

   cex <- .findcex(txt, font=font.mono, x1=1.5, x2=8.2, y1=2.0, y2=8.0)

   ypos <- seq(8.5, 1.5, length.out=length(txt))

   langswitch <- FALSE

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   text(1.5, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=ifelse(grepl(":", txt), 2, 1), col=col.help)

   click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

   if (identical(click, "i")) {
      if (lang == "de") {
         lang <- "en"
      } else {
         lang <- "de"
      }
      assign("lang", lang, envir=.chesstrainer)
      langswitch <- TRUE
   }

   if (langswitch)
      .showsettings(tab.save)

   #.erase(1, 1, 9, 9)

   return()

}
