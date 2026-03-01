.colorsettings <- function(cols.all, pos, flip, mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove) {

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

   .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
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
         .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
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

.cexsettings <- function(pos, flip, mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove) {

   cat(.text("currentsettings"))

   tab <- data.frame(cex = c("cex.top", "cex.bot", "cex.eval", "cex.coords", "cex.matdiff", "cex.plots", "cex.glyphs"),
                     val = c(.get("cex.top"), .get("cex.bot"), .get("cex.eval"), .get("cex.coords"), .get("cex.matdiff"), .get("cex.plots"), .get("cex.glyphs")))
   tab$explanation <- .text("cexsetexpl")
   names(tab) <- c("", "", "")

   coords <- .get("coords")
   timed <- .get("timed")
   zenmode <- .get("zenmode")
   assign("coords", TRUE, envir=.chesstrainer)
   assign("timed", FALSE, envir=.chesstrainer)
   assign("zenmode", FALSE, envir=.chesstrainer)

   .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
   .draweval(0.2, flip=flip)

   while (TRUE) {
      print(tab, right=FALSE, print.gap=3)
      cat("\n")
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
         .redrawall(pos, flip, mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
         .draweval(0.2, flip=flip)
      }
   }

   assign("coords", coords, envir=.chesstrainer)
   assign("timed", timed, envir=.chesstrainer)
   assign("zenmode", zenmode, envir=.chesstrainer)

   return()

}

.miscsettings <- function(multiplier, adjustwrong, adjusthint, evalsteps, timepermove, idletime, mintime, sleepadj) {

   cat(.text("currentsettings"))

   tab <- data.frame(setting = c("multiplier", "adjustwrong", "adjusthint", "evalsteps", "timepermove", "idletime", "mintime", "sleepadj"),
                     val     = c(multiplier, adjustwrong, adjusthint, evalsteps, timepermove, idletime, mintime, sleepadj))
   tab$explanation <- .text("miscsetexpl")
   names(tab) <- c("", "", "")

   while (TRUE) {
      print(tab, right=FALSE, print.gap=3)
      cat("\n")
      resp <- readline(prompt=.text("settingwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[1-8]+$", resp)) {
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
         if (setno %in% c(2,3,5,8))
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

.lichesssettings <- function(speeds, ratings, lichessdb, barlen, invertbar, cachedir) {

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=.get("lwd")+3)

   cex <- 1

   title.xpos <- 1.5
   title.ypos <- 8 - 1.4 * c(0:4)
   text(title.xpos, title.ypos[1], .text("speed"),     pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[2], .text("rating"),    pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[3], .text("lichessdb"), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5, title.ypos[3], .text("delcache"), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[4], .text("barlen"),    pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[5], .text("invertbar"), pos=4, cex=cex, family=font.mono, col=col.help, font=2)

   speeds.opts <- c("ultraBullet", "bullet", "blitz", "rapid", "classical", "correspondence")
   speeds.xpos <- 2.2 + 1.1 * (seq_along(speeds.opts) - 1)
   speeds.ypos <- title.ypos[1] - 0.4 * (title.ypos[1]-title.ypos[2])
   speeds.txt  <- c("ultra", "bullet", "blitz", "rapid", "classic", "corresp")
   speeds.on   <- speeds.opts %in% strsplit(speeds, ",", fixed=TRUE)[[1]]
   speeds.box  <- list()
   for (i in seq_along(speeds.txt)) {
      speeds.box[[i]] <- .drawbutton(speeds.xpos[i], speeds.ypos, text=speeds.txt[i], len=max(nchar(speeds.txt)), on=speeds.on[i], cex=cex)
   }

   ratings.opts <- c(0, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2500)
   ratings.xpos <- 2 + 0.74 * (seq_along(ratings.opts) - 1)
   ratings.ypos <- title.ypos[2] - 0.4 * (title.ypos[1]-title.ypos[2])
   ratings.txt  <- ratings.opts
   ratings.on   <- ratings.opts %in% strsplit(ratings, ",", fixed=TRUE)[[1]]
   ratings.box  <- list()
   for (i in seq_along(ratings.txt)) {
      ratings.box[[i]] <- .drawbutton(ratings.xpos[i], ratings.ypos, text=ratings.txt[i], len=max(nchar(ratings.txt)), on=ratings.on[i], cex=cex)
   }

   lichessdb.opts <- c("lichess", "masters")
   lichessdb.xpos <- 2.2 + 1.2 * (seq_along(lichessdb.opts) - 1)
   lichessdb.ypos <- title.ypos[3] - 0.4 * (title.ypos[1]-title.ypos[2])
   lichessdb.txt  <- c("players", "masters")
   lichessdb.on   <- lichessdb.opts == lichessdb
   lichessdb.box  <- list()
   for (i in seq_along(lichessdb.txt)) {
      lichessdb.box[[i]] <- .drawbutton(lichessdb.xpos[i], lichessdb.ypos, text=lichessdb.txt[i], len=max(nchar(lichessdb.txt)), on=lichessdb.on[i], cex=cex)
   }

   delcache.opts <- c("players", "masters")
   delcache.xpos <- 5.7 + 1.2 * (seq_along(delcache.opts) - 1)
   delcache.ypos <- title.ypos[3] - 0.4 * (title.ypos[1]-title.ypos[2])
   delcache.txt  <- delcache.opts
   delcache.on   <- c(FALSE, FALSE)
   delcache.box  <- list()
   for (i in seq_along(delcache.txt)) {
      delcache.box[[i]] <- .drawbutton(delcache.xpos[i], delcache.ypos, text=delcache.txt[i], len=max(nchar(delcache.txt)), on=delcache.on[i], cex=cex)
   }

   barlen.xpos <- c(2,8)
   barlen.ypos <- title.ypos[4] - 0.4 * (title.ypos[1]-title.ypos[2])
   barlen.box  <- .drawslider(x=barlen.xpos, barlen.ypos, xlab=c(10,100), cex=cex)
   .updateslider(NULL, barlen.ypos, oldval=barlen, xlim=barlen.xpos, range=c(10,100), round=TRUE, cex=cex)

   invertbar.opts <- c("No", "Yes")
   invertbar.xpos <- 2 + 0.8 * (seq_along(invertbar.opts) - 1)
   invertbar.ypos <- title.ypos[5] - 0.4 * (title.ypos[1]-title.ypos[2])
   invertbar.txt  <- invertbar.opts
   invertbar.on   <- c(!invertbar, invertbar)
   invertbar.box  <- list()
   for (i in seq_along(invertbar.txt)) {
      invertbar.box[[i]] <- .drawbutton(invertbar.xpos[i], invertbar.ypos, text=invertbar.txt[i], len=max(nchar(invertbar.txt)), on=invertbar.on[i], cex=cex)
   }

   while (TRUE) {

      plt <- par("plt")

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(resp)) {

         xy <- .calcxy(resp[1], resp[2], plt)

         hit <- sapply(speeds.box, function(coords) xy[1] >= coords[1] & xy[2] >= coords[2] & xy[1] <= coords[3] & xy[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            speeds.on[i] <- !speeds.on[i]
            if (all(!speeds.on)) {
               speeds.on[i] <- !speeds.on[i]
               next
            }
            .drawbutton(speeds.xpos[i], speeds.ypos, text=speeds.txt[i], len=max(nchar(speeds.txt)), on=speeds.on[i], cex=cex)
            next
         }

         hit <- sapply(ratings.box, function(coords) xy[1] >= coords[1] & xy[2] >= coords[2] & xy[1] <= coords[3] & xy[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (all(!ratings.on)) {
               ratings.on[i] <- !ratings.on[i]
               next
            }
            ratings.on[i] <- !ratings.on[i]
            .drawbutton(ratings.xpos[i], ratings.ypos, text=ratings.txt[i], len=max(nchar(ratings.txt)), on=ratings.on[i], cex=cex)
            next
         }

         hit <- sapply(lichessdb.box, function(coords) xy[1] >= coords[1] & xy[2] >= coords[2] & xy[1] <= coords[3] & xy[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (lichessdb.on[i])
               next
            lichessdb.on <- !lichessdb.on
            for (i in seq_along(lichessdb.txt)) {
               .drawbutton(lichessdb.xpos[i], lichessdb.ypos, text=lichessdb.txt[i], len=max(nchar(lichessdb.txt)), on=lichessdb.on[i], cex=cex)
            }
            next
         }

         hit <- sapply(delcache.box, function(coords) xy[1] >= coords[1] & xy[2] >= coords[2] & xy[1] <= coords[3] & xy[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            .texttop(.text("rlydelcache", lichessdb.txt[i]))
            answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
            answer <- .confirm(answer)
            if (answer) {
               .texttop(.text("deletingcache"), sleep=1)
               unlink(file.path(cachedir, lichessdb.opts[i], "*"))
            }
            .texttop("")
            next
         }

         hit <- xy[1] >= barlen.box[1] & xy[2] >= barlen.box[2] & xy[1] <= barlen.box[3] & xy[2] <= barlen.box[4]
         if (hit) {
            barlen <- .updateslider(xy[1], barlen.ypos, oldval=barlen, xlim=barlen.xpos, range=c(10,100), round=TRUE, cex=cex)
            next
         }

         hit <- sapply(invertbar.box, function(coords) xy[1] >= coords[1] & xy[2] >= coords[2] & xy[1] <= coords[3] & xy[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (invertbar.on[i])
               next
            invertbar.on <- !invertbar.on
            for (i in seq_along(invertbar.txt)) {
               .drawbutton(invertbar.xpos[i], invertbar.ypos, text=invertbar.txt[i], len=max(nchar(invertbar.txt)), on=invertbar.on[i], cex=cex)
            }
            next
         }

      }

      if (identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-["))
         break

   }

   speeds    <- paste0(speeds.opts[speeds.on],   collapse=",")
   ratings   <- paste0(ratings.opts[ratings.on], collapse=",")
   lichessdb <- lichessdb.opts[lichessdb.on]
   invertbar <- invertbar.on[2]

   out <- list(speeds=speeds, ratings=ratings, lichessdb=lichessdb, barlen=barlen, invertbar=invertbar)

   #.erase(1, 1, 9, 9)

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

   if (identical(click, "ctrl-L")) {
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
