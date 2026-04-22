.vizsettings <- function(cols.all, pos, flip=FALSE, show=TRUE, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove) {

   mode       <- .get("mode")
   showcoords <- .get("showcoords")
   timed      <- .get("timed")
   zenmode    <- .get("zenmode")
   x2y2       <- .get("x2y2")
   score      <- .get("score")
   lasteval   <- .get("lasteval")
   assign("mode", "add", envir=.chesstrainer)
   assign("timed", FALSE, envir=.chesstrainer)
   assign("zenmode", FALSE, envir=.chesstrainer)
   assign("x2y2", c(2,5), envir=.chesstrainer)

   pos <- .get("pos")
   pos[7:8,1:2] <- ""

   tab1 <- data.frame(x=cols.all, val=sapply(cols.all, function(x) .get(x), USE.NAMES=FALSE))
   tab1$explanation <- .text("colsetexpl")

   tab2 <- data.frame(x = c("cex.top", "cex.bot", "cex.eval", "cex.coords", "cex.matdiff", "cex.plots", "cex.glyphs"),
                      val = c(.get("cex.top"), .get("cex.bot"), .get("cex.eval"), .get("cex.coords"), .get("cex.matdiff"), .get("cex.plots"), .get("cex.glyphs")))
   tab2$explanation <- .text("cexsetexpl")

   tab <- rbind(tab1, c("", "", ""), tab2)

   tab <- rbind(tab, c("", "", ""))
   tab <- rbind(tab, c("scheme.brown", "", .text("scheme_brown")))
   tab <- rbind(tab, c("scheme.green", "", .text("scheme_green")))
   tab <- rbind(tab, c("scheme.blue",  "", .text("scheme_blue")))
   tab <- rbind(tab, c("scheme.gray",  "", .text("scheme_gray")))

   tab3 <- data.frame(x = "showcoords", val=ifelse(showcoords, .text("yes"), .text("no")), explanation=.text("showcoordsexpl"))
   tab <- rbind(tab, c("", "", ""), tab3)

   names(tab) <- c("", "", "")

   numbers.col    <- which(startsWith(tab[,1], "col."))
   numbers.cex    <- which(startsWith(tab[,1], "cex."))
   numbers.scheme <- which(startsWith(tab[,1], "scheme."))
   number.coords  <- which(tab[,1] == "showcoords")

   .redrawall(pos, flip, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
   .addrect(4, 5, col=.get("col.hint"))
   .addrect(4, 3, col=.get("col.wrong"))
   .addrect(4, 4, col=.get("col.rect"))
   .drawmatdiff(pos, flip=FALSE, force=TRUE)
   .drawsquare(0, 4, flip, col=.get("col.square.be"))
   .drawsquare(0, 5, flip, col=.get("col.square.be"))
   .addrect(0, 4, .get("col.bg"), lwdadj=2)
   .addrect(0, 5, .get("col.bg"), lwdadj=2)
   .drawcircle(4, 6)
   .drawarrow(3, 7, 6, 7)
   .drawarrow(3, 8, 6, 8, col=adjustcolor(.get("col.best"), alpha.f=0.7))
   .drawsideindicator("w", flip=flip)
   .drawsideindicator("b", flip=flip, clear=FALSE)
   .drawevalbar(0.2, flip=flip)
   .drawlibar(totals=c(110,10,90))
   .drawtimer(settings=TRUE)
   .drawglyph("!!")
   rect(1.2, 1.2, 3.8, 3.8, col=.get("col.bg"), border=.get("col.border"), lwd=.get("lwd")+3)

   while (TRUE) {
      .flush()
      cat(.text("currentsettings"))
      print(tab, right=FALSE, print.gap=3)
      cat("\n")
      resp <- readline(prompt=.text("whichsetting"))
      if (identical(resp, ""))
         break
      if (grepl("^[0-9]+$", resp)) {
         number <- round(as.numeric(resp))
         if (number < 1 || number > nrow(tab))
            next
         if (number %in% which(tab[,1] == ""))
            next
         if (number %in% numbers.col)
            val <- readline(prompt=.text("colval", tab[number,2]))
         if (number %in% numbers.cex)
            val <- readline(prompt=.text("cexval", tab[number,2]))
         if (number %in% numbers.scheme) {
            if (number == which(tab[,1] == "scheme.brown"))
               scheme <- c(col.bg="#211b12", col.fg="#7a6d59", col.square.l="#f0d9b5", col.square.d="#b58863", col.top="#b09d7f", col.bot="#b09d7f", col.help="#b09d7f", col.border="#63462e", col.rect="darkseagreen4")
            if (number == which(tab[,1] == "scheme.green"))
               scheme <- c(col.bg="#211b12", col.fg="#5b7a59", col.square.l="#ffffdd", col.square.d="#86a666", col.top="#aabbaa", col.bot="#aabbaa", col.help="#aabbaa", col.border="#335533", col.rect="gray40")
            if (number == which(tab[,1] == "scheme.blue"))
               scheme <- c(col.bg="#101010", col.fg="#596e7a", col.square.l="#dee3e6", col.square.d="#8ca2ad", col.top="#7f99b0", col.bot="#7f99b0", col.help="#7f99b0", col.border="#2e5163", col.rect="gray40")
            if (number == which(tab[,1] == "scheme.gray"))
               scheme <- c(col.bg="#101010", col.fg="#797979", col.square.l="#a9a9a9", col.square.d="#6b6b6b", col.top="#aeaeae", col.bot="#aeaeae", col.help="#aeaeae", col.border="#646464", col.rect="darkseagreen4")
            tab[which(tab[,1] %in% names(scheme)),2] <- unname(scheme)
            for (j in 1:length(scheme))
               assign(names(scheme[j]), unname(scheme[j]), envir=.chesstrainer)
         }
         if (number %in% c(numbers.col, numbers.cex)) {
            val <- trimws(val)
            if (identical(val, ""))
               next
            if (number %in% numbers.col) {
               if (!(is.element(val, colors()) || grepl("^#(?:[0-9A-Fa-f]{6}|[0-9A-Fa-f]{2})$", val)))
                  next
               assign(tab[number,1], val, envir=.chesstrainer)
               tab[number,2] <- val
            }
            if (number %in% numbers.cex) {
               if (!grepl("^[0-9.]+$", val))
                  next
               cex <- as.numeric(val)
               cex[cex < 0.1] <- 0.1
               assign(tab[number,1], cex, envir=.chesstrainer)
               tab[number,2] <- cex
            }
         }
         if (number == number.coords) {
            showcoords <- !showcoords
            assign("showcoords", showcoords, envir=.chesstrainer)
            tab[number.coords,2] <- ifelse(showcoords, .text("yes"), .text("no"))
         }
         .redrawall(pos, flip, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop="Lorem ipsum", sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove)
         .addrect(4, 5, col=.get("col.hint"))
         .addrect(4, 3, col=.get("col.wrong"))
         .addrect(4, 4, col=.get("col.rect"))
         .drawmatdiff(pos, flip=FALSE, force=TRUE)
         .drawsquare(0, 4, flip, col=.get("col.square.be"))
         .drawsquare(0, 5, flip, col=.get("col.square.be"))
         .addrect(0, 4, .get("col.bg"), lwdadj=2)
         .addrect(0, 5, .get("col.bg"), lwdadj=2)
         .drawcircle(4, 6)
         .drawarrow(3, 7, 6, 7)
         .drawarrow(3, 8, 6, 8, col=adjustcolor(.get("col.best"), alpha.f=0.7))
         .drawsideindicator("w", flip=flip)
         .drawsideindicator("b", flip=flip, clear=FALSE)
         .drawevalbar(0.2, flip=flip)
         .drawlibar(totals=c(110,10,90))
         .drawtimer(settings=TRUE)
         .drawglyph("!!")
         rect(1.2, 1.2, 3.8, 3.8, col=.get("col.bg"), border=.get("col.border"), lwd=.get("lwd")+3)
      }
   }

   assign("mode", mode, envir=.chesstrainer)
   assign("timed", timed, envir=.chesstrainer)
   assign("zenmode", zenmode, envir=.chesstrainer)
   assign("x2y2", x2y2, envir=.chesstrainer)
   assign("score", score, envir=.chesstrainer)
   assign("lasteval", lasteval, envir=.chesstrainer)

   return()

}

.miscsettings <- function(multiplier, adjustwrong, adjusthint, timepermove, movestoshow, idletime, mintime, evalsteps, sleepadj) {

   col.bg     <- .get("col.bg")
   col.help   <- .get("col.help")
   col.border <- .get("col.border")
   font.mono  <- .get("font.mono")
   col.text   <- .get("col.square.d")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.border, lwd=.get("lwd")+3)

   cex <- 1
   cex.mult <- 0.9

   title.xpos <- 1.5
   title.ypos <- c(8.2 - 1.16 * c(0:6))

   text(title.xpos, title.ypos[1], .text("multiplier"),  pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[2], .text("adjustwrong"), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[2], .text("adjusthint"),  pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[3], .text("timepermove"), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[3], .text("movestoshow"), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[4], .text("idletime"),    pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[5], .text("mintime"),     pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[6], .text("evalsteps"),   pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[6], .text("sleepadj"),    pos=4, cex=cex, family=font.mono, col=col.help, font=2)

   multiplier.xpos <- c(1.7,8)
   multiplier.ypos <- title.ypos[1] - 0.4 * (title.ypos[1]-title.ypos[2])
   multiplier.box  <- .drawslider(x=multiplier.xpos, multiplier.ypos, xlab=c(1,1), cex=cex*cex.mult)
   .updateslider(NULL, multiplier.ypos, oldval=multiplier, xlim=multiplier.xpos, range=c(0,1), round=0.01, cex=cex*cex.mult)

   adjustwrong.xpos <- c(1.7,4.5)
   adjustwrong.ypos <- title.ypos[2] - 0.4 * (title.ypos[2]-title.ypos[3])
   adjustwrong.box  <- .drawslider(x=adjustwrong.xpos, adjustwrong.ypos, xlab=c(0,100), cex=cex*cex.mult)
   .updateslider(NULL, adjustwrong.ypos, oldval=adjustwrong, xlim=adjustwrong.xpos, range=c(0,100), round=TRUE, cex=cex*cex.mult)

   adjusthint.xpos <- c(5.4,8)
   adjusthint.ypos <- title.ypos[2] - 0.4 * (title.ypos[2]-title.ypos[3])
   adjusthint.box  <- .drawslider(x=adjusthint.xpos, adjusthint.ypos, xlab=c(0,100), cex=cex*cex.mult)
   .updateslider(NULL, adjusthint.ypos, oldval=adjusthint, xlim=adjusthint.xpos, range=c(0,100), round=TRUE, cex=cex*cex.mult)

   timepermove.xpos <- c(1.7,4.5)
   timepermove.ypos <- title.ypos[3] - 0.4 * (title.ypos[3]-title.ypos[4])
   timepermove.box  <- .drawslider(x=timepermove.xpos, timepermove.ypos, xlab=c(1,20), cex=cex*cex.mult)
   .updateslider(NULL, timepermove.ypos, oldval=timepermove, xlim=timepermove.xpos, range=c(1,20), round=TRUE, cex=cex*cex.mult)

   movestoshow.xpos <- c(5.4,8)
   movestoshow.ypos <- title.ypos[3] - 0.4 * (title.ypos[3]-title.ypos[4])
   movestoshow.box  <- .drawslider(x=movestoshow.xpos, movestoshow.ypos, xlab=c(0,20), cex=cex*cex.mult)
   .updateslider(NULL, movestoshow.ypos, oldval=movestoshow, xlim=movestoshow.xpos, range=c(0,20), round=TRUE, cex=cex*cex.mult)

   idletime.xpos <- c(1.7,8)
   idletime.ypos <- title.ypos[4] - 0.4 * (title.ypos[4]-title.ypos[5])
   idletime.box  <- .drawslider(x=idletime.xpos, idletime.ypos, xlab=c(1,240), cex=cex*cex.mult)
   .updateslider(NULL, idletime.ypos, oldval=idletime, xlim=idletime.xpos, range=c(1,240), round=TRUE, cex=cex*cex.mult)

   mintime.xpos <- c(1.7,8)
   mintime.ypos <- title.ypos[5] - 0.4 * (title.ypos[5]-title.ypos[6])
   mintime.box  <- .drawslider(x=mintime.xpos, mintime.ypos, xlab=c(1,240), cex=cex*cex.mult)
   .updateslider(NULL, mintime.ypos, oldval=mintime, xlim=mintime.xpos, range=c(1,240), round=TRUE, cex=cex*cex.mult)

   evalsteps.xpos <- c(1.7,4.5)
   evalsteps.ypos <- title.ypos[6] - 0.4 * (title.ypos[6]-title.ypos[7])
   evalsteps.box  <- .drawslider(x=evalsteps.xpos, evalsteps.ypos, xlab=c(2,20), cex=cex*cex.mult)
   .updateslider(NULL, evalsteps.ypos, oldval=evalsteps, xlim=evalsteps.xpos, range=c(2,20), round=TRUE, cex=cex*cex.mult)

   sleepadj.xpos <- c(5.4,8)
   sleepadj.ypos <- title.ypos[6] - 0.4 * (title.ypos[6]-title.ypos[7])
   sleepadj.box  <- .drawslider(x=sleepadj.xpos, sleepadj.ypos, xlab=c(0,2), cex=cex*cex.mult)
   .updateslider(NULL, sleepadj.ypos, oldval=sleepadj, xlim=sleepadj.xpos, range=c(0,2), round=0.1, cex=cex*cex.mult)

   .mousedownfun <- function(button,x,y) {
      if (length(button) == 0L)
         button <- 3
      button <<- button
      xy1 <<- .calcxy(x, y, plt)
      return(NULL)
   }

   .mouseupfun <- function(button,x,y) {
      xy2 <<- .calcxy(x, y, plt)
      return(1)
   }

   while (TRUE) {

      plt <- par("plt")

      button <- NULL
      xy1 <- NULL
      xy2 <- NULL

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onMouseUp=.mouseupfun, onKeybd=.keyfun)

      if (is.numeric(resp)) {

         if (button == 2)
            break

         if (button != 0)
            next

         hit <- xy1[1] >= multiplier.box[1] & xy1[2] >= multiplier.box[2] & xy1[1] <= multiplier.box[3] & xy1[2] <= multiplier.box[4]
         if (hit) {
            multiplier <- .updateslider(xy2[1], multiplier.ypos, oldval=multiplier, xlim=multiplier.xpos, range=c(0,1), round=0.01, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= adjustwrong.box[1] & xy1[2] >= adjustwrong.box[2] & xy1[1] <= adjustwrong.box[3] & xy1[2] <= adjustwrong.box[4]
         if (hit) {
            adjustwrong <- .updateslider(xy2[1], adjustwrong.ypos, oldval=adjustwrong, xlim=adjustwrong.xpos, range=c(0,100), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= adjusthint.box[1] & xy1[2] >= adjusthint.box[2] & xy1[1] <= adjusthint.box[3] & xy1[2] <= adjusthint.box[4]
         if (hit) {
            adjusthint <- .updateslider(xy2[1], adjusthint.ypos, oldval=adjusthint, xlim=adjusthint.xpos, range=c(0,100), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= timepermove.box[1] & xy1[2] >= timepermove.box[2] & xy1[1] <= timepermove.box[3] & xy1[2] <= timepermove.box[4]
         if (hit) {
            timepermove <- .updateslider(xy2[1], timepermove.ypos, oldval=timepermove, xlim=timepermove.xpos, range=c(1,20), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= movestoshow.box[1] & xy1[2] >= movestoshow.box[2] & xy1[1] <= movestoshow.box[3] & xy1[2] <= movestoshow.box[4]
         if (hit) {
            movestoshow <- .updateslider(xy2[1], movestoshow.ypos, oldval=movestoshow, xlim=movestoshow.xpos, range=c(0,20), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= idletime.box[1] & xy1[2] >= idletime.box[2] & xy1[1] <= idletime.box[3] & xy1[2] <= idletime.box[4]
         if (hit) {
            idletime <- .updateslider(xy2[1], idletime.ypos, oldval=idletime, xlim=idletime.xpos, range=c(1,20), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= mintime.box[1] & xy1[2] >= mintime.box[2] & xy1[1] <= mintime.box[3] & xy1[2] <= mintime.box[4]
         if (hit) {
            mintime <- .updateslider(xy2[1], mintime.ypos, oldval=mintime, xlim=mintime.xpos, range=c(0,240), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= evalsteps.box[1] & xy1[2] >= evalsteps.box[2] & xy1[1] <= evalsteps.box[3] & xy1[2] <= evalsteps.box[4]
         if (hit) {
            evalsteps <- .updateslider(xy2[1], evalsteps.ypos, oldval=evalsteps, xlim=evalsteps.xpos, range=c(2,20), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= sleepadj.box[1] & xy1[2] >= sleepadj.box[2] & xy1[1] <= sleepadj.box[3] & xy1[2] <= sleepadj.box[4]
         if (hit) {
            sleepadj <- .updateslider(xy2[1], sleepadj.ypos, oldval=sleepadj, xlim=sleepadj.xpos, range=c(0,2), round=0.1, cex=cex*cex.mult)
            next
         }

      }

      if (identical(resp, "F6") || identical(resp, "\r") || identical(resp, "ctrl-J") || identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-[") || identical(resp, " "))
         break

   }

   out <- list(multiplier=multiplier, adjustwrong=adjustwrong, adjusthint=adjusthint, timepermove=timepermove, movestoshow=movestoshow, idletime=idletime, mintime=mintime, evalsteps=evalsteps, sleepadj=sleepadj)

   #.erase(1, 1, 9, 9)

   return(out)

}

.sfsettings <- function(sfproc, sfrun, sfpath, usesfcache, depth1, depth2, depth3, sflim, multipv1, multipv2, threads, hash, hintdepth, monthssfcache) {

   lang       <- .get("lang")
   col.bg     <- .get("col.bg")
   col.help   <- .get("col.help")
   col.border <- .get("col.border")
   font.mono  <- .get("font.mono")
   col.text   <- .get("col.square.d")
   switch1    <- .get("switch1")
   switch2    <- .get("switch2")
   cachedir   <- .get("cachedir")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.border, lwd=.get("lwd")+3)

   cex <- 0.9
   cex.mult <- 0.8

   title.xpos <- 1.5
   title.ypos <- c(8.2, 7.8, 7.2 - 1.02 * c(0:6))
   sfpath2 <- sfpath
   pathlen <- nchar(sfpath2)
   maxlen <- 80
   if (pathlen >= maxlen)
      sfpath2 <- paste0("...", substr(sfpath2, max(1,nchar(sfpath2)-maxlen), nchar(sfpath2)))

   text(title.xpos, title.ypos[1], .text("sfpath", sfpath2), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[2], .text("sfrunning"),       pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[2], .text("usecacheshort:"),  pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[3], .text("depth1"),          pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[3], .text("multipv1"),        pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[4], .text("depth2"),          pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[4], .text("multipv2"),        pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[5], .text("depth3"),          pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[5], .text("hintdepth"),       pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[6], .text("sflim_level"),     pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[6], .text("sflim_elo"),       pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[7], .text("threads"),         pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5.2,        title.ypos[7], .text("hash"),            pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[8], .text("monthscache"),     pos=4, cex=cex, family=font.mono, col=col.help, font=2)

   sfrun.on   <- c(sfrun, !sfrun)
   sfrun.xpos <- 3.7 + 0.6 * (seq_along(sfrun.on) - 1)
   sfrun.ypos <- title.ypos[2]
   sfrun.txt  <- c(.text("on"), .text("off"))
   sfrun.box  <- list()
   for (i in seq_along(sfrun.txt)) {
      sfrun.box[[i]] <- .drawbutton(sfrun.xpos[i], sfrun.ypos, text=sfrun.txt[i], len=max(nchar(sfrun.txt)), on=sfrun.on[i], cex=cex)
   }

   usesfcache.opts <- c(TRUE, TRUE)
   if (lang == "en")
      usesfcache.xpos <- 6.8 + 0.6 * (seq_along(usesfcache.opts) - 1)
   if (lang == "de")
      usesfcache.xpos <- 7.1 + 0.7 * (seq_along(usesfcache.opts) - 1)
   usesfcache.ypos <- title.ypos[2]
   usesfcache.txt  <- c(.text("yes"), .text("no"))
   usesfcache.on   <- c(usesfcache, !usesfcache)
   usesfcache.box  <- list()
   for (i in seq_along(usesfcache.txt)) {
      usesfcache.box[[i]] <- .drawbutton(usesfcache.xpos[i], usesfcache.ypos, text=usesfcache.txt[i], len=max(nchar(usesfcache.txt)), on=usesfcache.on[i], cex=cex)
   }

   depth1.xpos <- c(1.7,4.5)
   depth1.ypos <- title.ypos[3] - 0.4 * (title.ypos[3]-title.ypos[4])
   depth1.box  <- .drawslider(x=depth1.xpos, depth1.ypos, xlab=c(1,50), cex=cex*cex.mult)
   .updateslider(NULL, depth1.ypos, oldval=depth1, xlim=depth1.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)

   depth2.xpos <- c(1.7,4.5)
   depth2.ypos <- title.ypos[4] - 0.4 * (title.ypos[4]-title.ypos[5])
   depth2.box  <- .drawslider(x=depth2.xpos, depth2.ypos, xlab=c(1,50), cex=cex*cex.mult)
   .updateslider(NULL, depth2.ypos, oldval=depth2, xlim=depth2.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)

   depth3.xpos <- c(1.7,4.5)
   depth3.ypos <- title.ypos[5] - 0.4 * (title.ypos[5]-title.ypos[6])
   depth3.box  <- .drawslider(x=depth3.xpos, depth3.ypos, xlab=c(1,50), cex=cex*cex.mult)
   .updateslider(NULL, depth3.ypos, oldval=depth3, xlim=depth3.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)

   multipv1.xpos <- c(5.4,8)
   multipv1.ypos <- title.ypos[3] - 0.4 * (title.ypos[3]-title.ypos[4])
   multipv1.box  <- .drawslider(x=multipv1.xpos, multipv1.ypos, xlab=1:5, cex=cex*cex.mult)
   .updateslider(NULL, multipv1.ypos, oldval=multipv1, xlim=multipv1.xpos, range=c(1,5), round=TRUE, cex=cex*cex.mult, text=FALSE)

   multipv2.xpos <- c(5.4,8)
   multipv2.ypos <- title.ypos[4] - 0.4 * (title.ypos[4]-title.ypos[5])
   multipv2.box  <- .drawslider(x=multipv2.xpos, multipv2.ypos, xlab=1:5, cex=cex*cex.mult)
   .updateslider(NULL, multipv2.ypos, oldval=multipv2, xlim=multipv2.xpos, range=c(1,5), round=TRUE, cex=cex*cex.mult, text=FALSE)

   hintdepth.xpos <- c(5.4,8)
   hintdepth.ypos <- title.ypos[5] - 0.4 * (title.ypos[5]-title.ypos[6])
   hintdepth.box  <- .drawslider(x=hintdepth.xpos, hintdepth.ypos, xlab=c(2,20), cex=cex*cex.mult)
   .updateslider(NULL, hintdepth.ypos, oldval=hintdepth, xlim=hintdepth.xpos, range=c(2,20), round=TRUE, cex=cex*cex.mult)

   if (is.na(sflim)) {
      sflim_level <- 20
      sflim_elo   <- 3190
   } else {
      if (sflim >= 0 && sflim <= 20) {
         sflim_level <- sflim
         sflim_elo   <- 3190
      } else {
         sflim_level <- 20
         sflim_elo   <- sflim
      }
   }

   sflim_level.xpos <- c(1.7,4.5)
   sflim_level.ypos <- title.ypos[6] - 0.4 * (title.ypos[6]-title.ypos[7])
   sflim_level.box  <- .drawslider(x=sflim_level.xpos, sflim_level.ypos, xlab=c(0,20), cex=cex*cex.mult)
   .updateslider(NULL, sflim_level.ypos, oldval=sflim_level, xlim=sflim_level.xpos, range=c(0,20), round=TRUE, cex=cex*cex.mult)

   sflim_elo.xpos <- c(5.4,8)
   sflim_elo.ypos <- title.ypos[6] - 0.4 * (title.ypos[6]-title.ypos[7])
   sflim_elo.box  <- .drawslider(x=sflim_elo.xpos, sflim_elo.ypos, xlab=c(1320,3190), cex=cex*cex.mult)
   .updateslider(NULL, sflim_elo.ypos, oldval=sflim_elo, xlim=sflim_elo.xpos, range=c(1320,3190), round=10, cex=cex*cex.mult)

   threads.xpos <- c(1.7,4.5)
   threads.ypos <- title.ypos[7] - 0.4 * (title.ypos[7]-title.ypos[8])
   threads.box  <- .drawslider(x=threads.xpos, threads.ypos, xlab=1:8, cex=cex*cex.mult)
   .updateslider(NULL, threads.ypos, oldval=threads, xlim=threads.xpos, range=c(1,8), round=TRUE, cex=cex*cex.mult, text=FALSE)

   hash.opts <- round(2^(6:11))
   hash.xpos <- c(5.4,8)
   hash.ypos <- title.ypos[7] - 0.4 * (title.ypos[7]-title.ypos[8])
   hash.box  <- .drawslider(x=hash.xpos, hash.ypos, xlab=2^(6:11), cex=cex*cex.mult)
   hash <- which(hash == hash.opts)
   .updateslider(NULL, hash.ypos, oldval=hash, xlim=hash.xpos, range=c(1,6), round=TRUE, cex=cex*cex.mult, text=FALSE)
   hash <- hash.opts[hash]

   monthssfcache.xpos <- c(1.7,6)
   monthssfcache.ypos <- title.ypos[8] - 0.4 * (title.ypos[8]-title.ypos[9])
   monthssfcache.box  <- .drawslider(x=monthssfcache.xpos, monthssfcache.ypos, xlab=c(1,60), cex=cex*cex.mult)
   .updateslider(NULL, monthssfcache.ypos, oldval=monthssfcache, xlim=monthssfcache.xpos, range=c(1,60), round=TRUE, cex=cex*cex.mult)

   delcache.xpos <- 7.4
   delcache.ypos <- title.ypos[8] - 0.4 * (title.ypos[8]-title.ypos[9])
   delcache.txt  <- .text("delcache")
   delcache.on   <- FALSE
   delcache.box  <- list()
   delcache.box[[1]] <- .drawbutton(delcache.xpos, delcache.ypos, text=delcache.txt, len=max(nchar(delcache.txt)), on=delcache.on, cex=cex)

   .mousedownfun <- function(button,x,y) {
      if (length(button) == 0L)
         button <- 3
      button <<- button
      xy1 <<- .calcxy(x, y, plt)
      return(NULL)
   }

   .mouseupfun <- function(button,x,y) {
      xy2 <<- .calcxy(x, y, plt)
      return(1)
   }

   while (TRUE) {

      plt <- par("plt")

      button <- NULL
      xy1 <- NULL
      xy2 <- NULL

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onMouseUp=.mouseupfun, onKeybd=.keyfun)

      if (is.numeric(resp)) {

         if (button == 2)
            break

         if (button != 0)
            next

         hit <- xy1[1] >= 1.5 & xy1[2] >= title.ypos[1]-(title.ypos[1]-title.ypos[2])/2 & xy1[1] <= 8.5 & xy1[2] <= 8.7
         if (hit) {
            oldpath <- sfpath
            if (.Platform$OS.type == "windows") {
               tmp <- choose.files(caption="", multi=FALSE)
               if (length(sfpath) == 0L)
                  next
            } else {
               eval(expr=switch1)
               tmp <- readline(prompt=.text("sfenterpath"))
               eval(expr=switch2)
               if (identical(tmp, ""))
                  next
               tmp <- suppressWarnings(normalizePath(tmp))
               if (file.exists(tmp)) {
                  .texttop(.text("sfpathsuccess"), sleep=2)
                  .texttop("")
               } else {
                  .texttop(.text("sfpathfail"), sleep=2)
                  .texttop("")
                  next
               }
            }
            sfpath <- tmp
            if (oldpath != sfpath) {
               # (re)start stockfish if the path is new
               rect(title.xpos-0.1, title.ypos[1]-(title.ypos[2]-title.ypos[1])/2, 8.7, title.ypos[1]+(title.ypos[2]-title.ypos[1])/2, col=col.bg, border=col.bg)
               sfpath2 <- sfpath
               pathlen <- nchar(sfpath2)
               if (pathlen >= maxlen)
                  sfpath2 <- paste0("...", substr(sfpath2, max(1,nchar(sfpath2)-maxlen), nchar(sfpath2)))
               text(title.xpos, title.ypos[1], .text("sfpath", sfpath2), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
               tmp <- .sf.stop(sfproc, sfrun)
               sfproc <- tmp$sfproc
               sfrun  <- tmp$sfrun
               tmp <- .sf.start(sfproc, sfrun, sfpath, threads, hash)
               sfproc <- tmp$sfproc
               sfrun  <- tmp$sfrun
            }
            next
         }

         hit <- sapply(sfrun.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (i == 1) {
               if (sfrun)
                  next
               # start Stockfish
               tmp <- .sf.start(sfproc, sfrun, sfpath, threads, hash)
               sfproc <- tmp$sfproc
               sfrun  <- tmp$sfrun
            }
            if (i == 2) {
               # stop Stockfish
               tmp <- .sf.stop(sfproc, sfrun)
               sfproc <- tmp$sfproc
               sfrun  <- tmp$sfrun
            }
            sfrun.on <- c(sfrun, !sfrun)
            for (i in seq_along(sfrun.txt)) {
               .drawbutton(sfrun.xpos[i], sfrun.ypos, text=sfrun.txt[i], len=max(nchar(sfrun.txt)), on=sfrun.on[i], cex=cex)
            }
            next
         }

         hit <- sapply(usesfcache.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (usesfcache.on[i])
               next
            usesfcache.on <- !usesfcache.on
            for (i in seq_along(usesfcache.txt)) {
               .drawbutton(usesfcache.xpos[i], usesfcache.ypos, text=usesfcache.txt[i], len=max(nchar(usesfcache.txt)), on=usesfcache.on[i], cex=cex)
            }
            next
         }

         hit <- xy1[1] >= depth1.box[1] & xy1[2] >= depth1.box[2] & xy1[1] <= depth1.box[3] & xy1[2] <= depth1.box[4]
         if (hit) {
            depth1 <- .updateslider(xy2[1], depth1.ypos, oldval=depth1, xlim=depth1.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)
            if (depth1 > depth2) {
               xnew <- (depth1 - 1) * (depth1.xpos[2] - depth1.xpos[1]) / (50 - 1) + depth1.xpos[1]
               depth2 <- .updateslider(xnew, depth2.ypos, oldval=depth2, xlim=depth2.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)
            }
            next
         }

         hit <- xy1[1] >= depth2.box[1] & xy1[2] >= depth2.box[2] & xy1[1] <= depth2.box[3] & xy1[2] <= depth2.box[4]
         if (hit) {
            depth2 <- .updateslider(xy2[1], depth2.ypos, oldval=depth2, xlim=depth2.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)
            if (depth2 < depth1) {
               xnew <- (depth2 - 1) * (depth2.xpos[2] - depth2.xpos[1]) / (50 - 1) + depth2.xpos[1]
               depth1 <- .updateslider(xnew, depth1.ypos, oldval=depth1, xlim=depth1.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)
            }
            next
         }

         hit <- xy1[1] >= depth3.box[1] & xy1[2] >= depth3.box[2] & xy1[1] <= depth3.box[3] & xy1[2] <= depth3.box[4]
         if (hit) {
            depth3 <- .updateslider(xy2[1], depth3.ypos, oldval=depth3, xlim=depth3.xpos, range=c(1,50), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= multipv1.box[1] & xy1[2] >= multipv1.box[2] & xy1[1] <= multipv1.box[3] & xy1[2] <= multipv1.box[4]
         if (hit) {
            multipv1 <- .updateslider(xy2[1], multipv1.ypos, oldval=multipv1, xlim=multipv1.xpos, range=c(1,5), round=TRUE, cex=cex*cex.mult, text=FALSE)
            next
         }

         hit <- xy1[1] >= multipv2.box[1] & xy1[2] >= multipv2.box[2] & xy1[1] <= multipv2.box[3] & xy1[2] <= multipv2.box[4]
         if (hit) {
            multipv2 <- .updateslider(xy2[1], multipv2.ypos, oldval=multipv2, xlim=multipv2.xpos, range=c(1,5), round=TRUE, cex=cex*cex.mult, text=FALSE)
            next
         }

         hit <- xy1[1] >= hintdepth.box[1] & xy1[2] >= hintdepth.box[2] & xy1[1] <= hintdepth.box[3] & xy1[2] <= hintdepth.box[4]
         if (hit) {
            hintdepth <- .updateslider(xy2[1], hintdepth.ypos, oldval=hintdepth, xlim=hintdepth.xpos, range=c(2,20), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= sflim_level.box[1] & xy1[2] >= sflim_level.box[2] & xy1[1] <= sflim_level.box[3] & xy1[2] <= sflim_level.box[4]
         if (hit) {
            sflim_level <- .updateslider(xy2[1], sflim_level.ypos, oldval=sflim_level, xlim=sflim_level.xpos, range=c(0,20), round=TRUE, cex=cex*cex.mult)
            if (sflim_level < 20 && sflim_elo < 3190)
               sflim_elo <- .updateslider(sflim_elo.xpos[2], sflim_elo.ypos, oldval=sflim_elo, xlim=sflim_elo.xpos, range=c(1320,3190), round=10, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= sflim_elo.box[1] & xy1[2] >= sflim_elo.box[2] & xy1[1] <= sflim_elo.box[3] & xy1[2] <= sflim_elo.box[4]
         if (hit) {
            sflim_elo <- .updateslider(xy2[1], sflim_elo.ypos, oldval=sflim_elo, xlim=sflim_elo.xpos, range=c(1320,3190), round=10, cex=cex*cex.mult)
            if (sflim_level < 20 && sflim_elo < 3190)
               sflim_level <- .updateslider(sflim_level.xpos[2], sflim_level.ypos, oldval=sflim_level, xlim=sflim_level.xpos, range=c(0,20), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= threads.box[1] & xy1[2] >= threads.box[2] & xy1[1] <= threads.box[3] & xy1[2] <= threads.box[4]
         if (hit) {
            threads <- .updateslider(xy2[1], threads.ypos, oldval=threads, xlim=threads.xpos, range=c(1,8), round=TRUE, cex=cex*cex.mult, text=FALSE)
            next
         }

         hit <- xy1[1] >= hash.box[1] & xy1[2] >= hash.box[2] & xy1[1] <= hash.box[3] & xy1[2] <= hash.box[4]
         if (hit) {
            hash <- which(hash == hash.opts)
            hash <- .updateslider(xy2[1], hash.ypos, oldval=hash, xlim=hash.xpos, range=c(1,6), round=TRUE, cex=cex*cex.mult, text=FALSE)
            hash <- hash.opts[hash]
            next
         }

         hit <- xy1[1] >= monthssfcache.box[1] & xy1[2] >= monthssfcache.box[2] & xy1[1] <= monthssfcache.box[3] & xy1[2] <= monthssfcache.box[4]
         if (hit) {
            monthssfcache <- .updateslider(xy2[1], monthssfcache.ypos, oldval=monthssfcache, xlim=monthssfcache.xpos, range=c(1,60), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- sapply(delcache.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            .texttop(.text("rlydelcache", "Stockfish"))
            answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
            answer <- .confirm(answer)
            if (answer) {
               .texttop(.text("deletingcache"), sleep=1)
               unlink(file.path(cachedir, "stockfish", "*"))
            }
            .texttop("")
            next
         }

      }

      if (identical(resp, "F7") || identical(resp, "\r") || identical(resp, "ctrl-J") || identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-[") || identical(resp, " "))
         break

   }

   if (sflim_level == 20 && sflim_elo == 3190) {
      sflim <- NA
   } else {
      if (sflim_level < 20) {
         sflim <- sflim_level
      } else {
         sflim <- sflim_elo
      }
   }

   if (sfrun)
      .sf.setoptions(sfproc, threads, hash)

   usesfcache <- usesfcache.on[1]

   out <- list(sfproc=sfproc, sfrun=sfrun, sfpath=sfpath, depth1=depth1, depth2=depth2, depth3=depth3, sflim=sflim, multipv1=multipv1, multipv2=multipv2, threads=threads, hash=hash, hintdepth=hintdepth, monthssfcache=monthssfcache, usesfcache=usesfcache)

   #.erase(1, 1, 9, 9)

   return(out)

}

.lichesssettings <- function(speeds, ratings, lichessdb, uselicache, barlen, invertbar, token, monthslicache, isonline) {

   col.bg     <- .get("col.bg")
   col.help   <- .get("col.help")
   col.border <- .get("col.border")
   font.mono  <- .get("font.mono")
   col.text   <- .get("col.square.d")
   switch1    <- .get("switch1")
   switch2    <- .get("switch2")
   cachedir   <- .get("cachedir")
   mode       <- .get("mode")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.border, lwd=.get("lwd")+3)

   cex <- 1
   cex.mult <- 0.9

   title.xpos <- 1.5
   title.ypos <- 8.4 - 1.25 * c(0:5)
   text(title.xpos, title.ypos[1], .text("speed"),         pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[2], .text("rating"),        pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[3], .text("lichessdb"),     pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(4.2,        title.ypos[3], .text("usecacheshort"), pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(6,          title.ypos[3], .text("delcache"),      pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[4], .text("monthscache"),   pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[5], .text("barlen"),        pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(title.xpos, title.ypos[6], .text("invertbar"),     pos=4, cex=cex, family=font.mono, col=col.help, font=2)
   text(5,          title.ypos[6], .text("entertoken"),    pos=4, cex=cex, family=font.mono, col=col.help, font=2)

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
   ratings.txt  <- c(400, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2500)
   ratings.on   <- ratings.opts %in% strsplit(ratings, ",", fixed=TRUE)[[1]]
   ratings.box  <- list()
   for (i in seq_along(ratings.txt)) {
      ratings.box[[i]] <- .drawbutton(ratings.xpos[i], ratings.ypos, text=ratings.txt[i], len=max(nchar(ratings.txt)), on=ratings.on[i], cex=cex)
   }

   lichessdb.opts <- c("lichess", "masters")
   lichessdb.xpos <- 2.2 + 1.1 * (seq_along(lichessdb.opts) - 1)
   lichessdb.ypos <- title.ypos[3] - 0.4 * (title.ypos[1]-title.ypos[2])
   lichessdb.txt  <- c("players", "masters")
   lichessdb.on   <- lichessdb.opts == lichessdb
   lichessdb.box  <- list()
   for (i in seq_along(lichessdb.txt)) {
      lichessdb.box[[i]] <- .drawbutton(lichessdb.xpos[i], lichessdb.ypos, text=lichessdb.txt[i], len=max(nchar(lichessdb.txt)), on=lichessdb.on[i], cex=cex)
   }

   uselicache.opts <- c(TRUE, TRUE)
   uselicache.xpos <- 4.7 + 0.7 * (seq_along(uselicache.opts) - 1)
   uselicache.ypos <- title.ypos[3] - 0.4 * (title.ypos[1]-title.ypos[2])
   uselicache.txt  <- c(.text("yes"), .text("no"))
   uselicache.on   <- c(uselicache, !uselicache)
   uselicache.box  <- list()
   for (i in seq_along(uselicache.txt)) {
      uselicache.box[[i]] <- .drawbutton(uselicache.xpos[i], uselicache.ypos, text=uselicache.txt[i], len=max(nchar(uselicache.txt)), on=uselicache.on[i], cex=cex)
   }

   delcache.opts <- c("players", "masters")
   delcache.xpos <- 6.7 + 1.1 * (seq_along(delcache.opts) - 1)
   delcache.ypos <- title.ypos[3] - 0.4 * (title.ypos[1]-title.ypos[2])
   delcache.txt  <- delcache.opts
   delcache.on   <- c(FALSE, FALSE)
   delcache.box  <- list()
   for (i in seq_along(delcache.txt)) {
      delcache.box[[i]] <- .drawbutton(delcache.xpos[i], delcache.ypos, text=delcache.txt[i], len=max(nchar(delcache.txt)), on=delcache.on[i], cex=cex)
   }

   monthslicache.xpos <- c(1.7,8)
   monthslicache.ypos <- title.ypos[4] - 0.4 * (title.ypos[1]-title.ypos[2])
   monthslicache.box  <- .drawslider(x=monthslicache.xpos, monthslicache.ypos, xlab=c(1,60), cex=cex*cex.mult)
   .updateslider(NULL, monthslicache.ypos, oldval=monthslicache, xlim=monthslicache.xpos, range=c(1,60), round=TRUE, cex=cex*cex.mult)

   barlen.xpos <- c(1.7,8)
   barlen.ypos <- title.ypos[5] - 0.4 * (title.ypos[1]-title.ypos[2])
   barlen.box  <- .drawslider(x=barlen.xpos, barlen.ypos, xlab=c(10,100), cex=cex*cex.mult)
   .updateslider(NULL, barlen.ypos, oldval=barlen, xlim=barlen.xpos, range=c(10,100), round=TRUE, cex=cex*cex.mult)

   invertbar.opts <- c("No", "Yes")
   invertbar.xpos <- 2 + 0.7 * (seq_along(invertbar.opts) - 1)
   invertbar.ypos <- title.ypos[6] - 0.4 * (title.ypos[1]-title.ypos[2])
   invertbar.txt  <- c(.text("no"), .text("yes"))
   invertbar.on   <- c(!invertbar, invertbar)
   invertbar.box  <- list()
   for (i in seq_along(invertbar.txt)) {
      invertbar.box[[i]] <- .drawbutton(invertbar.xpos[i], invertbar.ypos, text=invertbar.txt[i], len=max(nchar(invertbar.txt)), on=invertbar.on[i], cex=cex)
   }

   text(5, invertbar.ypos, paste0(rep("*", min(30,nchar(token))), collapse=""), pos=4, cex=cex, family=font.mono, col=col.text, font=2)

   .mousedownfun <- function(button,x,y) {
      if (length(button) == 0L)
         button <- 3
      button <<- button
      xy1 <<- .calcxy(x, y, plt)
      return(NULL)
   }

   .mouseupfun <- function(button,x,y) {
      xy2 <<- .calcxy(x, y, plt)
      return(1)
   }

   while (TRUE) {

      plt <- par("plt")

      button <- NULL
      xy1 <- NULL
      xy2 <- NULL

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onMouseUp=.mouseupfun, onKeybd=.keyfun)

      if (is.numeric(resp)) {

         if (button == 2)
            break

         if (button != 0)
            next

         hit <- sapply(speeds.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            speeds.on[i] <- !speeds.on[i]
            if (all(!speeds.on)) {
               speeds.on[i] <- !speeds.on[i]
               next
            }
            .drawbutton(speeds.xpos[i], speeds.ypos, text=speeds.txt[i], len=max(nchar(speeds.txt)), on=speeds.on[i], cex=cex)
            speeds <- paste0(speeds.opts[speeds.on], collapse=",")
            next
         }

         hit <- sapply(ratings.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            ratings.on[i] <- !ratings.on[i]
            if (all(!ratings.on)) {
               ratings.on[i] <- !ratings.on[i]
               next
            }
            .drawbutton(ratings.xpos[i], ratings.ypos, text=ratings.txt[i], len=max(nchar(ratings.txt)), on=ratings.on[i], cex=cex)
            ratings <- paste0(ratings.opts[ratings.on], collapse=",")
            next
         }

         hit <- sapply(lichessdb.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (lichessdb.on[i])
               next
            lichessdb.on <- !lichessdb.on
            for (i in seq_along(lichessdb.txt)) {
               .drawbutton(lichessdb.xpos[i], lichessdb.ypos, text=lichessdb.txt[i], len=max(nchar(lichessdb.txt)), on=lichessdb.on[i], cex=cex)
            }
            lichessdb <- lichessdb.opts[lichessdb.on]
            next
         }

         hit <- sapply(uselicache.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (uselicache.on[i])
               next
            uselicache.on <- !uselicache.on
            for (i in seq_along(uselicache.txt)) {
               .drawbutton(uselicache.xpos[i], uselicache.ypos, text=uselicache.txt[i], len=max(nchar(uselicache.txt)), on=uselicache.on[i], cex=cex)
            }
            next
         }

         hit <- sapply(delcache.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
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

         hit <- xy1[1] >= barlen.box[1] & xy1[2] >= barlen.box[2] & xy1[1] <= barlen.box[3] & xy1[2] <= barlen.box[4]
         if (hit) {
            barlen <- .updateslider(xy2[1], barlen.ypos, oldval=barlen, xlim=barlen.xpos, range=c(10,100), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- xy1[1] >= monthslicache.box[1] & xy1[2] >= monthslicache.box[2] & xy1[1] <= monthslicache.box[3] & xy1[2] <= monthslicache.box[4]
         if (hit) {
            monthslicache <- .updateslider(xy2[1], monthslicache.ypos, oldval=monthslicache, xlim=monthslicache.xpos, range=c(1,60), round=TRUE, cex=cex*cex.mult)
            next
         }

         hit <- sapply(invertbar.box, function(coords) xy1[1] >= coords[1] & xy1[2] >= coords[2] & xy1[1] <= coords[3] & xy1[2] <= coords[4])
         if (any(hit)) {
            i <- which(hit)
            if (invertbar.on[i])
               next
            invertbar.on <- !invertbar.on
            for (i in seq_along(invertbar.txt)) {
               .drawbutton(invertbar.xpos[i], invertbar.ypos, text=invertbar.txt[i], len=max(nchar(invertbar.txt)), on=invertbar.on[i], cex=cex)
            }
            invertbar <- invertbar.on[2]
            next
         }

         hit <- xy1[1] >= 5 & xy1[2] >= invertbar.box[[1]][2] & xy1[1] <= 7 & xy1[2] <= (title.ypos[5]+0.2)
         if (hit) {
            eval(expr=switch1)
            tmp <- readline(prompt=.text("token"))
            eval(expr=switch2)
            if (identical(tmp, ""))
               next
            if (!isonline) {
               .texttop(.text("tokenonline"), sleep=2)
               .texttop("")
               next
            }
            uselicache <- .get("uselicache")
            assign("uselicache", FALSE, envir=.chesstrainer)
            assign("mode", "", envir=.chesstrainer)
            res.li <- .liquery(pos=.get("pos"), flip=FALSE, sidetoplay="w", sidetoplaystart="w", i=1, isonline=isonline, lichessdb="lichess", token=tmp, speeds="blitz,rapid,classical", ratings="1200,1400,1600,1800", barlen=50, invertbar=FALSE, texttop="", tokencheck=TRUE)
            assign("uselicache", uselicache, envir=.chesstrainer)
            assign("mode", mode, envir=.chesstrainer)
            if (is.null(res.li$out)) {
               .texttop(.text("tokensetfail"), sleep=2)
               .texttop("")
               next
            } else {
               token <- tmp
               .texttop(.text("tokensetsuccess"), sleep=2)
               .texttop("")
            }
            rect(4.8, invertbar.ypos-0.3, 8.5, invertbar.ypos+0.3, col=col.bg, border=NA)
            text(5, invertbar.ypos, paste0(rep("*", nchar(token)), collapse=""), pos=4, cex=cex, family=font.mono, col=col.text, font=2)
            next
         }

      }

      if (identical(resp, "i")) {
         uselicache <- .get("uselicache")
         contliquery <- .get("contliquery")
         assign("uselicache", FALSE, envir=.chesstrainer)
         assign("mode", "", envir=.chesstrainer)
         assign("contliquery", TRUE, envir=.chesstrainer)
         .liquery(.get("pos"), flip=FALSE, sidetoplay="w", sidetoplaystart="w", i=1, isonline=isonline, lichessdb, token, speeds, ratings, barlen, invertbar, texttop="", showout=TRUE, showlibar=FALSE)
         assign("uselicache", uselicache, envir=.chesstrainer)
         assign("mode", mode, envir=.chesstrainer)
         assign("contliquery", contliquery, envir=.chesstrainer)
         next
      }

      if (identical(resp, "F8") || identical(resp, "\r") || identical(resp, "ctrl-J") || identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-[") || identical(resp, " "))
         break

   }

   uselicache <- uselicache.on[1]

   out <- list(speeds=speeds, ratings=ratings, lichessdb=lichessdb, uselicache=uselicache, barlen=barlen, monthslicache=monthslicache, invertbar=invertbar, token=token)

   #.erase(1, 1, 9, 9)

   return(out)

}

.showsettings <- function(tab) {

   #.clearsideindicator()
   #.drawtimer(clear=TRUE)

   lwd <- tab$lwd

   lang <- .get("lang")
   tab$lang <- switch(lang, de = "Deutsch", en = "English")

   col.bg     <- .get("col.bg")
   col.help   <- .get("col.help")
   col.border <- .get("col.border")
   font.mono  <- .get("font.mono")

   seqdir <- tab$seqdir
   sfpath <- tab$sfpath

   tab.save <- tab

   if (!is.na(tab$sflim)) {
      if (tab$sflim <= 20) {
         tab$sflim <- paste(tab$sflim, "(level)")
      } else {
         tab$sflim <- paste(tab$sflim, "(Elo)")
      }
   } else {
      tab$sflim <- .text("none")
   }

   #tab$showeval <- paste0(names(tab$showeval[tab$showeval]), collapse=", ")
   tab$showeval <- paste0(tab$showeval, collapse="/")
   if (lang == "en") {
      tab$showeval <- gsub("TRUE", "Yes", tab$showeval, fixed=TRUE)
      tab$showeval <- gsub("FALSE", "No", tab$showeval, fixed=TRUE)
   }
   if (lang == "de") {
      tab$showeval <- gsub("TRUE", "Ja",    tab$showeval, fixed=TRUE)
      tab$showeval <- gsub("FALSE", "Nein", tab$showeval, fixed=TRUE)
   }

   if (tab$piecesymbols == 1)
      tab$piecesymbols <- "\U0000265A\U0000265B\U0000265C\U0000265D\U0000265E"
   if (tab$piecesymbols == 2)
      tab$piecesymbols <- "KQRBN"
   if (tab$piecesymbols == 3) {
      if (lang == "de")
         tab$piecesymbols <- "KDTLS"
   }

   tab$mar  <- paste0(tab$mar,  collapse="/")
   tab$mar2 <- paste0(tab$mar2, collapse="/")

   tab <- as.data.frame(tab)

   tab <- t(tab)
   tab <- cbind(tab, .text("explsettings"))
   colnames(tab) <- c("", "")

   tab[,1] <- gsub("TRUE",  .text("on"),  tab[,1], fixed=TRUE)
   tab[,1] <- gsub("FALSE", .text("off"), tab[,1], fixed=TRUE)

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
   sfpos <- grep("depth1", txt, fixed=TRUE)
   txt <- c(.text("generalsettings"), txt[1:(sfpos-1)], "", .text("seqdirsettings"), seqdir, "", .text("sfsettings"), txt[sfpos:length(txt)], "", .text("sfpathsettings"), sfpath)

   cex <- .findcex(txt, font=font.mono, x1=1.5, x2=8.2, y1=2.0, y2=8.0)

   ypos <- seq(8.5, 1.5, length.out=length(txt))

   langswitch <- FALSE

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.border, lwd=lwd+3)

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
