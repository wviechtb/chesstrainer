.progressgraph <- function(dat) {

   if (dat$round[1] == 1)
      dat <- rbind(data.frame(date=NA, round=0, score=100), dat)

   x <- dat

   col.top         <- .get("col.top")
   col.fg          <- .get("col.fg")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")
   mar             <- .get("mar")
   mar2            <- .get("mar2")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=.get("lwd")+3)

   usr <- NULL

   plot.scores <- function(x) {
      rect(1.3, 1.3, 8.7, 8.7, col=col.bg, border=NA)
      par(new=TRUE, mar=mar2)
      if (nrow(x) == 1L) {
         xlim <- c(x$round-1, x$round+1)
      } else {
         xlim <- range(x$round)
      }
      plot(NA, xlim=xlim, ylim=c(0,100), xlab=.text("round"), ylab=.text("score"),
           bty="l", las=1, col.axis=col.top, col.lab=col.top, xaxt="n")
      axis(side=1, at=x$round, col.axis=col.top)
      points(x$round, x$score, type="o", pch=21, lwd=2, col=col.square.l, bg=col.square.d)
      usr <<- par()$usr
      par(mar=mar, usr=c(1,9,1,9))
   }

   zoom <- 1

   while (TRUE) {

      plot.scores(x)

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (identical(click, "g") || identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[") || identical(click, " "))
         break

      if (identical(click, "{") || identical(click, "}")) {
         if (identical(click, "{")) {
            mar2 <- pmax(1, mar2 - 0.5)
         } else {
            mar2 <- mar2 + 0.5
         }
         .texttop(.text("maradj", mar2), sleep=0.5)
         assign("mar2", mar2, envir=.chesstrainer)
         next
      }

      if (is.numeric(click) && click[[3]] %in% c(0,2)) {
         if (click[[3]] == 2) { # right mouse button resets zoom or exits if zoomed out
            if (zoom == 1) {
               break
            } else {
               x <- dat
               zoom <- 1
            }
         }
         if (click[[3]] == 0) { # left mouse button to set first and second zoom point
            # but if click is outside of the graph (or more precisely, the board), then exit
            x1 <- grconvertX(click[[1]], from="ndc", to="user")
            y1 <- grconvertY(click[[2]], from="ndc", to="user")
            if (x1 < 1 || x1 > 9 || y1 < 1 || y1 > 9)
               break
            par(mar=mar2, usr=usr)
            x1 <- grconvertX(click[[1]], from="ndc", to="user")
            if (x1 < usr[1])
               x1 <- usr[1]
            if (x1 > usr[2])
               x1 <- usr[2]
            segments(x1, 0, x1, usr[4], lty="dotted", col=col.top)
            par(mar=mar, usr=c(1,9,1,9))
            click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)
            if (!is.numeric(click))
               next
            if (click[[3]] == 2)
               next
            par(mar=mar2, usr=usr)
            x2 <- grconvertX(click[[1]], from="ndc", to="user")
            if (x2 < usr[1])
               x2 <- usr[1]
            if (x2 > usr[2])
               x2 <- usr[2]
            segments(x2, 0, x2, usr[4], lty="dotted", col=col.top)
            segments(x1, 0, x2, 0, lty="dotted", col=col.top)
            segments(x1, usr[4], x2, usr[4], lty="dotted", col=col.top)
            Sys.sleep(0.5)
            par(mar=mar, usr=c(1,9,1,9))
            sel <- dat$round >= min(x1,x2) & dat$round <= max(x1,x2)
            if (sum(sel) == 0L)
               next
            zoom <- zoom + 1
            x <- dat[sel,]
         }
      }

   }

   #xpos <- axTicks(side=1)
   #segments(xpos, par("usr")[3], xpos, par("usr")[4], lty="dotted", col=.get("col.fg"))
   #ypos <- axTicks(side=2)
   #segments(par("usr")[1], ypos, par("usr")[2], ypos, lty="dotted", col=.get("col.fg"))

   par(new=FALSE, mar=mar)

   #.erase(1, 1, 9, 9)

   return(list(mar2=mar2))

}
