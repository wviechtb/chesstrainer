.progressgraph <- function(dat, lwd) {

   if (dat$played[1] == 1)
      dat <- rbind(data.frame(date=NA, played=0, score=100), dat)

   x <- dat

   col.top         <- .get("col.top")
   col.fg          <- .get("col.fg")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   usr <- NULL

   plot.scores <- function(x) {
      rect(1.3, 1.3, 8.7, 8.7, col=col.bg, border=NA)
      par(new=TRUE, mar=c(11,11,9,9))
      if (nrow(x) == 1L) {
         xlim <- c(x$played-1, x$played+1)
      } else {
         xlim <- range(x$played)
      }
      plot(NA, xlim=xlim, ylim=c(0,100), xlab="", ylab=.text("score"),
           bty="l", las=1, col.axis=col.top, col.lab=col.top, xaxt="n")
      axis(side=1, at=x$played, col.axis=col.top)
      points(x$played, x$score, type="o", pch=21, lwd=2, col=col.square.l, bg=col.square.d)
      usr <<- par()$usr
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
   }

   zoom <- 1

   while (TRUE) {

      plot.scores(x)

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (identical(click, "g") || identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[") || identical(click, "F12"))
         break

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
            par(mar=c(11,11,9,9), usr=usr)
            x1 <- grconvertX(click[[1]], from="ndc", to="user")
            if (x1 < usr[1])
               x1 <- usr[1]
            if (x1 > usr[2])
               x1 <- usr[2]
            segments(x1, 0, x1, usr[4], lty="dotted", col=col.top)
            par(mar=rep(5.2,4), usr=c(1,9,1,9))
            click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)
            if (!is.numeric(click))
               next
            if (click[[3]] == 2)
               next
            par(mar=c(11,11,9,9), usr=usr)
            x2 <- grconvertX(click[[1]], from="ndc", to="user")
            if (x2 < usr[1])
               x2 <- usr[1]
            if (x2 > usr[2])
               x2 <- usr[2]
            segments(x2, 0, x2, usr[4], lty="dotted", col=col.top)
            segments(x1, 0, x2, 0, lty="dotted", col=col.top)
            segments(x1, usr[4], x2, usr[4], lty="dotted", col=col.top)
            Sys.sleep(0.5)
            par(mar=rep(5.2,4), usr=c(1,9,1,9))
            sel <- dat$played >= min(x1,x2) & dat$played <= max(x1,x2)
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

   par(new=FALSE)

   #.waitforclick()

   #.erase(1, 1, 9, 9)

   return()

}
