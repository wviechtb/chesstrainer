.evalgraph <- function(x, i, lwd, mar) {

   col.top         <- .get("col.top")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")

   shade_segment <- function(x0, y0, x1, y1, evenval) {
      if (y0 >= evenval & y1 >= evenval) {
         polygon(c(x0, x1, x1, x0), c(evenval, evenval, y1, y0), col="gray55", border=NA)
      } else if (y0 <= evenval & y1 <= evenval) {
         polygon(c(x0, x1, x1, x0), c(evenval, evenval, y1, y0), col="black", border=NA)
      } else {
         x_cross <- x0 + (evenval - y0) * (x1 - x0) / (y1 - y0)
         if (y0 < evenval) {
            polygon(c(x0, x_cross, x0), c(y0, evenval, evenval), col="black", border=NA)
            polygon(c(x_cross, x1, x1, x_cross), c(evenval, evenval, y1, evenval), col="gray55", border=NA)
         } else {
            polygon(c(x0, x_cross, x0), c(y0, evenval, evenval), col="gray55", border=NA)
            polygon(c(x_cross, x1, x1, x_cross), c(evenval, evenval, y1, evenval), col="black", border=NA)
         }
      }
   }

   x$eval <- ifelse(x$eval >  9,  9, x$eval)
   x$eval <- ifelse(x$eval < -9, -9, x$eval)

   n <- nrow(x)

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   plot.eval <- function(x) {
      rect(1.3, 1.3, 8.7, 8.7, col=col.bg, border=NA)
      par(new=TRUE, mar=c(11,11,9,9))
      ys <- x$eval
      if (yvalue == "cp") {
         ylim <- c(-9, 9)
         yat <- seq(-8, 8, by=2)
         evanval <- 0
         ylab <- .text("evalgraph-y-cp")
      }
      if (yvalue == "wp") {
         ylim <- c(0, 100)
         yat <- seq(0, 90, by=10)
         evanval <- 50
         ylab <- .text("evalgraph-y-wp")
         ys <- 50 + 50 * (2 / (1 + exp(-0.00368208 * ys*100)) - 1)
      }
      plot(NA, xlim=c(1,n), ylim=ylim, xlab=.text("evalgraph-x"), ylab=ylab,
           bty="l", las=1, col.axis=col.top, col.lab=col.top, yaxt="n", xaxt="n")
      axis(side=1, at=1:n, col.axis=col.top)
      axis(side=2, at=yat, col.axis=col.top, las=1)
      #lines(1:n, ys, type="o", pch=21, lwd=2, col=col.square.l, bg=col.square.d)
      xs <- 1:n
      if (n > 2) {
         for (j in 1:(n-1)) {
            shade_segment(xs[j], ys[j], xs[j+1], ys[j+1], evenval=evanval)
         }
      } else {
         points(xs, ys, type="o", pch=21, lwd=2, col=col.square.l, bg=col.square.d)
      }
      segments(1, evanval, n, evanval, lty="dotted", col=col.top)
      lines(xs, ys, col="#d85000", lwd=3)
      if (i > 1 && i-1 != n)
         segments(i-1, -9, i-1, 9, lty="dotted", col=col.top)
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
   }

   yvalue <- "cp"

   while (TRUE) {

      plot.eval(x)

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(click) && click[[3]] == 2) # right mouse button exits
         break

      if (is.numeric(click) && click[[3]] == 1) {
         if (yvalue == "cp") {
            yvalue <- "wp"
            next
         }
         if (yvalue == "wp") {
            yvalue <- "cp"
            next
         }
      }

      if (identical(click, "g") || identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[") || identical(click, " "))
         break

   }

   par(new=FALSE, mar=rep(mar,4))

   #.erase(1, 1, 9, 9)

   return()

}
