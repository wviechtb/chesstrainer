.distributions <- function(scores, played, dayslp, lwd, multiplier) {

   n <- length(scores)

   if (n == 1L)
      return()

   par(xpd=FALSE)

   col.top         <- .get("col.top")
   col.fg          <- .get("col.fg")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")

   par(mfrow=c(2,2), pty="m")

   #########################################################################

   # histogram of 'scores'

   hist(scores, breaks="FD", las=1, col.axis=col.top, col.lab=col.top, col.main=col.fg,
        xlab=.text("score"), col=col.square.d, border=col.square.l, main=paste0("Histogram: ", .text("score")), xlim=c(0,100))
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   # histogram of 'played'

   hist(played, breaks="FD", las=1, col.axis=col.top, col.lab=col.top, col.main=col.fg,
        xlab=.text("played"), col=col.square.d, border=col.square.l, main=paste0("Histogram: ", .text("played")))
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   # histogram of 'days'
   if (length(c(na.omit(dayslp))) >= 2L) {
      hist(dayslp, breaks="FD", las=1, col.axis=col.top, col.lab=col.top, col.main=col.fg,
           xlab=.text("days"), col=col.square.d, border=col.square.l, main=paste0("Histogram: ", .text("days")))
   } else {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
   }
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   # scatterplot of 'played' versus 'scores'

   plot(NA, las=1, col.axis=col.top, col.lab=col.top, col=col.square.l, col.main=col.fg,
        bty="l", main=paste0(.text("played"), " vs. ", .text("score")), xlab=.text("played"), ylab=.text("score"),
        xlim=c(0,max(played, na.rm=TRUE)), ylim=c(0,100))
   xs <- seq(0, max(played, na.rm=TRUE), length.out=1000)
   ys <- 100 * multiplier^xs
   pt.cex <- max(0.1, 1 - 1/10 * log10(n)) # adjust point size based on n
   if (n >= 5) {
      tmp <- data.frame(scores=scores, played=played, offset=log(100))
      res <- lm(log(scores) ~ 0 + played + offset(offset), data=tmp)
      pred <- predict(res, newdata=data.frame(played=xs, offset=log(100)), interval="prediction")
      pred <- as.data.frame(pred)
      #polygon(c(xs,rev(xs)), exp(c(pred$lwr, rev(pred$upr))), border=NA, col=adjustcolor(col.bg, red.f=1.25, green.f=1.25, blue.f=1.25))
      lines(xs, exp(pred$fit), col=col.top, lwd=2)
   }
   lines(xs, ys, col=col.top, lty="dotted")
   points(jitter(played, amount=0.5), jitter(scores, amount=0.5), pch=21, col=col.square.l, bg=col.square.d, cex=pt.cex)
   #legend("topright", lty=c("dotted","solid"), lwd=c(1,1), col=col.top, legend=.text("plotlegend"), bg=col.bg, box.col=col.square.l)
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   par(mfrow=c(1,1), pty="s")

   box(which="figure", col=col.help.border, lwd=lwd+3)

   par(xpd=NA)

   .waitforclick()

   return()

}
