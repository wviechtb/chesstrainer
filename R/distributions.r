.distributions <- function(scores, played, age, difficulty, lwd, multiplier) {

   n <- length(scores)

   par(xpd=FALSE)

   col.top         <- .get("col.top")
   col.fg          <- .get("col.fg")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")

   #par(mfrow=c(3,2), pty="m")

   layout(matrix(c(1,2,3,4,5,5), nrow=3, byrow=TRUE))
   par(pty="m")

   #########################################################################

   # histogram of 'scores'

   hist(scores, breaks=20, las=1, col.axis=col.top, col.lab=col.top, col.main=col.fg,
        xlab=.text("score"), col=col.square.d, border=col.square.l, main=paste0("Histogram: ", .text("score")), xlim=c(0,100))
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   # histogram of 'played'

   hist(played, breaks=20, las=1, col.axis=col.top, col.lab=col.top, col.main=col.fg,
        xlab=.text("played"), col=col.square.d, border=col.square.l, main=paste0("Histogram: ", .text("played")))
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   # histogram of 'age'

   if (length(c(na.omit(age))) >= 2L) {
      hist(age, breaks=20, las=1, col.axis=col.top, col.lab=col.top, col.main=col.fg,
           xlab=.text("day", TRUE), col=col.square.d, border=col.square.l, main=paste0("Histogram: ", .text("age")), xlim=c(0,max(age, na.rm=TRUE)))
   } else {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
   }
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   # histogram of 'difficulty'

   if (length(c(na.omit(difficulty))) >= 2L) {
      hist(difficulty, breaks=20, las=1, col.axis=col.top, col.lab=col.top, col.main=col.fg,
           xlab=.text("difficulty"), col=col.square.d, border=col.square.l, main=paste0("Histogram: ", .text("difficulty")), xlim=c(0,max(difficulty, na.rm=TRUE)))
   } else {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
   }
   #box(which="figure", col=col.help.border, lwd=lwd)

   #########################################################################

   # scatterplot of 'played' versus 'scores'

   par(mar=c(5.4,15.4,5.4,15.4))

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

   par(mar=rep(5.2,4))

   #########################################################################

   # scatterplot of 'played' versus 'difficulty'

   #plot(NA, las=1, col.axis=col.top, col.lab=col.top, col=col.square.l, col.main=col.fg,
   #     bty="l", main=paste0(.text("played"), " vs. ", .text("difficulty")), xlab=.text("played"), ylab=.text("difficulty"),
   #     xlim=c(0,max(played, na.rm=TRUE)), ylim=c(0,max(difficulty, na.rm=TRUE)))
   #pt.cex <- max(0.1, 1 - 1/10 * log10(n)) # adjust point size based on n
   #points(jitter(played, amount=0.5), jitter(difficulty, amount=0.5), pch=21, col=col.square.l, bg=col.square.d, cex=pt.cex)

   #########################################################################

   #par(mfrow=c(1,1), pty="s")
   layout(1)
   par(pty="s")

   box(which="figure", col=col.help.border, lwd=lwd+3)

   par(xpd=NA)

   .waitforclick()

   return()

}
