.evalgraph <- function(x, lwd) {

   shade_segment <- function(x0, y0, x1, y1) {
      if (y0 >= 0 & y1 >= 0) {
         polygon(c(x0, x1, x1, x0), c(0, 0, y1, y0), col="gray55", border=NA)
      } else if (y0 <= 0 & y1 <= 0) {
         polygon(c(x0, x1, x1, x0), c(0, 0, y1, y0), col="black", border=NA)
      } else {
         x_cross <- x0 + (0 - y0) * (x1 - x0) / (y1 - y0)
         if (y0 < 0) {
            polygon(c(x0, x_cross, x0), c(y0, 0, 0), col="black", border=NA)
            polygon(c(x_cross, x1, x1, x_cross), c(0, 0, y1, 0), col="gray55", border=NA)
         } else {
            polygon(c(x0, x_cross, x0), c(y0, 0, 0), col="gray55", border=NA)
            polygon(c(x_cross, x1, x1, x_cross), c(0, 0, y1, 0), col="black", border=NA)
         }
      }
   }

   x$eval <- ifelse(x$eval >  9,  9, x$eval)
   x$eval <- ifelse(x$eval < -9, -9, x$eval)

   n <- nrow(x)

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   par(new=TRUE, mar=rep(11,4))

   plot(NA, xlim=c(1,n), ylim=c(-9,9), xlab=.text("evalgraph-x"), ylab=.text("evalgraph-y"),
        bty="l", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), yaxt="n", xaxt="n")
   axis(side=1, at=1:n, col.axis=.get("col.top"))
   axis(side=2, at=seq(-8,8,by=2), col.axis=.get("col.top"), las=1)

   #lines(1:n, x$eval, type="o", pch=21, lwd=2, col=.get("col.square.l"), bg=.get("col.square.d"))

   xs <- 1:n
   ys <- x$eval

   for (i in 1:(n-1)) {
      shade_segment(xs[i], ys[i], xs[i+1], ys[i+1])
   }

   segments(1, 0, n, 0, lty="dotted", col=.get("col.top"))

   lines(xs, ys, col="#d85000", lwd=3)

   par(new=FALSE, mar=rep(5.2,4))

   .waitforclick()

   #.erase(1, 1, 9, 9)

   return()

}
