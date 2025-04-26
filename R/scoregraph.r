.scoregraph <- function(x, lwd) {

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   par(new=TRUE, mar=rep(11,4))

   plot(NA, xlim=range(x$played), ylim=c(0,100), xlab="", ylab="Score",
        bty="l", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), xaxt="n")
   axis(side=1, at=x$played, col.axis=.get("col.top"))

   #xpos <- axTicks(side=1)
   #segments(xpos, par("usr")[3], xpos, par("usr")[4], lty="dotted", col=.get("col.fg"))
   #ypos <- axTicks(side=2)
   #segments(par("usr")[1], ypos, par("usr")[2], ypos, lty="dotted", col=.get("col.fg"))

   points(x$played, x$score, type="o", pch=21, lwd=2, col=.get("col.square.l"), bg=.get("col.square.d"))

   par(new=FALSE, mar=rep(5.2,4))

   .waitforclick()

   return()

}
