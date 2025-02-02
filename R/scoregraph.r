.scoregraph <- function(x, lwd) {

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   par(new=TRUE, mar=rep(11,4))

   plot(x$played, x$score, type="o", pch=19, lwd=2,
        xlab="", ylab="Score", ylim=c(0,100), bty="l", las=1,
        col=.get("col.fg"), col.axis=.get("col.fg"), col.lab=.get("col.fg"), xaxt="n")
   axis(side=1, at=x$played, col.axis=.get("col.fg"))

   par(new=FALSE, mar=rep(5.2,4))

   getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button, x, y) return(""), onKeybd=function(key) return(""))

   invisible()

}
