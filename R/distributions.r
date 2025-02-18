.distributions <- function(scores, played, dayslp, lwd) {

   if (length(scores) == 1L)
      return(invisible())

   par(mfrow=c(2,2), pty="m")

   hist(scores, breaks="FD", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col.main=.get("col.fg"),
        xlab=.text("score"), col=.get("col.square.d"), border=.get("col.square.l"), main=paste0("Histogram: ", .text("score")), xlim=c(0,100))
   #box(which="figure", col=.get("col.help.border"), lwd=lwd)
   hist(played, breaks="FD", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col.main=.get("col.fg"),
        xlab=.text("played"), col=.get("col.square.d"), border=.get("col.square.l"), main=paste0("Histogram: ", .text("played")))
   #box(which="figure", col=.get("col.help.border"), lwd=lwd)
   if (length(c(na.omit(dayslp))) >= 2L) {
      hist(dayslp, breaks="FD", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col.main=.get("col.fg"),
           xlab=.text("days"), col=.get("col.square.d"), border=.get("col.square.l"), main=paste0("Histogram: ", .text("days")))
   } else {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
   }
   #box(which="figure", col=.get("col.help.border"), lwd=lwd)
   plot(jitter(played, amount=0.5), jitter(scores, amount=0.5), las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col=.get("col.square.l"), col.main=.get("col.fg"),
        pch=21, bg=.get("col.square.d"), bty="l", main=paste0(.text("played"), " vs. ", .text("score")), xlab=.text("played"), ylab=.text("score"),
        ylim=c(0,100))
   #box(which="figure", col=.get("col.help.border"), lwd=lwd)

   par(mfrow=c(1,1), pty="s")

   box(which="figure", col=.get("col.help.border"), lwd=lwd+3)

   getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button, x, y) return(""), onKeybd=function(key) return(""))

   invisible()

}
