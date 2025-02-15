.distributions <- function(scores, played, dayslp, lwd) {

   if (length(scores) == 1L)
      return(invisible())

   par(mfrow=c(2,2))

   hist(scores, breaks="FD", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col.main=.get("col.fg"),
        xlab=.text("score"), col=.get("col.square.d"), border=.get("col.square.l"), main=paste0("Histogram: ", .text("score")))
   hist(played, breaks="FD", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col.main=.get("col.fg"),
        xlab=.text("played"), col=.get("col.square.d"), border=.get("col.square.l"), main=paste0("Histogram: ", .text("played")))
   if (length(c(na.omit(dayslp))) >= 2L) {
      hist(dayslp, breaks="FD", las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col.main=.get("col.fg"),
           xlab=.text("days"), col=.get("col.square.d"), border=.get("col.square.l"), main=paste0("Histogram: ", .text("days")))
   } else {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
   }
   plot(played, scores, las=1, col.axis=.get("col.top"), col.lab=.get("col.top"), col=.get("col.square.l"), col.main=.get("col.fg"),
        pch=21, bg=.get("col.square.d"), bty="l", main=paste0(.text("played"), " vs. ", .text("score")), xlab=.text("played"), ylab=.text("score"))

   par(mfrow=c(1,1))

   getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button, x, y) return(""), onKeybd=function(key) return(""))

   invisible()

}
