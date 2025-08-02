.sessiongraph <- function(seqsplayed, mean.scores, playtime, lwd) {

   seqsplayed.total <- sum(seqsplayed)

   col.top         <- .get("col.top")
   col.fg          <- .get("col.fg")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=col.bg, border=col.help.border, lwd=lwd+3)

   par(new=TRUE, mar=rep(11,4))

   plot(NA, xlim=c(1,seqsplayed.total), ylim=c(min(round(unlist(mean.scores)))-1, max(round(unlist(mean.scores)))+1),
        xlab="", ylab=.text("meanscore"), bty="l", las=1, col.axis=col.top, col.lab=col.top, xaxt="n")
   axis(side=1, at=seq_len(seqsplayed.total), col.axis=col.top)

   start.pos <- 0

   for (j in 1:length(seqsplayed)) {
      segments(start.pos + 1, mean.scores[[j]][1], start.pos + seqsplayed[j], mean.scores[[j]][1], lty="dotted", col=col.top)
      points(start.pos + seq_len(seqsplayed[j]), mean.scores[[j]], type="o", pch=21, lwd=2, col=col.square.l, bg=col.square.d)
      start.pos <- start.pos + max(seqsplayed[j])
   }

   par(new=FALSE, mar=rep(5.2,4), usr=c(1,9,1,9))

   .texttop(.text("playtime", .totaltime(playtime)))

   .waitforclick()

   #.erase(1, 1, 9, 9)

   return()

}
