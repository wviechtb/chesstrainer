.sessiongraph <- function(seqsplayed, mean.scores, playtime, lwd, mar) {

   isnull <- seqsplayed == 0
   seqsplayed <- seqsplayed[!isnull]
   mean.scores <- mean.scores[!isnull]

   seqsplayed.total <- sum(seqsplayed)

   col.top         <- .get("col.top")
   col.fg          <- .get("col.fg")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   par(new=TRUE, mar=c(11,11,9,9))

   ylim <- range(round(unlist(mean.scores))) + c(-1,1)

   plot(NA, xlim=c(1,seqsplayed.total), ylim=ylim, xlab="", ylab=.text("meanscore"),
        bty="l", las=1, col.axis=col.top, col.lab=col.top, xaxt="n")
   axis(side=1, at=seq_len(seqsplayed.total), col.axis=col.top)

   start.pos <- 0

   for (j in 1:length(seqsplayed)) {
      segments(start.pos + 1, mean.scores[[j]][1], start.pos + seqsplayed[j], mean.scores[[j]][1], lty="dotted", col=col.top)
      points(start.pos + seq_len(seqsplayed[j]), mean.scores[[j]], type="o", pch=21, lwd=2, col=col.square.l, bg=col.square.d)
      start.pos <- start.pos + max(seqsplayed[j])
   }

   par(new=FALSE, mar=rep(mar,4), usr=c(1,9,1,9))

   .texttop(.text("playtime", .totaltime(playtime)))

   .waitforclick()

   #.erase(1, 1, 9, 9)

   return()

}
