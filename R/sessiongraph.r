.sessiongraph <- function(seqsplayed, mean.scores, playtime, lwd, mar, mar2) {

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

   plot.session <- function() {
      rect(1.3, 1.3, 8.7, 8.7, col=col.bg, border=NA)
      par(new=TRUE, mar=mar2)
      ylim <- range(round(unlist(mean.scores))) + c(-1,1)
      plot(NA, xlim=c(1,seqsplayed.total), ylim=ylim, xlab=.text("round"), ylab=.text("meanscore"),
           bty="l", las=1, col.axis=col.top, col.lab=col.top, xaxt="n")
      axis(side=1, at=seq_len(seqsplayed.total), col.axis=col.top)
      start.pos <- 0
      for (j in 1:length(seqsplayed)) {
         segments(start.pos + 1, mean.scores[[j]][1], start.pos + seqsplayed[j], mean.scores[[j]][1], lty="dotted", col=col.top)
         points(start.pos + seq_len(seqsplayed[j]), mean.scores[[j]], type="o", pch=21, lwd=2, col=col.square.l, bg=col.square.d)
         start.pos <- start.pos + max(seqsplayed[j])
      }
      par(mar=mar, usr=c(1,9,1,9))
      .texttop(.text("playtime", .totaltime(playtime)))
   }

   while (TRUE) {

      plot.session()

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (identical(click, "F11") || identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[") || identical(click, " "))
         break

      if (is.numeric(click))
         break

      if (identical(click, "{") || identical(click, "}")) {
         if (identical(click, "{")) {
            mar2 <- pmax(1, mar2 - 0.5)
         } else {
            mar2 <- mar2 + 0.5
         }
         .texttop(.text("maradj", mar2), sleep=0.5)
         next
      }

   }

   par(new=FALSE, mar=mar)

   #.erase(1, 1, 9, 9)

   return(list(mar2=mar2))

}
