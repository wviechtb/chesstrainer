.historygraph <- function(player, date.start, playtime, seqsplayed, lwd) {

   # load the session history for the player
   player.file <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions", paste0(player, ".rds"))
   dat <- readRDS(player.file)

   # add current session info to dat
   dat <- rbind(dat, data.frame(date.start=date.start, date.end=NA, playtime=playtime, seqsplayed=seqsplayed))

   # this cannot happen after adding the current session info
   if (nrow(dat) == 0L) {
      .texttop(.text("nosessionhistory"), sleep=1.5)
      return()
   }

   col.top         <- .get("col.top")
   col.fg          <- .get("col.fg")
   col.bg          <- .get("col.bg")
   col.square.l    <- .get("col.square.l")
   col.square.d    <- .get("col.square.d")
   col.help.border <- .get("col.help.border")

   # collapse sessions that were played on the same day
   days  <- as.Date(dat$date.start, format="%Y-%m-%d")
   agg   <- aggregate(dat[c("playtime", "seqsplayed")], by=list(day=days), FUN=sum)

   # compute totals
   total.playtime  <- sum(agg$playtime)
   total.seqsplayed <- sum(agg$seqsplayed)

   # round playtime
   agg$playtime <- round(agg$playtime / 60, 1)

   # make the line width a function of the number of lines
   plotlwd <- max(0.2, 5 - 0.02*nrow(agg))

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=col.bg, border=col.help.border, lwd=lwd+3)

   plot.playtime <- function() {
      rect(1+0.4, 1+0.4, 9-0.4, 9-0.4, col=col.bg, border=NA)
      par(new=TRUE, mar=rep(11,4))
      plot(agg$day, agg$playtime, type="h", lwd=plotlwd, col=col.square.l, ylim=c(0, max(agg$playtime)),
           xlab=.text("days", FALSE), ylab=.text("historyplaytime"), bty="l", las=1,
           col.axis=col.top, col.lab=col.top, col.main=col.fg)
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
      .texttop(.text("totalplaytime", .totaltime(total.playtime)))
      return("playtime")
   }

   plot.seqsplayed <- function() {
      rect(1+0.4, 1+0.4, 9-0.4, 9-0.4, col=col.bg, border=NA)
      par(new=TRUE, mar=rep(11,4))
      plot(agg$day, agg$seqsplayed, type="h", lwd=plotlwd, col=col.square.l, ylim=c(0, max(agg$seqsplayed)),
           xlab=.text("days", FALSE), ylab=.text("historyseqsplayed"), bty="l", las=1,
           col.axis=col.top, col.lab=col.top, col.main=col.fg)
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
      .texttop(.text("totalseqsplayed", total.seqsplayed))
      return("seqsplayed")
   }

   whichplot <- plot.playtime()

   while (TRUE) {

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))

      if (identical(resp, "\r") || identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-[") || identical(resp, "F12"))
         break

      if (identical(resp, "Right") || identical(resp, "Left")) {
         if (whichplot == "playtime") {
            whichplot <- plot.seqsplayed()
            next
         }
         if (whichplot == "seqsplayed") {
            whichplot <- plot.playtime()
            next
         }
      }

   }

   par(new=FALSE)

   #.erase(1, 1, 9, 9)

   return()

}
