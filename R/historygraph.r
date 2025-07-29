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
   day <- as.Date(dat$date.start, format="%Y-%m-%d")
   agg.day <- aggregate(dat[c("playtime", "seqsplayed")], by=list(day=day), FUN=sum)

   # compute the week and month totals
   week  <- format(agg.day$day, "%Y-%W")
   month <- as.Date(paste0(format(agg.day$day, "%Y-%m"), "-01"))
   agg.week  <- aggregate(agg.day[c("playtime", "seqsplayed")], by=list(week=week), FUN=sum)
   agg.month <- aggregate(agg.day[c("playtime", "seqsplayed")], by=list(month=month), FUN=sum)

   # figure out start date of each week in agg.week
   agg.week$year <- substr(agg.week$week, 1, 4)
   agg.week$weeknum <- as.integer(substr(agg.week$week, 6, 7))
   agg.week$week <- as.Date(paste0(agg.week$year, "-01-01")) + (agg.week$weeknum * 7) - as.integer(format(as.Date(paste0(agg.week$year, "-01-01")), "%u")) + 1

   # compute totals
   total.playtime  <- sum(agg.day$playtime)
   total.seqsplayed <- sum(agg.day$seqsplayed)

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=col.bg, border=col.help.border, lwd=lwd+3)

   plot.playtime <- function(x) {
      # make the line width a function of the number of lines
      plotlwd <- max(0.2, 5 - 0.02*nrow(x))
      x$playtime <- round(x$playtime / 60, 1)
      rect(1+0.4, 1+0.4, 9-0.4, 9-0.4, col=col.bg, border=NA)
      par(new=TRUE, mar=rep(11,4))
      plot(x[[1]], x$playtime, type="h", lwd=plotlwd, col=col.square.l, ylim=c(0, max(x$playtime)),
           xlab=.text(timeframe, FALSE), ylab=.text("historyplaytime"), bty="l", las=1,
           col.axis=col.top, col.lab=col.top, col.main=col.fg)
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
      .texttop(.text("totalplaytime", .totaltime(total.playtime)))
   }

   plot.seqsplayed <- function(x) {
      # make the line width a function of the number of lines
      plotlwd <- max(0.2, 5 - 0.02*nrow(x))
      rect(1+0.4, 1+0.4, 9-0.4, 9-0.4, col=col.bg, border=NA)
      par(new=TRUE, mar=rep(11,4))
      plot(x[[1]], x$seqsplayed, type="h", lwd=plotlwd, col=col.square.l, ylim=c(0, max(x$seqsplayed)),
           xlab=.text(timeframe, FALSE), ylab=.text("historyseqsplayed"), bty="l", las=1,
           col.axis=col.top, col.lab=col.top, col.main=col.fg)
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
      .texttop(.text("totalseqsplayed", total.seqsplayed))
   }

   # defaults
   timeframe <- "day"
   whichplot <- "playtime"
   agg <- agg.day

   while (TRUE) {

      if (whichplot == "playtime")
         plot.playtime(agg)

      if (whichplot == "seqsplayed")
         plot.seqsplayed(agg)

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))

      if (identical(resp, "\r") || identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-[") || identical(resp, "F12"))
         break

      if (identical(resp, "Down") || identical(resp, "Up")) {
         if (whichplot == "playtime") {
            whichplot <- "seqsplayed"
            next
         }
         if (whichplot == "seqsplayed") {
            whichplot <- "playtime"
            next
         }
      }

      if (identical(resp, "Right")) {
         if (timeframe == "day" && nrow(agg.week) > 1L) {
            timeframe <- "week"
            agg <- agg.week
         }
         if (timeframe == "week" && nrow(agg.month) > 1L) {
            timeframe <- "month"
            agg <- agg.month
         }
         if (timeframe == "month" && nrow(agg.day) > 1L) {
            timeframe <- "day"
            agg <- agg.day
         }
      }

      if (identical(resp, "Left")) {
         if (timeframe == "day" && nrow(agg.month) > 1L) {
            timeframe <- "month"
            agg <- agg.month
         }
         if (timeframe == "week" && nrow(agg.day) > 1L) {
            timeframe <- "day"
            agg <- agg.day
         }
         if (timeframe == "month" && nrow(agg.week) > 1L) {
            timeframe <- "week"
            agg <- agg.week
         }
      }

   }

   par(new=FALSE)

   #.erase(1, 1, 9, 9)

   return()

}
