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
   dat.day <- aggregate(dat[c("playtime", "seqsplayed")], by=list(day=day), FUN=sum)

   # compute the week and month totals
   week  <- format(dat.day$day, "%Y-%W")
   month <- as.Date(paste0(format(dat.day$day, "%Y-%m"), "-01"))
   dat.week  <- aggregate(dat.day[c("playtime", "seqsplayed")], by=list(week=week), FUN=sum)
   dat.month <- aggregate(dat.day[c("playtime", "seqsplayed")], by=list(month=month), FUN=sum)

   # figure out start date of each week in dat.week
   dat.week$year <- substr(dat.week$week, 1, 4)
   dat.week$weeknum <- as.integer(substr(dat.week$week, 6, 7))
   dat.week$week <- as.Date(paste0(dat.week$year, "-01-01")) + (dat.week$weeknum * 7) - as.integer(format(as.Date(paste0(dat.week$year, "-01-01")), "%u")) + 1

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=col.bg, border=col.help.border, lwd=lwd+3)

   plot.playtime <- function(x) {
      # make the line width a function of the number of lines
      plotlwd <- max(0.2, 5 - 0.02*nrow(x))
      total.playtime <- sum(x$playtime)
      x$playtime <- round(x$playtime / 60, 1)
      rect(1+0.4, 1+0.4, 9-0.4, 9-0.4, col=col.bg, border=NA)
      par(new=TRUE, mar=rep(11,4))
      if (nrow(x) == 1L) {
         xlim <- c(x[[1]]-1, x[[1]]+1)
      } else {
         xlim <- NULL
      }
      plot(x[[1]], x$playtime, type="h", lwd=plotlwd, col=col.square.l,
           xlim=xlim, ylim=c(0, max(x$playtime)), bty="l", las=1,
           xlab=.text(timeframe, FALSE), ylab=.text("historyplaytime"),
           col.axis=col.top, col.lab=col.top, col.main=col.fg)
      usr <<- par()$usr
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
      .texttop(.text("totalplaytime", .totaltime(total.playtime)))
   }

   plot.seqsplayed <- function(x) {
      # make the line width a function of the number of lines
      plotlwd <- max(0.2, 5 - 0.02*nrow(x))
      total.seqsplayed <- sum(x$seqsplayed)
      rect(1+0.4, 1+0.4, 9-0.4, 9-0.4, col=col.bg, border=NA)
      par(new=TRUE, mar=rep(11,4))
      if (nrow(x) == 1L) {
         xlim <- c(x[[1]]-1, x[[1]]+1)
      } else {
         xlim <- NULL
      }
      plot(x[[1]], x$seqsplayed, type="h", lwd=plotlwd, col=col.square.l,
           xlim=xlim, ylim=c(0, max(x$seqsplayed)), bty="l", las=1,
           xlab=.text(timeframe, FALSE), ylab=.text("historyseqsplayed"),
           col.axis=col.top, col.lab=col.top, col.main=col.fg)
      usr <<- par()$usr
      par(mar=rep(5.2,4), usr=c(1,9,1,9))
      .texttop(.text("totalseqsplayed", total.seqsplayed))
   }

   # defaults
   timeframe <- "day"
   whichplot <- "playtime"
   agg <- dat.day

   while (TRUE) {

      if (whichplot == "playtime")
         plot.playtime(agg)

      if (whichplot == "seqsplayed")
         plot.seqsplayed(agg)

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))

      if (identical(click, "\r") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[") || identical(click, "F12"))
         break

      if (is.numeric(click) && click[[3]] %in% c(0,2)) {
         if (click[[3]] == 2) { # right mouse button resets zoom
            if (timeframe == "day")
               agg <- dat.day
            if (timeframe == "week")
               agg <- dat.week
            if (timeframe == "month")
               agg <- dat.month
         }
         if (click[[3]] == 0) { # left mouse button to set first and second zoom point
            par(mar=rep(11,4), usr=usr)
            x1 <- grconvertX(click[[1]], from="ndc", to="user")
            if (x1 < usr[1])
               x1 <- usr[1]
            if (x1 > usr[2])
               x1 <- usr[2]
            segments(x1, 0, x1, usr[4], lty="dotted", col=col.top)
            par(mar=rep(5.2,4), usr=c(1,9,1,9))
            click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))
            if (!is.numeric(click))
               next
            par(mar=rep(11,4), usr=usr)
            x2 <- grconvertX(click[[1]], from="ndc", to="user")
            if (x2 < usr[1])
               x2 <- usr[1]
            if (x2 > usr[2])
               x2 <- usr[2]
            segments(x2, 0, x2, usr[4], lty="dotted", col=col.top)
            segments(x1, 0, x2, 0, lty="dotted", col=col.top)
            segments(x1, usr[4], x2, usr[4], lty="dotted", col=col.top)
            Sys.sleep(0.5)
            par(mar=rep(5.2,4), usr=c(1,9,1,9))
            sel <- agg[[1]] >= min(x1,x2) & agg[[1]] <= max(x1,x2)
            if (sum(sel) == 0L)
               next
            agg <- agg[agg[[1]] >= min(x1,x2) & agg[[1]] <= max(x1,x2),]
         }
      }

      if (identical(click, "Down") || identical(click, "Up") || (is.numeric(click) && click[[3]] == 1)) {
         if (whichplot == "playtime") {
            whichplot <- "seqsplayed"
            next
         }
         if (whichplot == "seqsplayed") {
            whichplot <- "playtime"
            next
         }
      }

      if (identical(click, "Right")) {
         if (timeframe == "day" && nrow(dat.week) > 1L) {
            timeframe <- "week"
            agg <- dat.week
         }
         if (timeframe == "week" && nrow(dat.month) > 1L) {
            timeframe <- "month"
            agg <- dat.month
         }
         if (timeframe == "month" && nrow(dat.day) > 1L) {
            timeframe <- "day"
            agg <- dat.day
         }
      }

      if (identical(click, "Left")) {
         if (timeframe == "day" && nrow(dat.month) > 1L) {
            timeframe <- "month"
            agg <- dat.month
         }
         if (timeframe == "week" && nrow(dat.day) > 1L) {
            timeframe <- "day"
            agg <- dat.day
         }
         if (timeframe == "month" && nrow(dat.week) > 1L) {
            timeframe <- "week"
            agg <- dat.week
         }
      }

   }

   par(new=FALSE)

   #.erase(1, 1, 9, 9)

   return()

}
