.selectplayer <- function(current, seqdir, lwd, mustselect=FALSE) {

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   # find all players in the sequence files

   files <- list.files(seqdir, full.names=TRUE, pattern=".rds$")
   dat <- lapply(files, readRDS)

   players <- sort(unique(unlist(lapply(dat, function(x) names(x$player)))))
   nplayers <- length(players)

   rect(1.2, 1.2, 8.8, 8.8, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   if (nplayers == 0L) {

      # if there are no players, then prompt for a new player name

      string <- .text("newplayername")

      ypos.string <- 7.5
      ypos.string.bot <- 7
      ypos.string.top <- 8

      cex <- .findcex(paste0(string, paste0(rep("A",15), collapse="")), font=font.mono, x1=1.8, x2=8, y1=7, y2=8)

   } else {

      string <- .text("enterplayer")

      # if there are players in the sequence files, show a list of them

      tmp.played <- lapply(players, function(player) {
         x <- lapply(dat, function(x) tail(x$player[[player]]$played,1))
         x[sapply(x, is.null)] <- 0
         unlist(x)
      })
      tmp.played <- do.call(cbind, tmp.played)
      total.played <- round(apply(tmp.played, 2, sum, na.rm=TRUE))

      tmp.date <- sapply(players, function(player) {
         player.file <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions", paste0(player, ".rds"))
         if (!file.exists(player.file))
            return(NA)
         tmp <- readRDS(player.file)
         return(tail(tmp$date.end, 1))
      })
      tmp.date[current == players] <- Sys.time()
      last.session <- format(as.POSIXct(tmp.date), "%Y-%m-%d %H:%M:%S")
      last.session[is.na(last.session)] <- ""
      last.session <- unname(last.session)

      tab <- data.frame(players, total.played, last.session)
      names(tab) <- c(.text("player"), .text("played"), .text("lastsession"))
      tab[[2]] <- as.character(tab[[2]])
      txt <- capture.output(print(tab, right=FALSE, print.gap=3))
      txt <- c(txt[1], "", txt[-1], "")

      ypos1 <- 8
      ypos2 <- max(2.5, 8-0.5*length(players))

      cex <- .findcex(paste0(string, paste0(rep("A",15), collapse="")), font=font.mono, x1=1.5, x2=8, y1=ypos1, y2=ypos2)

      ypos <- seq(ypos1, ypos2, length.out=length(txt))
      ypos.players <- ypos[-c(1:2,length(ypos))]
      txt.players <- txt[-c(1:2,length(ypos))]

      segments(1.8, ypos[2], 8, ypos[2], col=col.help)
      segments(1.8, ypos[length(ypos)], 8, ypos[length(ypos)], col=col.help)

      font <- c(2,rep(1, length(txt)-1))
      font[which(current == players)+2] <- 2 # highlight the current player
      text(1.8, ypos, txt, pos=4, cex=cex, offset=0, family=font.mono, font=font, col=.get("col.help"))

      dist <- (ypos[1] - ypos[2]) / 2
      ypos.string <- tail(ypos, 1) - 3 * dist
      ypos.string.bot <- ypos.string - dist
      ypos.string.top <- ypos.string + dist

   }

   sw.string <- strwidth(string, family=font.mono, cex=cex)

   text(1.8, ypos.string, string, pos=4, offset=0, cex=cex, family=font.mono, col=col.help)

   val <- ""
   sw.val <- 0

   while (TRUE) {

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (nplayers > 0L && is.numeric(resp)) {
         x <- grconvertX(resp[[1]], from="ndc", to="user")
         y <- grconvertY(resp[[2]], from="ndc", to="user")
         if (x >= 1.5 && x <= 8) {
            click <- which(y < ypos.players + dist & y > ypos.players - dist)
            if (length(click) == 1L) {
               player <- players[click]
               if (current %in% players) {
                  rect(1.8, ypos.players[which(current == players)]-dist, 8.5, ypos.players[which(current == players)]+dist, col=col.bg, border=NA)
                  text(1.8, ypos.players[which(current == players)], txt.players[which(current == players)], pos=4, offset=0, cex=cex, family=font.mono, col=.get("col.help"))
               }
               rect(1.8, ypos.players[click]-dist, 8.5, ypos.players[click]+dist, col=col.bg, border=NA)
               text(1.8, ypos.players[click], txt.players[click], pos=4, offset=0, cex=cex, family=font.mono, font=2, col=.get("col.help"))
               Sys.sleep(1)
               break
            }
         }
         next
      }

      if (identical(resp, "\033") || identical(resp, "ctrl-[")) {
         if (mustselect) {
            rect(1.8+sw.string, ypos.string.bot, 8.5, ypos.string.top, col=col.bg, border=NA)
            val <- ""
            sw.val <- 0
            next
         } else {
            player <- current
            break
         }
      }

      if (grepl("^[0-9A-Za-z_]+$", resp)) {
         if (nchar(val) > 15L)
            next
         char <- resp
         val <- paste0(val, resp, collapse="")
         text(1.8+sw.string+sw.val, ypos.string, char, pos=4, offset=0, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

      if (identical(resp, "\r") || identical(resp, "ctrl-J")) {
         val <- paste0(val, collapse="")
         if (identical(val, "")) {
            if (mustselect) {
               next
            } else {
               player <- current
               break
            }
         }
         if (grepl("^[0-9]+$", val) && nplayers > 0L) {
            val <- round(as.numeric(val))
            if (val >= 1L && val <= nplayers) {
               player <- players[val]
               rect(1.8, ypos.players[val]-dist, 8.5, ypos.players[val]+dist, col=col.bg, border=NA)
               text(1.8, ypos.players[val], txt.players[val], pos=4, offset=0, cex=cex, family=font.mono, font=2, col=.get("col.help"))
               if (current %in% players) {
                  rect(1.8, ypos.players[which(current == players)]-dist, 8.5, ypos.players[which(current == players)]+dist, col=col.bg, border=NA)
                  text(1.8, ypos.players[which(current == players)], txt.players[which(current == players)], pos=4, offset=0, cex=cex, family=font.mono, col=.get("col.help"))
               }
               Sys.sleep(1)
               break
            } else {
               next
            }
         } else {
            player <- make.names(val) # just in case
         }
         break
      }

      if (identical(resp, "\b") || identical(resp, "ctrl-H")) {
         if (nchar(val) > 1L) {
            val <- substr(val, 1, nchar(val)-1)
         } else {
            val <- ""
         }
         rect(1.8+sw.string, ypos.string.bot, 8.7, ypos.string.top, col=col.bg, border=NA)
         text(1.8+sw.string, ypos.string, val, pos=4, offset=0, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

   }

   return(player)

}

.selectplayerconsole <- function(current, seqdir, mustselect=FALSE) {

   files <- list.files(seqdir, full.names=TRUE, pattern=".rds$")
   dat <- lapply(files, readRDS)

   players <- sort(unique(unlist(lapply(dat, function(x) names(x$player)))))
   nplayers <- length(players)

   if (nplayers == 0L) {
      while (TRUE) {
         player <- readline(prompt=.text("newplayername"))
         if (!identical(player, "")) {
            player <- make.names(player) # just in case
            break
         }
      }
   } else {
      cat(.text("foundplayers"))
      tmp <- data.frame(players)
      names(tmp) <- .text("player")
      print(tmp)
      cat("\n")
      while (TRUE) {
         player <- readline(prompt=.text("enterplayer"))
         if (identical(player, "")) {
            if (mustselect) {
               next
            } else {
               player <- current
               break
            }
         }
         if (grepl("^[0-9]+$", player)) {
            player <- round(as.numeric(player))
            if (player >= 1L && player <= nplayers) {
               player <- players[player]
               break
            }
         } else {
            player <- make.names(player) # just in case
         }
         break
      }
   }

   return(player)

}

.removeplayer <- function(player, seqdir) {

   files <- list.files(seqdir, full.names=TRUE, pattern=".rds$")

   if (length(files) >= 1L) {
      for (i in 1:length(files)) {
         tmp <- readRDS(files[i])
         if (!is.null(tmp$player[[player]])) {
            tmp$player[[player]] <- NULL
            saveRDS(tmp, file=files[i])
         }
      }
   }

   player.file <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions", paste0(player, ".rds"))

   if (file.exists(player.file))
      file.remove(player.file)

}
