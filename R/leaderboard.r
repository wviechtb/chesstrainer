.leaderboard <- function(seqdir, files, lwd) {

   #.clearsideindicator()
   #.drawtimer(clear=TRUE)

   tmp <- lapply(file.path(seqdir, files), readRDS)
   tmp.scores <- lapply(tmp, function(x) sapply(x$player, function(x) tail(x$score,1)))
   players <- unique(unlist(lapply(tmp.scores, function(x) names(x))))
   nplayers <- length(players)

   if (nplayers >= 1) {

      col.bg          <- .get("col.bg")
      col.help        <- .get("col.help")
      col.help.border <- .get("col.help.border")
      font.mono       <- .get("font.mono")

      tmp.scores <- lapply(players, function(player) {
         x <- lapply(tmp, function(x) tail(x$player[[player]]$score,1))
         x[sapply(x, is.null)] <- 100
         unlist(x)
      })
      tmp.scores <- do.call(cbind, tmp.scores)
      tmp.scores[is.na(tmp.scores)] <- 100
      rownames(tmp.scores) <- files
      tmp.scores[tmp.scores == 0] <- NA_real_

      mean.scores <- round(apply(tmp.scores, 2, mean, na.rm=TRUE), digits=1)
      sd.scores   <- round(apply(tmp.scores, 2, sd, na.rm=TRUE), digits=1)
      min.scores  <- apply(tmp.scores, 2, min, na.rm=TRUE)
      max.scores  <- apply(tmp.scores, 2, max, na.rm=TRUE)

      tmp.played <- lapply(players, function(player) {
         x <- lapply(tmp, function(x) tail(x$player[[player]]$played,1))
         x[sapply(x, is.null)] <- 0
         unlist(x)
      })
      tmp.played <- do.call(cbind, tmp.played)
      total.played <- round(apply(tmp.played, 2, sum, na.rm=TRUE))

      tmp <- data.frame(players, mean.scores, sd.scores, min.scores, max.scores, total.played)
      names(tmp) <- c(.text("player"), .text("score"), "SD", "Min", "Max", .text("played"))
      tmp <- tmp[order(tmp[[2]]),]
      rownames(tmp) <- NULL

      txt <- capture.output(print(tmp, print.gap=3))
      txt <- c(txt[1], "", txt[2:length(txt)], "")

      rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

      ypos1 <- min(8.5, 5+nplayers*0.35)
      ypos2 <- max(1.5, 5-nplayers*0.35)
      ypos <- seq(ypos1, ypos2, length.out=length(txt))

      segments(1.8, ypos[2], 8, ypos[2], col=col.help)
      segments(1.8, ypos[length(ypos)], 8, ypos[length(ypos)], col=col.help)

      cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=ypos1, y2=ypos2, mincex=1.5)

      text(1.8, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=c(2,rep(1, length(txt)-1)), col=col.help)

      .waitforclick()

      #.erase(1, 1, 9, 9)

   }

   return(nplayers)

}
