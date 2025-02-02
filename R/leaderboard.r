.leaderboard <- function(seqdir, files, lwd) {

   tmp <- lapply(file.path(seqdir, files), readRDS)
   tmp.scores <- lapply(tmp, function(x) sapply(x$player, function(x) tail(x$score,1)))
   players <- unique(unlist(lapply(tmp.scores, function(x) names(x))))
   nplayers <- length(players)

   if (nplayers >= 1) {

      tmp.scores <- lapply(players, function(player) {
         x <- lapply(tmp, function(x) tail(x$player[[player]]$score,1))
         x[sapply(x, is.null)] <- 100
         unlist(x)
      })
      tmp.scores <- do.call(cbind, tmp.scores)
      tmp.scores[is.na(tmp.scores)] <- 100
      rownames(tmp.scores) <- files
      tmp.scores[tmp.scores == 0] <- NA_real_

      mean.scores <- round(apply(tmp.scores, 2, mean, na.rm=TRUE))
      sd.scores   <- round(apply(tmp.scores, 2, sd, na.rm=TRUE))
      min.scores  <- apply(tmp.scores, 2, min, na.rm=TRUE)
      max.scores  <- apply(tmp.scores, 2, max, na.rm=TRUE)

      tmp.played <- lapply(players, function(player) {
         x <- lapply(tmp, function(x) tail(x$player[[player]]$played,1))
         x[sapply(x, is.null)] <- 0
         unlist(x)
      })
      tmp.played <- do.call(cbind, tmp.played)
      tmp.played[is.na(tmp.played)] <- 0
      total.played <- round(apply(tmp.played, 2, sum))

      tmp <- data.frame(players, mean.scores, sd.scores, min.scores, max.scores, total.played)
      names(tmp) <- c(.text("player"), .text("score"), "SD", "Min", "Max", .text("played"))
      tmp <- tmp[order(tmp[[2]]),]
      rownames(tmp) <- NULL

      txt <- capture.output(print(tmp, print.gap=3))
      txt <- c(txt[1], paste0(rep("-", max(nchar(txt))), collapse=""), txt[2:length(txt)])

      rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

      maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
      maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
      cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.95)

      text(1+0.5, seq(min(8.5, 5+nplayers*0.25), max(1.5, 5-nplayers*0.25), length.out=length(txt)),
           txt, pos=4, cex=cex, family=.get("font.mono"), col=.get("col.help"))

      getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button, x, y) return(""), onKeybd=function(key) return(""))

   }

   invisible()

}
