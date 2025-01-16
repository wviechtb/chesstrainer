.selectplayer <- function(current, seqdir, mustselect=FALSE) {

   files <- list.files(seqdir, full.names=TRUE, pattern=".rds$")
   dat <- lapply(files, readRDS)

   players <- unique(unlist(lapply(dat, function(x) names(x$score))))
   nplayers <- length(players)

   if (nplayers == 0L) {
      while (TRUE) {
         player <- readline(prompt=.text("newplayername"))
         if (!identical(player, ""))
            break
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
         pos.score <- which(names(tmp$score) == player)
         if (length(pos.score) != 0)
            tmp$score <- tmp$score[-pos.score]
         pos.played <- which(names(tmp$played) == player)
         if (length(pos.played) != 0)
            tmp$played <- tmp$played[-pos.played]
         pos.date <- which(names(tmp$date) == player)
         if (length(pos.date) != 0)
            tmp$date <- tmp$date[-pos.date]
         if (length(pos.score) != 0 || length(pos.played) != 0 || length(pos.date) != 0)
            saveRDS(tmp, file=files[i])
      }
   }

}
