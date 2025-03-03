.selectplayer <- function(current, seqdir, mustselect=FALSE) {

   files <- list.files(seqdir, full.names=TRUE, pattern=".rds$")
   dat <- lapply(files, readRDS)

   players <- unique(unlist(lapply(dat, function(x) names(x$player))))
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

}
