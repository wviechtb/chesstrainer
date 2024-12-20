.is.even <- function(x) x %% 2 == 0

.get <- function(x)
   get(x, envir=.chesstrainer)

.is.null <- function(x) {

   if (length(x) == 0L)
      return(logical(0))

   return(sapply(x, is.null))

}

.colorpick <- function(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, lwd) {

   cat(.text("colcurrent"))

   tab <- data.frame(col = c("col.bg", "col.fg", "col.square.l", "col.square.d", "col.top", "col.bot", "col.help", "col.help.border", "col.hint", "col.wrong", "col.rect", "col.annot", "col.side.w", "col.side.b"),
                     val = c(.get("col.bg"), .get("col.fg"), .get("col.square.l"), .get("col.square.d"), .get("col.top"), .get("col.bot"), .get("col.help"), .get("col.help.border"), .get("col.hint"), .get("col.wrong"), .get("col.rect"), .get("col.annot"), .get("col.side.w"), .get("col.side.b")))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop="Lorem ipsum")
   .addrect(4, 5, col=.get("col.hint"), lwd=lwd)
   .addrect(4, 3, col=.get("col.wrong"), lwd=lwd)
   .addrect(4, 4, col=.get("col.rect"), lwd=lwd)
   .addcircle(4, 6, lwd=lwd)
   .drawsideindicator(i, flip)
   .drawsideindicator(i+1, flip, clear=FALSE)

   while (TRUE) {
      resp <- readline(prompt=.text("colwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[0-9]+$", resp)) {
         colno <- round(as.numeric(resp))
         if (colno < 1 || colno > nrow(tab))
            next
         col <- readline(prompt=.text("colval", tab[colno,2]))
         assign(tab[colno,1], col, envir=.chesstrainer)
         .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop="Lorem ipsum")
         .addrect(4, 5, col=.get("col.hint"), lwd=lwd)
         .addrect(4, 3, col=.get("col.wrong"), lwd=lwd)
         .addrect(4, 4, col=.get("col.rect"), lwd=lwd)
         .addcircle(4, 6, lwd=lwd)
         .drawsideindicator(i, flip)
         .drawsideindicator(i+1, flip, clear=FALSE)
      }
   }

   invisible()

}
