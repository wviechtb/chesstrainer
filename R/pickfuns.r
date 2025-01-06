.colorpick <- function(cols.all, pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, random) {

   cat(.text("colcurrent"))

   tab <- data.frame(col=cols.all, val=unname(sapply(cols.all, function(x) .get(x))))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, random)
   .addrect(4, 5, col=.get("col.hint"), lwd=lwd)
   .addrect(4, 3, col=.get("col.wrong"), lwd=lwd)
   .addrect(4, 4, col=.get("col.rect"), lwd=lwd)
   .drawsquare(0, 4, col=.get("col.square.be"))
   .drawsquare(0, 5, col=.get("col.square.be"))
   .addrect(0, 4, offset=0.028, .get("col.bg"), lwd+2)
   .addrect(0, 5, offset=0.028, .get("col.bg"), lwd+2)
   .addcircle(4, 6, lwd=lwd)
   .drawsideindicator("w", flip)
   .drawsideindicator("b", flip, clear=FALSE)
   .draweval(0, flip)

   while (TRUE) {
      resp <- readline(prompt=.text("colwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[0-9]+$", resp)) {
         colno <- round(as.numeric(resp))
         if (colno < 1 || colno > nrow(tab))
            next
         col <- readline(prompt=.text("colval", tab[colno,2]))
         if (identical(col, ""))
            next
         assign(tab[colno,1], col, envir=.chesstrainer)
         tab[colno,2] <- col
         .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, random)
         .addrect(4, 5, col=.get("col.hint"), lwd=lwd)
         .addrect(4, 3, col=.get("col.wrong"), lwd=lwd)
         .addrect(4, 4, col=.get("col.rect"), lwd=lwd)
         .drawsquare(0, 4, col=.get("col.square.be"))
         .drawsquare(0, 5, col=.get("col.square.be"))
         .addrect(0, 4, offset=0.028, .get("col.bg"), lwd+2)
         .addrect(0, 5, offset=0.028, .get("col.bg"), lwd+2)
         .addcircle(4, 6, lwd=lwd)
         .drawsideindicator("w", flip)
         .drawsideindicator("b", flip, clear=FALSE)
         .draweval(0, flip)
      }
   }

   invisible()

}

.cexpick <- function(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, random) {

   cat(.text("cexcurrent"))

   tab <- data.frame(cex = c("cex.top", "cex.bot", "cex.eval"),
                     val = c(.get("cex.top"), .get("cex.bot"), .get("cex.eval")))
   names(tab) <- c("", "")
   print(tab, right=FALSE, print.gap=3)

   cat("\n")

   .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, random)
   .draweval(-0.2, flip)

   while (TRUE) {
      resp <- readline(prompt=.text("cexwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[0-9]+$", resp)) {
         cexno <- round(as.numeric(resp))
         if (cexno < 1 || cexno > nrow(tab))
            next
         cex <- readline(prompt=.text("cexval", tab[cexno,2]))
         if (identical(cex, ""))
            next
         cex <- as.numeric(cex)
         cex[cex < 0.1] <- 0.1
         assign(tab[cexno,1], cex, envir=.chesstrainer)
         tab[cexno,2] <- cex
         .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop="Lorem ipsum", sidetoplay, random)
         .draweval(-0.2, flip)
      }
   }

   invisible()

}
