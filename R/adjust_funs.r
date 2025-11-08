.setscore <- function(score, lwd) {

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   scoreold <- score

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   cex <- 1.2

   string.cur <- .text("currentscore")
   string.new <- .text("newscore")

   sw.string <- max(strwidth(string.cur, family=font.mono, cex=cex), strwidth(string.new, family=font.mono, cex=cex))

   text(1.5,           7.5, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
   text(1.5+sw.string, 7.5, score,      pos=4, cex=cex, family=font.mono, col=col.help)
   text(1.5,           6.5, string.new, pos=4, cex=cex, family=font.mono, col=col.help)

   val <- ""
   sw.val <- 0

   while (TRUE) {

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(resp))
         break

      if (identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-["))
         break

      if (grepl("^[0-9]+$", resp)) {
         if (nchar(val) > 3)
            next
         num <- resp
         val <- paste0(val, resp, collapse="")
         text(1.5+sw.string+sw.val, 6.5, num, pos=4, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

      if (identical(resp, "\r") || identical(resp, "ctrl-J")) {
         val <- as.numeric(paste0(val, collapse=""))
         if (is.na(val))
            break
         val[val < 0] <- 0
         val[val > 100] <- 100
         score <- val
         break
      }

      if (identical(resp, "\b") || identical(resp, "ctrl-H")) {
         if (nchar(val) > 1L) {
            val <- substr(val, 1, nchar(val)-1)
         } else {
            val <- ""
         }
         rect(1.5+sw.string, 6, 8.5, 7, col=col.bg, border=NA)
         text(1.5+sw.string, 6.5, val, pos=4, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

   }

   if (scoreold != score) {
      rect(1.5, 6, 8.5, 8, col=col.bg, border=NA)
      text(1.5,           7.5, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
      text(1.5+sw.string, 7.5, score,      pos=4, cex=cex, family=font.mono, col=col.help)
      Sys.sleep(1)
   }

   #.erase(1, 1, 9, 9)

   return(score)

}

.setexpval <- function(expval, scores, lwd) {

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   expvalold <- expval

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   cex <- 1.2

   string.cur <- .text("currentexpval")
   string.new <- .text("newexpval")

   sw.string <- max(strwidth(string.cur, family=font.mono, cex=cex), strwidth(string.new, family=font.mono, cex=cex))

   text(1.5,           7.5, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
   text(1.5+sw.string, 7.5, expval,     pos=4, cex=cex, family=font.mono, col=col.help)
   text(1.5,           6.5, string.new, pos=4, cex=cex, family=font.mono, col=col.help)

   val <- ""
   sw.val <- 0

   while (TRUE) {

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(resp))
         break

      if (identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-["))
         break

      if (grepl("^[0-9.]+$", resp)) {
         if (nchar(val) > 4)
            next
         num <- resp
         val <- paste0(val, resp, collapse="")
         text(1.5+sw.string+sw.val, 6.5, num, pos=4, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

      if (identical(resp, "\r") || identical(resp, "ctrl-J")) {
         val <- as.numeric(paste0(val, collapse=""))
         if (is.na(val))
            break
         val[val < 0] <- 0
         expval <- val
         break
      }

      if (identical(resp, "\b") || identical(resp, "ctrl-H")) {
         if (nchar(val) > 1L) {
            val <- substr(val, 1, nchar(val)-1)
         } else {
            val <- ""
         }
         rect(1.5+sw.string, 6, 8.5, 7, col=col.bg, border=NA)
         text(1.5+sw.string, 6.5, val, pos=4, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

   }

   if (expvalold != expval) {
      rect(1.5, 6, 8.5, 8, col=col.bg, border=NA)
      text(1.5,           7.5, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
      text(1.5+sw.string, 7.5, expval,     pos=4, cex=cex, family=font.mono, col=col.help)
      Sys.sleep(1)
   }

   #.erase(1, 1, 9, 9)

   return(expval)

}

.setseqno <- function(seqno, k, lwd) {

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   seqnoold <- seqno

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   cex <- 1.2

   string.cur <- .text("currentseqno")
   string.new <- .text("newseqno", k)

   sw.string <- max(strwidth(string.cur, family=font.mono, cex=cex), strwidth(string.new, family=font.mono, cex=cex))

   text(1.5,           7.5, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
   text(1.5+sw.string, 7.5, seqno,      pos=4, cex=cex, family=font.mono, col=col.help)
   text(1.5,           6.5, string.new, pos=4, cex=cex, family=font.mono, col=col.help)

   val <- ""
   sw.val <- 0

   while (TRUE) {

      resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(resp))
         break

      if (identical(resp, "q") || identical(resp, "\033") || identical(resp, "ctrl-["))
         break

      if (grepl("^[0-9]+$", resp)) {
         if (nchar(val)+1 > nchar(k))
            next
         num <- resp
         val <- paste0(val, resp, collapse="")
         text(1.5+sw.string+sw.val, 6.5, num, pos=4, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

      if (identical(resp, "\r") || identical(resp, "ctrl-J")) {
         val <- as.numeric(paste0(val, collapse=""))
         if (is.na(val))
            break
         if (val < 1 || val > k) {
            val <- ""
            sw.val <- 0
            rect(1.5+sw.string, 6, 8.5, 7, col=col.bg, border=NA)
            next
         } else {
            seqno <- val
         }
         break
      }

      if (identical(resp, "\b") || identical(resp, "ctrl-H")) {
         if (nchar(val) > 1L) {
            val <- substr(val, 1, nchar(val)-1)
         } else {
            val <- ""
         }
         rect(1.5+sw.string, 6, 8.5, 7, col=col.bg, border=NA)
         text(1.5+sw.string, 6.5, val, pos=4, cex=cex, family=font.mono, col=col.help)
         sw.val <- strwidth(val, family=font.mono, cex=cex)
      }

   }

   if (seqnoold != seqno) {
      rect(1.5, 6, 8.5, 8, col=col.bg, border=NA)
      text(1.5,           7.5, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
      text(1.5+sw.string, 7.5, seqno,      pos=4, cex=cex, family=font.mono, col=col.help)
      Sys.sleep(1)
   }

   #.erase(1, 1, 9, 9)

   return(seqno)

}
