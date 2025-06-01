.bookmarks <- function(seqdir, seqdirpos, texttop, lwd) {

   # if there is no .bookmarks file, exit

   if (!file.exists(file.path(seqdir[seqdirpos], ".bookmarks"))) {
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return("")
   }

   # if .bookmarks is empty, read.table() generates an error so we exit again

   tmp <- try(read.table(file.path(seqdir[seqdirpos], ".bookmarks"), header=FALSE), silent=TRUE)

   if (inherits(tmp, "try-error")) {
      try(file.remove(file.path(seqdir[seqdirpos], ".bookmarks")), silent=TRUE)
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return("")
   }

   # remove duplicates and keep only the existing bookmarks

   bookmarks <- tmp[[1]]
   bookmarks <- unique(tmp[[1]])
   bookmarks <- bookmarks[is.element(bookmarks, list.files(seqdir[seqdirpos], pattern=".rds$"))]

   # if there is nothing left, again exit

   if (length(bookmarks) == 0L) {
      try(file.remove(file.path(seqdir[seqdirpos], ".bookmarks")), silent=TRUE)
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return("")
   }

   bookmark <- ""
   num <- 0
   keymode <- "s"
   whichnum <- 1

   #.clearsideindicator()
   #.drawtimer(clear=TRUE)

   cex <- .drawbookmarks(bookmarks, lwd)

   while (TRUE) {

      if (length(bookmarks) == 0L)
         break

      rect(1.4, 1.9, 8.7, 2.2, col=.get("col.bg"), border=NA)

      if (keymode=="s")
         text(1+0.5, 2, .text("selbookmark", ifelse(num==0, "", num)), pos=4, cex=cex, family=.get("font.mono"), font=1, col=.get("col.help"))

      if (keymode=="r")
         text(1+0.5, 2, .text("bookmarktoremove", ifelse(num==0, "", num)), pos=4, cex=cex, family=.get("font.mono"), font=1, col=.get("col.help"))

      if (keymode=="p" && whichnum==1)
         text(1+0.5, 2, .text("bookmarktomove", ifelse(num==0, "", num)), pos=4, cex=cex, family=.get("font.mono"), font=1, col=.get("col.help"))

      if (keymode=="p" && whichnum==2)
         text(1+0.5, 2, .text("bookmarknewpos", ifelse(num==0, "", num)), pos=4, cex=cex, family=.get("font.mono"), font=1, col=.get("col.help"))

      key <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=function(key) return(key))

      # Escape or q to exit

      if (identical(key, "\033") || identical(key, "ctrl-[") || identical(key, "q"))
         break

      # F1 to show the help

      if (identical(key, "F1")) {
         .showhelp.bookmarks(lwd=lwd)
         .drawbookmarks(bookmarks, lwd)
         next
      }

      # number = add number to num

      if (is.element(key, 0:9)) {
         num <- paste0(num, key, collapse="")
         num <- as.numeric(num)
         next
      }

      # enter = enter selected number

      if (identical(key, "\r") || identical(key, "ctrl-J")) {
         if (keymode=="s" && num==0)
            break
         if (num < 1 || num > length(bookmarks)) {
            num <- 0
            whichnum <- 1
            keymode <- "s"
            next
         }
         if (keymode=="s") {
            bookmark <- bookmarks[num]
            break
         }
         if (keymode=="r") {
            bookmarks <- bookmarks[-num]
            num <- 0
            whichnum <- 1
            keymode <- "s"
            cex <- .drawbookmarks(bookmarks, lwd,)
            next
         }
         if (keymode=="p") {
            if (whichnum==1) {
               posfrom <- num
               num <- 0
               whichnum <- 2
               next
            }
            if (whichnum==2) {
               posto <- num
               num <- 0
               whichnum <- 1
               keymode <- "s"
               bookmarktomove <- bookmarks[posfrom]
               bookmarks <- bookmarks[-posfrom]
               bookmarks <- append(bookmarks, bookmarktomove, after=posto-1)
               .drawbookmarks(bookmarks, lwd)
               next
            }
         }
      }

      # backspace = remove last entered number

      if (identical(key, "\b") || identical(key, "ctrl-H")) {
         if (nchar(num) == 1) {
            num <- 0
         } else {
            num <- as.numeric(substr(num, 1, nchar(num)-1))
         }
         next
      }

      # r or e = go into removal mode

      if (identical(key, "r") || identical(key, "e")) {
         keymode <- "r"
         num <- 0
         whichnum <- 1
         next
      }

      # a = remove all bookmarks

      if (identical(key, "a")) {
         rect(1.4, 1.5, 8.7, 2.4, col=.get("col.bg"), border=NA)
         text(1+0.5, 2, .text("rlydelallbookmarks"), pos=4, cex=cex, family=.get("font.mono"), font=1, col=.get("col.help"))
         answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=function(key) return(key))
         if (.confirm(answer)) {
            bookmarks <- c()
            break
         }
      }

      # p = enter into change position mode

      if (identical(key, "p")) {
         keymode <- "p"
         num <- 0
         whichnum <- 1
         next
      }

   }

   if (length(bookmarks) == 0L) {
      try(file.remove(file.path(seqdir[seqdirpos], ".bookmarks")), silent=TRUE)
   } else {
      write.table(data.frame(bookmarks), file=file.path(seqdir[seqdirpos], ".bookmarks"), col.names=FALSE, row.names=FALSE, quote=FALSE)
   }

   #.erase(1, 1, 9, 9)

   return(bookmark)

}

.drawbookmarks <- function(bookmarks, lwd, cex) {

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   tab <- data.frame(sub("\\.rds$", "", bookmarks))
   names(tab) <- ""
   txt <- capture.output(print(tab, right=FALSE, print.gap=2))[-1]
   txt <- c(.text("bookmark"), paste0(rep("-", max(nchar(txt))), collapse=""), txt)

   font.mono <- .get("font.mono")
   maxsw <- max(strwidth(txt, family=font.mono))
   maxsh <- strheight("A", family=font.mono) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.9)

   text(1+0.5, seq(8, max(2.5, 8-0.5*length(bookmarks)), length.out=length(txt)), txt, pos=4, cex=cex,
        family=font.mono, font=c(2,rep(1, length(txt)-1)), col=.get("col.help"))

   return(cex=cex)

}

.showhelp.bookmarks <- function(lwd) {

   lang <- .get("lang")

   if (lang == "en") {

      txt <- c(
      "Bookmark help:",
      " <number> - select a bookmark",
      "r         - remove a bookmark",
      "a         - remove all bookmarks",
      "p         - change position of a bookmark",
      "F1        - show this help",
      "q         - exit help"
      )

   }

   if (lang == "de") {

      txt <- c(
      "Lesezeichen Hilfe:",
      "<Nummer> - Lesezeichen ausw\U000000E4hlen",
      "e        - Lesezeichen entfernen",
      "a        - alle Lesezeichen entfernen",
      "p        - Position von Lesezeichen \U000000E4ndern",
      "F1       - diese Hilfe anzeigen",
      "q        - Hilfe beenden"
      )

   }

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   font.mono <- .get("font.mono")
   maxsw <- max(strwidth(txt, family=font.mono))
   maxsh <- strheight("A", family=font.mono) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.8)

   text(1+0.5, seq(7, 4, length.out=length(txt)), txt, pos=4, cex=cex,
        family=font.mono, font=c(2,rep(1, length(txt)-1)), col=.get("col.help"))

   .waitforclick()

}
