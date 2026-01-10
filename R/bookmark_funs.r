.bookmarks <- function(seqdir, seqdirpos, texttop, lwd) {

   col.bg    <- .get("col.bg")
   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")

   # if there is no .bookmarks file, exit

   if (!file.exists(file.path(seqdir[seqdirpos], ".bookmarks"))) {
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return(NA)
   }

   # if .bookmarks is empty, read.table() generates an error so we exit again

   tmp <- try(read.table(file.path(seqdir[seqdirpos], ".bookmarks"), header=FALSE), silent=TRUE)

   if (inherits(tmp, "try-error")) {
      try(file.remove(file.path(seqdir[seqdirpos], ".bookmarks")), silent=TRUE)
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return(NA)
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
      return(NA)
   }

   bookmark <- ""
   num <- 0
   keymode <- "s"
   whichnum <- 1

   #.clearsideindicator()
   #.drawtimer(clear=TRUE)

   tmp <- .drawbookmarks(bookmarks, lwd)
   cex <- tmp$cex
   ypos <- tmp$ypos
   dist <- tmp$dist

   while (TRUE) {

      if (length(bookmarks) == 0L)
         break

      rect(1.4, 1.9, 8.7, 2.2, col=col.bg, border=NA)

      if (keymode=="s")
         text(1.8, 2, .text("selbookmark", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="r")
         text(1.8, 2, .text("bookmarktoremove", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="p" && whichnum==1)
         text(1.8, 2, .text("bookmarktomove", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="p" && whichnum==2)
         text(1.8, 2, .text("bookmarknewpos", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      key <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(key)) {
         x <- grconvertX(key[[1]], from="ndc", to="user")
         y <- grconvertY(key[[2]], from="ndc", to="user")
         if (x >= 1.5 && x <= 8) {
            click <- which(y < ypos + dist & y > ypos - dist)
            if (length(click) == 1L) {
               bookmark <- bookmarks[click]
               break
            }
         }
         next
      }

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
         newnum <- paste0(num, key, collapse="")
         newnum <- as.numeric(newnum)
         if (nchar(newnum) > nchar(length(bookmarks))) {
            next
         } else {
            num <- newnum
         }
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
            tmp <- .drawbookmarks(bookmarks, lwd)
            cex <- tmp$cex
            ypos <- tmp$ypos
            dist <- tmp$dist
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
         rect(1.4, 1.5, 8.7, 2.4, col=col.bg, border=NA)
         text(1.8, 2, .text("rlydelallbookmarks"), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)
         answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
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

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   tab <- data.frame(sub("\\.rds$", "", bookmarks))
   names(tab) <- ""
   txt <- capture.output(print(tab, right=FALSE, print.gap=2))[-1]
   txt <- c(.text("bookmark"), "", txt, "")

   ypos1 <- 8
   ypos2 <- max(2.5, 8-0.75*length(bookmarks))
   ypos <- seq(ypos1, ypos2, length.out=length(txt))

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=ypos1, y2=ypos2, mincex=1.1)

   segments(1.8, ypos[2], 8, ypos[2], col=col.help)
   segments(1.8, ypos[length(ypos)], 8, ypos[length(ypos)], col=col.help)

   text(1.8, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=c(2,rep(1, length(txt)-1)), col=col.help)

   dist <- (ypos[1] - ypos[2]) / 2
   ypos <- ypos[3:(length(ypos)-1)]

   return(list(cex=cex, ypos=ypos, dist=dist))

}

.showhelp.bookmarks <- function(lwd) {

   lang <- .get("lang")

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   if (lang == "en") {

      txt <- c(
      "Bookmark help:",
      "<number> - select a bookmark (or click on bookmark)",
      "r        - remove a bookmark",
      "a        - remove all bookmarks",
      "p        - change position of a bookmark",
      "F1       - show this help")

   }

   if (lang == "de") {

      txt <- c(
      "Lesezeichen Hilfe:",
      "<Nummer> - Lesezeichen ausw\U000000E4hlen (oder auf das Lesezeichen klicken)",
      "e        - Lesezeichen entfernen",
      "a        - alle Lesezeichen entfernen",
      "p        - Position von Lesezeichen \U000000E4ndern",
      "F1       - diese Hilfe anzeigen")

   }

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=4.5, y2=7, mincex=1.1)
   ypos <- seq(7, 4.5, length.out=length(txt))

   text(1.8, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=c(2,rep(1, length(txt)-1)), col=col.help)

   .waitforclick()

}
