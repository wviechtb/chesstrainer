.bookmarks <- function(seqdir, seqdirpos) {

   col.bg    <- .get("col.bg")
   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")

   # if there is no .bookmarks file, exit

   if (!file.exists(file.path(seqdir[seqdirpos], ".bookmarks"))) {
      .texttop(.text("nobookmarks"), sleep=1)
      return(NA)
   }

   # try to read the .bookmarks file; if it is empty, read.table() generates an error so we exit again

   tmp <- try(read.table(file.path(seqdir[seqdirpos], ".bookmarks"), header=FALSE), silent=TRUE)

   if (inherits(tmp, "try-error")) {
      try(file.remove(file.path(seqdir[seqdirpos], ".bookmarks")), silent=TRUE)
      .texttop(.text("nobookmarks"), sleep=1)
      return(NA)
   }

   # remove duplicates and keep only the existing bookmarks

   bookmarks <- unique(tmp[[1]])
   bookmarks <- bookmarks[is.element(bookmarks, list.files(seqdir[seqdirpos], pattern="\\.rds$"))]

   # if there is nothing left, again exit

   if (length(bookmarks) == 0L) {
      try(file.remove(file.path(seqdir[seqdirpos], ".bookmarks")), silent=TRUE)
      .texttop(.text("nobookmarks"), sleep=1)
      return(NA)
   }

   bookmark <- ""
   num <- 0L
   keymode <- "select"
   whichpos <- 1L

   tmp <- .drawbookmarks(bookmarks)
   cex <- tmp$cex
   ypos <- tmp$ypos
   dist <- tmp$dist
   segpos <- tmp$segpos

   while (TRUE) {

      if (length(bookmarks) == 0L)
         break

      dev.hold()

      rect(1.4, 1.9, 8.7, 2.2, col=col.bg, border=NA)

      if (keymode=="select")
         text(1.8, 2, .text("selbookmark", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="remove")
         text(1.8, 2, .text("bookmarktoremove", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="position" && whichpos==1L)
         text(1.8, 2, .text("bookmarktomove", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="position" && whichpos==2L)
         text(1.8, 2, .text("bookmarknewpos", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      dev.flush()

      key <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(key)) {
         x <- grconvertX(key[[1]], from="ndc", to="user")
         y <- grconvertY(key[[2]], from="ndc", to="user")
         if (x >= 1.5 && x <= 8.2 & y <= segpos[1] & y >= segpos[2]) {
            click <- which(y < ypos + dist & y > ypos - dist)
            if (length(click) == 1L) {
               if (keymode=="select") {
                  bookmark <- bookmarks[click]
                  .drawbookmarks(bookmarks, sel=click)
                  break
               }
               if (keymode=="remove") {
                  bookmarks <- bookmarks[-click]
                  num <- 0L
                  whichpos <- 1L
                  keymode <- "select"
                  tmp <- .drawbookmarks(bookmarks)
                  cex <- tmp$cex
                  ypos <- tmp$ypos
                  dist <- tmp$dist
                  segpos <- tmp$segpos
               }
               if (keymode=="position") {
                  if (whichpos==1L) {
                     posfrom <- click
                     num <- 0L
                     whichpos <- 2L
                     next
                  }
                  if (whichpos==2L) {
                     posto <- click
                     num <- 0L
                     whichpos <- 1L
                     keymode <- "select"
                     bookmarktomove <- bookmarks[posfrom]
                     bookmarks <- bookmarks[-posfrom]
                     bookmarks <- append(bookmarks, bookmarktomove, after=posto-1)
                     .drawbookmarks(bookmarks)
                     next
                  }
               }
            }
            next
         } else {
            break
         }
      }

      # Escape or q to exit

      if (identical(key, "\033") || identical(key, "ctrl-[") || identical(key, "q") || identical(key, ">")) {
         if (keymode == "remove") {
            num <- 0L
            whichpos <- 1L
            keymode <- "select"
            next
         }
         if (keymode == "position") {
            num <- 0L
            whichpos <- 1L
            keymode <- "select"
            next
         }
         break
      }

      # F1 to show the help

      if (identical(key, "F1")) {
         .showhelp.bookmarks()
         .drawbookmarks(bookmarks)
         next
      }

      # number = add number to num

      if (is.element(key, 0:9)) {
         newnum <- paste0(num, key, collapse="")
         newnum <- as.integer(newnum)
         if (nchar(newnum) > nchar(length(bookmarks))) {
            next
         } else {
            num <- newnum
         }
         next
      }

      # enter = enter selected number

      if (identical(key, "\r") || identical(key, "ctrl-J")) {
         if (keymode=="select" && num==0L)
            break
         if (num < 1L || num > length(bookmarks)) {
            num <- 0L
            whichpos <- 1L
            keymode <- "select"
            next
         }
         if (keymode=="select") {
            bookmark <- bookmarks[num]
            .drawbookmarks(bookmarks, sel=num)
            break
         }
         if (keymode=="remove") {
            bookmarks <- bookmarks[-num]
            num <- 0L
            whichpos <- 1L
            keymode <- "select"
            tmp <- .drawbookmarks(bookmarks)
            cex <- tmp$cex
            ypos <- tmp$ypos
            dist <- tmp$dist
            next
         }
         if (keymode=="position") {
            if (whichpos==1L) {
               posfrom <- num
               num <- 0L
               whichpos <- 2L
               next
            }
            if (whichpos==2L) {
               posto <- num
               num <- 0L
               whichpos <- 1L
               keymode <- "select"
               bookmarktomove <- bookmarks[posfrom]
               bookmarks <- bookmarks[-posfrom]
               bookmarks <- append(bookmarks, bookmarktomove, after=posto-1)
               .drawbookmarks(bookmarks)
               next
            }
         }
      }

      # backspace = remove last entered number

      if (identical(key, "\b") || identical(key, "ctrl-H")) {
         if (nchar(num) == 1L) {
            num <- 0L
         } else {
            num <- as.numeric(substr(num, 1L, nchar(num)-1L))
         }
         next
      }

      # r = go into removal mode

      if (identical(key, "r")) {
         keymode <- "remove"
         num <- 0L
         whichpos <- 1L
         next
      }

      # R = remove all bookmarks

      if (identical(key, "R")) {
         rect(1.4, 1.5, 8.7, 2.4, col=col.bg, border=NA)
         text(1.8, 2, .text("rlydelallbookmarks"), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)
         answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
         if (.confirm(answer)) {
            bookmarks <- NULL
            break
         }
      }

      # p = enter into change position mode

      if (identical(key, "p") && length(bookmarks) >= 2L) {
         keymode <- "position"
         num <- 0L
         whichpos <- 1L
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

.drawbookmarks <- function(bookmarks, sel=NULL) {

   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")

   dev.hold()

   .drawbox()

   bookmarks2 <- bookmarks
   bookmarkslen <- nchar(bookmarks2)
   maxlen <- 80L
   bookmarks2 <- sapply(bookmarks2, function(x) if (nchar(x) >= maxlen) paste0("...", substr(x, max(1L,nchar(x)-maxlen), nchar(x))) else x, USE.NAMES=FALSE)

   tab <- data.frame(sub("\\.rds$", "", bookmarks2))
   names(tab) <- ""
   txt <- capture.output(print(tab, right=FALSE, print.gap=2))[-1]
   txt <- c(.text("bookmarks"), "", txt, "")

   ypos1 <- 8
   ypos2 <- max(2.5, 8-0.75*length(bookmarks2))
   ypos <- seq(ypos1, ypos2, length.out=length(txt))

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8.2, y1=ypos1, y2=ypos2, mincex=1.1)

   segpos <- c(ypos[2], ypos[length(ypos)])
   segments(1.8, segpos[1], 8.2, segpos[1], col=col.help)
   segments(1.8, segpos[2], 8.2, segpos[2], col=col.help)

   bold <- c(2,rep(1,length(txt)-1))
   if (!is.null(sel))
      bold[sel+2] <- 2
   text(1.8, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=bold, col=col.help)

   dist <- (ypos[1] - ypos[2]) / 2
   ypos <- ypos[3:(length(ypos)-1)]

   dev.flush()

   return(list(cex=cex, ypos=ypos, dist=dist, segpos=segpos))

}

.showhelp.bookmarks <- function() {

   lang <- .get("lang")

   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")

   if (lang == "en") {

      txt <- c(
      "Bookmark help:",
      "<number> - select a bookmark (or click on bookmark)",
      "r        - remove a bookmark",
      "R        - remove all bookmarks",
      "p        - change position of a bookmark",
      "F1       - show this help")

   }

   if (lang == "de") {

      txt <- c(
      "Lesezeichen Hilfe:",
      "<Nummer> - Lesezeichen ausw\U000000E4hlen (oder auf das Lesezeichen klicken)",
      "r        - Lesezeichen entfernen",
      "R        - alle Lesezeichen entfernen",
      "p        - Position von Lesezeichen \U000000E4ndern",
      "F1       - diese Hilfe anzeigen")

   }

   dev.hold()

   .drawbox()

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=4.5, y2=7, mincex=1.1)
   ypos <- seq(7, 4.5, length.out=length(txt))

   text(1.8, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=c(2,rep(1, length(txt)-1)), col=col.help)

   dev.flush()

   .waitforclick()

}
