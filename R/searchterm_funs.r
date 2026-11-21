.searchterms <- function(seqdir, seqdirpos, cursearchterm) {

   col.bg    <- .get("col.bg")
   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")

   switch1  <- .get("switch1")
   switch2  <- .get("switch2")

   showwhich <- .get("showwhich")

   # if there is no .searchterms file, create it

   if (!file.exists(file.path(seqdir[seqdirpos], ".searchterms")))
      file.create(file.path(seqdir[seqdirpos], ".searchterms"))

   # try to read the .searchterms file; if it is empty, read.table() generates an error

   dat <- try(read.table(file.path(seqdir[seqdirpos], ".searchterms"), sep="\t", header=FALSE, na.strings=""), silent=TRUE)

   if (inherits(dat, "try-error")) {

      if (cursearchterm == "") {
         .texttop(.text("nosearchterms"), sleep=1)
         return(NULL)
      }

      searchterms <- character(0)
      descriptions <- character(0)

   } else {

      # select searchterms column and remove duplicates

      searchterms <- dat[[1]]
      descriptions <- dat[[2]]

   }

   cursearchterm2 <- cursearchterm
   cursearchtermlen <- nchar(cursearchterm2)
   maxlen <- 42L
   cursearchterm2 <- if (nchar(cursearchterm2) >= maxlen) paste0(substr(cursearchterm2, 1L, max(1L,nchar(cursearchterm2)-maxlen)), "...") else cursearchterm2

   searchterm <- ""
   num <- 0L
   keymode <- "select"
   whichpos <- 1L

   tmp <- .drawsearchterms(searchterms, descriptions, showwhich)
   cex <- tmp$cex
   ypos <- tmp$ypos
   dist <- tmp$dist
   segpos <- tmp$segpos

   while (TRUE) {

      dev.hold()

      rect(1.4, 1.3, 8.7, 2.2, col=col.bg, border=NA)

      if (keymode=="select")
         text(1.8, 2, .text("selsearchterm", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="remove")
         text(1.8, 2, .text("searchtermtoremove", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="position" && whichpos==1L)
         text(1.8, 2, .text("searchtermtomove", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="position" && whichpos==2L)
         text(1.8, 2, .text("searchtermnewpos", ifelse(num==0L, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      dev.flush()

      key <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(key)) {
         if (length(searchterms) == 0L)
            next
         x <- grconvertX(key[[1]], from="ndc", to="user")
         y <- grconvertY(key[[2]], from="ndc", to="user")
         if (x >= 1.5 && x <= 8.2 & y <= segpos[1] & y >= segpos[2]) {
            click <- which(y < ypos + dist & y > ypos - dist)
            if (length(click) == 1L) {
               if (keymode=="select") {
                  searchterm <- searchterms[click]
                  .drawsearchterms(searchterms, descriptions, showwhich, sel=click)
                  Sys.sleep(0.5)
                  break
               }
               if (keymode=="remove") {
                  searchterms <- searchterms[-click]
                  descriptions <- descriptions[-click]
                  num <- 0L
                  whichpos <- 1L
                  keymode <- "select"
                  tmp <- .drawsearchterms(searchterms, descriptions, showwhich)
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
                     searchtermtomove <- searchterms[posfrom]
                     descriptiontomove <- descriptions[posfrom]
                     searchterms <- searchterms[-posfrom]
                     descriptions <- descriptions[-posfrom]
                     searchterms <- append(searchterms, searchtermtomove, after=posto-1)
                     descriptions <- append(descriptions, descriptiontomove, after=posto-1)
                     .drawsearchterms(searchterms, descriptions, showwhich)
                     next
                  }
               }
            }
            next
         } else {
            break
         }
      }

      # comma, q, or Escape to exit

      if (.iskey(key, "saveselsearch") || .iskey(key, "quit") || .iskey(key, "escape")) {
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

      if (.iskey(key, "showhelp")) {
         .showhelp.searchterms()
         .drawsearchterms(searchterms, descriptions, showwhich)
         next
      }

      # number = add number to num

      if (is.element(key, 0:9)) {
         newnum <- paste0(num, key, collapse="")
         newnum <- as.integer(newnum)
         if (nchar(newnum) > nchar(length(searchterms))) {
            next
         } else {
            num <- newnum
         }
         next
      }

      # enter = enter selected number

      if (.iskey(key, "enter")) {
         if (keymode=="select" && num==0L)
            break
         if (num < 1L || num > length(searchterms)) {
            num <- 0L
            whichpos <- 1L
            keymode <- "select"
            next
         }
         if (keymode=="select") {
            searchterm <- searchterms[num]
            .drawsearchterms(searchterms, descriptions, showwhich, sel=num)
            Sys.sleep(0.5)
            break
         }
         if (keymode=="remove") {
            searchterms <- searchterms[-num]
            descriptions <- descriptions[-num]
            num <- 0L
            whichpos <- 1L
            keymode <- "select"
            tmp <- .drawsearchterms(searchterms, descriptions, showwhich)
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
               searchtermtomove <- searchterms[posfrom]
               descriptiontomove <- descriptions[posfrom]
               searchterms <- searchterms[-posfrom]
               descriptions <- descriptions[-posfrom]
               searchterms <- append(searchterms, searchtermtomove, after=posto-1)
               descriptions <- append(descriptions, descriptiontomove, after=posto-1)
               .drawsearchterms(searchterms, descriptions, showwhich)
               next
            }
         }
      }

      # backspace = remove last entered number

      if (.iskey(key, "backspace")) {
         if (nchar(num) == 1L) {
            num <- 0L
         } else {
            num <- as.numeric(substr(num, 1L, nchar(num)-1L))
         }
         next
      }

      # s or a = save/add searchterm

      if (.iskey(key, "searchsave") && cursearchterm != "") {
         rect(1.4, 1.5, 8.7, 2.4, col=col.bg, border=NA)
         text(1.8, 2, .text("savesearchterm"), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)
         text(1.8, 1.7, .text("searchterm", cursearchterm2), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)
         answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
         if (identical(answer, "\r") || identical(answer, "ctrl-J") || .confirm(answer)) {
            searchterms <- c(searchterms, cursearchterm)
            eval(expr=switch1)
            description <- readline(prompt=style_bold(.text("description")))
            eval(expr=switch2)
            descriptions <- c(descriptions, description)
            tmp <- .drawsearchterms(searchterms, descriptions, showwhich)
            cex <- tmp$cex
            ypos <- tmp$ypos
            dist <- tmp$dist
            segpos <- tmp$segpos
         }
         next
      }

      # r = go into removal mode

      if (.iskey(key, "searchremove") && length(searchterms) >= 1L) {
         keymode <- "remove"
         num <- 0L
         whichpos <- 1L
         next
      }

      # R = remove all searchterms

      if (.iskey(key, "searchremoveall")) {
         rect(1.4, 1.5, 8.7, 2.4, col=col.bg, border=NA)
         text(1.8, 2, .text("rlydelallsearchterms"), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)
         answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
         if (.confirm(answer)) {
            searchterms <- character(0)
            descriptions <- character(0)
            tmp <- .drawsearchterms(searchterms, descriptions, showwhich)
            cex <- tmp$cex
            ypos <- tmp$ypos
            dist <- tmp$dist
            segpos <- tmp$segpos
         }
         next
      }

      # p = enter into change position mode

      if (.iskey(key, "searchposition") && length(searchterms) >= 2L) {
         keymode <- "position"
         num <- 0L
         whichpos <- 1L
         next
      }

      # t = toggle between showing searchterms and descriptions

      if (.iskey(key, "searchtoggle") && length(searchterms) >= 1L) {
         showwhich <- ifelse(showwhich == "searchterms", "descriptions", "searchterms")
         tmp <- .drawsearchterms(searchterms, descriptions, showwhich)
         cex <- tmp$cex
         ypos <- tmp$ypos
         dist <- tmp$dist
         segpos <- tmp$segpos
         next
      }

      # e = to edit the searchterms/descriptions

      if (.iskey(key, "searchedit")) {
         tmp <- data.frame(searchterm=searchterms, description=descriptions)
         tmp <- edit(tmp)
         searchterms <- tmp$searchterm
         descriptions <- tmp$description
         tmp <- .drawsearchterms(searchterms, descriptions, showwhich)
         cex <- tmp$cex
         ypos <- tmp$ypos
         dist <- tmp$dist
         segpos <- tmp$segpos
         next
      }

   }

   if (length(searchterms) == 0L) {
      try(file.remove(file.path(seqdir[seqdirpos], ".searchterms")), silent=TRUE)
   } else {
      write.table(data.frame(searchterms, descriptions), file=file.path(seqdir[seqdirpos], ".searchterms"), sep="\t", na="", col.names=FALSE, row.names=FALSE, quote=FALSE)
   }

   #.erase(1, 1, 9, 9)

   return(list(searchterm=searchterm, showwhich=showwhich))

}

.drawsearchterms <- function(searchterms, descriptions, showwhich, sel=NULL) {

   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")

   dev.hold()

   .drawbox()

   if (length(searchterms) == 0L) {

      cex <- 1.1
      ypos <- NA_real_
      dist <- NA_real_
      segpos <- NA_real_
      text(1.8, 8, .text("searchterms"), pos=4, offset=0, cex=cex, family=font.mono, font=2, col=col.help)
      segments(1.8, 7.5, 8.2, 7.5, col=col.help)

   } else {

      if (showwhich == "descriptions")
         searchterms <- descriptions

      searchterms2 <- searchterms
      searchtermslen <- nchar(searchterms2)
      maxlen <- 80L
      searchterms2 <- sapply(searchterms2, function(x) if (nchar(x) >= maxlen) paste0("...", substr(x, max(1L,nchar(x)-maxlen), nchar(x))) else x, USE.NAMES=FALSE)

      tab <- data.frame(searchterms2)
      names(tab) <- ""
      txt <- capture.output(print(tab, right=FALSE, print.gap=2))[-1]
      txt <- c(.text("searchterms"), "", txt, "")

      ypos1 <- 8
      ypos2 <- max(2.5, 8-0.75*length(searchterms2))
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

   }

   dev.flush()

   return(list(cex=cex, ypos=ypos, dist=dist, segpos=segpos))

}

.showhelp.searchterms <- function() {

   lang <- .get("lang")

   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")

   if (lang == "en") {

      txt <- c(
      "Help:",
      "<number> - select a query (or click on the query)",
      "s        - save a query",
      "r        - remove a query",
      "R        - remove all queries",
      "p        - change position of a query",
      "t        - toggle between queries and descriptions",
      "e        - edit queries / descriptions",
      "F1       - show this help")

   }

   if (lang == "de") {

      txt <- c(
      "Hilfe:",
      "<Nummer> - Suche ausw\U000000E4hlen (oder auf die Suche klicken)",
      "s        - Suche sichern",
      "r        - Suche entfernen",
      "R        - alle Suchen entfernen",
      "p        - Position von Suche \U000000E4ndern",
      "t        - zwischen Suchen und Beschreibungen wechseln",
      "e        - Suchen / Beschreibungen bearbeiten",
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
