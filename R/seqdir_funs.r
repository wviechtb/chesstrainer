.seqdirsettings <- function(seqdir, seqdirpos) {

   col.bg    <- .get("col.bg")
   col.help  <- .get("col.help")
   font.mono <- .get("font.mono")
   switch1   <- .get("switch1")
   switch2   <- .get("switch2")

   num <- 0
   keymode <- "s"
   whichnum <- 1

   tmp <- .drawseqdir(seqdir, seqdirpos)
   cex <- tmp$cex
   ypos <- tmp$ypos
   dist <- tmp$dist

   while (TRUE) {

      rect(1.4, 1.9, 8.7, 2.2, col=col.bg, border=NA)

      if (keymode=="s")
         text(1.8, 2, .text("selseqdir", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="r")
         text(1.8, 2, .text("seqdirtoremove", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="p" && whichnum==1)
         text(1.8, 2, .text("seqdirtomove", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      if (keymode=="p" && whichnum==2)
         text(1.8, 2, .text("seqdirnewpos", ifelse(num==0, "", num)), pos=4, offset=0, cex=cex, family=font.mono, font=1, col=col.help)

      key <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(key)) {
         x <- grconvertX(key[[1]], from="ndc", to="user")
         y <- grconvertY(key[[2]], from="ndc", to="user")
         if (x >= 1.5 && x <= 8) {
            click <- which(y < ypos + dist & y > ypos - dist)
            if (length(click) == 1L) {
               if (keymode=="s") {
                  seqdirpos <- click
                  .drawseqdir(seqdir, seqdirpos)
                  Sys.sleep(1)
                  break
               }
               if (keymode=="r") {
                  seqdir <- seqdir[-click]
                  if (click <= seqdirpos)
                     seqdirpos <- max(1,seqdirpos-1)
                  tmp <- .drawseqdir(seqdir, seqdirpos)
                  cex <- tmp$cex
                  ypos <- tmp$ypos
                  dist <- tmp$dist
                  num <- 0
                  whichnum <- 1
                  keymode <- "s"
               }
               if (keymode=="p") {
                  if (whichnum==1) {
                     posfrom <- click
                     num <- 0
                     whichnum <- 2
                     next
                  }
                  if (whichnum==2) {
                     posto <- click
                     num <- 0
                     whichnum <- 1
                     keymode <- "s"
                     seqdirtomove <- seqdir[posfrom]
                     seqdir <- seqdir[-posfrom]
                     seqdir <- append(seqdir, seqdirtomove, after=posto-1)
                     .drawseqdir(seqdir, seqdirpos)
                     next
                  }
               }
            }
         }
         next
      }

      # Escape or q to exit

      if (identical(key, "F9") || identical(key, "q") || identical(key, "\033") || identical(key, "ctrl-["))
         break

      # F1 to show the help

      if (identical(key, "F1")) {
         .showhelp.seqdir()
         .drawseqdir(seqdir, seqdirpos)
         next
      }

      # number = add number to num

      if (is.element(key, 0:9)) {
         newnum <- paste0(num, key, collapse="")
         newnum <- as.numeric(newnum)
         if (nchar(newnum) > nchar(length(seqdir))) {
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
         if (num < 1 || num > length(seqdir)) {
            num <- 0
            whichnum <- 1
            keymode <- "s"
            next
         }
         if (keymode=="s") {
            seqdirpos <- num
            num <- 0
            whichnum <- 1
            break
         }
         if (keymode=="r") {
            if (length(seqdir) == 1L) {
               .texttop(.text("cannotremovesingleseqdir"), sleep=2)
               .texttop("")
               num <- 0
               whichnum <- 1
               keymode <- "s"
               next
            }
            seqdir <- seqdir[-num]
            if (num <= seqdirpos)
               seqdirpos <- max(1,seqdirpos-1)
            num <- 0
            whichnum <- 1
            keymode <- "s"
            tmp <- .drawseqdir(seqdir, seqdirpos)
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
               seqdirtomove <- seqdir[posfrom]
               seqdir <- seqdir[-posfrom]
               seqdir <- append(seqdir, seqdirtomove, after=posto-1)
               .drawseqdir(seqdir, seqdirpos)
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

      # a or h = add directory

      if (keymode=="s" && identical(key, "a") || identical(key, "h")) {
         if (.Platform$OS.type == "windows") {
            seqdirnew <- choose.dir(caption="")
            if (is.na(seqdirnew))
               next
         } else {
            eval(expr=switch1)
            seqdirnew <- readline(prompt=.text("addseqdir"))
            eval(expr=switch2)
            if (identical(seqdirnew, ""))
               next
            if (!dir.exists(seqdirnew)) {
               .texttop(.text("dirdoesnotexistcreate"))
               answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
               if (identical(answer, "\r") || identical(answer, "ctrl-J") || .confirm(answer)) {
                  succces <- dir.create(seqdirnew, showWarnings=FALSE, recursive=TRUE)
                  if (!succces) {
                     .texttop(.text("dircreateerror"), sleep=2)
                     .texttop("")
                     next
                  }
               }
               .texttop("")
            }
         }
         seqdirnew <- normalizePath(seqdirnew)
         seqdir <- c(seqdir, seqdirnew)
         tmp <- .drawseqdir(seqdir, seqdirpos)
         cex <- tmp$cex
         ypos <- tmp$ypos
         dist <- tmp$dist
         keymode <- "s"
         num <- 0
         whichnum <- 1
         next
      }

      # r or e = go into removal mode

      if (identical(key, "r") || identical(key, "e")) {
         keymode <- "r"
         num <- 0
         whichnum <- 1
         next
      }

      # p = enter into change position mode

      if (identical(key, "p")) {
         keymode <- "p"
         num <- 0
         whichnum <- 1
         next
      }

   }

   #.erase(1, 1, 9, 9)

   return(list(seqdir=seqdir, seqdirpos=seqdirpos))

}

.drawseqdir <- function(seqdir, seqdirpos) {

   col.bg     <- .get("col.bg")
   col.help   <- .get("col.help")
   col.border <- .get("col.border")
   font.mono  <- .get("font.mono")

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.border, lwd=.get("lwd")+3)

   seqdir2 <- seqdir
   seqdirlen <- nchar(seqdir2)
   maxlen <- 80
   seqdir2 <- unname(sapply(seqdir2, function(x) if (nchar(x) >= maxlen) paste0("...", substr(x, max(1,nchar(x)-maxlen), nchar(x))) else x))

   tab <- data.frame(seqdir2)
   names(tab) <- ""
   txt <- capture.output(print(tab, right=FALSE, print.gap=2))[-1]
   txt <- c(.text("directory"), "", txt, "")

   ypos1 <- 8
   ypos2 <- max(2.5, 8-0.75*length(seqdir2))
   ypos <- seq(ypos1, ypos2, length.out=length(txt))

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=ypos1, y2=ypos2, mincex=1.1)

   segments(1.8, ypos[2], 8, ypos[2], col=col.help)
   segments(1.8, ypos[length(ypos)], 8, ypos[length(ypos)], col=col.help)

   bold <- c(2, rep(1,length(txt)-1))
   bold[seqdirpos+2] <- 2
   text(1.8, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=bold, col=col.help)

   dist <- (ypos[1] - ypos[2]) / 2
   ypos <- ypos[3:(length(ypos)-1)]

   return(list(cex=cex, ypos=ypos, dist=dist))

}

.showhelp.seqdir <- function() {

   lang <- .get("lang")

   col.bg     <- .get("col.bg")
   col.help   <- .get("col.help")
   col.border <- .get("col.border")
   font.mono  <- .get("font.mono")

   if (lang == "en") {

      txt <- c(
      "Sequence directory help:",
      "<number> - select a directory (or click on directory)",
      "a        - add a directory",
      "r        - remove a directory",
      "p        - change position of a directory",
      "F1       - show this help")

   }

   if (lang == "de") {

      txt <- c(
      "Sequenzverzeichnis Hilfe:",
      "<Nummer> - Verzeichnis ausw\U000000E4hlen (oder auf das Verzeichnis klicken)",
      "h        - Verzeichnis hinzuf\U000000FCgen",
      "e        - Verzeichnis entfernen",
      "p        - Position von Verzeichnis \U000000E4ndern",
      "F1       - diese Hilfe anzeigen")

   }

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.border, lwd=.get("lwd")+3)

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=4.5, y2=7, mincex=1.1)
   ypos <- seq(7, 4.5, length.out=length(txt))

   text(1.8, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=c(2,rep(1, length(txt)-1)), col=col.help)

   .waitforclick()

}
