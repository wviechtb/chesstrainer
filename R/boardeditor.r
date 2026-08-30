.boardeditor <- function(pos, flip, sidetoplay) {

   attr(pos,"moves50") <- 0

   curfen <- .genfen(pos, flip, sidetoplay, sidetoplay, i=1)
   oldfen <- NULL
   oldrochade <- NULL

   if (is.null(attr(pos,"rochade"))) {
      attr(pos,"rochade") <- c((pos[1,5] == "WK" && pos[1,8] == "WR"), (pos[1,5] == "WK" && pos[1,1] == "WR"),
                               (pos[8,5] == "BK" && pos[8,8] == "BR"), (pos[8,5] == "BK" && pos[8,1] == "BR"))
   }

   pos <- .expandpos(pos)

   .boardeditor.drawboard(pos, flip, sidetoplay)

   click1.x <- NULL
   click1.y <- NULL
   click2.x <- NULL
   click2.y <- NULL
   empty.square <- TRUE
   button <- 0L
   rochade.sel <- ""

   verbose <- .get("verbose")

   mousedown <- function(buttons, x, y) {
      if (identical(buttons, 0L)) {
         xuser <- grconvertX(x, from="ndc", to="user")
         yuser <- grconvertY(y, from="ndc", to="user")
         if (xuser > 10.25 && xuser < 11.25 && yuser > 1.50-0.10 && yuser < 1.50+0.10)
            rochade.sel <<- "Bottom O-O"
         if (xuser > 10.25 && xuser < 11.25 && yuser > 1.25-0.10 && yuser < 1.25+0.10)
            rochade.sel <<- "Bottom O-O-O"
         if (xuser > 10.25 && xuser < 11.25 && yuser > 10.50-0.10 && yuser < 10.50+0.10)
            rochade.sel <<- "Top O-O"
         if (xuser > 10.25 && xuser < 11.25 && yuser > 10.25-0.10 && yuser < 10.25+0.10)
            rochade.sel <<- "Top O-O-O"
         if (!identical(rochade.sel, ""))
            return(NULL)
      }
      squares <- .calcsquarebe(x,y,plt)
      pos.x <- squares[1]
      pos.y <- squares[2]
      #cat("pos.x =", pos.x, "pos.y =", pos.y, "\n")
      if (pos.x >= 11 || pos.x <= 0) # when clicking above or below the row with the pieces, exit
         return(NULL)
      if (pos.y <= 1 || pos.y >= 10) # when clicking left or right of the board, exit
         return(NULL)
      click1.x <<- pos.x
      click1.y <<- pos.y
      click2.x <<- pos.x
      click2.y <<- pos.y
      button <<- buttons[1]
      if (pos[ifelse(flip, 11-pos.x, pos.x), ifelse(flip, 11-pos.y, pos.y)] == "") # when clicking on an empty square, exit
         return(NULL)
      empty.square <<- FALSE
      if (identical(buttons, 0L))
         .addrect(pos.x, pos.y, col=col.rect)
      return(NULL)
   }

   dragmousemove <- function(buttons, x, y) {
      if (!empty.square) {
         squares <- .calcsquarebe(x,y,plt)
         pos.x <- squares[1]
         pos.y <- squares[2]
         #cat("pos.x =", pos.x, "pos.y =", pos.y, "\n")
         pos.x[pos.x < 1] <- 1
         pos.x[pos.x > 10] <- 10
         if (isTRUE(pos.x != click2.x) || isTRUE(pos.y != click2.y)) {
            if (click2.y >= 2 && click2.y <= 9)
               .boardeditor.rmrect(click2.x, click2.y, flip=flip)
            if (pos.y >= 2 && pos.y <= 9)
               .addrect(pos.x, pos.y, col=col.rect)
         }
         click2.x <<- pos.x
         click2.y <<- pos.y
      }
      return(NULL)
   }

   mouseup <- function(buttons, x, y) {
      .boardeditor.rmrect(click2.x, click2.y, flip=flip)
      empty.square <<- TRUE
      return(1)
   }

   col      <- .get("col.bot")
   col.bg   <- .get("col.bg")
   col.rect <- .get("col.rect")
   switch1  <- .get("switch1")
   switch2  <- .get("switch2")

   cex <- .get("cex.top")
   cex.fen <- cex * 0.8

   text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex.fen)

   while (TRUE) {

      plt <- par("plt")

      curfen <- .genfen(.shrinkpos(pos), flip, sidetoplay, sidetoplay, i=1)

      if (!identical(curfen, oldfen)) {
         dev.hold()
         rect(0, 0.2, 12, 0.8, col=col.bg, border=NA)
         text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex.fen)
         dev.flush()
      }

      .boardeditor.rochade(new=attr(pos,"rochade"), old=oldrochade, flip=flip)
      oldrochade <- attr(pos,"rochade")

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=.keyfun)

      if (!identical(rochade.sel, "")) {
         tmp <- .shrinkpos(pos)
         if (rochade.sel == "Bottom O-O") {
            if (flip) {
               if (tmp[8,5] == "BK" && tmp[8,8] == "BR")
                  attr(pos,"rochade")[3] <- !attr(pos,"rochade")[3]
            } else {
               if (tmp[1,5] == "WK" && tmp[1,8] == "WR")
                  attr(pos,"rochade")[1] <- !attr(pos,"rochade")[1]
            }
         }
         if (rochade.sel == "Bottom O-O-O") {
            if (flip) {
               if (tmp[8,5] == "BK" && tmp[8,1] == "BR")
                  attr(pos,"rochade")[4] <- !attr(pos,"rochade")[4]
            } else {
               if (tmp[1,5] == "WK" && tmp[1,1] == "WR")
                  attr(pos,"rochade")[2] <- !attr(pos,"rochade")[2]
            }
         }
         if (rochade.sel == "Top O-O") {
            if (flip) {
               if (tmp[1,5] == "WK" && tmp[1,8] == "WR")
                  attr(pos,"rochade")[1] <- !attr(pos,"rochade")[1]
            } else {
               if (tmp[8,5] == "BK" && tmp[8,8] == "BR")
                  attr(pos,"rochade")[3] <- !attr(pos,"rochade")[3]
            }
         }
         if (rochade.sel == "Top O-O-O") {
            if (flip) {
               if (tmp[1,5] == "WK" && tmp[1,1] == "WR")
                  attr(pos,"rochade")[2] <- !attr(pos,"rochade")[2]
            } else {
               if (tmp[8,5] == "BK" && tmp[8,1] == "BR")
                  attr(pos,"rochade")[4] <- !attr(pos,"rochade")[4]
            }
         }
         rochade.sel <- ""
         if (!identical(oldrochade, attr(pos,"rochade")))
             oldfen <- NULL
         next
      }

      keys <- c("q", "\033", "ctrl-[", "n", "f", "s", "c", "e", "o", "F1", "ctrl-F", "ctrl-P")

      if (is.character(click) && !is.element(click, keys))
         next

      ######################################################################

      # q or escape to quit the board editor

      if (identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[")) {
         tmp <- pos[2:9,2:9]
         if (sum(tmp == "WK") != 1L || sum(tmp == "BK") != 1L) {
            .texttop(.text("kingswrong"), sleep=2, xadj=1, yadj=2, showlast=FALSE)
            next
         }
         ischeck <- c(.isattacked(tmp, xy=c(which(tmp=="WK", arr.ind=TRUE)), attackcolor="b"),
                      .isattacked(tmp, xy=c(which(tmp=="BK", arr.ind=TRUE)), attackcolor="w"))
         if (sum(ischeck) == 2L) {
            .texttop(.text("doublecheck"), sleep=2, xadj=1, yadj=2, showlast=FALSE)
            next
         }
         if ((ischeck[1] && sidetoplay=="b") || (ischeck[2] && sidetoplay=="w")) {
            .texttop(.text("wrongsidecheck"), sleep=2, xadj=1, yadj=2, showlast=FALSE)
            next
         }
         if (any(tmp[c(1,8),] == "WP") || any(tmp[c(1,8),] == "BP")) {
            .texttop(.text("pawns18"), sleep=2, xadj=1, yadj=2, showlast=FALSE)
            next
         }
         break
      }

      # n to reset the board into the starting position

      if (identical(click, "n")) {
         pos <- .get("boardeditorpos")
         sidetoplay <- "w"
         oldfen <- NULL
         oldrochade <- NULL
         .boardeditor.drawboard(pos, flip, sidetoplay)
         next
      }

      # f to flip the board

      if (identical(click, "f")) {
         flip <- !flip
         oldfen <- NULL
         oldrochade <- NULL
         .boardeditor.drawboard(pos, flip, sidetoplay)
         next
      }

      # s to switch sidetoplay

      if (identical(click, "s")) {
         oldfen <- NULL
         sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
         .drawsideindicator(sidetoplay, flip=flip, adj=1)
         next
      }

      # c to clear the board

      if (identical(click, "c")) {
         pos[2:9,2:9] <- ""
         attr(pos,"rochade") <- rep(FALSE, 4)
         attr(pos,"ispp") <- NULL
         attr(pos,"y1") <- NULL
         oldfen <- NULL
         oldrochade <- NULL
         .boardeditor.drawboard(pos, flip, sidetoplay)
         next
      }

      # e to enter FEN

      if (identical(click, "e")) {
         eval(expr=switch1)
         fen <- readline(prompt=.text("enterfen"))
         eval(expr=switch2)
         if (identical(fen, ""))
            next
         tmp <- .expandfen(fen)
         norochade <- tmp$norochade
         fen <- tmp$fen
         isvalidfen <- .validatefen(fen)
         if (isvalidfen) {
            tmp <- .fentopos(fen)
            pos <- tmp$pos
            sidetoplay <- tmp$sidetoplay
            if (norochade) {
               attr(pos,"rochade") <- c((pos[1,5] == "WK" && pos[1,8] == "WR"), (pos[1,5] == "WK" && pos[1,1] == "WR"),
                                        (pos[8,5] == "BK" && pos[8,8] == "BR"), (pos[8,5] == "BK" && pos[8,1] == "BR"))
            } else {
               if (pos[1,5] != "WK" || pos[1,8] != "WR")
                  attr(pos,"rochade")[1] <- FALSE
               if (pos[1,5] != "WK" || pos[1,1] != "WR")
                  attr(pos,"rochade")[2] <- FALSE
               if (pos[8,5] != "BK" || pos[8,8] != "BR")
                  attr(pos,"rochade")[3] <- FALSE
               if (pos[8,5] != "BK" || pos[8,1] != "BR")
                  attr(pos,"rochade")[4] <- FALSE
            }
            pos <- .expandpos(pos)
            oldfen <- NULL
            oldrochade <- NULL
            .boardeditor.drawboard(pos, flip, sidetoplay)
         } else {
            .texttop(.text("notvalidfen"), sleep=2, xadj=1, yadj=2, showlast=FALSE)
         }
         next
      }

      # o to open the position on lichess.org

      if (identical(click, "o")) {
         eval(expr=switch1)
         fen <- .genfen(.shrinkpos(pos), flip, sidetoplay, sidetoplay, i=1)
         cat(fen, "\n")
         eval(expr=switch2)
         if (flip) {
            fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE), "?color=black")
         } else {
            fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE), "?color=white")
         }
         browseURL(fen)
         next
      }

      # F1 to show the help

      if (identical(click, "F1")) {
         .showhelp.boardeditor()
         oldfen <- NULL
         oldrochade <- NULL
         .boardeditor.drawboard(pos, flip, sidetoplay)
         next
      }

      # ctrl-f to print and copy the FEN to the clipboard

      if (identical(click, "ctrl-F")) {
         #eval(expr=switch1)
         cat(curfen, "\n")
         #eval(expr=switch2)
         clipr::write_clip(curfen, object_type="character")
         next
      }

      # ctrl-p to print 'pos'

      if (identical(click, "ctrl-P")) {
         #eval(expr=switch1)
         print(pos)
         #eval(expr=switch2)
         next
      }

      ######################################################################

      # if click is an actual click (and drag) on the board

      if (verbose) {
         cat("Click 1: ", click1.x, ", ", click1.y, sep="")
         cat("\n")
         cat("Click 2: ", click2.x, ", ", click2.y, sep="")
         cat("\n")
      }

      # when clicking too fast, click may not be registered, so start over

      if (is.null(click1.x) || is.null(click2.x) || is.null(click1.y) || is.null(click2.y))
         next

      if (is.na(click1.x) || is.na(click2.x) || is.na(click1.y) || is.na(click2.y))
         next

      # don't do anything when dropping a piece on the 1st or 10th row

      if (click2.x <= 1 || click2.x >= 10)
         next

      oldpos <- pos
      oldfen <- curfen

      pos <- .boardeditor.updateboard(pos, move=c(click1.x, click1.y, click2.x, click2.y), flip=flip, button=button)

      click1.x <- NULL
      click1.y <- NULL
      click2.x <- NULL
      click2.y <- NULL

      if (!identical(oldpos[2:9,2:9], pos[2:9,2:9])) {
         attr(pos,"ispp") <- NULL
         attr(pos,"y1") <- NULL
      }

      tmp <- .shrinkpos(pos)

      if (tmp[1,5] != "WK" || tmp[1,8] != "WR")
         attr(pos,"rochade")[1] <- FALSE
      if (tmp[1,5] != "WK" || tmp[1,1] != "WR")
         attr(pos,"rochade")[2] <- FALSE
      if (tmp[8,5] != "BK" || tmp[8,8] != "BR")
         attr(pos,"rochade")[3] <- FALSE
      if (tmp[8,5] != "BK" || tmp[8,1] != "BR")
         attr(pos,"rochade")[4] <- FALSE

      curfen <- .genfen(.shrinkpos(pos), flip, sidetoplay, sidetoplay, i=1)

      .drawsideindicator(sidetoplay, flip=flip, adj=1, clear=FALSE)

   }

   pos <- .shrinkpos(pos)

   if (is.null(attr(pos,"rochade"))) {

      # if the rochade attribute is NULL, then assume availability based on king and rook positions

      attr(pos,"rochade") <- c((pos[1,5] == "WK" && pos[1,8] == "WR"), (pos[1,5] == "WK" && pos[1,1] == "WR"),
                               (pos[8,5] == "BK" && pos[8,8] == "BR"), (pos[8,5] == "BK" && pos[8,1] == "BR"))

   } else {

      # fix any incorrect castling availability values

      if (pos[1,5] != "WK" || pos[1,8] != "WR")
         attr(pos,"rochade")[1] <- FALSE
      if (pos[1,5] != "WK" || pos[1,1] != "WR")
         attr(pos,"rochade")[2] <- FALSE
      if (pos[8,5] != "BK" || pos[8,8] != "BR")
         attr(pos,"rochade")[3] <- FALSE
      if (pos[8,5] != "BK" || pos[8,1] != "BR")
         attr(pos,"rochade")[4] <- FALSE

   }

   attr(pos,"ischeck") <- c(.isattacked(pos, xy=c(which(pos=="WK", arr.ind=TRUE)), attackcolor="b"),
                            .isattacked(pos, xy=c(which(pos=="BK", arr.ind=TRUE)), attackcolor="w"))

   return(list(pos=pos, flip=flip, sidetoplay=sidetoplay))

}

############################################################################

.expandpos <- function(pos) {

   attribs <- attributes(pos)

   pos <- rbind("", pos, "")
   pos <- cbind("", pos, "")
   rownames(pos) <- 0:9
   colnames(pos) <- 0:9
   pos[1,3:8]  <- c("WK","WQ","WR","WB","WN","WP")
   pos[10,3:8] <- c("BK","BQ","BR","BB","BN","BP")

   attr(pos,"moves50") <- attribs$moves50
   attr(pos,"rochade") <- attribs$rochade
   attr(pos,"ispp")    <- attribs$ispp
   attr(pos,"y1")      <- attribs$y1

   return(pos)

}

.shrinkpos <- function(pos) {

   attribs <- attributes(pos)

   pos <- pos[2:9,2:9]

   attr(pos,"moves50") <- attribs$moves50
   attr(pos,"rochade") <- attribs$rochade
   attr(pos,"ispp")    <- attribs$ispp
   attr(pos,"y1")      <- attribs$y1

   if (is.null(attr(pos,"moves50"))) {
      attr(pos,"moves50") <- 0
   } else {
      attr(pos,"moves50") <- as.numeric(attr(pos,"moves50"))
   }

   return(pos)

}

############################################################################

.showhelp.boardeditor <- function() {

   lang <- .get("lang")

   col.bg     <- .get("col.bg")
   col.help   <- .get("col.help")
   col.border <- .get("col.border")
   font.mono  <- .get("font.mono")

   if (lang == "en") {

      txt <- c(
      "Help for the board editor:",
      "",
      "left-click and drag  - move a piece",
      "right-click and drag - copy a piece",
      "right-click          - delete the piece",
      "",
      "f  - flip the board",
      "s  - set which side plays the first move",
      "n  - reset the board into the starting position",
      "c  - clear the board",
      "e  - enter the FEN for a given position",
      "o  - open the current position on lichess.org",
      "F1 - show this help")

   }

   if (lang == "de") {

      txt <- c(
      "Hilfe f\U000000FCr den Bretteditor:",
      "",
      "Linksklick und ziehen  - Figur bewegen",
      "Rechtsklick und ziehen - Figur kopieren",
      "Rechtsklick            - Figur l\U000000F6schen",
      "",
      "f  - Brett umdrehen",
      "s  - festlegen, welche Seite den ersten Zug macht",
      "n  - Brett in die Ausgangsposition zur\U000000FCcksetzen",
      "c  - Brett leer r\U000000E4umen",
      "e  - die FEN f\U000000FCr eine bestimmte Stellung eingeben",
      "o  - die aktuelle Stellung auf lichess.org \U000000F6ffnen",
      "F1 - diese Hilfe anzeigen")

   }

   .drawbox(2.18, 2.18, 9.82, 9.82)

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=4, y2=8, mincex=1.1)
   ypos <- seq(8, 4, length.out=length(txt))

   text(2.5, ypos, txt, pos=4, cex=cex, family=font.mono, font=ifelse(grepl(":", txt), 2, 1), col=col.help)

   .waitforclick()

   #.erase(2, 2, 10, 10)

   return()

}
