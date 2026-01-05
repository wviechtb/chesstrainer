.boardeditor <- function(pos, flip, sidetoplay, lwd, verbose, switch1, switch2) {

   curfen <- .genfen(pos, flip, sidetoplay, i=1)
   oldfen <- curfen

   pos <- .expandpos(pos)

   .boardeditor.drawboard(pos, flip, sidetoplay, lwd)

   click1.x <- NULL
   click1.y <- NULL
   click2.x <- NULL
   click2.y <- NULL
   empty.square <- TRUE
   button <- 0L

   mousedown <- function(buttons, x, y) {
      squares <- .calcsquarebe(x,y,plt)
      pos.x <- squares[1]
      pos.y <- squares[2]
      if (pos.x >= 11 || pos.x <= 0)
         return(NULL)
      if (pos.y <= 1 || pos.y >= 10)
         return(NULL)
      click1.x <<- pos.x
      click1.y <<- pos.y
      click2.x <<- pos.x
      click2.y <<- pos.y
      button <<- buttons
      if (flip && pos[11-pos.x, 11-pos.y] == "")
         return(NULL)
      if (!flip && pos[pos.x, pos.y] == "")
         return(NULL)
      empty.square <<- FALSE
      if (identical(buttons, 0L))
         .addrect(pos.x, pos.y, col=col.rect, lwd=lwd)
      return(NULL)
   }

   dragmousemove <- function(buttons, x, y) {
      if (!empty.square) {
         squares <- .calcsquarebe(x,y,plt)
         pos.x <- squares[1]
         pos.y <- squares[2]
         pos.x[pos.x < 1] <- 1
         pos.x[pos.x > 10] <- 10
         if (isTRUE(pos.x != click2.x) || isTRUE(pos.y != click2.y)) {
            .boardeditor.rmrect(click2.x, click2.y, lwd=lwd)
            .addrect(pos.x, pos.y, col=col.rect, lwd=lwd)
         }
         click2.x <<- pos.x
         click2.y <<- pos.y
      }
      return(NULL)
   }

   mouseup <- function(buttons, x, y) {
      .boardeditor.rmrect(click2.x, click2.y, lwd=lwd)
      empty.square <<- TRUE
      return(1)
   }

   col      <- .get("col.bot")
   col.bg   <- .get("col.bg")
   col.rect <- .get("col.rect")

   cex <- .get("cex.top") * 0.8

   text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex)

   while (TRUE) {

      plt <- par("plt")

      if (curfen != oldfen) {
         rect(0, 0.2, 12, 0.8, col=col.bg, border=NA)
         text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex)
      }

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=.keyfun)

      keys <- c("q", "\033", "ctrl-[", "n", "f", "s", "c", "r", "o", "F1", "F9", "ctrl-F")

      if (is.character(click) && !is.element(click, keys))
         next

      ######################################################################

      # q or escape to quit the board editor

      if (identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[")) {
         tmp <- pos[2:9,2:9]
         if (sum(tmp == "WK") != 1L || sum(tmp == "BK") != 1L) {
            .texttop(.text("missingkings"), sleep=2, xadj=1, yadj=2)
            next
         }
         ischeck <- c(.isattacked(tmp, xy=c(which(tmp=="WK", arr.ind=TRUE)), attackcolor="b"),
                      .isattacked(tmp, xy=c(which(tmp=="BK", arr.ind=TRUE)), attackcolor="w"))
         if (sum(ischeck) == 2L) {
            .texttop(.text("doublecheck"), sleep=2, xadj=1, yadj=2)
            next
         }
         if ((ischeck[1] && sidetoplay=="b") || (ischeck[2] && sidetoplay=="w")) {
            .texttop(.text("wrongsidecheck"), sleep=2, xadj=1, yadj=2)
            next
         }
         break
      }

      # n to reset the board into the starting position

      if (identical(click, "n")) {
         pos <- .get("boardeditorpos")
         curfen <- .genfen(.shrinkpos(pos), flip, sidetoplay, i=1)
         oldfen <- curfen
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex)
         next
      }

      # f to flip the board

      if (identical(click, "f")) {
         flip <- !flip
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex)
         next
      }

      # s to switch sidetoplay

      if (identical(click, "s")) {
         oldfen <- curfen
         sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
         curfen <- .genfen(.shrinkpos(pos), flip, sidetoplay, i=1)
         .drawsideindicator(sidetoplay, flip=flip, adj=1)
         next
      }

      # c to clear the board

      if (identical(click, "c")) {
         pos[2:9,2:9] <- ""
         curfen <- .genfen(.shrinkpos(pos), flip, sidetoplay, i=1)
         oldfen <- curfen
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex)
         next
      }

      # r to enter rochade availability

      if (identical(click, "r")) {
         eval(expr=switch1)
         resp <- readline(prompt=.text("enterrochade"))
         if (grepl("^K?Q?k?q?$", resp)) {
            oldfen <- curfen
            attr(pos, "rochade") <- c(grepl("K", resp), grepl("Q", resp), grepl("k", resp), grepl("q", resp))
            curfen <- .genfen(.shrinkpos(pos), flip, sidetoplay, i=1)
         } else {
            cat(.text("notcorrectrochade"))
         }
         eval(expr=switch2)
         next
      }

      # o to enter FEN

      if (identical(click, "o")) {
         eval(expr=switch1)
         fen <- readline(prompt=.text("enterfen"))
         eval(expr=switch2)
         if (identical(fen, ""))
            next
         tmp <- .expandfen(fen)
         norochade <- tmp$norochade
         fen <- tmp$fen
         isvalidfen <- .validatefen(fen)
         oldfen <- curfen
         if (isvalidfen) {
            tmp <- .fentopos(fen)
            pos <- tmp$pos
            sidetoplay <- tmp$sidetoplay
            if (norochade) {
               rochade <- c((pos[1,5] == "WK" && pos[1,8] == "WR"), (pos[1,5] == "WK" && pos[1,1] == "WR"),
                            (pos[8,5] == "BK" && pos[8,8] == "BR"), (pos[8,5] == "BK" && pos[8,1] == "BR"))
               attr(pos, "rochade") <- rochade
            }
            curfen <- .genfen(pos, flip, sidetoplay, i=1)
            pos <- .expandpos(pos)
            .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         } else {
            .texttop(.text("notvalidfen"), sleep=2, xadj=1, yadj=2)
         }
         next
      }

      # F1 to show the help

      if (identical(click, "F1")) {
         .showhelp.boardeditor(lwd=lwd)
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         text(6, 0.5, paste("FEN: ", curfen), col=col, cex=cex)
         next
      }

      # F9 to print the FEN and open the position on lichess.org

      if (identical(click, "F9")) {
         eval(expr=switch1)
         fen <- .genfen(.shrinkpos(pos), flip, sidetoplay, i=1)
         cat(fen, "\n")
         eval(expr=switch2)
         clipr::write_clip(fen, object_type="character")
         fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE))
         browseURL(fen)
         next
      }

      # ctrl-f to print and copy the FEN to the clipboard

      if (identical(click, "ctrl-F")) {
         eval(expr=switch1)
         fen <- .genfen(.shrinkpos(pos), flip, sidetoplay, i=1)
         cat(fen, "\n")
         eval(expr=switch2)
         clipr::write_clip(fen, object_type="character")
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
      oldfen <- .genfen(.shrinkpos(oldpos), flip, sidetoplay, i=1)

      pos <- .boardeditor.updateboard(pos, move=c(click1.x, click1.y, click2.x, click2.y), flip=flip, button=button)

      if (!identical(oldpos[2:9,2:9], pos[2:9,2:9])) {
         attr(pos, "ispp") <- NULL
         attr(pos, "y1") <- NULL
      }

      curfen <- .genfen(.shrinkpos(pos), flip, sidetoplay, i=1)

   }

   pos <- .shrinkpos(pos)

   rochade <- attr(pos, "rochade")

   if (is.null(rochade)) {

      # if castling availability is not available, then assume availability based on king and rook positions ([a])

      rochade <- c((pos[1,5] == "WK" && pos[1,8] == "WR"), (pos[1,5] == "WK" && pos[1,1] == "WR"),
                   (pos[8,5] == "BK" && pos[8,8] == "BR"), (pos[8,5] == "BK" && pos[8,1] == "BR"))

   } else {

      # fix any incorrect castling availability values

      if (pos[1,5] != "WK" || pos[1,8] != "WR")
         rochade[1] <- FALSE
      if (pos[1,5] != "WK" || pos[1,1] != "WR")
         rochade[2] <- FALSE
      if (pos[8,5] != "BK" || pos[8,8] != "BR")
         rochade[3] <- FALSE
      if (pos[8,5] != "BK" || pos[8,1] != "BR")
         rochade[4] <- FALSE

   }

   attr(pos, "rochade") <- rochade

   ischeck <- c(.isattacked(pos, xy=c(which(pos=="WK", arr.ind=TRUE)), attackcolor="b"),
                .isattacked(pos, xy=c(which(pos=="BK", arr.ind=TRUE)), attackcolor="w"))
   attr(pos,"ischeck") <- ischeck

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

   attr(pos, "moves50") <- attribs$moves50
   attr(pos, "rochade") <- attribs$rochade
   attr(pos, "ispp") <- attribs$ispp
   attr(pos, "y1") <- attribs$y1

   return(pos)

}

.shrinkpos <- function(pos) {

   attribs <- attributes(pos)

   pos <- pos[2:9,2:9]

   attr(pos,"moves50") <- attribs$moves50
   attr(pos,"rochade") <- attribs$rochade
   attr(pos, "ispp") <- attribs$ispp
   attr(pos, "y1") <- attribs$y1

   if (is.null(attr(pos,"moves50"))) {
      attr(pos,"moves50") <- 0
   } else {
      attr(pos,"moves50") <- as.numeric(attr(pos,"moves50"))
   }

   return(pos)

}

############################################################################

.showhelp.boardeditor <- function(lwd) {

   lang <- .get("lang")

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

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
      "r  - enter castling availability",
      "o  - enter the FEN for a given position",
      "F1 - show this help",
      "F9 - open the current position on lichess.org")

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
      "r  - Rochadem\U000000F6glichkeiten eingeben",
      "o  - die FEN f\U000000FCr eine bestimmte Stellung eingeben",
      "F1 - diese Hilfe anzeigen",
      "F9 - die aktuelle Stellung auf lichess.org \U000000F6ffnen")

   }

   rect(2.2, 2.2, 9.8, 9.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   cex <- .findcex(txt, font=font.mono, x1=1.8, x2=8, y1=4, y2=8, mincex=1.1)
   ypos <- seq(8, 4, length.out=length(txt))

   text(2.5, ypos, txt, pos=4, cex=cex, family=font.mono, font=ifelse(grepl(":", txt), 2, 1), col=col.help)

   .waitforclick()

   #.erase(2, 2, 10, 10)

   return()

}
