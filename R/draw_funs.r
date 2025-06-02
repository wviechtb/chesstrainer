.drawsquare <- function(x, y, col=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")))
   rect(y, x, y+1, x+1, col=col, border=NA)

.drawpiece <- function(x, y, piece) {
   if (piece != "") {
      txt <- paste0("rasterImage(img.", piece, ",", y, ",", x, ",", y+1, ",", x+1, ")")
      eval(parse(text=txt), envir=.chesstrainer)
   }
}

.drawboard <- function(pos, flip=FALSE, inhibit=FALSE) {

   col.bg <- .get("col.bg")
   col.fg <- .get("col.fg")
   col.square.l <- .get("col.square.l")
   col.square.d <- .get("col.square.d")

   if (dev.cur() == 1L) {
      dev.new(bg=col.bg, title="chesstrainer")
      if (inhibit) {
         Sys.sleep(0.2)
         dev.control(displaylist="inhibit")
      }
   }

   par(xpd=NA, pty="s", mar=rep(5.2,4), fg=col.fg, bg=col.bg)

   mat <- outer(1:8, 1:8, function(x,y) .is.even(x+y))

   image(1:8+0.5, 1:8+0.5, mat, col=c(col.square.l, col.square.d), xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)

   if (flip) {
      par(mgp=c(3,0.5,0))
      axis(side=1, 1:8+0.5, rev(LETTERS[1:8]), las=1, tick=FALSE, col.axis=col.fg)
      par(mgp=c(3,0.8,0))
      axis(side=2, 1:8+0.5, rev(1:8),          las=1, tick=FALSE, col.axis=col.fg)
   } else {
      par(mgp=c(3,0.5,0))
      axis(side=1, 1:8+0.5, LETTERS[1:8],      las=1, tick=FALSE, col.axis=col.fg)
      par(mgp=c(3,0.8,0))
      axis(side=2, 1:8+0.5, 1:8,               las=1, tick=FALSE, col.axis=col.fg)
   }

   if (flip) {
      for (i in 1:8) {
         for (j in 1:8) {
            .drawpiece(i, j, pos[9-i,9-j])
         }
      }
   } else {
      for (i in 1:8) {
         for (j in 1:8) {
            .drawpiece(i, j, pos[i,j])
         }
      }
   }

}

.redrawpos <- function(pos, posold, flip=FALSE) {

   if (missing(posold)) {

      mat <- outer(1:8, 1:8, function(x,y) .is.even(x+y))
      par(new=TRUE)
      image(1:8+0.5, 1:8+0.5, mat, col=c(.get("col.square.l"), .get("col.square.d")), xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)
      par(new=FALSE)

      if (flip) {
         for (i in 1:8) {
            for (j in 1:8) {
               #.drawsquare(i, j)
               .drawpiece(i, j, pos[9-i,9-j])
            }
         }
      } else {
         for (i in 1:8) {
            for (j in 1:8) {
               #.drawsquare(i, j)
               .drawpiece(i, j, pos[i,j])
            }
         }
      }

   } else {

      if (flip) {
         for (i in 1:8) {
            for (j in 1:8) {
               if (pos[9-i,9-j] != posold[9-i,9-j]) {
                  .drawsquare(i, j)
                  .drawpiece(i, j, pos[9-i,9-j])
               }
            }
         }
      } else {
         for (i in 1:8) {
            for (j in 1:8) {
               if (pos[i,j] != posold[i,j]) {
                  .drawsquare(i, j)
                  .drawpiece(i, j, pos[i,j])
               }
            }
         }
      }

   }

}

.redrawall <- function(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove) {

   .drawboard(pos, flip)
   .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
   .texttop(texttop)
   .drawcheck(pos, flip=flip)
   if (mode == "test" && timed) {
      .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
   } else {
      .drawsideindicator(sidetoplay, flip)
   }

   return()

}

.boardeditor.drawboard <- function(pos, flip=FALSE, sidetoplay, lwd) {

   col.bg <- .get("col.bg")
   col.fg <- .get("col.fg")
   col.square.l <- .get("col.square.l")
   col.square.d <- .get("col.square.d")
   col.square.be <- .get("col.square.be")

   par(xpd=NA, pty="s", mar=rep(2.2,4), fg=col.fg, bg=col.bg)

   mat <- matrix(1, nrow=10, ncol=10)
   mat[2:9,2:9] <- outer(1:8, 1:8, function(x,y) ifelse(.is.even(x+y), 2, 3))

   image(1:10+0.5, 1:10+0.5, mat, col=c(col.bg, col.square.d, col.square.l), xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)

   for (j in 3:8) {
      .drawsquare(1, j, col=col.square.be)
      .addrect(1, j, offset=0.028, col.bg, lwd+2)
      .drawsquare(10, j, col=col.square.be)
      .addrect(10, j, offset=0.028, col.bg, lwd+2)
   }

   if (flip) {
      for (i in 1:10) {
         for (j in 2:9) {
            .drawpiece(i, j, pos[11-i,11-j])
         }
      }
   } else {
      for (i in 1:10) {
         for (j in 2:9) {
            .drawpiece(i, j, pos[i,j])
         }
      }
   }

   .drawsideindicator(sidetoplay, flip, adj=1)

}

.updateboard <- function(pos, move, flip, autoprom, volume, verbose, draw=TRUE) {

   x1 <- unname(move[[1]])
   y1 <- unname(move[[2]])
   x2 <- unname(move[[3]])
   y2 <- unname(move[[4]])

   ispp <- ""
   pawnmove <- FALSE

   rochade <- attr(pos,"rochade")

   if (is.null(rochade))
      rochade <- rep(TRUE, 4)

   if (draw)
      .rmcheck(pos, flip=flip)

   # determine which piece is being moved

   if (flip) {
      piece <- pos[9-x1,9-y1]
   } else {
      piece <- pos[x1,y1]
   }

   # check for a two-square move of a pawn from its original position (since this may enable en passant)

   if (flip) {
      if (x1 == 2 && x2 == 4 && piece == "BP")
         ispp <- "b"
      if (x1 == 7 && x2 == 5 && piece == "WP")
         ispp <- "w"
   } else {
      if (x1 == 2 && x2 == 4 && piece == "WP")
         ispp <- "w"
      if (x1 == 7 && x2 == 5 && piece == "BP")
         ispp <- "b"
   }

   # check if a pawn move was made (since this resets the 50 move counter)

   if (piece %in% c("WP","BP"))
      pawnmove <- TRUE

   # check for rochade and en passant

   isrochade <- ""
   isenpassant <- ""

   if (flip) {

      # check for rochade

      if (identical(c(x1,y1), c(8,4)) && piece == "WK" && identical(c(x2,y2), c(8,2))) {
         isrochade <- "0-0"
         pos[1,6] <- "WR"
         pos[1,8] <- ""
         if (draw) {
            .drawsquare(8, 1)
            .drawsquare(8, 3)
            .drawpiece(8, 3, "WR")
         }
      }
      if (identical(c(x1,y1), c(8,4)) && piece == "WK" && identical(c(x2,y2), c(8,6))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         if (draw) {
            .drawsquare(8, 8)
            .drawsquare(8, 5)
            .drawpiece(8, 5, "WR")
         }
      }
      if (identical(c(x1,y1), c(1,4)) && piece == "BK" && identical(c(x2,y2), c(1,2))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         if (draw) {
            .drawsquare(1, 1)
            .drawsquare(1, 3)
            .drawpiece(1, 3, "BR")
         }
      }
      if (identical(c(x1,y1), c(1,4)) && piece == "BK" && identical(c(x2,y2), c(1,6))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         if (draw) {
            .drawsquare(1, 8)
            .drawsquare(1, 5)
            .drawpiece(1, 5, "BR")
         }
      }

      # check for en passant

      if (identical(attr(pos,"ispp"), "b") && piece == "WP" && x1 == 4 && attr(pos,"y1") == y2) {
         isenpassant <- "w"
         pos[5,9-y2] <- ""
         if (draw) .drawsquare(4, y2)
      }
      if (identical(attr(pos,"ispp"), "w") && piece == "BP" && x1 == 5 && attr(pos,"y1") == y2) {
         isenpassant <- "b"
         pos[4,9-y2] <- ""
         if (draw) .drawsquare(5, y2)
      }

   } else {

      # check for rochade

      if (identical(c(x1,y1), c(1,5)) && piece == "WK" && identical(c(x2,y2), c(1,7))) {
         isrochade <- "0-0"
         pos[1,6] <- "WR"
         pos[1,8] <- ""
         if (draw) {
            .drawsquare(1, 8)
            .drawsquare(1, 6)
            .drawpiece(1, 6, "WR")
         }
      }
      if (identical(c(x1,y1), c(1,5)) && piece == "WK" && identical(c(x2,y2), c(1,3))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         if (draw) {
            .drawsquare(1, 1)
            .drawsquare(1, 4)
            .drawpiece(1, 4, "WR")
         }
      }
      if (identical(c(x1,y1), c(8,5)) && piece == "BK" && identical(c(x2,y2), c(8,7))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         if (draw) {
            .drawsquare(8, 8)
            .drawsquare(8, 6)
            .drawpiece(8, 6, "BR")
         }
      }
      if (identical(c(x1,y1), c(8,5)) && piece == "BK" && identical(c(x2,y2), c(8,3))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         if (draw) {
            .drawsquare(8, 1)
            .drawsquare(8, 4)
            .drawpiece(8, 4, "BR")
         }
      }

      # check for en passant

      if (identical(attr(pos,"ispp"), "b") && piece == "WP" && x1 == 5 && attr(pos,"y1") == y2) {
         isenpassant <- "w"
         pos[5,y2] <- ""
         if (draw) .drawsquare(5, y2)
      }
      if (identical(attr(pos,"ispp"), "w") && piece == "BP" && x1 == 4 && attr(pos,"y1") == y2) {
         isenpassant <- "b"
         pos[4,y2] <- ""
         if (draw) .drawsquare(4, y2)
      }

   }

   # check for pawn promotion

   promotionpiece <- ""
   col.square.be <- .get("col.square.be")

   if (flip) {
      if (x1 == 7 && x2 == 8 && piece == "BP") {
         if (autoprom) {
            promotionpiece <- paste0("B", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])
         } else {
            sapply(8:5, function(x2) .drawsquare(x2, y2, col=col.square.be))
            mapply(.drawpiece, x=8:5, y=rep(y2,4), piece=c("BQ","BN","BR","BB"))
            while (TRUE) {
               click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.pickpromotionpiece)
               if (identical(click, c(8,y2)))
                  promotionpiece <- "BQ"
               if (identical(click, c(7,y2)))
                  promotionpiece <- "BN"
               if (identical(click, c(6,y2)))
                  promotionpiece <- "BR"
               if (identical(click, c(5,y2)))
                  promotionpiece <- "BB"
               if (promotionpiece != "")
                  break
            }
            sapply(8:5, function(x2) .drawsquare(x2, y2))
            mapply(.drawpiece, x=8:5, y=rep(y2,4), piece=pos[1:5,9-y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("B", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
      if (x1 == 2 && x2 == 1 && piece == "WP") {
         if (autoprom) {
            promotionpiece <- paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])
         } else {
            sapply(1:4, function(x2) .drawsquare(x2, y2, col=col.square.be))
            mapply(.drawpiece, x=1:4, y=rep(y2,4), piece=c("WQ","WN","WR","WB"))
            while (TRUE) {
               click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.pickpromotionpiece)
               if (identical(click, c(1,y2)))
                  promotionpiece <- "WQ"
               if (identical(click, c(2,y2)))
                  promotionpiece <- "WN"
               if (identical(click, c(3,y2)))
                  promotionpiece <- "WR"
               if (identical(click, c(4,y2)))
                  promotionpiece <- "WB"
               if (promotionpiece != "")
                  break
            }
            sapply(1:4, function(x2) .drawsquare(x2, y2))
            mapply(.drawpiece, x=1:4, y=rep(y2,4), piece=pos[8:4,9-y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
   } else {
      if (x1 == 7 && x2 == 8 && piece == "WP") {
         if (autoprom) {
            promotionpiece <- paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])
         } else {
            sapply(8:5, function(x2) .drawsquare(x2, y2, col=col.square.be))
            mapply(.drawpiece, x=8:5, y=rep(y2,4), piece=c("WQ","WN","WR","WB"))
            while (TRUE) {
               click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.pickpromotionpiece)
               if (identical(click, c(8,y2)))
                  promotionpiece <- "WQ"
               if (identical(click, c(7,y2)))
                  promotionpiece <- "WN"
               if (identical(click, c(6,y2)))
                  promotionpiece <- "WR"
               if (identical(click, c(5,y2)))
                  promotionpiece <- "WB"
               if (promotionpiece != "")
                  break
            }
            sapply(8:5, function(x2) .drawsquare(x2, y2))
            mapply(.drawpiece, x=8:5, y=rep(y2,4), piece=pos[8:5,y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
      if (x1 == 2 && x2 == 1 && piece == "BP") {
         if (autoprom) {
            promotionpiece <- paste0("B", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])
         } else {
            sapply(1:4, function(x2) .drawsquare(x2, y2, col=col.square.be))
            mapply(.drawpiece, x=1:4, y=rep(y2,4), piece=c("BQ","BN","BR","BB"))
            while (TRUE) {
               click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.pickpromotionpiece)
               if (identical(click, c(1,y2)))
                  promotionpiece <- "BQ"
               if (identical(click, c(2,y2)))
                  promotionpiece <- "BN"
               if (identical(click, c(3,y2)))
                  promotionpiece <- "BR"
               if (identical(click, c(4,y2)))
                  promotionpiece <- "BB"
               if (promotionpiece != "")
                  break
            }
            sapply(1:4, function(x2) .drawsquare(x2, y2))
            mapply(.drawpiece, x=1:4, y=rep(y2,4), piece=pos[1:4,y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("B", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
   }

   if (draw) {
      .drawsquare(x1, y1)
      .drawsquare(x2, y2)
   }

   if (flip) {

      if (draw) .drawpiece(x2, y2, piece)

      iscapture <- pos[9-x2,9-y2] != "" || isenpassant != ""

      if (iscapture) {
         playsound(system.file("sounds", "capture.ogg", package="chesstrainer"), volume=volume)
      } else {
         playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
      }

      pos[9-x2,9-y2] <- piece
      pos[9-x1,9-y1] <- ""

      if (promotionpiece != "") {
         pos[9-x2,9-y2] <- promotionpiece
         if (draw) {
            .drawsquare(x2, y2)
            .drawpiece(x2, y2, pos[9-x2,9-y2])
         }
      }

   } else {

      if (draw) .drawpiece(x2, y2, piece)

      iscapture <- pos[x2,y2] != "" || isenpassant != ""

      if (iscapture) {
         playsound(system.file("sounds", "capture.ogg", package="chesstrainer"), volume=volume)
      } else {
         playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
      }

      pos[x2,y2] <- piece
      pos[x1,y1] <- ""

      if (promotionpiece != "") {
         pos[x2,y2] <- promotionpiece
         if (draw) {
            .drawsquare(x2, y2)
            .drawpiece(x2, y2, pos[x2,y2])
         }
      }

   }

   # check which kings are in check

   ischeck <- c(.isattacked(pos, xy=c(which(pos=="WK", arr.ind=TRUE)), attackcolor="b"),
                .isattacked(pos, xy=c(which(pos=="BK", arr.ind=TRUE)), attackcolor="w"))

   piecename <- substr(piece, 2, 2)
   piecename <- ifelse(piecename == "P", "", piecename)

   if (flip) {
      if (identical(isrochade, "")) {
         move <- paste0(piecename, letters[9-y1], 9-x1, ifelse(iscapture, "x", "-"), letters[9-y2], 9-x2, ifelse(promotionpiece != "", paste0("=", substr(promotionpiece,2,2)), ""), ifelse(any(ischeck), "+", ""))
      } else {
         move <- paste0(isrochade, ifelse(any(ischeck), "+", ""))
      }
   } else {
      if (identical(isrochade, "")) {
         move <- paste0(piecename, letters[y1], x1, ifelse(iscapture, "x", "-"), letters[y2], x2, ifelse(promotionpiece != "", paste0("=", substr(promotionpiece,2,2)), ""), ifelse(any(ischeck), "+", ""))
      } else {
         move <- paste0(isrochade, ifelse(any(ischeck), "+", ""))
      }
   }

   if (pos[1,5] != "WK")
      rochade[1:2] <- FALSE
   if (pos[8,5] != "BK")
      rochade[3:4] <- FALSE
   if (pos[1,8] != "WR")
      rochade[1] <- FALSE
   if (pos[1,1] != "WR")
      rochade[2] <- FALSE
   if (pos[8,8] != "BR")
      rochade[3] <- FALSE
   if (pos[8,1] != "BR")
      rochade[4] <- FALSE

   attr(pos,"move") <- move
   attr(pos,"ispp") <- ispp
   attr(pos,"y1") <- y1
   attr(pos,"rochade") <- rochade
   attr(pos,"ischeck") <- ischeck

   if (draw)
      .drawcheck(pos, flip=flip)

   if (pawnmove || iscapture) {
      attr(pos,"moves50") <- 0
   } else {
      attr(pos,"moves50") <- attr(pos,"moves50") + 1
   }

   if (verbose) {
      cat("Move: ", move, "\n\n", sep="")
      printpos <- pos
      printpos[printpos == ""] <- "\U00000B7"
      printpos[printpos == "WP"] <- "\U000265F"
      printpos[printpos == "WR"] <- "\U000265C"
      printpos[printpos == "WN"] <- "\U000265E"
      printpos[printpos == "WB"] <- "\U000265D"
      printpos[printpos == "WK"] <- "\U000265A"
      printpos[printpos == "WQ"] <- "\U000265B"
      printpos[printpos == "BP"] <- "\U0002659"
      printpos[printpos == "BR"] <- "\U0002656"
      printpos[printpos == "BN"] <- "\U0002658"
      printpos[printpos == "BB"] <- "\U0002657"
      printpos[printpos == "BK"] <- "\U0002654"
      printpos[printpos == "BQ"] <- "\U0002655"
      if (flip) {
         print(printpos[,8:1], quote=FALSE)
      } else {
         print(printpos[8:1,], quote=FALSE)
      }
      attribs <- attributes(pos)
      attribs$dim <- NULL
      cat("\n")
      print(attribs)
   }

   return(pos)

}

.boardeditor.updateboard <- function(pos, move, flip, button) {

   x1 <- unname(move[[1]])
   y1 <- unname(move[[2]])
   x2 <- unname(move[[3]])
   y2 <- unname(move[[4]])

   if (button == 2L && x1 == x2 && y1 == y2 && x1 > 1 && x1 < 10 && y1 > 1 && y1 < 10) {

      .drawsquare(x1, y1)

      if (flip) {
         pos[11-x1,11-y1] <- ""
      } else {
         pos[x1,y1] <- ""
      }

   } else {

      if (button == 0L && x1 > 1 && x1 < 10 && y1 > 1 && y1 < 10)
         .drawsquare(x1, y1)
      if (button == 0L && x2 > 1 && x2 < 10 && y2 > 1 && y2 < 10)
         .drawsquare(x2, y2)

      if (flip) {
         if (x2 > 1 && x2 < 10 && y2 > 1 && y2 < 10) {
            .drawpiece(x2, y2, pos[11-x1,11-y1])
            pos[11-x2,11-y2] <- pos[11-x1,11-y1]
         }
         if (button == 0L && x1 > 1 && x1 < 10)
            pos[11-x1,11-y1] <- ""
      } else {
         if (x2 > 1 && x2 < 10 && y2 > 1 && y2 < 10) {
            .drawpiece(x2, y2, pos[x1,y1])
            pos[x2,y2] <- pos[x1,y1]
         }
         if (button == 0L && x1 > 1 && x1 < 10)
            pos[x1,y1] <- ""
      }

   }

   return(pos)

}

.addrect <- function(x, y, offset=0.028, col, lwd)
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd, border=col, ljoin=1)

.rmrect <- function(x, y, offset=0.028, lwd)
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), ljoin=1)

.boardeditor.rmrect <- function(x, y, offset=0.028, lwd) {
   if (is.na(x) || is.na(y))
      return()
   if (x <= 1 || x >= 10 || y <= 1 || y >= 10) {
      rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=.get("col.bg"), ljoin=1)
   } else {
      rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), ljoin=1)
   }
}

.drawcircle <- function(x, y, lwd)
   symbols(y+0.5, x+0.5, circles=0.45, inches=FALSE, lwd=lwd+2, fg=.get("col.annot"), add=TRUE)

.drawcircles <- function(circles, lwd) {
   if (nrow(circles) == 0L)
      return()
   apply(circles, 1, function(x) .drawcircle(x[1], x[2], lwd=lwd))
}

.drawcheck <- function(pos, flip) {

   ischeck <- attr(pos, "ischeck")

   if (sum(ischeck) == 0L)
      return()

   color <- c("w","b")[ischeck]
   piece <- paste0(toupper(color), "K", collapse="")

   xy <- c(which(pos==piece, arr.ind=TRUE))

   if (flip)
      xy <- 9-xy

   n <- 64
   radius <- 0.5
   x.cent <- xy[2] + 0.5
   y.cent <- xy[1] + 0.5

   xs <- seq(0, 1, length.out=n)

   z <- outer(xs, xs, function(x,y) {
      dx <- x - 0.5
      dy <- y - 0.5
      dist <- sqrt(dx^2 + dy^2)
      alpha <- ifelse(dist <= 0.5, 1 - (dist / 0.5), 0)
      return(alpha)
   })

   z <- z * 1.5
   z[z > 1] <- 1

   img <- array(0, dim=c(n,n,4))
   img[,,1] <- 1
   img[,,4] <- z

   xl <- x.cent - radius
   xr <- x.cent + radius
   yb <- y.cent - radius
   yt <- y.cent + radius

   rasterImage(img, xl, yb, xr, yt)

   .drawpiece(xy[1], xy[2], piece)

   return()

}

.rmcheck <- function(pos, flip) {

   ischeck <- attr(pos, "ischeck")

   if (sum(ischeck) == 0L)
      return()

   color <- c("w","b")[ischeck]
   piece <- paste0(toupper(color), "K", collapse="")

   xy <- c(which(pos==piece, arr.ind=TRUE))

   if (flip)
      xy <- 9-xy

   x <- xy[1]
   y <- xy[2]

   .drawsquare(x, y, col=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")))
   .drawpiece(x, y, piece)

}

.drawarrow <- function(y1, x1, y2, x2, lwd, col=.get("col.annot")) {

   x1 <- x1 + 0.5
   y1 <- y1 + 0.5
   x2 <- x2 + 0.5
   y2 <- y2 + 0.5

   length <- min(max(0.25, lwd*4*0.015), 0.45)
   width  <- min(max(0.15, lwd*4*0.010), 0.40)

   slp <- (y2-y1) / (x2-x1)

   if (is.infinite(slp)) {
      x3 <- x1
      y3 <- y2 + ifelse(y1 > y2, 1, -1) * length
   } else {
      int <- y1 - slp*x1
      alen <- y2-y1
      clen <- x2-x1
      blen <- sqrt(alen^2+clen^2)
      if (x2 > x1) {
         x3 <- x1 + (blen-length)/sqrt(1+slp^2)
         y3 <- int + slp*x3
      } else {
         x3 <- x1 - (blen-length)/sqrt(1+slp^2)
         y3 <- int + slp*x3
      }
   }

   if (is.infinite(slp)) {
      x4 <- x3 + width
      y4 <- y3
      x5 <- x3 - width
      y5 <- y3
   } else {
      x4 <- x3 + slp*width/sqrt(1+slp^2)
      y4 <- y3 - width/sqrt(1+slp^2)
      x5 <- x3 - slp*width/sqrt(1+slp^2)
      y5 <- y3 + width/sqrt(1+slp^2)
   }

   segments(x1, y1, x3, y3, col=col, lwd=lwd*4, lend=1)
   polygon(c(x4,x5,x2),c(y4,y5,y2), col=col, border=NA)

}

.drawarrows <- function(arrows, lwd, hint=FALSE, evalvals, sidetoplay) {
   if (nrow(arrows) == 0L)
      return()
   if (hint) {
      n <- nrow(arrows)
      col.best <- .get("col.best")
      if (n == 1) {
         .drawarrow(arrows[1,1], arrows[1,2], arrows[1,3], arrows[1,4], lwd, col=adjustcolor(col.best, alpha.f=0.6))
      } else {
         if(sidetoplay == "b")
            evalvals <- -1 * evalvals
         diffs <- max(evalvals) - evalvals
         alphas <- exp(-0.5 * diffs) * c(0.65, rep(0.50, n-1))
         alphas[alphas <= 0.1] <- 0
         for (j in 1:nrow(arrows)) {
            .drawarrow(arrows[j,1], arrows[j,2], arrows[j,3], arrows[j,4], lwd, col=adjustcolor(col.best, alpha.f=alphas[j]))
         }
      }
   } else {
      apply(arrows, 1, function(x) .drawarrow(x[1], x[2], x[3], x[4], lwd=lwd))
   }
}

.rmannot <- function(pos, circles, arrows, flip) {

   oldpos <- pos

   if (nrow(circles) >= 1L) {
      if (flip) {
         oldpos[9-circles[,1], 9-circles[,2]] <- "x"
      } else {
         oldpos[circles[,1], circles[,2]] <- "x"
      }
   }

   if (nrow(arrows) >= 1L) {
      if (flip) {
         for (j in 1:nrow(arrows)) {
            oldpos[9-arrows[j,1]:arrows[j,3], 9-arrows[j,2]:arrows[j,4]] <- "x"
         }
      } else {
         for (j in 1:nrow(arrows)) {
            oldpos[arrows[j,1]:arrows[j,3], arrows[j,2]:arrows[j,4]] <- "x"
         }
      }
   }

   .redrawpos(pos, oldpos, flip=flip)

}

.texttop <- function(txt, sleep=0, left=FALSE) {

   if (length(txt) == 0L)
      return()

   xleft   <- 0
   xright  <- 10
   ybottom <- 9.2
   ytop    <- grconvertY(dev.size()[2], from="inches", to="user")-0.2
   xcenter <- (xleft + xright) / 2

   rect(xleft, ybottom-0.2, xright, ytop+0.2, col=.get("col.bg"), border=NA)

   if (!identical(txt, "")) {
      txt <- gsub("\\n", "\n", txt, fixed=TRUE)
      txt <- strsplit(txt, "\n", fixed=TRUE)[[1]]
      max_line_width <- max(strwidth(txt))
      max_line_height <- strheight("A")
      total_text_height <- length(txt) * max_line_height
      cex <- min(.get("cex.top"), (xright-xleft) / max_line_width, (ytop-ybottom) / total_text_height)
      maxwidth <- max(strwidth(txt, cex=cex))
      if (maxwidth > 9.5)
         cex <- cex * 0.95
      ypos <- seq(from = min(ytop,    (ytop - ybottom) / 2 + ybottom + (length(txt) - 1) * 1 * max_line_height * cex),
                  to   = max(ybottom, (ytop - ybottom) / 2 + ybottom - (length(txt) - 1) * 1 * max_line_height * cex),
                  length.out = length(txt))
      if (left) {
         maxwidth <- max(strwidth(txt, cex=cex))
         margin <- max(0, (10 - maxwidth) / 2)
         xcenter <- margin
      }
      col.top <- .get("col.top")
      for (i in seq_along(txt)) {
         text(x=xcenter, y=ypos[i], labels=txt[i], cex=cex, col=col.top, adj=c(ifelse(left,0,0.5),0.5))
      }
   }

   Sys.sleep(sleep)

   txt <- paste(txt, collapse="\n")
   return(txt)

}

.textbot <- function(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode="default", onlyshow=FALSE, onlyi=FALSE, onlyscore=FALSE) {

   lang   <- .get("lang")
   cex    <- .get("cex.bot")
   font   <- .get("font.mono")
   col    <- .get("col.bot")
   col.bg <- .get("col.bg")

   redraw <- !any(onlyi, onlyshow, onlyscore)

   if (redraw)
      rect(-2, -1, 12, 0.6, col=col.bg, border=NA)

   if (lang == "en") {

      if (mode == "add") {
         if (onlyshow) {
            text(0, 0.30, paste0("      ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.30, paste0("Show: ", ifelse(show, "Yes", "No")), pos=4, cex=cex, family=font, col=col)
         }
         if (onlyi) {
            text(0, 0.15, paste0("      ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.15, paste0("      ", i), pos=4, cex=cex, family=font, col=col)
         }
         if (redraw) {
            text(0, 0.45, paste0("Mode: ", "Add"), pos=4, cex=cex, family=font, col=col)
            text(0, 0.30, paste0("Show: ", ifelse(show, "Yes", "No")), pos=4, cex=cex, family=font, col=col)
            text(0, 0.15, paste0("Move: ", i), pos=4, cex=cex, family=font, col=col)
         }
      }

      selmode <- switch(selmode,
                        score_random  = "score, at random",
                        score_highest = "score, highest next",
                        played_random = "play frequency, at random",
                        played_lowest = "play frequency, lowest next",
                        days_random   = "date, at random",
                        days_oldest   = "date, oldest next",
                        sequential    = "sequential",
                        default       = "default")

      if (mode == "test") {
         if (onlyi) {
            text(0, 0.00, paste0("        ", paste0(rep("\U00002588",9), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.00, paste0("        ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
         }
         if (onlyscore) {
            text(9, 0.00, paste0("        ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(9, 0.00, paste0("Score:  ", score), pos=4, cex=cex, family=font, col=col)
         }
         if (redraw) {
            seqname <- substr(seqname, 1, nchar(seqname)-4)
            text(0, 0.45, paste0("Mode:   ", "Test (selection: ", selmode, ")"), pos=4, cex=cex, family=font, col=col)
            text(0, 0.30, paste0("Name:   ", "(", seqnum, ") ", seqname), pos=4, cex=cex, family=font, col=col)
            text(0, 0.15, paste0("Player: ", player), pos=4, cex=cex, family=font, col=col)
            text(0, 0.00, paste0("Move:   ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
            text(9, 0.15, paste0("Played: ", played), pos=4, cex=cex, family=font, col=col)
            text(9, 0.00, paste0("Score:  ", score), pos=4, cex=cex, family=font, col=col)
         }
      }

      if (mode == "play") {
         if (onlyi) {
            text(0, 0.15, paste0("        ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.15, paste0("        ", i), pos=4, cex=cex, family=font, col=col)
         }
         if (redraw) {
            text(0, 0.45, paste0("Mode:   ", "Play"), pos=4, cex=cex, family=font, col=col)
            text(0, 0.30, paste0("Player: ", player), pos=4, cex=cex, family=font, col=col)
            text(0, 0.15, paste0("Move:   ", i), pos=4, cex=cex, family=font, col=col)
         }
      }

   }

   if (lang == "de") {

      if (mode == "add") {
         if (onlyshow) {
            text(0, 0.30, paste0("        ", paste0(rep("\U00002588",4), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.30, paste0("        ", ifelse(show, "Ja", "Nein")), pos=4, cex=cex, family=font, col=col)
         }
         if (onlyi) {
            text(0, 0.15, paste0("        ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.15, paste0("        ", i), pos=4, cex=cex, family=font, col=col)
         }
         if (redraw) {
            text(0, 0.45, paste0("Modus:  ", "Hinzuf\U000000FCgen"), pos=4, cex=cex, family=font, col=col)
            text(0, 0.30, paste0("Zeigen: ", ifelse(show, "Ja", "Nein")), pos=4, cex=cex, family=font, col=col)
            text(0, 0.15, paste0("Zug:    ", i), pos=4, cex=cex, family=font, col=col)
         }
      }

      selmode <- switch(selmode,
                        score_random  = "Punktewert, zuf\U000000E4llig",
                        score_highest = "Punktewert, h\U000000F6chster",
                        played_random = "Spielh\U000000E4ufigkeit, zuf\U000000E4llig",
                        played_lowest = "Spielh\U000000E4ufigkeit, niedrigste",
                        days_random   = "Datum, zuf\U000000E4llig",
                        days_oldest   = "Datum, \U000000E4ltestes",
                        sequential    = "sequenziell",
                        default       = "default")

      if (mode == "test") {
         if (onlyi) {
            text(0, 0.00, paste0("         ", paste0(rep("\U00002588",9), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.00, paste0("         ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
         }
         if (onlyscore) {
            text(9, 0.00, paste0("          ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(9, 0.00, paste0("          ", score), pos=4, cex=cex, family=font, col=col)
         }
         if (redraw) {
            seqname <- substr(seqname, 1, nchar(seqname)-4)
            text(0, 0.45, paste0("Modus:   ", "Test (Selektion: ", selmode, ")"), pos=4, cex=cex, family=font, col=col)
            text(0, 0.30, paste0("Name:    ", "(", seqnum, ") ", seqname), pos=4, cex=cex, family=font, col=col)
            text(0, 0.15, paste0("Spieler: ", player), pos=4, cex=cex, family=font, col=col)
            text(0, 0.00, paste0("Zug:     ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col)
            text(9, 0.15, paste0("Gespielt: ", played), pos=4, cex=cex, family=font, col=col)
            text(9, 0.00, paste0("Punkte:   ", score), pos=4, cex=cex, family=font, col=col)
         }
      }

      if (mode == "play") {
         if (onlyi) {
            text(0, 0.15, paste0("         ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2)
            text(0, 0.15, paste0("         ", i), pos=4, cex=cex, family=font, col=col)
         }
         if (redraw) {
            text(0, 0.45, paste0("Modus:   ", "Spielen"), pos=4, cex=cex, family=font, col=col)
            text(0, 0.30, paste0("Spieler: ", player), pos=4, cex=cex, family=font, col=col)
            text(0, 0.15, paste0("Zug:     ", i), pos=4, cex=cex, family=font, col=col)
         }
      }

   }

}

.drawsideindicator <- function(sidetoplay, flip, adj=0, clear=TRUE) {

   col.side.w <- .get("col.side.w")
   col.side.b <- .get("col.side.b")

   indsize <- 0.25

   if (clear)
      .clearsideindicator(adj)

   if (flip) {
      if (sidetoplay == "w") {
         rect(9.25+adj, 9.0+adj, 9.25+indsize+adj, 9.0-indsize+adj, border=NA, col=col.side.w)
      } else {
         rect(9.25+adj, 1.0+adj, 9.25+indsize+adj, 1.0+indsize+adj, border=NA, col=col.side.b)
      }
   } else {
      if (sidetoplay == "w") {
         rect(9.25+adj, 1.0+adj, 9.25+indsize+adj, 1.0+indsize+adj, border=NA, col=col.side.w)
      } else {
         rect(9.25+adj, 9.0+adj, 9.25+indsize+adj, 9.0-indsize+adj, border=NA, col=col.side.b)
      }
   }

}

.drawtimer <- function(movestoplay, movesplayed, timetotal, timepermove, clear=FALSE, settings=FALSE) {

   col.bg        <- .get("col.bg")
   col.square.be <- .get("col.square.be")
   col.time.fast <- .get("col.time.fast")
   col.time.slow <- .get("col.time.slow")

   xpos <- 9.4 + ifelse(settings, 0.25, 0)
   indsize <- 0.25

   if (clear) {
      rect(xpos, 1, xpos+indsize, 9, border=col.bg, col=col.bg, lwd=6)
      return()
   }

   if (settings) {

      rect(xpos, 1, xpos+indsize, 9, border=NA, col=col.time.fast)
      rect(xpos, 1, xpos+indsize, 6, border=NA, col=col.square.be)
      rect(xpos, 1, xpos+indsize, 4, border=NA, col=col.time.slow)

   } else {

      hlines <- seq(1, 9, length.out=movestoplay+1)

      timepermitted <- timepermove * movestoplay
      prop   <- min(0.99, timetotal / timepermitted)
      intime <- timetotal <= movesplayed * timepermove
      top    <- 9 - 8*prop
      pos.top.hlines <- movestoplay-movesplayed+1

      rect(xpos, max(hlines[pos.top.hlines],top), xpos+indsize, 9, col=col.bg, border=col.square.be)

      #segments(xpos, hlines[(pos.top.hlines):(movestoplay+1)], xpos+indsize, hlines[(pos.top.hlines):(movestoplay+1)], col=col.square.be, lwd=2)
      #segments(xpos, hlines[1:(pos.top.hlines)], xpos+indsize, hlines[1:(pos.top.hlines)], col=col.bg, lwd=2)

      rect(xpos, 1, xpos+indsize, hlines[pos.top.hlines], border=NA, col=col.square.be)

      # draw the progress rectangle in the appropriate color
      if (intime) {
         rect(xpos, 1, xpos+indsize, top, border=NA, col=col.time.fast)
      } else {
         rect(xpos, 1, xpos+indsize, top, border=NA, col=col.time.slow)
      }

      #segments(xpos, hlines, xpos+indsize, hlines, col=col.square.be, lwd=2)

   }

   # outline
   rect(xpos, 1, xpos+indsize, 9, border=col.square.be, lwd=2)

}

.clearsideindicator <- function(adj=0) {

   indsize <- 0.25
   col.bg <- .get("col.bg")
   rect(9.25+adj, 1.0+adj, 9.25+indsize+adj, 1.0+indsize+adj, border=NA, col=col.bg)
   rect(9.25+adj, 9.0+adj, 9.25+indsize+adj, 9.0-indsize+adj, border=NA, col=col.bg)

}

.draweval <- function(val=NA_real_, last=NA_real_, flip=FALSE, clear=FALSE, eval=TRUE, evalsteps=10) {

   if (!eval)
      return()

   col.bg <- .get("col.bg")

   xpos <- 0.12
   indsize <- 0.25

   if (clear || length(val) == 0L || is.na(val)) {
      rect(xpos, 1, xpos+indsize, 9, border=NA, col=col.bg)
      return()
   }

   if (length(val) == 0L)
      return()

   col.fg <- .get("col.fg")
   col.side.w <- .get("col.side.w")
   col.side.b <- .get("col.side.b")

   maxval <- 9.9
   ismate <- abs(val) >= 99.9
   valtxt <- formatC(val, format="f", digits=1)
   val <- min(max(val, -maxval), maxval)
   if (ismate) {
      val <- (val + maxval) / (2*maxval) * 8 + 1 # either 1 or 9
   } else {
      val <- (val + maxval) / (2*maxval) * 7.8 + 1.1 # ranges from 1.1 to 8.9
   }

   if (length(last) == 0L)
      last <- NA_real_

   last <- min(max(last, -maxval), maxval)
   last <- (last + maxval) / (2*maxval) * 8 + 1

   steps <- evalsteps
   props <- seq(0, 1, length.out=steps)^(1/5)

   if (flip) {
      if (is.na(last)) {
         rect(xpos, 10-val, xpos+indsize, 9, border=NA, col=col.side.w)
         rect(xpos, 1, xpos+indsize, 10-val, border=NA, col=col.side.b)
      } else {
         rect(xpos, 10-last, xpos+indsize, 9, border=NA, col=col.side.w)
         rect(xpos, 1, xpos+indsize, 10-last, border=NA, col=col.side.b)
         vals <- last + (val - last) * props
         col.side.bar <- ifelse(val > last, col.side.w, col.side.b)
         for (i in 2:steps) {
            rect(xpos, 10-vals[i-1], xpos+indsize, 10-vals[i], border=NA, col=col.side.bar)
         }
      }
      if (val > 5) {
         text(xpos + indsize/2, 8.9, valtxt, cex=.get("cex.eval"), col=col.side.b)
      } else {
         text(xpos + indsize/2, 1.1, valtxt, cex=.get("cex.eval"), col=col.side.w)
      }
   } else {
      if (is.na(last)) {
         rect(xpos, val, xpos+indsize, 9, border=NA, col=col.side.b)
         rect(xpos, 1, xpos+indsize, val, border=NA, col=col.side.w)
      } else {
         rect(xpos, last, xpos+indsize, 9, border=NA, col=col.side.b)
         rect(xpos, 1, xpos+indsize, last, border=NA, col=col.side.w)
         vals <- last + (val - last) * props
         col.side.bar <- ifelse(val > last, col.side.w, col.side.b)
         for (i in 2:steps) {
            rect(xpos, vals[i-1], xpos+indsize, vals[i], border=NA, col=col.side.bar)
         }
      }
      if (val > 5) {
         text(xpos + indsize/2, 1.1, valtxt, cex=.get("cex.eval"), col=col.side.b)
      } else {
         text(xpos + indsize/2, 8.9, valtxt, cex=.get("cex.eval"), col=col.side.w)
      }
   }

   segments(xpos, 5, xpos+indsize, col=col.fg)

   return()

}

.startcomment <- function(txt, lwd) {

   rect(1.2, 1.2, 8.8, 8.8, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   xleft   <- 1.5
   xright  <- 8.5
   ybottom <- 1.5
   ytop    <- 8.5
   xtext   <- (xleft + xright) / 2
   center  <- TRUE

   txt <- gsub("\\n", "\n", txt, fixed=TRUE)
   txt <- strsplit(txt, "\n", fixed=TRUE)[[1]]
   if (startsWith(txt[1], "\\l")) {
      xtext <- xleft
      center  <- FALSE
      txt[1] <- gsub("\\l", "", txt[1], fixed=TRUE)
   }
   max_line_width <- max(strwidth(txt))
   max_line_height <- strheight("A")
   total_text_height <- length(txt) * max_line_height
   cex <- min(.get("cex.top"), (xright-xleft) / max_line_width, (ytop-ybottom) / total_text_height)
   ypos <- seq(from = min(ytop,    (ytop - ybottom) / 2 + ybottom + (length(txt) - 1) * 1 * max_line_height * cex),
               to   = max(ybottom, (ytop - ybottom) / 2 + ybottom - (length(txt) - 1) * 1 * max_line_height * cex),
               length.out = length(txt))
   col.top <- .get("col.top")
   for (i in seq_along(txt)) {
      if (center) {
         text(x=xtext, y=ypos[i], labels=txt[i], cex=cex, col=col.top)
      } else {
         text(x=xtext, y=ypos[i], labels=txt[i], cex=cex, col=col.top, pos=4)
      }
   }

   .waitforclick()

}

.erase <- function(x1, y1, x2, y2, steps=50) {

   col.bg <- .get("col.bg")

   xmid <- (x1 + x2) / 2
   ymid <- (y1 + y2) / 2

   x1pos <- seq(xmid, x1, length.out=steps)
   x2pos <- seq(xmid, x2, length.out=steps)
   y1pos <- seq(ymid, y1, length.out=steps)
   y2pos <- seq(ymid, y2, length.out=steps)

   for (j in 1:steps)
      rect(x1pos[j], y1pos[j], x2pos[j], y2pos[j], col=col.bg, border=NA)

}

.quit <- function() {

   .erase(-1,-1,11,11)

   return()

   cols <- col2rgb(.get("col.bg"))

   steps <- 50

   for (i in 1:steps) {
      rect(-1, -1, 11, 11, col=rgb(cols[1], cols[2], cols[3], 60, maxColorValue=255), border=NA)
      Sys.sleep(1/steps)
   }

   rect(-1, -1, 11, 11, col=.get("col.bg"), border=NA)

}
