.drawsquare <- function(x, y, flip, col=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), adj=0) {
   rect(y, x, y+1, x+1, col=col, border=NA)
   .drawcoords(x=x, y=y, flip=flip, adj=adj)
}

.drawcoords <- function(x, y, flip, adj=0) {

   if (.get("coords")) {
      cex.coords <- .get("cex.coords")
      col.square.l <- .get("col.square.l")
      col.square.d <- .get("col.square.d")
      if (flip) {
         if (x == 1+adj)
            text(y+0.08, 1.08+adj, letters[8:1][y-adj], col=ifelse(.is.even(y+adj), col.square.d, col.square.l), cex=cex.coords, font=2)
         if (y == 8+adj)
            text(8.92+adj, 1+x-0.08, (8:1)[x-adj],      col=ifelse(.is.even(x+adj), col.square.l, col.square.d), cex=cex.coords, font=2)
      } else {
         if (x == 1+adj)
            text(y+0.08, 1.08+adj, letters[1:8][y-adj], col=ifelse(.is.even(y+adj), col.square.d, col.square.l), cex=cex.coords, font=2)
         if (y == 8+adj)
            text(8.92+adj, 1+x-0.08, (1:8)[x-adj],      col=ifelse(.is.even(x+adj), col.square.l, col.square.d), cex=cex.coords, font=2)
      }
   }

}

.drawpiece <- function(x, y, piece) {

   if (piece != "") {
      flip <- ifelse(.get("upsidedown"), ".flip", "")
      txt <- paste0("rasterImage(img.", piece, flip, ",", y, ",", x, ",", y+1, ",", x+1, ")")
      eval(parse(text=txt), envir=.chesstrainer)
   }

}

.drawboard <- function(pos, flip=FALSE) {

   col.bg <- .get("col.bg")
   col.fg <- .get("col.fg")
   col.square.l <- .get("col.square.l")
   col.square.d <- .get("col.square.d")
   mar <- .get("mar")

   if (dev.cur() == 1L) {
      dev.new(bg=col.bg, title="chesstrainer")
      if (.get("inhibit")) {
         Sys.sleep(0.2)
         dev.control(displaylist="inhibit")
      }
   }

   par(xpd=NA, pty="s", mar=mar, fg=col.fg, bg=col.bg)

   mat <- outer(1:8, 1:8, function(x,y) .is.even(x+y))

   image(1:8+0.5, 1:8+0.5, mat, col=c(col.square.l, col.square.d), xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)

   mapply(.drawcoords, rep(1,8), 1:8, MoreArgs=list(flip=flip))
   mapply(.drawcoords, 1:8, rep(8,8), MoreArgs=list(flip=flip))

   #if (flip) {
   #   par(mgp=c(3,0.5,0))
   #   axis(side=1, 1:8+0.5, rev(LETTERS[1:8]), las=1, tick=FALSE, col.axis=col.fg)
   #   par(mgp=c(3,0.8,0))
   #   axis(side=2, 1:8+0.5, rev(1:8),          las=1, tick=FALSE, col.axis=col.fg)
   #} else {
   #   par(mgp=c(3,0.5,0))
   #   axis(side=1, 1:8+0.5, LETTERS[1:8],      las=1, tick=FALSE, col.axis=col.fg)
   #   par(mgp=c(3,0.8,0))
   #   axis(side=2, 1:8+0.5, 1:8,               las=1, tick=FALSE, col.axis=col.fg)
   #}

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

   .drawmatdiff(pos, flip, force=TRUE)

}

.redrawpos <- function(pos, posold, flip=FALSE, drawcheck=TRUE) {

   if (missing(posold)) {

      mat <- outer(1:8, 1:8, function(x,y) .is.even(x+y))
      par(new=TRUE)
      image(1:8+0.5, 1:8+0.5, mat, col=c(.get("col.square.l"), .get("col.square.d")), xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)
      par(new=FALSE)

      mapply(.drawcoords, rep(1,8), 1:8, MoreArgs=list(flip=flip))
      mapply(.drawcoords, 1:8, rep(8,8), MoreArgs=list(flip=flip))

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

   } else {

      #.rmcheck(posold, flip=flip)

      if (flip) {
         for (i in 1:8) {
            for (j in 1:8) {
               if (pos[9-i,9-j] != posold[9-i,9-j]) {
                  .drawsquare(i, j, flip=flip)
                  .drawpiece(i, j, pos[9-i,9-j])
               }
            }
         }
      } else {
         for (i in 1:8) {
            for (j in 1:8) {
               if (pos[i,j] != posold[i,j]) {
                  .drawsquare(i, j, flip=flip)
                  .drawpiece(i, j, pos[i,j])
               }
            }
         }
      }

   }

   .drawmatdiff(pos, flip)

   if (drawcheck)
      .drawcheck(pos, flip=flip)

}

.redrawall <- function(pos, flip, mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, movestoplay, movesplayed, timetotal, timepermove) {

   .drawboard(pos, flip)
   .textbot(mode, show, showcomp, player, seqname, seqnum, opening, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
   .texttop(texttop)
   .drawcheck(pos, flip=flip)
   if (mode == "test" && .get("timed")) {
      .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
   } else {
      .drawsideindicator(sidetoplay, flip=flip)
   }

}

.boardeditor.drawboard <- function(pos, flip, sidetoplay) {

   col.bg <- .get("col.bg")
   col.fg <- .get("col.fg")
   col.square.l <- .get("col.square.l")
   col.square.d <- .get("col.square.d")
   col.square.be <- .get("col.square.be")

   par(xpd=NA, pty="s", mar=rep(2.2,4), fg=col.fg, bg=col.bg)

   mat <- matrix(1, nrow=10, ncol=10)
   mat[2:9,2:9] <- outer(1:8, 1:8, function(x,y) ifelse(.is.even(x+y), 2, 3))

   image(1:10+0.5, 1:10+0.5, mat, col=c(col.bg, col.square.d, col.square.l), xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)

   mapply(.drawcoords, rep(2,8), 2:9, MoreArgs=list(flip=flip, adj=1))
   mapply(.drawcoords, 2:9, rep(9,8), MoreArgs=list(flip=flip, adj=1))

   for (j in 3:8) {
      .drawsquare(1, j, flip=flip, col=col.square.be, adj=100)
      .addrect(1, j, col.bg, lwdadj=2)
      .drawsquare(10, j, flip=flip, col=col.square.be)
      .addrect(10, j, col.bg, lwdadj=2)
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

   .drawsideindicator(sidetoplay, flip=flip, adj=1)

}

.updateboard <- function(pos, move, flip, autoprom, draw=TRUE, x2y2=TRUE) {

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
            .drawsquare(8, 1, flip=flip)
            .drawsquare(8, 3, flip=flip)
            .drawpiece(8, 3, "WR")
         }
      }
      if (identical(c(x1,y1), c(8,4)) && piece == "WK" && identical(c(x2,y2), c(8,6))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         if (draw) {
            .drawsquare(8, 8, flip=flip)
            .drawsquare(8, 5, flip=flip)
            .drawpiece(8, 5, "WR")
         }
      }
      if (identical(c(x1,y1), c(1,4)) && piece == "BK" && identical(c(x2,y2), c(1,2))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         if (draw) {
            .drawsquare(1, 1, flip=flip)
            .drawsquare(1, 3, flip=flip)
            .drawpiece(1, 3, "BR")
         }
      }
      if (identical(c(x1,y1), c(1,4)) && piece == "BK" && identical(c(x2,y2), c(1,6))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         if (draw) {
            .drawsquare(1, 8, flip=flip)
            .drawsquare(1, 5, flip=flip)
            .drawpiece(1, 5, "BR")
         }
      }

      # check for en passant

      if (identical(attr(pos,"ispp"), "b") && piece == "WP" && x1 == 4 && attr(pos,"y1") == y2) {
         isenpassant <- "w"
         pos[5,9-y2] <- ""
         if (draw) .drawsquare(4, y2, flip=flip)
      }
      if (identical(attr(pos,"ispp"), "w") && piece == "BP" && x1 == 5 && attr(pos,"y1") == y2) {
         isenpassant <- "b"
         pos[4,9-y2] <- ""
         if (draw) .drawsquare(5, y2, flip=flip)
      }

   } else {

      # check for rochade

      if (identical(c(x1,y1), c(1,5)) && piece == "WK" && identical(c(x2,y2), c(1,7))) {
         isrochade <- "0-0"
         pos[1,6] <- "WR"
         pos[1,8] <- ""
         if (draw) {
            .drawsquare(1, 8, flip=flip)
            .drawsquare(1, 6, flip=flip)
            .drawpiece(1, 6, "WR")
         }
      }
      if (identical(c(x1,y1), c(1,5)) && piece == "WK" && identical(c(x2,y2), c(1,3))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         if (draw) {
            .drawsquare(1, 1, flip=flip)
            .drawsquare(1, 4, flip=flip)
            .drawpiece(1, 4, "WR")
         }
      }
      if (identical(c(x1,y1), c(8,5)) && piece == "BK" && identical(c(x2,y2), c(8,7))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         if (draw) {
            .drawsquare(8, 8, flip=flip)
            .drawsquare(8, 6, flip=flip)
            .drawpiece(8, 6, "BR")
         }
      }
      if (identical(c(x1,y1), c(8,5)) && piece == "BK" && identical(c(x2,y2), c(8,3))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         if (draw) {
            .drawsquare(8, 1, flip=flip)
            .drawsquare(8, 4, flip=flip)
            .drawpiece(8, 4, "BR")
         }
      }

      # check for en passant

      if (identical(attr(pos,"ispp"), "b") && piece == "WP" && x1 == 5 && attr(pos,"y1") == y2) {
         isenpassant <- "w"
         pos[5,y2] <- ""
         if (draw) .drawsquare(5, y2, flip=flip)
      }
      if (identical(attr(pos,"ispp"), "w") && piece == "BP" && x1 == 4 && attr(pos,"y1") == y2) {
         isenpassant <- "b"
         pos[4,y2] <- ""
         if (draw) .drawsquare(4, y2, flip=flip)
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
            sapply(8:5, function(x2) .drawsquare(x2, y2, flip=flip, col=col.square.be))
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
            sapply(8:5, function(x2) .drawsquare(x2, y2, flip=flip))
            mapply(.drawpiece, x=8:5, y=rep(y2,4), piece=pos[1:4,9-y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("B", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
      if (x1 == 2 && x2 == 1 && piece == "WP") {
         if (autoprom) {
            promotionpiece <- paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])
         } else {
            sapply(1:4, function(x2) .drawsquare(x2, y2, flip=flip,, col=col.square.be))
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
            sapply(1:4, function(x2) .drawsquare(x2, y2, flip=flip))
            mapply(.drawpiece, x=1:4, y=rep(y2,4), piece=pos[8:5,9-y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
   } else {
      if (x1 == 7 && x2 == 8 && piece == "WP") {
         if (autoprom) {
            promotionpiece <- paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])
         } else {
            sapply(8:5, function(x2) .drawsquare(x2, y2, flip=flip, col=col.square.be))
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
            sapply(8:5, function(x2) .drawsquare(x2, y2, flip=flip))
            mapply(.drawpiece, x=8:5, y=rep(y2,4), piece=pos[8:5,y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("W", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
      if (x1 == 2 && x2 == 1 && piece == "BP") {
         if (autoprom) {
            promotionpiece <- paste0("B", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])
         } else {
            sapply(1:4, function(x2) .drawsquare(x2, y2, flip=flip, col=col.square.be))
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
            sapply(1:4, function(x2) .drawsquare(x2, y2, flip=flip))
            mapply(.drawpiece, x=1:4, y=rep(y2,4), piece=pos[1:4,y2])
            if (!is.na(move[[6]]) && !identical(promotionpiece, paste0("B", strsplit(sub("+" ,"", move[[6]], fixed=TRUE), "=", fixed=TRUE)[[1]][2])))
               return("prommistake")
         }
      }
   }

   if (draw) {
      .drawsquare(x1, y1, flip=flip)
      .drawsquare(x2, y2, flip=flip)
   }

   if (flip) {

      if (draw)
         .drawpiece(x2, y2, piece)

      iscapture <- pos[9-x2,9-y2] != "" || isenpassant != ""

      if (draw) {
         if (iscapture) {
            playsound(system.file("sounds", "capture.ogg", package="chesstrainer"))
         } else {
            playsound(system.file("sounds", "move.ogg", package="chesstrainer"))
         }
      }

      pos[9-x2,9-y2] <- piece
      pos[9-x1,9-y1] <- ""

      if (promotionpiece != "") {
         pos[9-x2,9-y2] <- promotionpiece
         if (draw) {
            .drawsquare(x2, y2, flip=flip)
            .drawpiece(x2, y2, pos[9-x2,9-y2])
         }
      }

   } else {

      if (draw)
         .drawpiece(x2, y2, piece)

      iscapture <- pos[x2,y2] != "" || isenpassant != ""

      if (draw) {
         if (iscapture) {
            playsound(system.file("sounds", "capture.ogg", package="chesstrainer"))
         } else {
            playsound(system.file("sounds", "move.ogg", package="chesstrainer"))
         }
      }

      pos[x2,y2] <- piece
      pos[x1,y1] <- ""

      if (promotionpiece != "") {
         pos[x2,y2] <- promotionpiece
         if (draw) {
            .drawsquare(x2, y2, flip=flip)
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

   moves50 <- attr(pos,"moves50")

   if (is.null(moves50) || length(moves50) == 0L) {
      attr(pos,"moves50") <- 0
   } else {
      if (pawnmove || iscapture) {
         attr(pos,"moves50") <- 0
      } else {
         attr(pos,"moves50") <- moves50 + 1
      }
   }

   if (draw && .get("verbose")) {
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

   if (draw && (iscapture || promotionpiece != ""))
      .drawmatdiff(pos, flip)

   if (x2y2)
      assign("x2y2", c(x2,y2), envir=.chesstrainer)

   return(pos)

}

.boardeditor.updateboard <- function(pos, move, flip, button) {

   x1 <- unname(move[[1]])
   y1 <- unname(move[[2]])
   x2 <- unname(move[[3]])
   y2 <- unname(move[[4]])

   if (x1 == x2 && y1 == y2 && x1 > 1 && x1 < 10 && y1 > 1 && y1 < 10) {

      if (button == 2L) {
         .drawsquare(x1, y1, flip=flip, adj=1)
         if (flip) {
            pos[11-x1,11-y1] <- ""
         } else {
            pos[x1,y1] <- ""
         }
      }

   } else {

      if (button == 0L && x1 > 1 && x1 < 10 && y1 > 1 && y1 < 10)
         .drawsquare(x1, y1, flip=flip, adj=1)
      if (x2 > 1 && x2 < 10 && y2 > 1 && y2 < 10)
         .drawsquare(x2, y2, flip=flip, adj=1)

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

.addrect <- function(x, y, col, lwdadj=0) {
   lwd <- .get("lwd")
   offset <- 0.028
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+lwdadj, border=col, ljoin=1)
}

.rmrect <- function(x, y, flip) {
   if (is.null(x) || is.null(y) || is.na(x) || is.na(y))
      return()
   lwd <- .get("lwd")
   offset <- 0.028
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), ljoin=1)
   .drawcoords(x, y, flip)
}

.boardeditor.rmrect <- function(x, y, flip) {
   if (is.null(x) || is.null(y) || is.na(x) || is.na(y))
      return()
   lwd <- .get("lwd")
   offset <- 0.028
   if (x <= 1 || x >= 10 || y <= 1 || y >= 10) {
      rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=.get("col.bg"), ljoin=1)
   } else {
      rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), ljoin=1)
      .drawcoords(x, y, flip, adj=1)
   }
}

.drawcircle <- function(x, y)
   symbols(y+0.5, x+0.5, circles=0.45, inches=FALSE, lwd=.get("lwd")+2, fg=.get("col.annot"), add=TRUE)

.drawcircles <- function(circles) {
   if (nrow(circles) == 0L)
      return()
   apply(circles, 1, function(x) .drawcircle(x[1], x[2]))
}

.drawcheck <- function(pos, flip) {

   ischeck <- attr(pos, "ischeck")

   if (sum(ischeck) == 0L || sum(ischeck) == 2L) {
      assign("checkpos", c(NA,NA), envir=.chesstrainer)
      return()
   }

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

   assign("checkpos", xy, envir=.chesstrainer)

}

.rmcheck <- function(pos, flip) {

   ischeck <- attr(pos, "ischeck")

   assign("checkpos", c(NA,NA), envir=.chesstrainer)

   if (sum(ischeck) == 0L || sum(ischeck) == 2L)
      return()

   color <- c("w","b")[ischeck]
   piece <- paste0(toupper(color), "K", collapse="")

   xy <- c(which(pos==piece, arr.ind=TRUE))

   if (flip)
      xy <- 9-xy

   x <- xy[1]
   y <- xy[2]

   .drawsquare(x, y, flip=flip, col=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")))
   .drawpiece(x, y, piece)

}

.drawarrow <- function(y1, x1, y2, x2, col=.get("col.annot")) {

   lwd <- .get("lwd")

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

.drawarrows <- function(arrows, hint=FALSE, evalvals, sidetoplay) {
   if (nrow(arrows) == 0L)
      return()
   if (hint) {
      n <- nrow(arrows)
      col.best <- .get("col.best")
      if (n == 1) {
         .drawarrow(arrows[1,1], arrows[1,2], arrows[1,3], arrows[1,4], col=adjustcolor(col.best, alpha.f=0.6))
      } else {
         if(sidetoplay == "b")
            evalvals <- -1 * evalvals
         diffs <- max(evalvals) - evalvals
         alphas <- exp(-0.5 * diffs) * c(0.65, rep(0.50, n-1))
         alphas[alphas <= 0.1] <- 0
         for (j in 1:nrow(arrows)) {
            .drawarrow(arrows[j,1], arrows[j,2], arrows[j,3], arrows[j,4], col=adjustcolor(col.best, alpha.f=alphas[j]))
         }
      }
   } else {
      apply(arrows, 1, function(x) .drawarrow(x[1], x[2], x[3], x[4]))
   }
}

.drawglyph <- function(glyph) {

   if (!.isglyph(glyph))
      return()

   x2y2 <- .get("x2y2")

   x <- x2y2[1]
   y <- x2y2[2]

   col <- switch(glyph,
                 "!"  = "#22ac38",
                 "!!" = "#168226",
                 "?"  = "#e69f00",
                 "??" = "#df5353",
                 "!?" = "#ea45d8",
                 "?!" = "#56b4e9",
                 "#999999"
                 )

   xoff   <-  0.940
   yoff   <-  0.940
   xsoff  <- -0.025
   ysoff  <-  0.025
   radius <-  0.200

   symbols(y+yoff+ysoff, x+xoff+xsoff, circles=radius, inches=FALSE, lwd=1, fg=NA, bg="#666666", add=TRUE)
   symbols(y+yoff, x+xoff, circles=radius, inches=FALSE, lwd=1, fg=NA, bg=col, add=TRUE)
   text(y+yoff, x+xoff, glyph, font=2, col="white", offset=0, cex=.get("cex.glyphs"))

}

.drawannot <- function(circles=NULL, arrows=NULL, harrows=NULL, glyph=NULL, hint, evalvals, sidetoplay) {

   if (!is.null(circles) && nrow(circles) >= 1L)
      .drawcircles(circles)
   if (!is.null(arrows) && nrow(arrows) >= 1L)
      .drawarrows(arrows)
   if (!is.null(harrows) && nrow(harrows) >= 1L)
      .drawarrows(harrows, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
   .drawglyph(glyph)

}

.rmannot <- function(pos, circles=NULL, arrows=NULL, glyph=NULL, flip) {

   oldpos <- pos

   if (!is.null(circles) && nrow(circles) >= 1L) {
      if (flip) {
         oldpos[9-circles[,1], 9-circles[,2]] <- "x"
      } else {
         oldpos[circles[,1], circles[,2]] <- "x"
      }
   }

   if (!is.null(arrows) && nrow(arrows) >= 1L) {
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

   if (.isglyph(glyph)) {

      x2y2 <- .get("x2y2")

      x1 <- x2y2[1]
      y1 <- x2y2[2]
      x2 <- min(8, x1+1)
      y2 <- min(8, y1+1)

      if (flip) {
         oldpos[9-(x1:x2), 9-(y1:y2)] <- "x"
      } else {
         oldpos[x1:x2, y1:y2] <- "x"
      }

   }

   if (any(oldpos == "x")) {

      .redrawpos(pos, oldpos, flip=flip, drawcheck=FALSE)

      if (.isglyph(glyph)) {

         if (x2 == 8) {
            rect(1, 9, 9.22, 9.32, col=.get("col.bg"), border=NA)
            .drawmatdiff(pos, flip, force=TRUE)
         }
         if (y2 == 8) {
            rect(9, 1, 9.32, 9.22, col=.get("col.bg"), border=NA)
         }

      }

      ischeck <- attr(pos, "ischeck")

      if (any(ischeck)) {

         checkpos <- as.numeric(.get("checkpos"))

         if (flip)
            checkpos <- 9-checkpos

         xpos <- which(oldpos == "x", arr.ind=TRUE)

         if (any(apply(xpos, 1, function(x) isTRUE(x[1] == checkpos[1] && x[2] == checkpos[2]))))
            .drawcheck(pos, flip=flip)

      }

   }

}

.texttop <- function(txt, sleep=0, left=FALSE, xadj=0, yadj=0) {

   if (length(txt) == 0L || is.na(txt))
      return()

   if (isTRUE(attr(txt, "left")))
      left <- TRUE

   xleft   <- 0 + xadj
   xright  <- 10 + xadj
   ybottom <- 9.5 + yadj
   ytop    <- grconvertY(dev.size()[2], from="inches", to="user") - 0.1
   xcenter <- (xleft + xright) / 2
   ymargin <- 0.1

   srt <- ifelse(.get("upsidedown"), 180, 0)

   rect(xleft, ybottom-ymargin, xright, ytop+ymargin, col=.get("col.bg"), border=NA)

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
         text(x=xcenter, y=ypos[i], labels=txt[i], cex=cex, col=col.top, adj=c(ifelse(left,0,0.5),0.5), srt=srt)
      }
   }

   Sys.sleep(sleep + .get("sleepadj"))

   if (sleep > 0)
      rect(xleft, ybottom-ymargin, xright, ytop+ymargin, col=.get("col.bg"), border=NA)

   txt <- paste(txt, collapse="\n")
   return(txt)

}

.textbot <- function(mode, show, showcomp, player, seqname, seqnum, opening="", score, rounds, age=NA, difficulty=NA, i, totalmoves, selmode="default", k, seqno, onlyshow=FALSE, onlyi=FALSE, onlyscore=FALSE, onlyeco=FALSE) {

   lang    <- .get("lang")
   cex     <- .get("cex.bot")
   font    <- .get("font.mono")
   col     <- .get("col.bot")
   col.bg  <- .get("col.bg")

   if (mode == "test" && .get("zenmode")) {
      rect(-2, -1, 12, 0.6, col=col.bg, border=NA)
      return()
   }

   if (.get("upsidedown")) {
      srt <- 180
      xleft  <- 9.80
      xright <- 0.80
   } else {
      srt <- 0
      xleft  <- 0.00
      xright <- 8.95
   }

   redraw <- !any(onlyi, onlyshow, onlyscore, onlyeco)

   if (redraw)
      rect(-2, -1, 12, 0.6, col=col.bg, border=NA)

   if (is.na(age)) {
      age <- ""
   } else {
      age <- formatC(age, format="f", digits=0)
   }

   if (is.na(difficulty)) {
      difficulty <- ""
   } else {
      difficulty <- formatC(difficulty, format="f", digits=0)
   }

   if (lang == "en") {

      if (mode == "add") {
         if (onlyshow) {
            text(xleft, 0.30, paste0("      ", paste0(rep("\U00002588",8), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.30, paste0("Show: ", ifelse(show, "Yes", "No"), ifelse(showcomp, "", " / No")), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (onlyi) {
            text(xleft, 0.15, paste0("      ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.15, paste0("      ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (onlyeco) {
            text(xleft, 0.00, paste0("      ", paste0(rep("\U00002588",140), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.00, paste0("      ", opening), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (redraw) {
            text(xleft, 0.45, paste0("Mode: ", "Add"), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.30, paste0("Show: ", ifelse(show, "Yes", "No"), ifelse(showcomp, "", " / No")), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.15, paste0("Move: ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.00, paste0("ECO:  ", opening), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
      }

      selmode <- switch(selmode,
                        score_random   = "score, at random",
                        score_highest  = "score, highest next",
                        rounds_random  = "rounds, at random",
                        rounds_lowest  = "rounds, lowest next",
                        age_random     = "age, at random",
                        age_oldest     = paste0("age, oldest next, ", seqno, "/", k),
                        diff_random    = "difficulty, at random",
                        diff_highest   = "difficulty, highest next",
                        sequential     = paste0("sequentially, alphabetically, ", seqno, "/", k),
                        sequential_len = paste0("sequentially, by length, ", seqno, "/", k),
                        sequential_mov = paste0("sequentially, by moves, ", seqno, "/", k),
                        default        = "default")

      if (mode == "test") {
         if (onlyi) {
            text(xleft, 0.00, paste0("        ", paste0(rep("\U00002588",9), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.00, paste0("        ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (onlyscore) {
            text(xright, 0.00, paste0("        ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xright, 0.00, paste0("Score:  ", score), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (redraw) {
            seqname <- substr(seqname, 1, nchar(seqname)-4)
            text(xleft, 0.45, paste0("Mode:   ", "Test (selection: ", selmode, ")"), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.30, paste0("Name:   ", "(", seqnum, ") ", seqname), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.15, paste0("Player: ", player), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.00, paste0("Move:   ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.45, paste0("Rounds: ", rounds), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.30, paste0("Age:    ", age), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.15, paste0("Diff:   ", difficulty), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.00, paste0("Score:  ", score), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
      }

      if (mode %in% c("play","analysis")) {
         if (onlyi) {
            text(xleft, 0.15, paste0("        ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.15, paste0("        ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (redraw) {
            text(xleft, 0.45, paste0("Mode:   ", ifelse(mode == "play", "Play", "Analysis")), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.30, paste0("Player: ", player), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.15, paste0("Move:   ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
      }

   }

   if (lang == "de") {

      if (mode == "add") {
         if (onlyshow) {
            text(xleft, 0.30, paste0("        ", paste0(rep("\U00002588",11), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.30, paste0("        ", ifelse(show, "Ja", "Nein"), ifelse(showcomp, "", " / Nein")), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (onlyi) {
            text(xleft, 0.15, paste0("        ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.15, paste0("        ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (onlyeco) {
            text(xleft, 0.00, paste0("        ", paste0(rep("\U00002588",140), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.00, paste0("        ", opening), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (redraw) {
            text(xleft, 0.45, paste0("Modus:  ", "Hinzuf\U000000FCgen"), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.30, paste0("Zeigen: ", ifelse(show, "Ja", "Nein"), ifelse(showcomp, "", " / Nein")), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.15, paste0("Zug:    ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.00, paste0("ECO:    ", opening), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
      }

      selmode <- switch(selmode,
                        score_random   = "Punktewert, zuf\U000000E4llig",
                        score_highest  = "Punktewert, h\U000000F6chster",
                        rounds_random  = "Runden, zuf\U000000E4llig",
                        rounds_lowest  = "Runden, niedrigste",
                        age_random     = "Alter, zuf\U000000E4llig",
                        age_oldest     = paste0("Alter, \U000000E4ltestes, ", seqno, "/", k),
                        diff_random    = "Schwierigkeit, zuf\U000000E4llig",
                        diff_highest   = "Schwierigkeit, h\U000000F6chster",
                        sequential     = paste0("sequenziell, alphabetisch, ", seqno, "/", k),
                        sequential_len = paste0("sequenziell, nach L\U000000E4nge, ", seqno, "/", k),
                        sequential_mov = paste0("sequenziell, nach Z\U000000FCgen, ", seqno, "/", k),
                        default        = "default")

      if (mode == "test") {
         if (onlyi) {
            text(xleft, 0.00, paste0("         ", paste0(rep("\U00002588",9), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.00, paste0("         ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (onlyscore) {
            text(xright, 0.00, paste0("          ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xright, 0.00, paste0("          ", score), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (redraw) {
            seqname <- substr(seqname, 1, nchar(seqname)-4)
            text(xleft, 0.45, paste0("Modus:   ", "Test (Selektion: ", selmode, ")"), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.30, paste0("Name:    ", "(", seqnum, ") ", seqname), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.15, paste0("Spieler: ", player), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.00, paste0("Zug:     ", i-1, " / ", totalmoves), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.45, paste0("Runden:   ", rounds), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.30, paste0("Alter:    ", age), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.15, paste0("Schwier:  ", difficulty), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xright, 0.00, paste0("Punkte:   ", score), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
      }

      if (mode %in% c("play","analysis")) {
         if (onlyi) {
            text(xleft, 0.15, paste0("         ", paste0(rep("\U00002588",3), collapse="")), pos=4, cex=cex, family=font, col=col.bg, font=2, srt=srt)
            text(xleft, 0.15, paste0("         ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
         }
         if (redraw) {
            text(xleft, 0.45, paste0("Modus:   ", ifelse(mode == "play", "Spielen", "Analyse")), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.30, paste0("Spieler: ", player), pos=4, cex=cex, family=font, col=col, srt=srt)
            text(xleft, 0.15, paste0("Zug:     ", i), pos=4, cex=cex, family=font, col=col, srt=srt)
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

   return(sidetoplay)

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

.draweval <- function(val=NA_real_, last=NA_real_, i=1, starteval=NA_real_, flip=FALSE, clear=FALSE, eval=TRUE, evalsteps=10) {

   col.bg <- .get("col.bg")

   xpos <- 0.12
   indsize <- 0.25

   if (!eval) {
      rect(xpos, 1, xpos+indsize, 9, border=NA, col=col.bg)
      return()
   }

   if (i == 1 && (length(last) == 0L || is.na(last)))
      last <- starteval

   if (i == 1 && (length(val) == 0L || is.na(val)))
      val <- starteval

   if (clear || length(val) == 0L || is.na(val)) {
      rect(xpos, 1, xpos+indsize, 9, border=NA, col=col.bg)
      return()
   }

   col.fg     <- .get("col.fg")
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

   props <- seq(0, 1, length.out=evalsteps)^(1/5)

   if (flip) {
      if (is.na(last)) {
         rect(xpos, 10-val, xpos+indsize, 9, border=NA, col=col.side.w)
         rect(xpos, 1, xpos+indsize, 10-val, border=NA, col=col.side.b)
      } else {
         rect(xpos, 10-last, xpos+indsize, 9, border=NA, col=col.side.w)
         rect(xpos, 1, xpos+indsize, 10-last, border=NA, col=col.side.b)
         vals <- last + (val - last) * props
         col.side.bar <- ifelse(val > last, col.side.w, col.side.b)
         for (i in 2:evalsteps) {
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
         for (i in 2:evalsteps) {
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

}

.startcomment <- function(txt) {

   rect(1.2, 1.2, 8.8, 8.8, col=.get("col.bg"), border=.get("col.help.border"), lwd=.get("lwd")+3)

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

.showbestmove <- function(pos, flip, sidetoplay, sidetoplaystart, i, circles, arrows, harrows, glyph, bestmove, evalval, hintdepth, sfproc, sfrun, depth1, multipv1, sflim) {

   texttop <- ""
   evalvals <- NULL

   if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
      .rmannot(pos, circles=circles, arrows=rbind(arrows, harrows), flip=flip)
      .drawannot(circles=circles, arrows=arrows)
   }
   harrows <- matrix(nrow=0, ncol=4)
   if (i == 1 && is.na(evalval[1])) {
      fen <- .genfen(pos, flip, sidetoplay, sidetoplaystart, i)
      res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay)
      evalval  <- res.sf$eval
      bestmove <- res.sf$bestmove
      matetype <- res.sf$matetype
      sfproc   <- res.sf$sfproc
      sfrun    <- res.sf$sfrun
   }
   if (bestmove[[1]][1] != "") { # bestmove comes from [d] (unless it was calculate above)
      nmoves <- length(bestmove)
      bestmovetxt <- rep(NA_character_, nmoves)
      evalvals <- rep(NA_real_, nmoves)
      for (j in 1:nmoves) {
         if (bestmove[[j]][1] == "")
            next
         bestmovetxt[j] <- .parsemove(bestmove[[j]], pos=pos, flip=flip, evalval=evalval[j], i=i, sidetoplay=sidetoplay, rename=TRUE, returnline=TRUE, hintdepth=hintdepth)$txt
         evalvals[j] <- evalval[j]
         bestx1 <- as.numeric(substr(bestmove[[j]][1], 2, 2))
         besty1 <- which(letters[1:8] == substr(bestmove[[j]][1], 1, 1))
         bestx2 <- as.numeric(substr(bestmove[[j]][1], 4, 4))
         besty2 <- which(letters[1:8] == substr(bestmove[[j]][1], 3, 3))
         if (flip) {
            bestx1 <- 9 - bestx1
            besty1 <- 9 - besty1
            bestx2 <- 9 - bestx2
            besty2 <- 9 - besty2
         }
         harrows <- rbind(harrows, c(bestx1, besty1, bestx2, besty2))
      }
      evalvals <- evalvals[!is.na(evalvals)]
      bestmovetxt <- bestmovetxt[!is.na(bestmovetxt)]
      .drawarrows(harrows, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
      .drawglyph(glyph)
      texttop <- .texttop(paste0(bestmovetxt, collapse="\n"), left=TRUE)
      attr(texttop, "left") <- TRUE
   } else {
      if (sfrun) {
         .texttop(.text("nobestmove"), sleep=1.5)
      } else {
         .texttop(.text("nomovewoutsf"), sleep=1.5)
      }
   }

   return(list(harrows=harrows, texttop=texttop, evalvals=evalvals))

}

.drawmatdiff <- function(pos, flip, force=FALSE) {

   oldscore <- .get("score")
   col      <- .get("col.bot")
   matdiff  <- .get("matdiff")
   cex.matdiff <- .get("cex.matdiff")

   p.p <- sum(pos == "WP") - sum(pos == "BP")
   p.n <- sum(pos == "WN") - sum(pos == "BN")
   p.b <- sum(pos == "WB") - sum(pos == "BB")
   p.r <- sum(pos == "WR") - sum(pos == "BR")
   p.q <- sum(pos == "WQ") - sum(pos == "BQ")

   mdiff <- c(p=p.p, n=p.n, b=p.b, r=p.r, q=p.q)

   #pieces <- c("P", "N", "B", "R", "Q")
   pieces <- c("\U0000265F", "\U0000265E", "\U0000265D", "\U0000265C", "\U0000265B")
   value  <- c(1, 3, 3, 5, 9)
   score  <- sum(mdiff *  value)

   assign("score", score, envir=.chesstrainer)

   if (!force && (score == oldscore || !matdiff))
      return()

   .clearmatdiff()

   upsidedown <- .get("upsidedown")
   srt <- ifelse(upsidedown, 180, 0)
   textpos <- ifelse(upsidedown, 4, 2)

   xpos1 <- 9
   xpos2 <- 9
   shift <- 0.10
   space <- 0.20
   ypos  <- 0.82

   if (score != 0) {

      txt <- paste0("+", abs(score), collapse="")

      if (score > 0) {
         text(xpos1, ifelse(flip, 10-ypos, ypos), txt, pos=textpos, cex=cex.matdiff*0.9, col=col, offset=0, srt=srt)
         xpos1 <- xpos1 - strwidth(txt, cex=cex.matdiff) - shift
      } else {
         text(xpos2, ifelse(flip, ypos, 10-ypos), txt, pos=textpos, cex=cex.matdiff*0.9, col=col, offset=0, srt=srt)
         xpos2 <- xpos2 - strwidth(txt, cex=cex.matdiff) - shift
      }

   }

   for (i in 1:5) {

      if (mdiff[i] > 0) {
         n <- mdiff[i]
         xpos1 <- xpos1 - (0:(n-1)) * shift
         text(xpos1, rep(ifelse(flip, 10-ypos, ypos), n) , pieces[i], pos=textpos, offset=0, cex=cex.matdiff*1.1, col=col, srt=srt)
         xpos1 <- min(xpos1) - space
      }

      if (mdiff[i] < 0) {
         n <- -mdiff[i]
         xpos2 <- xpos2 - (0:(n-1)) * shift
         text(xpos2, rep(ifelse(flip, ypos, 10-ypos), n) , pieces[i], pos=textpos, offset=0, cex=cex.matdiff*1.1, col=col, srt=srt)
         xpos2 <- min(xpos2) - space
      }

   }

}

.clearmatdiff <- function() {

   col.bg <- .get("col.bg")
   rect(5, 0.70, 9, 0.98, col=col.bg, border=NA)
   rect(5, 9.02, 9, 9.30, col=col.bg, border=NA)

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

.drawbutton <- function(x, y, text, len, cex, on) {

   font.mono  <- .get("font.mono")
   col.text   <- .get("col.bg")
   col.box    <- ifelse(on, .get("col.square.l"), .get("col.square.d"))
   col.border <- .get("col.top")

   height <- strheight("A", family=font.mono, cex=cex)
   width  <- strwidth(paste0(rep("A",len), collapse=""), family=font.mono, cex=cex)

   x1 <- x - width  / 1.6
   y1 <- y - height / 0.8
   x2 <- x + width  / 1.6
   y2 <- y + height / 0.8

   rect(x1, y1, x2, y2, col=col.box, border=col.border, lwd=2)
   text(x, y, text, family=font.mono, cex=cex, col=col.text, adj=c(0.5,0.5))

   return(c(x1,y1,x2,y2))

}

.drawslider <- function(x, y, xlab, cex) {

   font.mono <- .get("font.mono")
   col.line  <- .get("col.square.d")

   height <- strheight("A", family=font.mono, cex=cex)

   segments(x[1], y, x[2], y, col=col.line, lend=2)
   segments(x[1], y-0.06, x[1], y+0.06, col=col.line, lend=2)
   segments(x[2], y-0.06, x[2], y+0.06, col=col.line, lend=2)
   text(x[1], y - 1.6 * height, xlab[1], family=font.mono, cex=cex, col=col.line, adj=c(0.5,0.5))
   text(x[2], y - 1.6 * height, xlab[2], family=font.mono, cex=cex, col=col.line, adj=c(0.5,0.5))

   return(c(x[1], y-height, x[2], y+height))

}

.updateslider <- function(x, y, oldval, xlim, range, round, cex) {

   font.mono  <- .get("font.mono")
   col.slider <- .get("col.square.l")
   col.line   <- .get("col.square.d")
   col.bg     <- .get("col.bg")

   height <- strheight("A", family=font.mono, cex=cex)

   xold <- xlim[1] + (oldval - range[1]) / (range[2] - range[1]) * (xlim[2] - xlim[1])

   if (is.null(x)) {
      rect(xold-0.03, y-0.08, xold+0.03, y+0.08, col=col.slider, border=col.slider)
      text(xold, y + 1.6 * height, oldval, family=font.mono, cex=cex, col=col.line, adj=c(0.5,0.5))
      return()
   } else {
      rect(xold-0.1, y-0.12, xold+0.1, y+0.12, col=col.bg, border=col.bg)
      rect(max(1.25, xold-1), y+0.05, min(8.75,xold+1), y+3 * height, col=col.bg, border=col.bg)
      segments(max(xlim[1], xold-0.1), y, min(xlim[2], xold+0.1), y, col=col.line, lend=2)
      segments(xlim[1], y-0.06, xlim[1], y+0.06, col=col.line, lend=2)
      segments(xlim[2], y-0.06, xlim[2], y+0.06, col=col.line, lend=2)
   }

   rect(x-0.03, y-0.08, x+0.03, y+0.08, col=col.slider, border=col.slider)

   newval <- range[1] + (x - xlim[1]) / (xlim[2] - xlim[1]) * (range[2] - range[1])

   if (round)
      newval <- round(newval)

   text(x, y + 1.6 * height, newval, family=font.mono, cex=cex, col=col.line, adj=c(0.5,0.5))

   return(newval)

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
