.drawpiece <- function(x, y, piece) {
   if (piece != "") {
      txt <- paste0("rasterImage(img.", piece, ",", y, ",", x, ",", y+1, ",", x+1, ")")
      eval(parse(text=txt), envir=.chesstrainer)
   }
}

.drawboard <- function(pos, flip=FALSE) {

   col.bg <- .get("col.bg")
   col.fg <- .get("col.fg")
   col.square.l <- .get("col.square.l")
   col.square.d <- .get("col.square.d")

   if (dev.cur() == 1L)
      dev.new(bg=col.bg)

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

.boardeditor.drawboard <- function(pos, flip=FALSE, lwd) {

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

}

.updateboard <- function(pos, move, flip, volume, verbose) {

   x1 <- unname(move[[1]])
   y1 <- unname(move[[2]])
   x2 <- unname(move[[3]])
   y2 <- unname(move[[4]])

   isrochade <- ""
   isenpassent <- ""
   ispp <- ""
   pawnmove <- FALSE

   rochade <- attributes(pos)$rochade

   if (is.null(rochade))
      rochade <- rep(TRUE, 4)

   if (flip) {
      if (x1 == 2 && x2 == 4 && pos[9-x1,9-y1] == "BP")
         ispp <- "b"
      if (x1 == 7 && x2 == 5 && pos[9-x1,9-y1] == "WP")
         ispp <- "w"
   } else {
      if (x1 == 2 && x2 == 4 && pos[x1,y1] == "WP")
         ispp <- "w"
      if (x1 == 7 && x2 == 5 && pos[x1,y1] == "BP")
         ispp <- "b"
   }

   if (flip) {
      if (pos[9-x1,9-y1] %in% c("WP","BP"))
         pawnmove <- TRUE
   } else {
      if (pos[x1,y1] %in% c("WP","BP"))
         pawnmove <- TRUE
   }

   if (flip) {

      # check for rochade

      if (identical(c(x1,y1), c(9-1,9-5)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(9-1,9-7))) {
         isrochade <- "0-0"
         pos[1,6] <- "WR"
         pos[1,8] <- ""
         .drawsquare(9-1, 9-8)
         .drawsquare(9-1, 9-6)
         .drawpiece(9-1, 9-6, "WR")
      }
      if (identical(c(x1,y1), c(9-1,9-5)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(9-1,9-3))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         .drawsquare(9-1, 9-1)
         .drawsquare(9-1, 9-4)
         .drawpiece(9-1, 9-4, "WR")
      }
      if (identical(c(x1,y1), c(9-8,9-5)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(9-8,9-7))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         .drawsquare(9-8, 9-8)
         .drawsquare(9-8, 9-6)
         .drawpiece(9-8, 9-6, "BR")
      }
      if (identical(c(x1,y1), c(9-8,9-5)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(9-8,9-3))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         .drawsquare(9-8, 9-1)
         .drawsquare(9-8, 9-4)
         .drawpiece(9-8, 9-4, "BR")
      }

      # check for en passent

      if (identical(attr(pos, "ispp"), "b") && pos[9-x1,9-y1] == "WP" && x1 == 4 && attr(pos, "y1") == y2) {
         isenpassent <- "w"
         pos[4,y2] <- ""
         .drawsquare(4, y2)
      }
      if (identical(attr(pos, "ispp"), "w") && pos[9-x1,9-y1] == "BP" && x1 == 5 && attr(pos, "y1") == y2) {
         isenpassent <- "b"
         pos[5,y2] <- ""
         .drawsquare(5, y2)
      }

   } else {

      # check for rochade

      if (identical(c(x1,y1), c(1,5)) && pos[x1,y1] == "WK" && identical(c(x2,y2), c(1,7))) {
         isrochade <- "0-0"
         pos[1,6] <- "WR"
         pos[1,8] <- ""
         .drawsquare(1, 8)
         .drawsquare(1, 6)
         .drawpiece(1, 6, "WR")
      }
      if (identical(c(x1,y1), c(1,5)) && pos[x1,y1] == "WK" && identical(c(x2,y2), c(1,3))) {
         isrochade <- "0-0-0"
         pos[1,4] <- "WR"
         pos[1,1] <- ""
         .drawsquare(1, 1)
         .drawsquare(1, 4)
         .drawpiece(1, 4, "WR")
      }
      if (identical(c(x1,y1), c(8,5)) && pos[x1,y1] == "BK" && identical(c(x2,y2), c(8,7))) {
         isrochade <- "0-0"
         pos[8,6] <- "BR"
         pos[8,8] <- ""
         .drawsquare(8, 8)
         .drawsquare(8, 6)
         .drawpiece(8, 6, "BR")
      }
      if (identical(c(x1,y1), c(8,5)) && pos[x1,y1] == "BK" && identical(c(x2,y2), c(8,3))) {
         isrochade <- "0-0-0"
         pos[8,4] <- "BR"
         pos[8,1] <- ""
         .drawsquare(8, 1)
         .drawsquare(8, 4)
         .drawpiece(8, 4, "BR")
      }

      # check for en passent

      if (identical(attr(pos, "ispp"), "b") && pos[x1,y1] == "WP" && x1 == 5 && attr(pos, "y1") == y2) {
         isenpassent <- "w"
         pos[5,y2] <- ""
         .drawsquare(5, y2)
      }
      if (identical(attr(pos, "ispp"), "w") && pos[x1,y1] == "BP" && x1 == 4 && attr(pos, "y1") == y2) {
         isenpassent <- "b"
         pos[4,y2] <- ""
         .drawsquare(4, y2)
      }

   }

   .drawsquare(x1, y1)
   .drawsquare(x2, y2)

   if (flip) {
      .drawpiece(x2, y2, pos[9-x1,9-y1])
      if (pos[9-x2,9-y2] != "" || isenpassent != "") {
         playsound(system.file("sounds", "capture.ogg", package="chesstrainer"), volume=volume)
      } else {
         playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
      }
      iscapture <- pos[9-x2,9-y2] != "" || isenpassent != ""
      piece1 <- ifelse(substr(pos[9-x1,9-y1], 2, 2) == "P", "", substr(pos[9-x1,9-y1], 2, 2))
      piece2 <- ifelse(substr(pos[9-x2,9-y2], 2, 2) == "P", "", substr(pos[9-x2,9-y2], 2, 2))
      pos[9-x2,9-y2] <- pos[9-x1,9-y1]
      pos[9-x1,9-y1] <- ""
   } else {
      .drawpiece(x2, y2, pos[x1,y1])
      if (pos[x2,y2] != "" || isenpassent != "") {
         playsound(system.file("sounds", "capture.ogg", package="chesstrainer"), volume=volume)
      } else {
         playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
      }
      iscapture <- pos[x2,y2] != "" || isenpassent != ""
      piece1 <- ifelse(substr(pos[x1,y1], 2, 2) == "P", "", substr(pos[x1,y1], 2, 2))
      piece2 <- ifelse(substr(pos[x2,y2], 2, 2) == "P", "", substr(pos[x2,y2], 2, 2))
      pos[x2,y2] <- pos[x1,y1]
      pos[x1,y1] <- ""
   }

   if (flip) {
      if (identical(isrochade, "")) {
         move <- paste0(piece1, letters[9-y1], 9-x1, ifelse(iscapture, "x", "-"), piece2, letters[9-y2], 9-x2)
      } else {
         move <- isrochade
      }
   } else {
      if (identical(isrochade, "")) {
         move <- paste0(piece1, letters[y1], x1, ifelse(iscapture, "x", "-"), piece2, letters[y2], x2)
      } else {
         move <- isrochade
      }
   }

   if (verbose) {
      cat("Move: ", move, "\n\n", sep="")
      printpos <- pos
      printpos[printpos == ""] <- "."
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

   attr(pos, "move") <- move
   attr(pos, "ispp") <- ispp
   attr(pos, "y1") <- y1
   attr(pos, "rochade") <- rochade

   if (pawnmove || iscapture) {
      attr(pos, "moves50") <- 0
   } else {
      attr(pos, "moves50") <- attr(pos, "moves50") + 1
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
   if (x <= 1 || x >= 10 || y <= 1 || y >= 10) {
      rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=.get("col.bg"), ljoin=1)
   } else {
      rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), ljoin=1)
   }
}

.addcircle <- function(x, y, lwd)
   symbols(y+0.5, x+0.5, circles=0.45, inches=FALSE, lwd=lwd+2, fg=.get("col.annot"), add=TRUE)

.drawsquare <- function(x, y, col=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")))
   rect(y, x, y+1, x+1, col=col, border=NA)

.texttop <- function(text) {
   rect(-2, 9.2, 12, 10, col=.get("col.bg"), border=NA)
   if (!identical(text, "")) {
      text <- gsub("\\\\n", "\n", text)
      text(5, 9.5, text, cex=.get("cex.top"), col=.get("col.top"))
   }
   return(text)
}

.drawsideindicator <- function(sidetoplay, flip, clear=TRUE) {

   col.side.w <- .get("col.side.w")
   col.side.b <- .get("col.side.b")

   indsize <- 0.18

   if (clear) {
      col.bg <- .get("col.bg")
      rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col=col.bg)
      rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col=col.bg)
   }

   if (flip) {
      if (sidetoplay == "w") {
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col=col.side.w)
      } else {
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col=col.side.b)
      }
   } else {
      if (sidetoplay == "w") {
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col=col.side.w)
      } else {
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col=col.side.b)
      }
   }

}

.draweval <- function(x=NA_real_, flip=FALSE, clear=FALSE, eval=TRUE) {

   if (eval) {
      eval <- x
   } else {
      return()
   }

   col.bg <- .get("col.bg")
   indsize <- 0.18

   if (clear) {
      rect(9.75, 1, 9.75+indsize, 9, border=NA, col=col.bg)
      return()
   }

   col.side.w <- .get("col.side.w")
   col.side.b <- .get("col.side.b")

   maxeval <- 5
   eval.sav <- eval
   eval <- min(max(eval, -maxeval), maxeval)
   eval <- (eval + maxeval) / (2*maxeval) * 8 + 1

   if (is.na(eval)) {
      cols <- col2rgb(col.bg)
      rect(9.75, 1, 9.75+indsize, 9, border=NA, col=rgb(cols[1], cols[2], cols[3], 120, maxColorValue=255))
   } else {
      if (flip) {
         rect(9.75, 10-eval, 9.75+indsize, 9, border=NA, col=col.side.w)
         rect(9.75, 1, 9.75+indsize, 10-eval, border=NA, col=col.side.b)
         text(9.75 + indsize/2, 1.1, formatC(eval.sav, format="f", digits=1), cex=.get("cex.eval"))
      } else {
         rect(9.75, eval, 9.75+indsize, 9, border=NA, col=col.side.b)
         rect(9.75, 1, 9.75+indsize, eval, border=NA, col=col.side.w)
         text(9.75 + indsize/2, 1.1, formatC(eval.sav, format="f", digits=1), cex=.get("cex.eval"))
      }
   }

}

.redrawall <- function(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random) {

   .drawboard(pos, flip)
   .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
   .texttop(texttop)

   if (mode == "add")
      .drawsideindicator(sidetoplay, flip)

   invisible()

}

.quit <- function() {

   #return()

   cols <- col2rgb(.get("col.bg"))

   steps <- 50

   for (i in 1:steps) {
      rect(-1, -1, 11, 11, col=rgb(cols[1], cols[2], cols[3], 60, maxColorValue=255), border=NA)
      Sys.sleep(1/steps)
   }

   rect(-1, -1, 11, 11, col=.get("col.bg"), border=NA)

}
