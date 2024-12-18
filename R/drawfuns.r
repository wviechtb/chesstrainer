.drawpiece <- function(x, y, piece) {
   if (piece != "") {
      txt <- paste0("rasterImage(img.", piece, ",", y, ",", x, ",", y+1, ",", x+1, ")")
      eval(parse(text=txt), envir=.chesstrainer)
   }
}

.drawboard <- function(pos, flip=FALSE, newplot=TRUE) {

   bg <- .get("col.bg")
   fg <- .get("col.fg")

   if (dev.cur() == 1L)
      dev.new(bg=bg)

   if (newplot) {

      par(xpd=NA, pty="s", mar=rep(5,4)+0.2, fg=.get("col.fg"), bg=.get("col.bg"))

      mat <- outer(1:8, 1:8, function(x,y) .is.even(x+y))

      image(1:8+0.5, 1:8+0.5, col=c(.get("col.square.l"), .get("col.square.d")), mat, xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n", useRaster=TRUE)

      if (flip) {
         par(mgp=c(3,0.5,0))
         axis(side=1, 1:8+0.5, rev(LETTERS[1:8]), las=1, tick=FALSE, col.axis=fg)
         par(mgp=c(3,0.8,0))
         axis(side=2, 1:8+0.5, rev(1:8),          las=1, tick=FALSE, col.axis=fg)
      } else {
         par(mgp=c(3,0.5,0))
         axis(side=1, 1:8+0.5, LETTERS[1:8],      las=1, tick=FALSE, col.axis=fg)
         par(mgp=c(3,0.8,0))
         axis(side=2, 1:8+0.5, 1:8,               las=1, tick=FALSE, col.axis=fg)
      }

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

.updateboard <- function(pos, move, flip, volume, verbose) {

   x1 <- unname(move[[1]])
   y1 <- unname(move[[2]])
   x2 <- unname(move[[3]])
   y2 <- unname(move[[4]])

   isrochade <- ""
   isenpassent <- ""

   if (flip) {

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

      print(x1)
      print(y1)
      print(x2)
      print(y2)
      print(pos[9-x1,9-y1])
      print(pos[4,y2])
      print(pos[5,y2])

      if (pos[9-x1,9-y1] == "WP" && x1 == 4 && (y2 == y1-1 || y2 == y1+1)) {
         isenpassent <- "w"
         pos[4,y2] <- ""
      }
      if (pos[9-x1,9-y1] == "BP" && x1 == 5 && (y2 == y1-1 || y2 == y1+1)) {
         isenpassent <- "b"
         pos[5,y2] <- ""
      }

   } else {

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

      if (pos[x1,y1] == "WP" && x1 == 5 && (y2 == y1-1 || y2 == y1+1)) {
         isenpassent <- "w"
         pos[5,y2] <- ""
      }
      if (pos[x1,y1] == "BP" && x1 == 4 && (y2 == y1-1 || y2 == y1+1)) {
         isenpassent <- "b"
         pos[4,y2] <- ""
      }

   }

   .drawsquare(x1, y1)
   .drawsquare(x2, y2)

   if (isenpassent == "w") {
      if (flip) {
         .drawsquare(4, y2)
      } else {
         .drawsquare(5, y2)
      }
   }
   if (isenpassent == "b") {
      if (flip) {
         .drawsquare(5, y2)
      } else {
         .drawsquare(4, y2)
      }
   }

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

   if (verbose) {
      if (flip) {
         if (identical(isrochade, "")) {
            cat("Move: ", piece1, letters[9-y1], 9-x1, ifelse(iscapture, "x", "-"), piece2, letters[9-y2], 9-x2, "\n\n", sep="")
         } else {
            cat("Move: ", isrochade, "\n\n", sep="")
         }
      } else {
         if (identical(isrochade, "")) {
            cat("Move: ", piece1, letters[y1], x1, ifelse(iscapture, "x", "-"), piece2, letters[y2], x2, "\n\n", sep="")
         } else {
            cat("Move: ", isrochade, "\n\n", sep="")
         }
      }
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

   return(pos)

}

.addrect <- function(x, y, offset=0.028, col, lwd)
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd, border=col, ljoin=1)

.rmrect <- function(x, y, offset=0.028, lwd)
   rect(y+offset, x+offset, y+1-offset, x+1-offset, lwd=lwd+2, border=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), ljoin=1)

.addcircle <- function(x, y, lwd)
   symbols(y+0.5, x+0.5, circles=0.45, inches=FALSE, lwd=lwd+2, fg=.get("col.annot"), add=TRUE)

.drawsquare <- function(x, y)
   rect(y, x, y+1, x+1, col=ifelse(.is.even(x+y), .get("col.square.d"), .get("col.square.l")), border=NA)

.texttop <- function(text) {
   rect(-2, 9.2, 12, 10, col=.get("col.bg"), border=NA)
   if (!identical(text, "")) {
      text(5, 9.5, text, cex=1.4, col=.get("col.texttop"))
   }
   return(text)
}

.drawsideindicator <- function(i, flip) {

   indsize <- 0.18

   if (flip) {
      if (!.is.even(i)) {
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col="white")
      } else {
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col="black")
      }
   } else {
      if (.is.even(i)) {
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col="black")
      } else {
         rect(9.25, 9.0, 9.25+indsize, 9.0-indsize, border=NA, col=.get("col.bg"))
         rect(9.25, 1.0, 9.25+indsize, 1.0+indsize, border=NA, col="white")
      }
   }

}
