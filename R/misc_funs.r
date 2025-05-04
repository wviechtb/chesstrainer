.is.even <- function(x) x %% 2 == 0

.get <- function(x)
   unname(get(x, envir=.chesstrainer))

.is.null <- function(x) {

   if (length(x) == 0L)
      return(logical(0))

   return(sapply(x, is.null))

}

.calcsquare <- function(x, y, plt) {
   square.x <- floor((y - plt[3]) / (plt[4] - plt[3]) * 8 + 1)
   square.y <- floor((x - plt[1]) / (plt[2] - plt[1]) * 8 + 1)
   square.x[square.x < 1] <- 1
   square.x[square.x > 8] <- 8
   square.y[square.y < 1] <- 1
   square.y[square.y > 8] <- 8
   return(c(square.x, square.y))
}

.pickpromotionpiece <- function(buttons, x, y) {
   plt <- par("plt")
   squares <- .calcsquare(x,y,plt)
   pos.x <- squares[1]
   pos.y <- squares[2]
   return(c(pos.x,pos.y))
}

.genfen <- function(pos, flip, sidetoplay, i) {

   # change piece abbreviations to those used by FEN

   pos[pos == "WR"] <- "R"
   pos[pos == "WN"] <- "N"
   pos[pos == "WB"] <- "B"
   pos[pos == "WQ"] <- "Q"
   pos[pos == "WK"] <- "K"
   pos[pos == "WP"] <- "P"
   pos[pos == "BR"] <- "r"
   pos[pos == "BN"] <- "n"
   pos[pos == "BB"] <- "b"
   pos[pos == "BQ"] <- "q"
   pos[pos == "BK"] <- "k"
   pos[pos == "BP"] <- "p"
   pos[pos == ""] <- "."

   # generate the piece placement data

   fen <- c()

   for (j in 8:1) {
      fen[9-j] <- paste0(pos[j,], collapse="")
      matches <- gregexpr("\\.+", fen[9-j])
      regmatches(fen[9-j], matches) <- lapply(regmatches(fen[9-j], matches), function(dot_seq) sapply(dot_seq, nchar))
   }

   fen <- paste0(fen, collapse="/")

   # add the active color

   fen <- paste(fen, sidetoplay)

   # add castling availability

   rochade <- attr(pos,"rochade")

   if (identical(rochade, rep(FALSE,4)) || is.null(rochade)) {
      rochade <- "-"
   } else {
      rochade <- paste0(c("K", "Q", "k", "q")[rochade], collapse="")
   }

   fen <- paste(fen, rochade)

   # add en passant target square

   ispp <- attr(pos,"ispp")

   if (is.null(ispp))
      ispp <- ""

   if (!identical(ispp, "")) {
      if (ispp == "w") {
         if (flip) {
            enpassant <- paste0(letters[8:1][attr(pos,"y1")], 3, collapse="")
         } else {
            enpassant <- paste0(letters[1:8][attr(pos,"y1")], 3, collapse="")
         }
      } else {
         if (flip) {
            enpassant <- paste0(letters[8:1][attr(pos,"y1")], 6, collapse="")
         } else {
            enpassant <- paste0(letters[1:8][attr(pos,"y1")], 6, collapse="")
         }
      }
   } else {
      enpassant <- "-"
   }

   fen <- paste(fen, enpassant)

   # add halfmove clock

   fen <- paste(fen, attr(pos,"moves50"))

   # add fullmove number

   fen <- paste(fen, (i+1) %/% 2)

   return(fen)

}

.parseannot <- function(x, cols) {

   if (is.null(x) || identical(x, ""))
      return(matrix(nrow=0, ncol=cols))

   x <- strsplit(x, ";", fixed=TRUE)[[1]]
   x <- gsub("(", "", x, fixed=TRUE)
   x <- gsub(")", "", x, fixed=TRUE)
   x <- sapply(x, strsplit, ",", fixed=TRUE)
   x <- lapply(x, as.numeric)
   x <- do.call(rbind, x)
   x <- unname(x)
   return(x)

}

.parsesfmove <- function(move, pos, flip, evalval, rename=TRUE) {

   lang <- .get("lang")

   move <- strsplit(move, "")[[1]]

   if (flip) {
      letters8 <- letters[8:1]
      x1 <- as.numeric(which(move[2] == 8:1))
      y1 <- as.numeric(which(move[1] == letters8))
      x2 <- as.numeric(which(move[4] == 8:1))
      y2 <- as.numeric(which(move[3] == letters8))
      piece <- substr(pos[9-x1,9-y1], 2, 2)
   } else {
      letters8 <- letters[1:8]
      x1 <- as.numeric(which(move[2] == 1:8))
      y1 <- as.numeric(which(move[1] == letters8))
      x2 <- as.numeric(which(move[4] == 1:18))
      y2 <- as.numeric(which(move[3] == letters8))
      piece <- substr(pos[x1,y1], 2, 2)
   }

   # check for rochade

   rochade <- ""

   if (flip) {
      if (identical(c(x1,y1), c(9-1,9-5)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(9-1,9-7)))
         rochade <- "0-0"
      if (identical(c(x1,y1), c(9-1,9-5)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(9-1,9-3)))
         rochade <- "0-0-0"
      if (identical(c(x1,y1), c(9-8,9-5)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(9-8,9-7)))
         rochade <- "0-0"
      if (identical(c(x1,y1), c(9-8,9-5)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(9-8,9-3)))
         rochade <- "0-0-0"
   } else {
      if (identical(c(x1,y1), c(1,5)) && pos[x1,y1] == "WK" && identical(c(x2,y2), c(1,7)))
         rochade <- "0-0"
      if (identical(c(x1,y1), c(1,5)) && pos[x1,y1] == "WK" && identical(c(x2,y2), c(1,3)))
         rochade <- "0-0-0"
      if (identical(c(x1,y1), c(8,5)) && pos[x1,y1] == "BK" && identical(c(x2,y2), c(8,7)))
         rochade <- "0-0"
      if (identical(c(x1,y1), c(8,5)) && pos[x1,y1] == "BK" && identical(c(x2,y2), c(8,3)))
         rochade <- "0-0-0"
   }

   # check if a piece was captured (either the target square is not empty or a pawn moved diagonally)

   if (flip) {
      iscapture <- pos[9-x2,9-y2] != "" || (piece == "P" && y1 != y2)
   } else {
      iscapture <- pos[x2,y2] != "" || (piece == "P" && y1 != y2)
   }

   # check for pawn promotion

   if (length(move) == 5L) {
      promotiontxt <- paste0("=", toupper(move[5]))
   } else {
      promotiontxt <- ""
   }

   # construct the move text in long algebraic notation

   if (identical(rochade, "")) {
      txt <- paste0(piece, move[1], move[2], ifelse(iscapture, "x", "-"), move[3], move[4], promotiontxt, collapse="")
   } else {
      txt <- rochade
   }

   # rename pieces for other languages

   if (rename) {
      if (lang == "de") {
         txt <- sub("Q", "D", txt, fixed=TRUE)
         txt <- sub("B", "L", txt, fixed=TRUE)
         txt <- sub("N", "S", txt, fixed=TRUE)
         txt <- sub("R", "T", txt, fixed=TRUE)
      }
   }

   # remove P for pawns

   txt <- sub("P", "", txt, fixed=TRUE)

   # add evaluation

   if (!is.na(evalval)) {
      evalval <- formatC(evalval, format="f", digits=2, flag="+")
      txt <- paste0(txt, " (", evalval, ")", collapse="")
   }

   out <- list(txt=txt, x1=x1, y1=y1, x2=x2, y2=y2)

   return(out)

}

.printverbose <- function(selected, seqno, filename, lastseq, flip, useflip, replast, oldmode, i,
                          seqname, seqnum, score, played, totalmoves, show, comment, bestmove, evalval,
                          texttop, scoreadd, sidetoplay, givehint1, givehint2, mistake, timetotal,
                          movesplayed, movestoplay, drawcircles, drawarrows, showstartcom, pos) {

   cat("\n")
   cat("selected:     ", selected, "\n")
   cat("seqno:        ", seqno, "\n")
   cat("filename:     ", filename, "\n")
   cat("lastseq:      ", lastseq, "\n")
   cat("flip:         ", flip, "\n")
   cat("useflip:      ", useflip, "\n")
   cat("replast:      ", replast, "\n")
   cat("oldmode:      ", oldmode, "\n")
   cat("i:            ", i, "\n")
   cat("seqname:      ", seqname, "\n")
   cat("seqnum:       ", seqnum, "\n")
   cat("score:        ", score, "\n")
   cat("played:       ", played, "\n")
   cat("totalmoves:   ", totalmoves, "\n")
   cat("show:         ", show, "\n")
   cat("comment:      ", comment, "\n")
   cat("bestmove:     ", bestmove, "\n")
   cat("evalval:      ", evalval, "\n")
   cat("texttop:      ", texttop, "\n")
   cat("scoreadd:     ", scoreadd, "\n")
   cat("sidetoplay:   ", sidetoplay, "\n")
   cat("givehint1:    ", givehint1, "\n")
   cat("givehint2:    ", givehint2, "\n")
   cat("mistake:      ", mistake, "\n")
   cat("timetotal:    ", timetotal, "\n")
   cat("movesplayed:  ", movesplayed, "\n")
   cat("movestoplay:  ", movestoplay, "\n")
   cat("drawcircles:  ", drawcircles, "\n")
   cat("drawarrows:   ", drawarrows, "\n")
   cat("showstartcom: ", showstartcom, "\n")
   cat("\n")

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

   return()

}

.waitforclick <- function()
   getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button, x, y) return(""), onKeybd=function(key) return(""))
