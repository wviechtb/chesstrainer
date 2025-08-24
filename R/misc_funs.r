.waitforclick <- function()
   getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(""), onKeybd=function(key) return(""))

.mousedownfun <- function(button,x,y) {
   if (length(button) == 0L)
      button <- 3
   return(c(x,y,button))
}

.keyfun <- function(key)
   return(key)

.print <- function(x) {
   n <- length(x)
   for (i in 1:n) {
      cat(x[i], "\n")
   }
   return()
}

.is.even <- function(x) x %% 2 == 0

.get <- function(x)
   unname(get(x, envir=.chesstrainer))

.is.null <- function(x) {

   if (length(x) == 0L)
      return(logical(0))

   return(sapply(x, is.null))

}

.findcex <- function(txt, font, x1, x2, y1, y2, mincex=1) {

   rootfun <- function(x)
      x1 + max(strwidth(txt, family=font, cex=x)) - x2

   res1 <- try(uniroot(rootfun, lower=0.01, upper=10), silent=TRUE)

   if (inherits(res1, "try-error"))
      return(mincex)

   ypos <- seq(y1, y2, length.out=length(txt))
   dist <- abs(ypos[2] - ypos[1])

   rootfun <- function(x)
      strheight("A", family=font, cex=x) - 0.8*dist

   res2 <- try(uniroot(rootfun, lower=0.01, upper=10), silent=TRUE)

   if (inherits(res2, "try-error"))
      return(res1$root)

   return(min(mincex, res1$root, res2$root))

}

.mistakediff <- function(x, dbl100pen=20) {
   x <- c(na.omit(x))
   n <- length(x)
   if (n <= 1L) {
      xdiff <- 0
   } else {
      x1 <- x[1:(n-1)]
      x2 <- x[2:n]
      mistake <- x2 > x1
      dbl100 <- x1 == 100 & x2 == 100
      xdiff <- x2 - x1
      xdiff[!mistake] <- 0
      xdiff[dbl100] <- dbl100pen
   }
   return(xdiff)
}

.totaltime <- function(x) {

   days <- x %/% (24 * 3600)
   x <- x %% (24 * 3600)

   hours <- x %/% 3600
   x <- x %% 3600

   minutes <- x %/% 60
   seconds <- x %% 60

   total <- paste0(minutes, " ", .text("minute", minutes == 0 || minutes > 1), ", ",
                  seconds, " ", .text("second", seconds == 0 || seconds > 1))

   if (hours > 0)
      total <- paste0(paste0(hours, " ", .text("hour", hours == 0 || hours > 1)), ", ", total)

   if (days > 0)
      total <- paste0(paste0(days, " ", .text("day", days == 0 || days > 1)), ", ", total)

   return(total)

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

.calcsquarebe <- function(x, y, plt) {
   square.x <- floor((y - plt[3]) / (plt[4] - plt[3]) * 10 + 1)
   square.y <- floor((x - plt[1]) / (plt[2] - plt[1]) * 10 + 1)
   square.x[square.x < 1] <- 1
   square.x[square.x > 10] <- 10
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

   moves50 <- attr(pos,"moves50")

   if (is.null(moves50))
      moves50 <- 0

   fen <- paste(fen, moves50)

   # add fullmove number

   fen <- paste(fen, (i+1) %/% 2)

   return(fen)

}

.validatefen <- function(fen) {

   # split fields and check that there are 6 of them

   fields <- unlist(strsplit(fen, " "))

   if (length(fields) != 6L)
      return(FALSE)

   # piece placement validation

   ranks <- unlist(strsplit(fields[1], "/"))

   if (length(ranks) != 8L)
      return(FALSE)

   # validate each rank

   pieces <- c("p","n","b","r","q","k","P","N","B","R","Q","K")

   for (rank in ranks) {
      squares <- 0
      chars <- unlist(strsplit(rank, ""))
      for (ch in chars) {
         if (ch %in% pieces) {
            squares <- squares + 1
         } else if (grepl("^[1-8]$", ch)) {
            squares <- squares + as.integer(ch)
         } else {
            return(FALSE)
         }
      }
      if (squares != 8)
         return(FALSE)
   }

   # validate side to move

   if (!fields[2] %in% c("w","b"))
      return(FALSE)

   # validate castling availability

   if (!grepl("^(-|[KQkq]+)$", fields[3]))
      return(FALSE)

   if (fields[3] != "-" && length(unique(strsplit(fields[3], "")[[1]])) != nchar(fields[3]))
      return(FALSE)

   # validate en passant target square

   if (!grepl("^(-|[a-h][36])$", fields[4]))
      return(FALSE)

   # validate halfmove clock (integer >= 0)

   if (!grepl("^[0-9]+$", fields[5]))
      return(FALSE)

   # validate fullmove number (integer >= 1)
   if (!grepl("^[1-9][0-9]*$", fields[6]))
      return(FALSE)

   return(TRUE)

}

.fentopos <- function(fen) {

   pos <- matrix("", nrow=8, ncol=8)

   fields <- unlist(strsplit(fen, " "))
   ranks <- unlist(strsplit(fields[1], "/"))

   pieces <- c("p","n","b","r","q","k","P","N","B","R","Q","K")

   for (i in 1:8) {
      chars <- unlist(strsplit(ranks[i], ""))
      j <- 0
      for (ch in chars) {
         if (grepl("^[1-8]$", ch)) {
            j <- j + as.integer(ch)
         }
         if (ch %in% pieces) {
            j <- j + 1
            pos[9-i,j] <- ch
         }
      }
   }

   pos[pos == "p"] <- "BP"
   pos[pos == "n"] <- "BN"
   pos[pos == "b"] <- "BB"
   pos[pos == "r"] <- "BR"
   pos[pos == "q"] <- "BQ"
   pos[pos == "k"] <- "BK"
   pos[pos == "P"] <- "WP"
   pos[pos == "N"] <- "WN"
   pos[pos == "B"] <- "WB"
   pos[pos == "R"] <- "WR"
   pos[pos == "Q"] <- "WQ"
   pos[pos == "K"] <- "WK"

   pos <- rbind("", pos, "")
   pos <- cbind("", pos, "")
   rownames(pos) <- 0:9
   colnames(pos) <- 0:9
   pos[1,3:8]  <- c("WK","WQ","WR","WB","WN","WP")
   pos[10,3:8] <- c("BK","BQ","BR","BB","BN","BP")

   attr(pos,"rochade") <- fields[3]

   return(list(pos=pos, sidetoplay=fields[2]))

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

.parsebestmove <- function(move, pos, flip, evalval, i, sidetoplay, rename, returnline, hintdepth) {

   lang <- .get("lang")

   move <- strsplit(move, "")

   tmp <- pos

   moves <- length(move)
   moves <- min(moves, hintdepth)

   if (!returnline)
      moves <- 1

   for (j in 1:moves) {

      if (flip) {
         letters8 <- letters[8:1]
         x1 <- as.numeric(which(move[[j]][2] == 8:1))
         y1 <- as.numeric(which(move[[j]][1] == letters8))
         x2 <- as.numeric(which(move[[j]][4] == 8:1))
         y2 <- as.numeric(which(move[[j]][3] == letters8))
         piece <- substr(tmp[9-x1,9-y1], 2, 2)
      } else {
         letters8 <- letters[1:8]
         x1 <- as.numeric(which(move[[j]][2] == 1:8))
         y1 <- as.numeric(which(move[[j]][1] == letters8))
         x2 <- as.numeric(which(move[[j]][4] == 1:18))
         y2 <- as.numeric(which(move[[j]][3] == letters8))
         piece <- substr(tmp[x1,y1], 2, 2)
      }

      if (j == 1) {
         bestx1 <- x1
         besty1 <- y1
         bestx2 <- x2
         besty2 <- y2
      }

      # check for rochade

      rochade <- ""

      if (flip) {
         if (identical(c(x1,y1), c(8,4)) && tmp[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(8,2)))
            rochade <- "0-0"
         if (identical(c(x1,y1), c(8,4)) && tmp[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(8,6)))
            rochade <- "0-0-0"
         if (identical(c(x1,y1), c(1,4)) && tmp[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(1,2)))
            rochade <- "0-0"
         if (identical(c(x1,y1), c(1,4)) && tmp[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(1,6)))
            rochade <- "0-0-0"
      } else {
         if (identical(c(x1,y1), c(1,5)) && tmp[x1,y1] == "WK" && identical(c(x2,y2), c(1,7)))
            rochade <- "0-0"
         if (identical(c(x1,y1), c(1,5)) && tmp[x1,y1] == "WK" && identical(c(x2,y2), c(1,3)))
            rochade <- "0-0-0"
         if (identical(c(x1,y1), c(8,5)) && tmp[x1,y1] == "BK" && identical(c(x2,y2), c(8,7)))
            rochade <- "0-0"
         if (identical(c(x1,y1), c(8,5)) && tmp[x1,y1] == "BK" && identical(c(x2,y2), c(8,3)))
            rochade <- "0-0-0"
      }

      # check if a piece was captured (either the target square is not empty or a pawn moved diagonally, which also covers en passant)

      if (flip) {
         iscapture <- tmp[9-x2,9-y2] != "" || (piece == "P" && y1 != y2)
      } else {
         iscapture <- tmp[x2,y2] != "" || (piece == "P" && y1 != y2)
      }

      # check for pawn promotion

      if (length(move[[j]]) == 5L) {
         promotiontxt <- paste0("=", toupper(move[[j]][5]))
      } else {
         promotiontxt <- ""
      }

      # determine if the move results in a check

      tmp.next <- .updateboard(tmp, move=data.frame(x1, y1, x2, y2, NA, ifelse(promotiontxt=="", NA, promotiontxt)), flip=flip, autoprom=TRUE, volume=0, verbose=FALSE, draw=FALSE)
      ischeck <- any(attr(tmp.next,"ischeck"))

      # construct the move text in long algebraic notation

      if (identical(rochade, "")) {
         txt.move <- paste0(piece, move[[j]][1], move[[j]][2], ifelse(iscapture, "x", "-"), move[[j]][3], move[[j]][4], promotiontxt, ifelse(ischeck, "+", ""), collapse="")
      } else {
         txt.move <- paste0(rochade, ifelse(ischeck, "+", ""))
      }

      # rename pieces for other languages

      if (rename) {
         #if (lang == "de") {
         #   txt.move <- sub("Q", "D", txt.move, fixed=TRUE)
         #   txt.move <- sub("B", "L", txt.move, fixed=TRUE)
         #   txt.move <- sub("N", "S", txt.move, fixed=TRUE)
         #   txt.move <- sub("R", "T", txt.move, fixed=TRUE)
         #}
         txt.move <- sub("K", "\U0000265A", txt.move, fixed=TRUE)
         txt.move <- sub("Q", "\U0000265B", txt.move, fixed=TRUE)
         txt.move <- sub("R", "\U0000265C", txt.move, fixed=TRUE)
         txt.move <- sub("B", "\U0000265D", txt.move, fixed=TRUE)
         txt.move <- sub("N", "\U0000265E", txt.move, fixed=TRUE)
      }

      # remove P for pawns

      txt.move <- sub("P", "", txt.move, fixed=TRUE)

      # construct text

      if (returnline) {

         movenum <- (i+j) %/% 2

         if (j == 1) {
            if (sidetoplay=="w") {
               txt.move <- paste0(movenum, ". ", txt.move, collapse="")
            } else {
               txt.move <- paste0(movenum, "... ", txt.move, collapse="")
            }
         } else {
            if (.is.even(i+j)) {
               txt.move <- paste0(" ", movenum, ". ", txt.move, collapse="")
            } else {
               txt.move <- txt.move
            }
         }

         if (is.na(evalval)) {
            txt.eval <- ""
         } else {
            txt.eval <- paste0(formatC(evalval, format="f", digits=ifelse(abs(evalval) >= 99.9, 1, 2), flag="+"), " ", collapse="")
         }

         if (j == 1)
            txt <- txt.eval

         txt <- paste0(txt, " ", txt.move, collapse=" ")

         tmp <- tmp.next

      } else {

         txt <- txt.move

      }

   }

   out <- list(txt=txt, x1=bestx1, y1=besty1, x2=bestx2, y2=besty2)

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
   cat("bestmove:     ", sapply(bestmove, head, 1), "\n")
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

   if (TRUE) {
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
   }

   if (flip) {
      print(printpos[,8:1], quote=FALSE)
   } else {
      print(printpos[8:1,], quote=FALSE)
   }

   return()

}

.islegal <- function(x1, y1, x2, y2, pos, flip, sidetoplay) {

   islegal <- FALSE

   # get the piece moved and the piece on the target square ("" if the target square is empty)
   if (flip) {
      piece <- pos[9-x1,9-y1]
      target <- pos[9-x2,9-y2]
   } else {
      piece <- pos[x1,y1]
      target <- pos[x2,y2]
   }

   # can never capture a king
   if (target %in% c("WK","BK"))
      return(FALSE)

   # check if moving the wrong color
   color <- tolower(substr(piece, 1, 1))
   if (sidetoplay != color)
      return(FALSE)

   # check if capturing own piece
   targetcolor <- tolower(substr(target, 1, 1)) # if target is "", then this remains ""
   if (sidetoplay == targetcolor)
      return(FALSE)

   # check if king move is legal
   if (piece %in% c("WK","BK")) {
      isrochade <- ""
      rochade <- attr(pos,"rochade")
      if (is.null(rochade))
         rochade <- rep(TRUE, 4)
      if (flip) {
         if (rochade[1] && identical(c(x1,y1), c(8,4)) && piece == "WK" && all(pos[1,6:7] == "") && identical(c(x2,y2), c(8,2)))
            islegal <- TRUE
         if (rochade[2] && identical(c(x1,y1), c(8,4)) && piece == "WK" && all(pos[1,2:4] == "") && identical(c(x2,y2), c(8,6)))
            islegal <- TRUE
         if (rochade[3] && identical(c(x1,y1), c(1,4)) && piece == "BK" && all(pos[8,6:7] == "") && identical(c(x2,y2), c(1,2)))
            islegal <- TRUE
         if (rochade[4] && identical(c(x1,y1), c(1,4)) && piece == "BK" && all(pos[8,2:4] == "") && identical(c(x2,y2), c(1,6)))
            islegal <- TRUE
      } else {
         if (rochade[1] && identical(c(x1,y1), c(1,5)) && piece == "WK" && all(pos[1,6:7] == "") && identical(c(x2,y2), c(1,7)))
            islegal <- TRUE
         if (rochade[2] && identical(c(x1,y1), c(1,5)) && piece == "WK" && all(pos[1,2:4] == "") && identical(c(x2,y2), c(1,3)))
            islegal <- TRUE
         if (rochade[3] && identical(c(x1,y1), c(8,5)) && piece == "BK" && all(pos[8,6:7] == "") && identical(c(x2,y2), c(8,7)))
            islegal <- TRUE
         if (rochade[4] && identical(c(x1,y1), c(8,5)) && piece == "BK" && all(pos[8,2:4] == "") && identical(c(x2,y2), c(8,3)))
            islegal <- TRUE
      }
      if (abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1)
         islegal <- TRUE
   }

   # check if bishop move is legal
   if (piece %in% c("WB","BB")) {
      if (abs(x1 - x2) == abs(y1 - y2)) {
         islegal <- TRUE
         if (flip) {
            inbetween <- pos[cbind((9-x1):(9-x2), (9-y1):(9-y2))]
         } else {
            inbetween <- pos[cbind(x1:x2, y1:y2)]
         }
         inbetween <- inbetween[-c(1,length(inbetween))]
         if (length(inbetween) > 0 && any(inbetween != ""))
            islegal <- FALSE
      }
   }

   # check if rook move is legal
   if (piece %in% c("WR","BR")) {
      if (x1 == x2 || y1 == y2) {
         islegal <- TRUE
         if (flip) {
            if (x1 == x2) {
               inbetween <- pos[cbind(9-x1, (9-y1):(9-y2))]
            } else {
               inbetween <- pos[cbind((9-x1):(9-x2), 9-y1)]
            }
         } else {
            if (x1 == x2) {
               inbetween <- pos[cbind(x1, y1:y2)]
            } else {
               inbetween <- pos[cbind(x1:x2, y1)]
            }
         }
         inbetween <- inbetween[-c(1,length(inbetween))]
         if (length(inbetween) > 0 && any(inbetween != ""))
            islegal <- FALSE
      }
   }

   # check if queen move is legal
   if (piece %in% c("WQ","BQ")) {
      if ((x1 == x2 || y1 == y2) || abs(x1 - x2) == abs(y1 - y2)) {
         islegal <- TRUE
         if (flip) {
            if ((x1 == x2 || y1 == y2)) {
               if (x1 == x2) {
                  inbetween <- pos[cbind(9-x1, (9-y1):(9-y2))]
               } else {
                  inbetween <- pos[cbind((9-x1):(9-x2), 9-y1)]
               }
            } else {
               inbetween <- pos[cbind((9-x1):(9-x2), (9-y1):(9-y2))]
            }
         } else {
            if ((x1 == x2 || y1 == y2)) {
               if (x1 == x2) {
                  inbetween <- pos[cbind(x1, y1:y2)]
               } else {
                  inbetween <- pos[cbind(x1:x2, y1)]
               }
            } else {
               inbetween <- pos[cbind(x1:x2, y1:y2)]
            }
         }
         inbetween <- inbetween[-c(1,length(inbetween))]
         if (length(inbetween) > 0 && any(inbetween != ""))
            islegal <- FALSE
      }
   }

   # check if knight move is legal
   if (piece %in% c("WN","BN")) {
      kmove <- c(abs(x1 - x2), abs(y1 - y2))
      if (identical(kmove, c(1,2)) || identical(kmove, c(2,1)))
         islegal <- TRUE
   }

   # check if white pawn move is legal
   if (piece == "WP") {
      if (flip) {
         if (y1 == y2 && (x1 - x2 == 1 || (x1 - x2 == 2 && x1 == 7 && pos[3,9-y1] == ""))) # regular pawn move
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x1 - x2 == 1 && target != "") # pawn capture
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x1 - x2 == 1 && x1 == 4 && pos[9-x1,9-y2] == "BP" && identical(attr(pos,"ispp"), "b") && identical(attr(pos,"y1"), y2)) # en passant
            islegal <- TRUE
      } else {
         if (y1 == y2 && (x2 - x1 == 1 || (x2 - x1 == 2 && x1 == 2 && pos[3,y1] == ""))) # regular pawn move
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x2 - x1 == 1 && target != "") # pawn capture
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x2 - x1 == 1 && x1 == 5 && pos[x1,y2] == "BP" && identical(attr(pos,"ispp"), "b") && identical(attr(pos,"y1"), y2)) # en passant
            islegal <- TRUE
      }
   }

   # check if black pawn move is legal
   if (piece == "BP") {
      if (flip) {
         if (y1 == y2 && (x2 - x1 == 1 || (x2 - x1 == 2 && x1 == 2 && pos[6,9-y1] == ""))) # regular pawn move
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x2 - x1 == 1 && target != "") # pawn capture
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x2 - x1 == 1 && x1 == 5 && pos[9-x1,9-y2] == "WP" && identical(attr(pos,"ispp"), "w") && identical(attr(pos,"y1"), y2)) # en passant
            islegal <- TRUE
      } else {
         if (y1 == y2 && (x1 - x2 == 1 || (x1 - x2 == 2 && x1 == 7 && pos[6,y1] == ""))) # regular pawn move
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x1 - x2 == 1 && target != "") # pawn capture
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x1 - x2 == 1 && x1 == 4 && pos[x1,y2] == "WP" && identical(attr(pos,"ispp"), "w") && identical(attr(pos,"y1"), y2)) # en passant
            islegal <- TRUE
      }
   }

   return(islegal)

}

.isattacked <- function(pos, xy, attackcolor) {

   isattacked <- FALSE

   x <- xy[1]
   y <- xy[2]

   if (is.null(x) || is.null(y) || is.na(x) || is.na(y))
      return(isattacked)

   attackcolor <- toupper(attackcolor)

   #########################################################################

   if (!isattacked) {
      attacker <- paste0(attackcolor,"P")
      if (attackcolor == "W") {
         coords <- cbind(x-1, c(y-1,y+1))
      } else {
         coords <- cbind(x+1, c(y-1,y+1))
      }
      coords <- coords[coords[,1] >= 1 & coords[,1] <= 8 & coords[,2] >= 1 & coords[,2] <= 8,,drop=FALSE]
      if (any(pos[coords] == attacker))
         isattacked <- TRUE
   }

   if (!isattacked) {
      kingoffsets <- matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), ncol=2, byrow=TRUE)
      attacker <- paste0(attackcolor,"K")
      coords <- sweep(kingoffsets, 2, xy, "+")
      coords <- coords[coords[,1] >= 1 & coords[,1] <= 8 & coords[,2] >= 1 & coords[,2] <= 8,,drop=FALSE]
      if (any(pos[coords] == attacker))
         isattacked <- TRUE
   }

   if (!isattacked) {
      knightoffsets <- matrix(c(-2,-1, -2,1, -1,-2, -1,2, 1,-2, 1,2, 2,-1, 2,1), ncol=2, byrow=TRUE)
      attacker <- paste0(attackcolor,"N")
      coords <- sweep(knightoffsets, 2, xy, "+")
      coords <- coords[coords[,1] >= 1 & coords[,1] <= 8 & coords[,2] >= 1 & coords[,2] <= 8,,drop=FALSE]
      if (any(pos[coords] == attacker))
         isattacked <- TRUE
   }

   if (!isattacked) {
      attacker <- paste0(attackcolor,"B")
      directions <- list(c(-1,-1), c(-1,1), c(1,-1), c(1,1))
      for (dir in directions) {
         r <- x + dir[1]
         c <- y + dir[2]
         while (r >= 1 && r <= 8 && c >= 1 && c <= 8) {
            piece <- pos[r,c]
            if (piece != "") {
               if (piece == attacker) {
                  isattacked <- TRUE
                  break
               } else {
                  break
               }
            }
            r <- r + dir[1]
            c <- c + dir[2]
         }
      }
   }

   if (!isattacked) {
      attacker <- paste0(attackcolor,"R")
      directions <- list(c(-1,0), c(1,0), c(0,-1), c(0,1))
      for (dir in directions) {
         r <- x + dir[1]
         c <- y + dir[2]
         while (r >= 1 && r <= 8 && c >= 1 && c <= 8) {
            piece <- pos[r,c]
            if (piece != "") {
               if (piece == attacker) {
                  isattacked <- TRUE
                  break
               } else {
                  break
               }
            }
            r <- r + dir[1]
            c <- c + dir[2]
         }
      }
   }

   if (!isattacked) {
      attacker <- paste0(attackcolor,"Q")
      directions <- list(c(-1,0), c(1,0), c(0,-1), c(0,1), c(-1,-1), c(-1,1), c(1,-1), c(1,1))
      for (dir in directions) {
         r <- x + dir[1]
         c <- y + dir[2]
         while (r >= 1 && r <= 8 && c >= 1 && c <= 8) {
            piece <- pos[r,c]
            if (piece != "") {
               if (piece == attacker) {
                  isattacked <- TRUE
                  break
               } else {
                  break
               }
            }
            r <- r + dir[1]
            c <- c + dir[2]
         }
      }
   }

   #########################################################################

   return(isattacked)

}
