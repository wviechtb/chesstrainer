.waitforclick <- function()
   getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(""), onKeybd=function(key) return(""))

.mousedownfun <- function(button,x,y) {
   if (length(button) == 0L)
      button <- 3
   return(c(x,y,button[1]))
}

.keyfun <- function(key)
   return(key)

.print <- function(x, addn=FALSE) {
   n <- length(x)
   for (i in 1:n) {
      cat(x[i], ifelse(addn, "\n\n", "\n"))
   }
   return()
}

.is.even <- function(x) x %% 2 == 0

.get <- function(x) {
   if (exists(x,envir=.chesstrainer)) {
      unname(get(x, envir=.chesstrainer))
   } else {
      return(NULL)
   }
}

.is.null <- function(x) {

   if (length(x) == 0L)
      return(logical(0))

   return(sapply(x, is.null))

}

.is.start.pos <- function(pos) {
   pos <- unname(pos[1:8,1:8])
   start.pos <- .get("pos")
   start.pos <- unname(start.pos[1:8,1:8])
   return(identical(pos, start.pos))
}

.flush <- function() {
   flush <- isTRUE(.get("flush"))
   if (flush)
      cat(c("\033[2J","\033[0;0H"))
   return(flush)
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
   #square.x[square.x < 1] <- 1
   #square.x[square.x > 8] <- 8
   #square.y[square.y < 1] <- 1
   #square.y[square.y > 8] <- 8
   return(c(square.x, square.y))
}

.calcxy <- function(x, y, plt) {
   x <- (x - plt[1]) / (plt[2] - plt[1]) * 8 + 1
   y <- (y - plt[3]) / (plt[4] - plt[3]) * 8 + 1
   return(c(x, y))
}

.calcsquarebe <- function(x, y, plt) {
   square.x <- floor((y - plt[3]) / (plt[4] - plt[3]) * 10 + 1)
   square.y <- floor((x - plt[1]) / (plt[2] - plt[1]) * 10 + 1)
   #square.x[square.x < 1] <- 1
   #square.x[square.x > 10] <- 10
   return(c(square.x, square.y))
}

.pickpromotionpiece <- function(buttons, x, y) {
   plt <- par("plt")
   squares <- .calcsquare(x,y,plt)
   pos.x <- squares[1]
   pos.y <- squares[2]
   return(c(pos.x,pos.y))
}

.genfen <- function(pos, flip, sidetoplay, sidetoplaystart, i) {

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

   fen <- NULL

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

   if (is.null(rochade) || identical(rochade, rep(FALSE,4)) || length(rochade) == 0L) {
      rochade <- "-"
   } else {
      rochade <- paste0(c("K", "Q", "k", "q")[rochade], collapse="")
   }

   fen <- paste(fen, rochade)

   # add en passant target square

   ispp <- attr(pos,"ispp")

   if (is.null(ispp) || length(ispp) == 0L)
      ispp <- ""

   if (identical(ispp, "")) {
      enpassant <- "-"
   } else {
      y1 <- attr(pos,"y1")
      if (ispp == "w") {
         if (flip) {
            if (pos[4,max(1,9-y1-1)] == "p" || pos[4,min(8,9-y1+1)] == "p") {
               enpassant <- paste0(letters[8:1][y1], 3, collapse="")
            } else {
               enpassant <- "-"
            }
         } else {
            if (pos[4,max(1,y1-1)] == "p" || pos[4,min(8,y1+1)] == "p") {
               enpassant <- paste0(letters[1:8][y1], 3, collapse="")
            } else {
               enpassant <- "-"
            }
         }
      } else {
         if (flip) {
            if (pos[5,max(1,9-y1-1)] == "P" || pos[5,min(8,9-y1+1)] == "P") {
               enpassant <- paste0(letters[8:1][y1], 6, collapse="")
            } else {
               enpassant <- "-"
            }
         } else {
            if (pos[5,max(1,y1-1)] == "P" || pos[5,min(8,y1+1)] == "P") {
               enpassant <- paste0(letters[1:8][y1], 6, collapse="")
            } else {
               enpassant <- "-"
            }
         }
      }
   }

   fen <- paste(fen, enpassant)

   # add halfmove clock

   moves50 <- attr(pos,"moves50")

   if (is.null(moves50) || length(moves50) == 0L)
      moves50 <- 0

   fen <- paste(fen, moves50)

   # add fullmove number

   fen <- paste(fen, (i + ifelse(sidetoplaystart=="b", 1, 0) + 1) %/% 2)

   return(fen)

}

.expandfen <- function(fen) {

   fields <- unlist(strsplit(fen, " ", fixed=TRUE))

   norochade <- FALSE

   if (length(fields) == 1L) {

      # if there is only one field, then it is only field 1; fill in the rest with default values

      fen <- paste(fen, "w - - 0 1")
      norochade <- TRUE

   }

   return(list(fen=fen, norochade=norochade))

}

.validatefen <- function(fen) {

   # split fields and check that there are 6 of them

   fields <- unlist(strsplit(fen, " ", fixed=TRUE))

   if (length(fields) != 6L)
      return(FALSE)

   # piece placement validation

   ranks <- unlist(strsplit(fields[1], "/", fixed=TRUE))

   if (length(ranks) != 8L)
      return(FALSE)

   # validate each rank

   pieces <- c("p","n","b","r","q","k","P","N","B","R","Q","K")

   for (rank in ranks) {
      squares <- 0
      chars <- unlist(strsplit(rank, "", fixed=TRUE))
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

   if (fields[3] != "-" && length(unique(strsplit(fields[3], "", fixed=TRUE)[[1]])) != nchar(fields[3]))
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

.fenpart <- function(fen, parts=1:3)
   paste(strsplit(fen, " ", fixed=TRUE)[[1]][parts], collapse=" ")

.fentopos <- function(fen) {

   pos <- matrix("", nrow=8, ncol=8)

   fields <- unlist(strsplit(fen, " ", fixed=TRUE))
   ranks <- unlist(strsplit(fields[1], "/", fixed=TRUE))

   pieces <- c("p","n","b","r","q","k","P","N","B","R","Q","K")

   for (i in 1:8) {
      chars <- unlist(strsplit(ranks[i], "", fixed=TRUE))
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

   rochade <- fields[3]

   if (identical(rochade, "-")) {
      rochade <- rep(FALSE, 4)
   } else {
      rochade <- c(grepl("K", rochade), grepl("Q", rochade), grepl("k", rochade), grepl("q", rochade))
   }

   attr(pos,"rochade") <- rochade
   attr(pos,"moves50") <- fields[5]
   attr(pos,"move")    <- NULL

   if (!identical(fields[4], "-")) {
      attr(pos,"ispp") <- ifelse(fields[2] == "w", "b", "w")
      letter <- substr(fields[4], 1, 1)
      y1 <- as.numeric(which(letters[1:8] == letter))
      attr(pos,"y1") <- y1
   }

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

.isglyph <- function(x) {
   x2y2 <- .get("x2y2")
   isTRUE(x != "") && !is.na(x2y2[1])
}

.lan2uci <- function(moves, sidetoplay) {

   moves <- gsub("=([QRBN])", "\\L\\1", moves, perl=TRUE)
   moves <- gsub("[RNBQK+#=x]", "", moves)

   if (any(moves %in% c("0-0", "0-0-0"))) {
      nmoves <- length(moves)
      moveseq <- seq_len(nmoves)
      if (sidetoplay == "w") {
         pos <- moveseq[!.is.even(moveseq)]
         moves[pos] <- sub("0-0",   "e1g1", moves[pos], fixed=TRUE)
         moves[pos] <- sub("0-0-0", "e1c1", moves[pos], fixed=TRUE)
         pos <- moveseq[.is.even(moveseq)]
         moves[pos] <- sub("0-0",   "e8g8", moves[pos], fixed=TRUE)
         moves[pos] <- sub("0-0-0", "e8c8", moves[pos], fixed=TRUE)
      } else {
         pos <- moveseq[!.is.even(moveseq)]
         moves[pos] <- sub("0-0",   "e8g8", moves[pos], fixed=TRUE)
         moves[pos] <- sub("0-0-0", "e8c8", moves[pos], fixed=TRUE)
         pos <- moveseq[.is.even(moveseq)]
         moves[pos] <- sub("0-0",   "e1g1", moves[pos], fixed=TRUE)
         moves[pos] <- sub("0-0-0", "e1c1", moves[pos], fixed=TRUE)
      }
   }

   moves <- gsub("-", "", moves, fixed=TRUE)
   return(moves)

}

.parsemove <- function(move, pos, flip, evalval, i, sidetoplay, rename, returnline, hintdepth, space="", san=NULL) {

   move <- strsplit(move, "", fixed=TRUE)
   nmoves.all <- length(move)
   move <- move[lengths(move) > 0]
   nmoves <- length(move)
   nmoves <- min(nmoves, hintdepth)

   if (returnline == 0)
      nmoves <- 1

   if (returnline == 2) {
      if (nmoves == 0)
         return("")
      txt.moves <- rep("", nmoves.all)
   }

   if (is.null(san))
      san <- .get("san")

   for (j in 1:nmoves) {

      # TODO: transform Lichess castling moves here?
      #if (move[[j]][1] == "e" && move[[j]][2] %in% c("1","8")) {
      #}

      if (flip) {
         letters8 <- letters[8:1]
         x1 <- as.numeric(which(move[[j]][2] == 8:1))
         y1 <- as.numeric(which(move[[j]][1] == letters8))
         x2 <- as.numeric(which(move[[j]][4] == 8:1))
         y2 <- as.numeric(which(move[[j]][3] == letters8))
         colorpiece <- pos[9-x1,9-y1]
         piece <- substr(pos[9-x1,9-y1], 2, 2)
      } else {
         letters8 <- letters[1:8]
         x1 <- as.numeric(which(move[[j]][2] == 1:8))
         y1 <- as.numeric(which(move[[j]][1] == letters8))
         x2 <- as.numeric(which(move[[j]][4] == 1:8))
         y2 <- as.numeric(which(move[[j]][3] == letters8))
         colorpiece <- pos[x1,y1]
         piece <- substr(pos[x1,y1], 2, 2)
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
         if (identical(c(x1,y1), c(8,4)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(8,2)))
            rochade <- "0-0"
         if (identical(c(x1,y1), c(8,4)) && pos[9-x1,9-y1] == "WK" && identical(c(x2,y2), c(8,6)))
            rochade <- "0-0-0"
         if (identical(c(x1,y1), c(1,4)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(1,2)))
            rochade <- "0-0"
         if (identical(c(x1,y1), c(1,4)) && pos[9-x1,9-y1] == "BK" && identical(c(x2,y2), c(1,6)))
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

      # check if a piece was captured (either the target square is not empty or a pawn moved diagonally, which also covers en passant)

      if (flip) {
         iscapture <- pos[9-x2,9-y2] != "" || (piece == "P" && y1 != y2)
      } else {
         iscapture <- pos[x2,y2] != "" || (piece == "P" && y1 != y2)
      }

      # check for pawn promotion

      if (length(move[[j]]) == 5L) {
         promotiontxt <- paste0("=", toupper(move[[j]][5]))
      } else {
         promotiontxt <- ""
      }

      # determine if the move results in a check

      pos.next <- .updateboard(pos, move=data.frame(x1, y1, x2, y2, NA, ifelse(promotiontxt=="", NA, promotiontxt)), flip=flip, autoprom=TRUE, draw=FALSE, x2y2=FALSE)
      ischeck <- any(attr(pos.next,"ischeck"))
      checksym <- ifelse(ischeck, "+", "")

      # construct the move text in short or long algebraic notation

      if (identical(rochade, "")) {
         if (san) {
            # use short algebraic notation
            if (piece=="P") {
               if (y1 == y2) {
                  # pawn moves vertically on the same file
                  txt.move <- paste0(move[[j]][3], move[[j]][4], promotiontxt, checksym, collapse="")
               } else {
                  # pawn moves diagonally; this must be a capture (including en passant)
                  txt.move <- paste0(move[[j]][1], "x", move[[j]][3], move[[j]][4], promotiontxt, checksym, collapse="")
               }
            }
            if (piece=="K") {
               txt.move <- paste0(piece, ifelse(iscapture, "x", ""), move[[j]][3], move[[j]][4], checksym, collapse="")
            }
            startletter <- ""
            startnumber <- ""
            if (piece=="R") {
               dirs <- list(
                  c(-1,  0),
                  c( 1,  0),
                  c( 0, -1),
                  c( 0,  1)
               )
               startxy <- character(0)
               for (d in dirs) {
                  r <- ifelse(flip, 9-x2, x2) + d[1]
                  c <- ifelse(flip, 9-y2, y2) + d[2]
                  while (r >= 1 && r <= 8 && c >= 1 && c <= 8) {
                     if (pos[r,c] == "") {
                        r <- r + d[1]
                        c <- c + d[2]
                        next
                     }
                     if (pos[r,c] == colorpiece)
                        startxy <- c(startxy, paste0(letters[c], r))
                     break
                  }
               }
               if (length(startxy) > 1) {
                  # there is more than one rook that can reach the square
                  if (sum(move[[j]][1] == substr(startxy, 1, 1)) == 1L) {
                     # there is a single rook on the file from which the rook is moving that can reach the target square
                     startletter <- move[[j]][1] # need to add the file
                  } else {
                     # there is more than one rook on the file from which the rook is moving that can reach the target square (this implies that the rook is moving vertically)
                     startnumber <- move[[j]][2] # need to add the rank
                  }
               }
               txt.move <- paste0(piece, startletter, startnumber, ifelse(iscapture, "x", ""), move[[j]][3], move[[j]][4], checksym, collapse="")
            }
            if (piece=="B") {
               dirs <- list(
                  c(-1, -1),
                  c( 1,  1),
                  c(-1,  1),
                  c( 1, -1)
               )
               startxy <- character(0)
               for (d in dirs) {
                  r <- ifelse(flip, 9-x2, x2) + d[1]
                  c <- ifelse(flip, 9-y2, y2) + d[2]
                  while (r >= 1 && r <= 8 && c >= 1 && c <= 8) {
                     if (pos[r,c] == "") {
                        r <- r + d[1]
                        c <- c + d[2]
                        next
                     }
                     if (pos[r,c] == colorpiece)
                        startxy <- c(startxy, paste0(letters[c], r))
                     break
                  }
               }
               if (length(startxy) > 1) {
                  # there is more than one bishop that can reach the square
                  if (sum(move[[j]][1] == substr(startxy, 1, 1)) == 1L) {
                     # there is a single bishop on the file from which the bishop is moving that can reach the target square
                     startletter <- move[[j]][1] # need to add the file
                  } else {
                     # there are two bishops on the file from which the bishop is moving that can reach the target square
                     if (sum(move[[j]][2] == substr(startxy, 2, 2)) == 1L) {
                        # there is a single bishop on the rank from which the bishop is moving that can reach the target square
                        startnumber <- move[[j]][2] # need to add the rank
                     } else {
                        # otherwise need to add the file and the rank
                        startletter <- move[[j]][1]
                        startnumber <- move[[j]][2]
                     }
                  }
               }
               txt.move <- paste0(piece, startletter, startnumber, ifelse(iscapture, "x", ""), move[[j]][3], move[[j]][4], checksym, collapse="")
            }
            if (piece=="Q") {
               dirs <- list(
                  c(-1, -1),
                  c( 1,  1),
                  c(-1,  1),
                  c( 1, -1),
                  c(-1,  0),
                  c( 1,  0),
                  c( 0, -1),
                  c( 0,  1)
               )
               startxy <- character(0)
               for (d in dirs) {
                  r <- ifelse(flip, 9-x2, x2) + d[1]
                  c <- ifelse(flip, 9-y2, y2) + d[2]
                  while (r >= 1 && r <= 8 && c >= 1 && c <= 8) {
                     if (pos[r,c] == "") {
                        r <- r + d[1]
                        c <- c + d[2]
                        next
                     }
                     if (pos[r,c] == colorpiece)
                        startxy <- c(startxy, paste0(letters[c], r))
                     break
                  }
               }
               if (length(startxy) > 1) {
                  if (sum(move[[j]][1] == substr(startxy, 1, 1)) == 1L) {
                     # there is a single queen on the file from which the queen is moving that can reach the target square
                     startletter <- move[[j]][1] # need to add the file
                  } else {
                     # there is more than one queen on the file from which the queen is moving that can reach the target square
                     if (sum(move[[j]][2] == substr(startxy, 2, 2)) == 1L) {
                        # there is a single queen on the rank from which the queen is moving that can reach the target square
                        startnumber <- move[[j]][2] # need to add the rank
                     } else {
                        # otherwise need to add the file and the rank
                        startletter <- move[[j]][1]
                        startnumber <- move[[j]][2]
                     }
                  }
               }
               txt.move <- paste0(piece, startletter, startnumber, ifelse(iscapture, "x", ""), move[[j]][3], move[[j]][4], checksym, collapse="")
            }
            if (piece=="N") {
               dirs <- list(
                  c( 2,  1),
                  c( 1,  2),
                  c(-1,  2),
                  c(-2,  1),
                  c(-2, -1),
                  c(-1, -2),
                  c( 1, -2),
                  c( 2, -1)
               )
               startxy <- character(0)
               for (d in dirs) {
                  r <- ifelse(flip, 9-x2, x2) + d[1]
                  c <- ifelse(flip, 9-y2, y2) + d[2]
                  if (r >= 1 && r <= 8 && c >= 1 && c <= 8) {
                     if (pos[r,c] == colorpiece)
                        startxy <- c(startxy, paste0(letters[c], r))
                  }
               }
               if (length(startxy) > 1) {
                  if (sum(move[[j]][1] == substr(startxy, 1, 1)) == 1L) {
                     # there is a single knight on the file from which the knight is moving that can reach the target square
                     startletter <- move[[j]][1] # need to add the file
                  } else {
                     # there is more than one knight on the file from which the knight is moving that can reach the target square
                     if (sum(move[[j]][2] == substr(startxy, 2, 2)) == 1L) {
                        # there is a single knight on the rank from which the knight is moving that can reach the target square
                        startnumber <- move[[j]][2] # need to add the rank
                     } else {
                        # otherwise need to add the file and the rank
                        startletter <- move[[j]][1]
                        startnumber <- move[[j]][2]
                     }
                  }
               }
               txt.move <- paste0(piece, startletter, startnumber, ifelse(iscapture, "x", ""), move[[j]][3], move[[j]][4], checksym, collapse="")
            }
         } else {
            txt.move <- paste0(piece, move[[j]][1], move[[j]][2], ifelse(iscapture, "x", "-"), move[[j]][3], move[[j]][4], promotiontxt, checksym, collapse="")
         }
      } else {
         txt.move <- paste0(rochade, checksym)
      }

      # change pieces to symbols

      if (rename)
         txt.move <- .rename(txt.move)

      # remove P for pawns

      txt.move <- sub("P", "", txt.move, fixed=TRUE)

      # construct text

      if (returnline == 0)
         txt <- txt.move

      if (returnline == 1) {

         if (!is.null(i)) {

            movenum <- (i+j) %/% 2

            if (j == 1) {
               if (sidetoplay=="w") {
                  txt.move <- paste0(movenum, ". ", txt.move, collapse="")
               } else {
                  txt.move <- paste0(movenum, "... ", txt.move, collapse="")
               }
            } else {
               if (.is.even(i+j)) {
                  txt.move <- paste0("   ", movenum, ". ", txt.move, collapse="")
               } else {
                  txt.move <- txt.move
               }
            }

         }

         if (is.na(evalval)) {
            txt.eval <- ""
         } else {
            #txt.eval <- paste0(.fmtx(evalval, digits=ifelse(abs(evalval) >= 99.9, 1, 2), flag="+"), " ", collapse="")
            txt.eval <- paste0(.fmtx(evalval, digits=1, flag="+"), "   ", collapse="")
         }

         if (j == 1)
            txt <- txt.eval

         txt <- paste0(txt, " ", txt.move, collapse=" ")

         pos <- pos.next

      }

      if (returnline == 2) {

         txt.moves[j] <- txt.move
         pos <- pos.next

      }

   }

   if (returnline == 2) {
      return(txt.moves)
   } else {
      out <- list(txt=txt, x1=bestx1, y1=besty1, x2=bestx2, y2=besty2)
      return(out)
   }

}

.printverbose <- function(selected, seqno, filename, lastseq, upsidedown, flip, unflip, replast, oldmode, i,
                          seqname, seqnum, score, rounds, totalmoves, show, showcomp, comment, bestmove, starteval,
                          evalval, texttop, scoreadd, sidetoplay, givehint1, givehint2, mistake,
                          timetotal, movesplayed, movestoplay, drawcircles, drawarrows, showstartcom, pos) {

   cat("\n")
   cat("selected:     ", selected, "\n")
   cat("seqno:        ", seqno, "\n")
   cat("filename:     ", filename, "\n")
   cat("lastseq:      ", lastseq, "\n")
   cat("upsidedown:   ", upsidedown, "\n")
   cat("flip:         ", flip, "\n")
   cat("unflip:       ", unflip, "\n")
   cat("replast:      ", replast, "\n")
   cat("oldmode:      ", oldmode, "\n")
   cat("i:            ", i, "\n")
   cat("seqname:      ", seqname, "\n")
   cat("seqnum:       ", seqnum, "\n")
   cat("score:        ", score, "\n")
   cat("rounds:       ", rounds, "\n")
   cat("totalmoves:   ", totalmoves, "\n")
   cat("show:         ", show, "\n")
   cat("showcomp:     ", showcomp, "\n")
   cat("comment:      ", comment, "\n")
   cat("bestmove:     ", sapply(bestmove, head, 1), "\n")
   cat("starteval:    ", starteval, "\n")
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

   pos <- .rename(pos, withcolor=TRUE)

   if (flip) {
      print(pos[,8:1], quote=FALSE)
   } else {
      print(pos[8:1,], quote=FALSE)
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
         if (y1 == y2 && target == "" && (x1 - x2 == 1 || (x1 - x2 == 2 && x1 == 7 && pos[3,9-y1] == ""))) # regular pawn move
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x1 - x2 == 1 && target != "") # pawn capture
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x1 - x2 == 1 && x1 == 4 && pos[9-x1,9-y2] == "BP" && identical(attr(pos,"ispp"), "b") && identical(attr(pos,"y1"), y2)) # en passant
            islegal <- TRUE
      } else {
         if (y1 == y2 && target == "" && (x2 - x1 == 1 || (x2 - x1 == 2 && x1 == 2 && pos[3,y1] == ""))) # regular pawn move
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
         if (y1 == y2 && target == "" && (x2 - x1 == 1 || (x2 - x1 == 2 && x1 == 2 && pos[6,9-y1] == ""))) # regular pawn move
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x2 - x1 == 1 && target != "") # pawn capture
            islegal <- TRUE
         if (abs(y1 - y2) == 1 && x2 - x1 == 1 && x1 == 5 && pos[9-x1,9-y2] == "WP" && identical(attr(pos,"ispp"), "w") && identical(attr(pos,"y1"), y2)) # en passant
            islegal <- TRUE
      } else {
         if (y1 == y2 && target == "" && (x1 - x2 == 1 || (x1 - x2 == 2 && x1 == 7 && pos[6,y1] == ""))) # regular pawn move
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

   return(isattacked)

}

.printdf <- function(df, align=NULL, print.gap=2) {

   align <- match.arg(align, c("left","right"), several.ok=TRUE)

   for (j in 1:ncol(df)) {
      df[[j]] <- format(df[[j]], justify=align[j])
      names(df)[j] <- format(c(names(df)[j], df[[j]]), justify=align[j])[1]
   }

   print(df, print.gap=print.gap)

}

.findmovetransp <- function(fen, flip, i, sub, dat, files, pos, contanalysis, movestoshow) {

   san <- .get("san")

   fenshort <- .fenpart(fen)
   seqident <- lapply(dat, function(x) {
      if (any(fenshort == x$fenshort[-1]) && identical(flip, x$flip) && !identical(sub$moves$fen[1:(i-1)], x$fen[1:(i-1)]) && identical(pos, x$pos)) {
         pos <- min(which(fenshort == x$fenshort[-1]))
         if (san) {
            nextmoves <- x$san[pos+c(1:max(1,movestoshow))]
         } else {
            nextmoves <- x$move[pos+c(1:max(1,movestoshow))]
         }
         nextmoves[is.na(nextmoves)] <- ""
         return(nextmoves)
      } else {
         return()
      }
   })
   notnull <- !sapply(seqident, is.null)
   seqident <- seqident[notnull]
   if (any(notnull)) {
      .texttop(.text("transpositions", length(seqident) == 1L))
      #eval(expr=.get("switch1"))
      cat(.text("transposseqs", length(seqident) == 1L))
      tab <- data.frame(files[notnull])
      colnames(tab) <- .text("sequence")
      if (movestoshow > 0) {
         nextmoves <- do.call(rbind, seqident)
         nextmoves <- .rename(nextmoves)
         tab <- cbind(tab, nextmoves)
      }
      rownames(tab) <- which(notnull)
      .printdf(tab, align=c("l",rep("r",movestoshow)))
      if (contanalysis)
         .waitforclick()
      #eval(expr=.get("switch2"))
   }

}

.findopening <- function(x, pos, flip, sidetoplay, sidetoplaystart, i, opening, openings, posnull, draw=TRUE) {

   nrows <- nrow(x)
   oldopening <- opening

   if (posnull && nrows >= 1L) {

      if (flip) {
         ucimoves <- paste(paste0(letters[9-x[,2]], 9-x[,1], letters[9-x[,4]], 9-x[,3]), collapse=" ")
      } else {
         ucimoves <- paste(paste0(letters[x[,2]], x[,1], letters[x[,4]], x[,3]), collapse=" ")
      }

      #print(ucimoves)

      openingmatch <- which(ucimoves == openings$uci)

      if (length(openingmatch) >= 1L) {
         opening <- openings[openingmatch[1],1:2]
         opening <- paste0(opening[1], " (", substr(opening[2], 1, 120), ")", collapse="")
         if (!identical(opening, oldopening) && draw)
            .textbot(opening=opening, onlyeco=TRUE)
      } else {
         fen <- .genfen(pos, flip, sidetoplay, sidetoplaystart, i)
         fen <- .fenpart(fen, parts=1:4)
         fenmatch <- which(fen == openings$epd)
         if (length(fenmatch) >= 1L) {
            opening <- openings[fenmatch[1],1:2]
            opening <- paste0(opening[1], " (", substr(opening[2], 1, 120), ")", collapse="")
            if (!identical(opening, oldopening) && draw)
               .textbot(opening=opening, onlyeco=TRUE)
         }
      }

   } else {

      .textbot(opening=" ", onlyeco=TRUE)
      opening <- ""

   }

   return(opening)

}

.numshort <- function(x, digits=1) {

   abs.x <- abs(x)

   suffix <- ifelse(abs.x >= 1e9, "B",
             ifelse(abs.x >= 1e6, "M",
             ifelse(abs.x >= 1e3, "K", "")))

   divisor <- ifelse(abs.x >= 1e9, 1e9,
              ifelse(abs.x >= 1e6, 1e6,
              ifelse(abs.x >= 1e3, 1e3, 1)))

   value <- round(x / divisor, digits)
   out <- paste0(value, suffix)

   out[divisor == 1] <- as.character(x[divisor == 1])
   return(out)

}

.checkseq <- function(dat, seqdir, files) {

   for (j in 1:length(dat)) {

      #print(j)

      sub <- dat[[j]]

      dosave <- FALSE

      if (ncol(sub$moves) != 13L) {

         dosave <- TRUE

         if (is.null(sub$moves$comment))
            sub$moves$comment <- ""
         if (is.null(sub$moves$circles))
            sub$moves$circles <- ""
         if (is.null(sub$moves$arrows))
            sub$moves$arrows <- ""
         if (is.null(sub$moves$glyph))
            sub$moves$glyph <- ""
         if (is.null(sub$moves$fen))
            sub$moves$fen <- ""

      }

      #sub$moves$san <- NULL
      #sub$startfen <- NULL

      if (is.null(sub$startfen) || is.null(sub$moves$san)) {

         dosave <- TRUE

         # get the flip setting
         flip <- sub$flip

         # get the starting position
         if (is.null(sub$pos)) {
            pos <- .get("pos")
         } else {
            pos <- sub$pos
         }

         # determine 'sidetoplay' based on the first piece moved
         if (flip) {
            piece <- pos[9-sub$moves[1,1], 9-sub$moves[1,2]]
         } else {
            piece <- pos[sub$moves[1,1], sub$moves[1,2]]
         }
         sidetoplay <- ifelse(startsWith(piece, "W"), "w", "b")

      }

      if (is.null(sub$startfen)) {

         # generate the FEN for the starting position
         fen <- .genfen(pos, flip=flip, sidetoplay=sidetoplay, sidetoplaystart=sidetoplay, i=1)

         # add the FEN to the sequence
         sub$startfen <- fen

      }

      if (is.null(sub$moves$san)) {

         if (nrow(sub$moves) == 0L) {

            sub$moves <- data.frame(x1=numeric(), y1=numeric(), x2=numeric(), y2=numeric(), show=logical(), move=character(), san=character(), eval=numeric(), comment=character(), circles=character(), arrows=character(), glyph=character(), fen=character())

         } else {

            # transform the moves from long algebraic notation to UCI notation
            tmp <- .lan2uci(sub$moves$move, sidetoplay=sidetoplay)

            # transform the moves from UCI notation to short algebraic notation
            tmp <- .parsemove(tmp, pos=pos, flip=flip, evalval=NA, i=NA, sidetoplay=sidetoplay, rename=FALSE, returnline=2, hintdepth=nrow(sub$moves), san=TRUE)

            # replace + with # for mates
            ismate <- grepl("#", sub$moves$move, fixed=TRUE)
            tmp[ismate] <- gsub("+", "#", tmp, fixed=TRUE)[ismate]

            # add the move in SAN to the sequence
            sub$moves$san <- tmp
            sub$moves <- sub$moves[c("x1","y1","x2","y2","show","move","san","eval","comment","circles","arrows","glyph","fen")]

         }

      }

      if (dosave)
         saveRDS(sub, file=file.path(seqdir, files[j]))

   }

   return()

}

.rename <- function(x, withcolor=FALSE) {

   pieces <- .get("pieces")

   if (withcolor) {
      x[x==""]   <- "\U000000B7"
      x[x=="WP"] <- "\U0000265F"
      x[x=="WR"] <- "\U0000265C"
      x[x=="WN"] <- "\U0000265E"
      x[x=="WB"] <- "\U0000265D"
      x[x=="WK"] <- "\U0000265A"
      x[x=="WQ"] <- "\U0000265B"
      x[x=="BP"] <- "\U00002659"
      x[x=="BR"] <- "\U00002656"
      x[x=="BN"] <- "\U00002658"
      x[x=="BB"] <- "\U00002657"
      x[x=="BK"] <- "\U00002654"
      x[x=="BQ"] <- "\U00002655"
   } else {
      if (pieces == 1) {
         x <- gsub("K", "\U0000265A", x, fixed=TRUE)
         x <- gsub("Q", "\U0000265B", x, fixed=TRUE)
         x <- gsub("R", "\U0000265C", x, fixed=TRUE)
         x <- gsub("B", "\U0000265D", x, fixed=TRUE)
         x <- gsub("N", "\U0000265E", x, fixed=TRUE)
      }
      if (pieces == 3) {
         lang <- .get("lang")
         if (lang == "de") {
            x <- gsub("K", "K", x, fixed=TRUE)
            x <- gsub("Q", "D", x, fixed=TRUE)
            x <- gsub("R", "T", x, fixed=TRUE)
            x <- gsub("B", "L", x, fixed=TRUE)
            x <- gsub("N", "S", x, fixed=TRUE)
         }
      }
   }

   return(x)

}

.cleancache <- function(dir, months) {

   files <- list.files(dir, full.names=TRUE)

   if (length(files) == 0L)
      return()

   mtimes <- file.mtime(files)
   ages <- difftime(Sys.time(), mtimes, units="days")
   old <- ages >= 30*months

   if (any(old))
      file.remove(files[old])

   return()

}

.fmtx <- function(x, digits, flag="")
   formatC(x, format="f", digits=digits, flag=flag)

.sub9 <- function(x) {
   m <- gregexpr("\\d+", x)
   regmatches(x, m) <- lapply(regmatches(x, m), function(num) as.character(9 - as.numeric(num)))
   return(x)
}

.doflip <- function(sub, pos, flip) {

   sub$flip <- flip
   sub$moves[1:4] <- 9-sub$moves[1:4]
   sub$moves$circles <- .sub9(sub$moves$circles)
   sub$moves$arrows  <- .sub9(sub$moves$arrows)
   attr(pos,"y1") <- 9-attr(pos,"y1")
   assign("x2y2", 9-.get("x2y2"), envir=.chesstrainer)
   if (!is.null(sub$symbolend)) {
      sub$symbolend$circlesvar <- .sub9(sub$symbolend$circlesvar)
      sub$symbolend$arrowsvar  <- .sub9(sub$symbolend$arrowsvar)
   }

   return(list(sub=sub, pos=pos))

}

.listseqs <- function(k, files, files.all, selected, scores.selected, age.selected, rounds.selected, difficulty.selected, probvals.selected) {

   if (k > 0L) {
      if (max(probvals.selected) == min(probvals.selected)) {
         bars <- rep(5, k)
      } else {
         bars <- round(5 * (probvals.selected - min(probvals.selected)) / (max(probvals.selected) - min(probvals.selected)))
      }
      bars <- sapply(bars, function(x) paste0(rep("*", x), collapse=""))
      tab <- data.frame(files, rounds.selected, .fmtx(age.selected, digits=1), scores.selected, .fmtx(difficulty.selected, digits=1), .fmtx(probvals.selected, digits=1), bars)
      tab$bars <- format(tab$bars, justify="left")
      names(tab) <- c(.text("sequence"), .text("rounds"), .text("age"), .text("score"), .text("diff"), "%", "")
      tab[[1]] <- substr(tab[[1]], 1, nchar(tab[[1]])-4) # remove .rds from name
      tab[[1]] <- format(tab[[1]], justify="left")
      names(tab)[1] <- format(c(names(tab)[1], tab[[1]]), justify="left")[1]
      if (!is.null(selected))
         rownames(tab) <- which(files.all %in% selected)
      txt <- capture.output(print(tab, print.gap=2))
      if (length(txt) > 50)
         txt <- c(txt, txt[1])
      .print(txt)
   } else {
      cat(.text("zeroseqsfound"))
   }

   return()

}
