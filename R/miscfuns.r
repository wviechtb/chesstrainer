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

   if (identical(rochade, rep(FALSE,4))) {
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
