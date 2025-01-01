.is.even <- function(x) x %% 2 == 0

.get <- function(x)
   unname(get(x, envir=.chesstrainer))

.is.null <- function(x) {

   if (length(x) == 0L)
      return(logical(0))

   return(sapply(x, is.null))

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

   rochade <- attributes(pos)$rochade

   if (identical(rochade, rep(FALSE,4))) {
      rochade <- "-"
   } else {
      rochade <- paste0(c("K", "Q", "k", "q")[rochade], collapse="")
   }

   fen <- paste(fen, rochade)

   # add en passent target square

   ispp <- attributes(pos)$ispp

   if (!identical(ispp, "")) {
      if (ispp == "w") {
         if (flip) {
            enpassent <- paste0(letters[8:1][attributes(pos)$y1], 3, collapse="")
         } else {
            enpassent <- paste0(letters[1:8][attributes(pos)$y1], 3, collapse="")
         }
      } else {
         if (flip) {
            enpassent <- paste0(letters[8:1][attributes(pos)$y1], 6, collapse="")
         } else {
            enpassent <- paste0(letters[1:8][attributes(pos)$y1], 6, collapse="")
         }
      }
   } else {
      enpassent <- "-"
   }

   fen <- paste(fen, enpassent)

   # add halfmove clock

   fen <- paste(fen, attributes(pos)$moves50)

   # add fullmove number

   fen <- paste(fen, (i+1) %/% 2)

   return(fen)

}
