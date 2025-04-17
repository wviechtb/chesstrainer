.chesstrainer <- new.env()

.onAttach <- function(libname, pkgname) {

   # load piece images

   assign("img.WB", png::readPNG(system.file("figures", "chess_blt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.WK", png::readPNG(system.file("figures", "chess_klt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.WN", png::readPNG(system.file("figures", "chess_nlt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.WP", png::readPNG(system.file("figures", "chess_plt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.WQ", png::readPNG(system.file("figures", "chess_qlt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.WR", png::readPNG(system.file("figures", "chess_rlt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.BB", png::readPNG(system.file("figures", "chess_bdt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.BK", png::readPNG(system.file("figures", "chess_kdt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.BN", png::readPNG(system.file("figures", "chess_ndt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.BP", png::readPNG(system.file("figures", "chess_pdt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.BQ", png::readPNG(system.file("figures", "chess_qdt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)
   assign("img.BR", png::readPNG(system.file("figures", "chess_rdt45.svg.png", package="chesstrainer"), native=TRUE), envir=.chesstrainer)

   # set default colors

   assign("col.bg",          "#211b12",       envir=.chesstrainer)
   assign("col.fg",          "#7a6d59",       envir=.chesstrainer)
   assign("col.square.l",    "#f0d9b5",       envir=.chesstrainer)
   assign("col.square.d",    "#b58863",       envir=.chesstrainer)
   assign("col.square.be",   "#6b6b6b",       envir=.chesstrainer)
   assign("col.top",         "#b09d7f",       envir=.chesstrainer)
   assign("col.bot",         "#b09d7f",       envir=.chesstrainer)
   assign("col.help",        "#b09d7f",       envir=.chesstrainer)
   assign("col.help.border", "#63462e",       envir=.chesstrainer)
   assign("col.hint",        "yellow",        envir=.chesstrainer)
   assign("col.best",        "#747a88",       envir=.chesstrainer)
   assign("col.wrong",       "red",           envir=.chesstrainer)
   assign("col.rect",        "darkseagreen4", envir=.chesstrainer)
   assign("col.annot",       "#00800080",     envir=.chesstrainer)
   assign("col.side.w",      "white",         envir=.chesstrainer)
   assign("col.side.b",      "black",         envir=.chesstrainer)

   # set default font

   assign("font.mono", "mono", envir=.chesstrainer)

   # create starting position matrix 'pos'

   pos <- matrix("", 8, 8)
   pos[2,] <- "WP"
   pos[1,1] <- pos[1,8] <- "WR"
   pos[1,2] <- pos[1,7] <- "WN"
   pos[1,3] <- pos[1,6] <- "WB"
   pos[1,4] <- "WQ"
   pos[1,5] <- "WK"
   pos[7,] <- "BP"
   pos[8,1] <- pos[8,8] <- "BR"
   pos[8,2] <- pos[8,7] <- "BN"
   pos[8,3] <- pos[8,6] <- "BB"
   pos[8,4] <- "BQ"
   pos[8,5] <- "BK"
   colnames(pos) <- LETTERS[1:8]
   rownames(pos) <- 1:8
   attr(pos, "rochade") <- rep(TRUE,4)
   attr(pos, "moves50") <- 0
   assign("pos", pos, envir=.chesstrainer)

   pos <- rbind("", pos, "")
   pos <- cbind("", pos, "")
   rownames(pos) <- 0:9
   colnames(pos) <- 0:9
   pos[1,3:8]  <- c("WK","WQ","WR","WB","WN","WP")
   pos[10,3:8] <- c("BK","BQ","BR","BB","BN","BP")
   assign("boardeditorpos", pos, envir=.chesstrainer)

   # welcome message

   loadmsg <- paste0("\nWelcome to the 'chesstrainer' package!\nFor an introduction, type: help(chesstrainer)\nTo start playing, type: play()\n")
   packageStartupMessage(loadmsg, domain=NULL, appendLF=TRUE)

}
