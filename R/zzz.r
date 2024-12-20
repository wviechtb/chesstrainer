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

   assign("col.bg",          "#211b12",        envir=.chesstrainer)
   assign("col.fg",          "#7a6d59",        envir=.chesstrainer)
   assign("col.square.l",    "#f0d9b5",        envir=.chesstrainer)
   assign("col.square.d",    "#b58863",        envir=.chesstrainer)
   assign("col.top",         "#b09d7f",        envir=.chesstrainer)
   assign("col.bot",         "#b09d7f",        envir=.chesstrainer)
   assign("col.help",        "#b09d7f",        envir=.chesstrainer)
   assign("col.help.border", "#63462e",        envir=.chesstrainer)
   assign("col.hint",        "yellow",         envir=.chesstrainer)
   assign("col.wrong",       "red",            envir=.chesstrainer)
   assign("col.rect",        "darkseagreen4",  envir=.chesstrainer)
   assign("col.annot",       rgb(0,0.5,0,0.5), envir=.chesstrainer)
   assign("col.side.w",      "white",          envir=.chesstrainer)
   assign("col.side.b",      "black",          envir=.chesstrainer)

   # set fonts and sizes

   assign("font.mono", "mono", envir=.chesstrainer)
   assign("cex.top",  1.4, envir=.chesstrainer)
   assign("cex.bot",  0.7, envir=.chesstrainer)

   # welcome message

   loadmsg <- paste0("\nWelcome to the 'chesstrainer' package!\nFor an introduction, type: help(chesstrainer)\nTo start playing, type: play()\n")
   packageStartupMessage(loadmsg, domain=NULL, appendLF=TRUE)

}
