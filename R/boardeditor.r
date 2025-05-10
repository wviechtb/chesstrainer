.boardeditor <- function(pos, flip, sidetoplay, lwd, verbose, switch1, switch2) {

   pos <- rbind("", pos, "")
   pos <- cbind("", pos, "")
   rownames(pos) <- 0:9
   colnames(pos) <- 0:9
   pos[1,3:8]  <- c("WK","WQ","WR","WB","WN","WP")
   pos[10,3:8] <- c("BK","BQ","BR","BB","BN","BP")

   .boardeditor.drawboard(pos, flip, sidetoplay, lwd)

   rochadespec <- FALSE

   while (TRUE) {

      click1.x <- NULL
      click1.y <- NULL
      click2.x <- NULL
      click2.y <- NULL
      empty.square <- FALSE
      button <- 0L

      plt <- par("plt")

      .calcsquarebe <- function(x, y) {
         square.x <- floor((y - plt[3]) / (plt[4] - plt[3]) * 10 + 1)
         square.y <- floor((x - plt[1]) / (plt[2] - plt[1]) * 10 + 1)
         square.x[square.x < 1] <- 1
         square.x[square.x > 10] <- 10
         return(c(square.x, square.y))
      }

      mousedown <- function(buttons, x, y) {
         squares <- .calcsquarebe(x,y)
         pos.x <- squares[1]
         pos.y <- squares[2]
         click1.x <<- pos.x
         click1.y <<- pos.y
         click2.x <<- pos.x
         click2.y <<- pos.y
         button <<- buttons
         if (flip && pos[11-pos.x, 11-pos.y] == "") {
            empty.square <<- TRUE
            return(NULL)
         }
         if (!flip && pos[pos.x, pos.y] == "") {
            empty.square <<- TRUE
            return(NULL)
         }
         empty.square <<- FALSE
         if (identical(buttons, 0L))
            .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd)
         return(NULL)
      }

      dragmousemove <- function(buttons, x, y) {

         if (!empty.square) {

            squares <- .calcsquarebe(x,y)
            pos.x <- squares[1]
            pos.y <- squares[2]

            .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd)

            if (isTRUE(pos.x != click2.x) || isTRUE(pos.y != click2.y))
               .boardeditor.rmrect(click2.x, click2.y, lwd=lwd)

            click2.x <<- pos.x
            click2.y <<- pos.y

         }

         return(NULL)

      }

      mouseup <- function(buttons, x, y) {
         .boardeditor.rmrect(click1.x, click1.y, lwd=lwd)
         .boardeditor.rmrect(click2.x, click2.y, lwd=lwd)
         return(1)
      }

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=function(key) return(key))

      keys <- c("q", "\033", "ctrl-[", "n", "f", "s", "c", "r", "F1")

      if (is.character(click) && !is.element(click, keys))
         next

      ######################################################################

      # q or escape to quit the board editor

      if (identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-["))
         break

      # n to reset the board into the starting position

      if (identical(click, "n")) {
         pos <- .get("boardeditorpos")
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         next
      }

      # f to flip the board

      if (identical(click, "f")) {
         flip <- !flip
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         next
      }

      # s to switch sidetoplay

      if (identical(click, "s")) {
         sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
         .drawsideindicator(sidetoplay, flip, adj=1)
         next
      }

      # c to clear the board

      if (identical(click, "c")) {
         pos[2:9,2:9] <- ""
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         next
      }

      # r to enter rochade availability

      if (identical(click, "r")) {
         eval(expr=switch1)
         resp <- readline(prompt=.text("enterrochade"))
         if (grepl("^K?Q?k?q?$", resp)) {
            rochade <- c(grepl("K", resp), grepl("Q", resp), grepl("k", resp), grepl("q", resp))
            rochadespec <- TRUE
         } else {
            cat(.text("notcorrectrochade"))
         }
         eval(expr=switch2)
         next
      }

      # F1 to show the help

      if (identical(click, "F1")) {
         .showhelp.boardeditor(lwd=lwd)
         .boardeditor.drawboard(pos, flip, sidetoplay, lwd)
         next
      }

      ######################################################################

      # if click is an actual click (and drag) on the board

      if (verbose) {
         cat("Click 1: ", click1.x, ", ", click1.y, sep="")
         cat("\n")
         cat("Click 2: ", click2.x, ", ", click2.y, sep="")
         cat("\n")
      }

      # when clicking too fast, click may not be registered, so start over

      if (is.null(click1.x) || is.null(click2.x) || is.null(click1.y) || is.null(click2.y))
         next

      if (is.na(click1.x) || is.na(click2.x) || is.na(click1.y) || is.na(click2.y))
         next

      # don't do anything when dropping a piece on the 1st or 10th row

      if (click2.x == 1 || click2.x == 10)
         next

      pos <- .boardeditor.updateboard(pos, move=c(click1.x, click1.y, click2.x, click2.y), flip=flip, button=button)

   }

   pos <- pos[2:9,2:9]

   # if rochade availability was not entered, then assume availability based on king and rook positions

   if (!rochadespec) {
      rochade <- c((pos[1,5] == "WK" && pos[1,8] == "WR"), (pos[1,5] == "WK" && pos[1,1] == "WR"),
                   (pos[8,5] == "BK" && pos[8,8] == "BR"), (pos[8,5] == "BK" && pos[8,1] == "BR"))
   }

   attr(pos, "rochade") <- rochade

   return(list(pos=pos, flip=flip, sidetoplay=sidetoplay))

}

############################################################################

.showhelp.boardeditor <- function(lwd) {

   lang <- .get("lang")

   if (lang == "en") {

      txt <- c(
      "Help for the board editor:",
      "",
      "left-click and drag  - move a piece",
      "right-click and drag - copy a piece",
      "right-click          - delete the piece",
      "",
      "f  - flip the board",
      "s  - set which side plays the first move",
      "n  - reset the board into the starting position",
      "c  - clear the board",
      "r  - enter castling availability",
      "F1 - show this help",
      "q  - quit the board editor"
      )

   }

   if (lang == "de") {

      txt <- c(
      "Hilfe f\U000000FCr den Bretteditor:",
      "",
      "Linksklick und ziehen  - Figur bewegen",
      "Rechtsklick und ziehen - Figur kopieren",
      "Rechtsklick            - Figur l\U000000F6schen",
      "",
      "f  - Brett umdrehen",
      "s  - festlegen, welche Seite den ersten Zug macht",
      "n  - Brett in die Ausgangsposition zur\U000000FCcksetzen",
      "c  - Brett leer r\U000000E4umen",
      "r  - Rochadem\U000000F6glichkeiten eingeben",
      "F1 - diese Hilfe anzeigen",
      "q  - Bretteditor beenden"
      )

   }

   rect(2.2, 2.2, 9.8, 9.8, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
   maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.8)

   text(2+0.5, seq(8, 4, length.out=length(txt)), txt, pos=4, cex=cex,
        family=.get("font.mono"), font=ifelse(grepl(":", txt), 2, 1), col=.get("col.help"))

   .waitforclick()

   .erase(2.3, 2.3, 9.7, 9.7)

   return()

}
