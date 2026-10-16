.showwelcome <- function(welcome=TRUE) {

   lang <- .get("lang")

   col.help <- .get("col.help")
   font.mono <- .get("font.mono")

   .mousedownfun <- function(button,x,y) {
      if (length(button) == 0L)
         button <- 3
      button <<- button
      xy1 <<- .calcxy(x, y, plt)
      return(NULL)
   }

   .mouseupfun <- function(button,x,y) {
      xy2 <<- .calcxy(x, y, plt)
      return(1)
   }

   if (lang == "en") {

      txt <- c(
      "\U00002013 Welcome to the Chess Trainer \U00002013",
      "",
      "",
      "")

      txt <- c(txt,
      readLines(textConnection(
"The chesstrainer allows you to create, save, and practice sequences
of chess moves (lines). It is particularly useful for learning and
memorizing chess openings, but can also be used to practice tactics,
solve chess puzzles, or memorize entire games. The trainer keeps
track of your performance and progress, giving more attention to
sequences that you find difficult. You can create your own sequences,
organize and select which ones to practice, and use various training
modes and additional features to improve your chess skills.

Please take some time to read the manual! The trainer offers a wide
range of features and keyboard shortcuts that may not be immediately
obvious. Reading the manual will help you understand how the trainer
works and how to make the most of its capabilities. You can press <m>
now to open up the manual in your browser. To switch the language,
press <Ctrl-l>. While the trainer is running, you can also press <F1>
to see an overview of the keyboard shortcuts."))
      )

      ypos <- seq(8, 3, length.out=length(txt))

   }

   if (lang == "de") {

      txt <- c(
      "\U00002013 Willkommen beim Schachtrainer \U00002013",
      "",
      "",
      "")

      txt <- c(txt,
      readLines(textConnection(
"Mit dem Schachtrainer kannst du Schachzugfolgen (Varianten)
erstellen, speichern und trainieren. Er eignet sich besonders zum
Erlernen und Einpr\U000000E4gen von Schacher\U000000F6ffnungen, kann aber auch zum \U000000DCben
von Taktiken, zum L\U000000F6sen von Schachaufgaben oder zum Auswendiglernen
ganzer Partien verwendet werden. Der Trainer verfolgt deine
Leistungen und Fortschritte und legt dabei besonderes Augenmerk auf
Varianten, die dir Schwierigkeiten bereiten. Du kannst eigene
Varianten erstellen, diese organisieren und ausw\U000000E4hlen, welche du
trainieren m\U000000F6chtest. Au\U000000DFerdem stehen dir verschiedene Trainingsmodi
und zus\U000000E4tzliche Funktionen zur Verf\U000000FCgung, mit denen du deine
Schachf\U000000E4higkeiten verbessern kannst.

Bitte nimm dir etwas Zeit, um das Handbuch zu lesen! Der Trainer
bietet zahlreiche Funktionen und Tastenkombinationen, die nicht
unbedingt auf den ersten Blick ersichtlich sind. Das Handbuch hilft
dir dabei, die Funktionsweise des Trainers zu verstehen und seine
M\U000000F6glichkeiten optimal zu nutzen. Du kannst jetzt <m> dr\U000000FCcken, um das
Handbuch in deinem Browser zu \U000000F6ffnen. Um die Sprache zu wechseln,
dr\U000000FCcke <Strg-l>. W\U000000E4hrend der Trainer l\U000000E4uft, kannst du
jederzeit <F1> dr\U000000FCcken, um eine \U000000DCbersicht der Tastenkombinationen
anzuzeigen."))
      )

      ypos <- seq(8, 2.5, length.out=length(txt))

   }

   langswitch <- FALSE

   cex <- .findcex(txt, font=font.mono, x1=2, x2=8, y1=4.0, y2=8.0, mincex=2)

   redraw <- TRUE

   while (TRUE) {

      plt <- par("plt")

      if (redraw) {
         dev.hold()
         .drawbox()
         text(5, ypos[1], txt[1], offset=0, cex=2*cex, family=font.mono, font=2, col=col.help)
         segments(1.8, ypos[3], 8.2, ypos[3], col=col.help)
         text(2, ypos[-1], txt[-1], pos=4, offset=0, cex=cex, family=font.mono, col=col.help)
         text(2.4, 1.8, .text("showwelcome"), pos=4, offset=0, cex=cex, family=font.mono, col=col.help)
         box.xpos <- 2.1
         box.ypos <- 1.8
         box <- .drawcheckbox(box.xpos, box.ypos, on=welcome, cex=cex)
         dev.flush()
      } else {
         redraw <- TRUE
      }

      button <- NULL
      xy1 <- NULL
      xy2 <- NULL

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onMouseUp=.mouseupfun, onKeybd=.keyfun)

      if (is.numeric(click)) {
         if (xy1[1] >= box[1] & xy1[2] >= box[2] & xy1[1] <= box[3] & xy1[2] <= box[4]) {
            welcome <- !welcome
            .drawcheckbox(box.xpos, box.ypos, on=welcome, cex=cex)
            next
         } else {
            break
         }
      }

      if (identical(click, "F1") || identical(click, "q") || identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "\033") || identical(click, "ctrl-["))
         break

      if (identical(click, "m")) {
         browseURL("https://wviechtb.github.io/chesstrainer/reference/chesstrainer-package.html")
         redraw <- FALSE
         next
      }

      if (identical(click, "t")) {
         print(help("chesstrainer", help_type="text"))
         redraw <- FALSE
         next
      }

      if (identical(click, "ctrl-L")) {
         if (lang == "de") {
            lang <- "en"
         } else {
            lang <- "de"
         }
         assign("lang", lang, envir=.chesstrainer)
         langswitch <- TRUE
         break
      }

   }

   if (langswitch)
      .showwelcome(welcome)

   #.erase(1, 1, 9, 9)

   return(welcome)

}
