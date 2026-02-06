.showhelp <- function(page=1) {

   #.clearsideindicator()
   #.drawtimer(clear=TRUE)

   lang <- .get("lang")

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   if (lang == "en") {

      txt.general1 <- c(
      "General shortcuts:",
      "<space>  - switch mode (test/add)",
      "\\ or \U000000E4   - switch to play mode",
      "p        - select a player",
      "Ctrl-r   - remove a player",
      "n        - start new/next sequence",
      "l        - list all (selected) sequences",
      "^ or 6   - specify the exponent value",
      "/ or ,   - select sequences",
      "|        - search for sequences (without selecting them)",
      "* or 8   - select all sequences",
      "?        - select all sequences that start with the same moves",
      "'        - select all sequences that include the same position",
      ";        - select all sequences that end in the same position",
      ".        - select the last saved sequence",
      ">        - select and manage bookmarks",
      "m        - choose a sequence selection mode",
      "d        - choose a difficulty calculation method",
      "w        - wait after completed sequences on/off",
      "-+       - decrease/increase the time between moves",
      "[]       - decrease/increase the sound volume",
      "()       - decrease/increase the line width",
      "{}       - decrease/increase the margin width",
      "i        - toggle the language (English/German)",
      "v        - evaluation bar on/off",
      "k        - material difference on/off",
      "C        - board coordinates on/off",
      "Ctrl-f   - copy the FEN of the current position to the clipboard",
      "<escape> - redraw the board / exit a menu",
      "F1       - show this help",
      "F2       - show the leaderboard and player statistics",
      "F3       - show settings",
      "F4       - settings for colors",
      "F5       - settings for text sizes",
      "F6       - settings for miscellanea",
      "F7       - settings for Stockfish",
      "F8       - add, remove, and select sequence directories",
      "F9       - open the current position on lichess.org",
      "F10      - show histograms/scatterplot",
      "F11      - show session info",
      "F12      - show session history",
      "q        - quit the trainer")

      txt.general2 <- c(
      "General shortcuts:",
      "\U00002190 and \U00002192  - go back / forward one move",
      "1 or \U00002191   - jump to the first player move or the beginning of the sequence/game",
      "2 or \U00002193   - jump to the end of the sequence/game",
      "3, 4, 5  - jump to the 1st quarter, middle, 3rd quarter of the sequence/game",
      "u        - recalculate the evaluations for the current sequence/game")

      txt.test <- c(
      "Shortcuts for the test mode:",
      "h        - get a hint (can be selected twice)",
      "t        - take back a score increase",
      "o        - manually enter the score for the current sequence",
      "z        - zen mode on/off",
      "x        - timed mode on/off",
      "<        - bookmark the current sequence",
      "r        - repeat the last sequence",
      "R        - automatically repeat sequences after a mistake on/off",
      "j        - jump to a sequence number (for sequential sequence selection modes)",
      "T        - set target score",
      "g        - show the progress graph for the current sequence",
      "G        - show the progress graph at the end of sequences on/off",
      "a and A  - extend the current sequence / position (switches to add mode)",
      "Ctrl-c   - copy the name of the current sequence to the clipboard",
      "Ctrl-d   - delete the current sequence")

      txt.add <- c(
      "Shortcuts for the add mode:",
      "f        - flip the board",
      "z and Z  - show moves (own / computer) on/off",
      "c        - add a comment to the current move",
      "e and E  - edit the comments / edit the sequence file",
      "h and H  - show the best move according to Stockfish (fast / deep evaluation)",
      "y        - continuous analysis on/off (also in analysis mode)",
      "t        - take back the last move",
      "0        - make the current position the starting position for the sequence",
      "b        - start the board editor",
      "g        - show the evaluation graph (also in play and analysis mode)",
      "s        - save the sequence")

      txt.play <- c(
      "Shortcuts for the play and analysis mode:",
      "t        - take back the last move (computer move and own move)",
      "\\ or \U000000E4   - switch between the play and analysis mode",
      "0        - store the current moves and position as the main variation",
      "9        - jump to the main variation",
      "a and A  - switch to add mode")

   }

   if (lang == "de") {

      txt.general1 <- c(
      "Allgemeine Tastenk\U000000FCrzel:",
      "<space>  - Modus wechseln (Test/Hinzuf\U000000FCgen)",
      "\\ oder \U000000E4 - in den Spielmodus wechseln",
      "p        - Spieler ausw\U000000E4hlen",
      "Strg-r   - Spieler l\U000000F6schen",
      "n        - neue/n\U000000E4chste Sequenz starten",
      "l        - alle (ausgew\U000000E4hlten) Sequenzen auflisten",
      "^ oder 6 - Exponentenwert angeben",
      "/ oder , - Sequenzen ausw\U000000E4hlen",
      "|        - nach Sequenzen suchen (ohne sie auszuw\U000000E4hlen)",
      "* oder 8 - alle Sequenzen ausw\U000000E4hlen",
      "?        - alle Sequenzen ausw\U000000E4hlen, die mit den gleichen Z\U000000FCgen anfangen",
      "'        - alle Sequenzen ausw\U000000E4hlen, die die gleiche Stellung beinhalten",
      ";        - alle Sequenzen ausw\U000000E4hlen, die mit der gleichen Stellung enden",
      ".        - zuletzt gespeicherte Sequenz ausw\U000000E4hlen",
      ">        - Lesezeichen ausw\U000000E4hlen und bearbeiten",
      "m        - Sequenzauswahlmodus ausw\U000000E4hlen",
      "d        - Methode zur Schwierigkeitsberechnung ausw\U000000E4hlen",
      "w        - Warten nach abgeschlossenen Sequenzen an/aus",
      "-+       - Zeit zwischen den Z\U000000FCgen verringern/erh\U000000F6hen",
      "[]       - Lautst\U000000E4rke verringern/erh\U000000F6hen",
      "()       - Linienbreite verringern/erh\U000000F6hen",
      "{}       - Randbreite verringern/erh\U000000F6hen",
      "i        - Sprache wechseln (Englisch/Deutsch)",
      "v        - Bewertungsbalken an/aus",
      "k        - Materialunterschied an/aus",
      "C        - Brettkoordinaten an/aus",
      "Strg-f   - FEN der aktuellen Stellung in die Zwischenablage kopieren",
      "<escape> - Brett neu zeichnen / ein Men\U000000FC verlassen",
      "F1       - diese Hilfe anzeigen",
      "F2       - Rangliste und Spielerstatistiken anzeigen",
      "F3       - Einstellungen anzeigen",
      "F4       - Einstellungen f\U000000FCr Farben",
      "F5       - Einstellungen f\U000000FCr Textgr\U000000F6\U000000DFen",
      "F6       - Einstellungen f\U000000FCr Sonstiges",
      "F7       - Einstellungen f\U000000FCr Stockfish",
      "F8       - Sequenzverzeichnisse hinzuf\U000000FCgen, entfernen und ausw\U000000E4hlen",
      "F9       - die aktuelle Stellung auf lichess.org \U000000F6ffnen",
      "F10      - Histogramme/Streudiagramm anzeigen",
      "F11      - Sitzungsinfos anzeigen",
      "F12      - Sitzungsverlauf anzeigen",
      "q        - Trainer beenden")

      txt.general2 <- c(
      "Allgemeine Tastenk\U000000FCrzel:",
      "\U00002190 und \U00002192  - einen Zug zur\U000000FCck / vorw\U000000E4rts gehen",
      "1 oder \U00002191 - zum ersten Spielerzug oder an den Anfang der Sequenz/Partie springen",
      "2 oder \U00002193 - zum Ende der Sequenz/Partie springen",
      "3, 4, 5  - zum 1. Viertel, zur Mitte, zum 3. Viertel der Sequenz/Partie springen",
      "u        - Bewertungen f\U000000FCr die aktuelle Sequenz/Partie neu berechnen")

      txt.test <- c(
      "Tastenk\U000000FCrzel f\U000000FCr den Test Modus:",
      "h        - einen Tipp bekommen (kann zweimal gew\U000000E4hlt werden)",
      "t        - Punktekerh\U000000F6hung zur\U000000FCcknehmen",
      "o        - Punktewert f\U000000FCr aktuelle Sequenz manuell eingeben",
      "z        - Zen Modus an/aus",
      "x        - Zeitgesteuerter Modus an/aus",
      "<        - Lesezeichen f\U000000FCr aktuelle Sequenz setzen",
      "r        - letzte Sequenz wiederholen",
      "R        - Sequenzen nach Fehler automatisch wiederholen an/aus",
      "j        - springe zu einer Sequenznummer (f\U000000FCr sequenzielle Sequenzauswahlmodi)",
      "T        - Zielwert festlegen",
      "g        - Fortschrittsdiagramm f\U000000FCr die aktuelle Sequenz anzeigen",
      "G        - Fortschrittsdiagramm am Ende der Sequenzen anzeigen an/aus",
      "a und A  - akutelle Sequenz / Stellung erweitern (wechselt zum Hinzuf\U000000FCgen Modus)",
      "Strg-c   - den Namen der aktuellen Sequenz in die Zwischenablage kopieren",
      "Strg-d   - aktuelle Sequenz l\U000000F6schen")

      txt.add <- c(
      "Tastenk\U000000FCrzel f\U000000FCr den Hinzuf\U000000FCgen Modus:",
      "f        - Brett umdrehen",
      "z und Z  - Z\U000000FCge zeigen (eigene / Computer) an/aus",
      "c        - Kommentar zum aktuellen Zug hinzuf\U000000FCgen",
      "e und E  - Kommentare editieren / Sequenzfile editieren",
      "h und H  - den besten Zug laut Stockfish anzeigen (schnelle / tiefe Analyse)",
      "y        - kontinuierliche Analyse an/aus (auch im Analysemodus)",
      "t        - den letzten Zug zur\U000000FCcknehmen",
      "0        - aktuelle Stellung zur Ausgangsstellung der Sequenz machen",
      "b        - Bretteditor starten",
      "g        - Bewertungsdiagram anzeigen (auch im Spiel- und Analysemodus)",
      "s        - Sequenz abspeichern"
      )

      txt.play <- c(
      "Tastenk\U000000FCrzel f\U000000FCr den Spiel- und Analysemodus:",
      "t        - den letzten Zug zur\U000000FCcknehmen (Computerzug und eigener Zug)",
      "\\ oder \U000000E4 - zwischen dem Spiel- und Analysemodus wechseln",
      "0        - aktuelle Zugfolge und Stellung als Hauptvariante sichern",
      "9        - zur Hauptvariante springen",
      "a und A  - in den Hinzuf\U000000FCgen Modus wechseln")

   }

   page1 <- c(txt.general1)
   page2 <- c(txt.general2, "", txt.test, "", txt.add, "", txt.play)

   if (length(page1) > length(page2)) {
      page2 <- c(page2, rep("", length(page1)-length(page2)))
   } else {
      page1 <- c(page1, rep("", length(page2)-length(page1)))
   }

   if (page == 1) {
      txt <- page1
   } else {
      txt <- page2
   }

   langswitch <- FALSE

   cex1 <- .findcex(page1, font=font.mono, x1=1.5, x2=8.2, y1=1.5, y2=8.5)
   cex2 <- .findcex(page2, font=font.mono, x1=1.5, x2=8.2, y1=1.5, y2=8.5)
   cex <- min(cex1, cex2)

   ypos <- seq(8.5, 1.8, length.out=length(txt))

   redraw <- TRUE

   while (TRUE) {

      if (redraw) {
         rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=.get("lwd")+3)
         text(1.5, ypos, txt, pos=4, offset=0, cex=cex, family=font.mono, font=ifelse(grepl(":", txt), 2, 1), col=col.help)
         text(8.6, 1.4, paste0(page, " / 2"), pos=2, offset=0, cex=cex, family=font.mono, col=col.help)
         if (lang == "en")
            text(1.5, 1.4, "\U00002190/\U00002192: next page; t: show manual; m: show manual online", pos=4, offset=0, cex=cex, family=font.mono, col=col.help)
         if (lang == "de")
            text(1.5, 1.4, "\U00002190/\U00002192: n\U000000E4chste Seite; t: Anleitung zeigen; m: Anleitung online zeigen", pos=4, offset=0, cex=cex, family=font.mono, col=col.help)
      } else {
         redraw <- TRUE
      }

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(click))
         break

      if (identical(click, "F1") || identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-[") || identical(click, " "))
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

      if (identical(click, "i")) {
         if (lang == "de") {
            lang <- "en"
         } else {
            lang <- "de"
         }
         assign("lang", lang, envir=.chesstrainer)
         langswitch <- TRUE
         break
      }

      if (page == 1) {
         txt <- page2
         page <- 2
      } else {
         txt <- page1
         page <- 1
      }

   }

   if (langswitch)
      .showhelp(page=page)

   #.erase(1, 1, 9, 9)

   return()

}
