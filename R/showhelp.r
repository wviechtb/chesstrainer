.showhelp <- function(lwd, page=1) {

   #.clearsideindicator()
   #.drawtimer(clear=TRUE)

   lang <- .get("lang")

   if (lang == "en") {

      txt.general <- c(
      "General shortcuts:",
      "<space>  - switch mode (test/add)",
      "\\ or #   - switch to play mode",
      "p        - select a player",
      "Ctrl-r   - remove a player",
      "n        - start new/next sequence",
      "l        - list all (selected) sequences",
      "/ or ,   - select sequences",
      "* or 8   - select all sequences",
      "^ or 6   - specify the exponent value",
      "?        - select all sequences with the same moves",
      "'        - select all sequences with the same position",
      ".        - select the last saved sequence",
      ">        - select and manage bookmarks",
      "m        - choose a sequence selection mode",
      "d        - choose a difficulty calculation method",
      "x        - timed mode on/off",
      "w        - wait after completed sequences on/off",
      "-+       - decrease/increase the time between moves",
      "[]       - decrease/increase the sound volume",
      "()       - decrease/increase the line width",
      "i        - toggle the language (English/German)",
      "v        - evaluation bar on/off",
      "Ctrl-c   - copy the FEN of the current position to the clipboard",
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

      txt.test <- c(
      "Shortcuts for the test mode:",
      "h        - get a hint (can be selected twice)",
      "t        - take back a score increase",
      "o        - manually enter the score for the current sequence",
      "<        - bookmark the current sequence",
      "r        - repeat the last sequence",
      "1 or \U00002191   - restart the current sequence",
      "R        - automatically repeat sequences after a mistake on/off",
      "g        - show the progress graph for the current sequence",
      "G        - show the progress graph at the end of the sequences on/off",
      "a        - extend the current sequence with more moves",
      "A        - extend the current position with more moves",
      "u        - recalculate the evaluations for the current sequence",
      "Ctrl-s   - copy the name of the current sequence to the clipboard",
      "Ctrl-d   - delete the current sequence")

      txt.add <- c(
      "Shortcuts for the add mode:",
      "f        - flip the board",
      "z        - show moves (play automatically) on/off",
      "c        - add a comment to the current move",
      "e and E  - edit the comments / edit the sequence file",
      "h and H  - show the best move according to Stockfish (fast / deep)",
      "0        - make the current position the starting position",
      "t or \U00002190   - take back the last move",
      "g        - show the evaluation graph",
      "b        - start the board editor",
      "s        - save the sequence")

      txt.play <- c(
      "Shortcuts for the play and analysis mode:",
      "g        - show the evaluation graph",
      "t        - take back the last move",
      "\\ or #   - switch between the play and analysis mode",
      "\U00002190 and \U00002192  - go back / forward one move",
      "1 or \U00002191   - jump to the start of the game",
      "2 or \U00002193   - jump to the end of the game",
      "0        - store the current game as the main variation",
      "9        - jump to the main variation")

   }

   if (lang == "de") {

      txt.general <- c(
      "Allgemeine Tastenk\U000000FCrzel:",
      "<space>  - Modus wechseln (Test/Hinzuf\U000000FCgen)",
      "\\ oder # - in den Spielmodus wechseln",
      "p        - Spieler ausw\U000000E4hlen",
      "Strg-r   - Spieler l\U000000F6schen",
      "n        - neue/n\U000000E4chste Sequenz starten",
      "l        - alle (ausgew\U000000E4hlten) Sequenzen auflisten",
      "/ oder , - Sequenzen ausw\U000000E4hlen",
      "* oder 8 - alle Sequenzen ausw\U000000E4hlen",
      "^ oder 6 - Exponentenwert angeben",
      "?        - alle Sequenzen mit den gleichen Z\U000000FCgen ausw\U000000E4hlen",
      "'        - alle Sequenzen mit der gleicher Position ausw\U000000E4hlen",
      ".        - zuletzt gespeicherte Sequenz ausw\U000000E4hlen",
      ">        - Lesezeichen ausw\U000000E4hlen und bearbeiten",
      "m        - Sequenzauswahlmodus ausw\U000000E4hlen",
      "d        - Methode zur Schwierigkeitsberechnung ausw\U000000E4hlen",
      "x        - Zeitgesteuerter Modus an/aus",
      "w        - Warten nach abgeschlossenen Sequenzen an/aus",
      "-+       - Zeit zwischen den Z\U000000FCgen verringern/erh\U000000F6hen",
      "[]       - Lautst\U000000E4rke verringern/erh\U000000F6hen",
      "()       - Linienbreite verringern/erh\U000000F6hen",
      "i        - Sprache wechseln (Englisch/Deutsch)",
      "v        - Bewertungsbalken an/aus",
      "Strg-c   - FEN der aktuellen Position in die Zwischenablage kopieren",
      "<escape> - Brett neu zeichnen / ein Men\U000000FC verlassen",
      "F1       - diese Hilfe anzeigen",
      "F2       - Rangliste und Spielerstatistiken anzeigen",
      "F3       - Einstellungen anzeigen",
      "F4       - Einstellungen f\U000000FCr Farben",
      "F5       - Einstellungen f\U000000FCr Textgr\U000000F6\U000000DFen",
      "F6       - Einstellungen f\U000000FCr Sonstiges",
      "F7       - Einstellungen f\U000000FCr Stockfish",
      "F8       - Sequenzverzeichnisse hinzuf\U000000FCgen, entfernen und ausw\U000000E4hlen",
      "F9       - die aktuelle Position auf lichess.org \U000000F6ffnen",
      "F10      - Histogramme/Streudiagramm anzeigen",
      "F11      - Sitzungsinfos anzeigen",
      "F12      - Sitzungsverlauf anzeigen",
      "q        - Trainer beenden")

      txt.test <- c(
      "Tastenk\U000000FCrzel f\U000000FCr den Test Modus:",
      "h        - einen Tipp bekommen (kann zweimal gew\U000000E4hlt werden)",
      "t        - Punktekerh\U000000F6hung zur\U000000FCcknehmen",
      "o        - Punktewert f\U000000FCr die aktuelle Sequenz manuell eingeben",
      "<        - Lesezeichen f\U000000FCr die aktuelle Sequenz setzen",
      "r        - die letzte Sequenz wiederholen",
      "1 oder \U00002191 - die aktuelle Sequenz neu starten",
      "R        - Sequenzen nach Fehler automatisch wiederholen an/aus",
      "g        - Fortschrittsdiagramm f\U000000FCr die aktuelle Sequenz anzeigen",
      "G        - Fortschrittsdiagramm am Ende der Sequenzen anzeigen an/aus",
      "a        - die aktuelle Sequenz mit Z\U000000FCgen erweitern",
      "A        - die aktuelle Position mit Z\U000000FCgen erweitern",
      "u        - Bewertungen f\U000000FCr die aktuelle Sequenz neu berechnen",
      "Strg-s   - den Namen der aktuellen Sequenz in die Zwischenablage kopieren",
      "Strg-d   - die aktuelle Sequenz l\U000000F6schen")

      txt.add <- c(
      "Tastenk\U000000FCrzel f\U000000FCr den Hinzuf\U000000FCgen Modus:",
      "f        - Brett umdrehen",
      "z        - Z\U000000FCge zeigen (automatisch spielen) an/aus",
      "c        - Kommentar zum aktuellen Zug hinzuf\U000000FCgen",
      "e und E  - Kommentare editieren / Sequenzfile editieren",
      "h und H  - den besten Zug laut Stockfish anzeigen (schnell / tief)",
      "0        - die aktuelle Position zur Ausgangsposition machen",
      "t oder \U00002190 - den letzten Zug zur\U000000FCcknehmen",
      "g        - Bewertungsdiagram anzeigen",
      "b        - Bretteditor starten",
      "s        - Sequenz abspeichern"
      )

      txt.play <- c(
      "Tastenk\U000000FCrzel f\U000000FCr den Spiel- und Analysemodus:",
      "g        - Bewertungsdiagram anzeigen",
      "t        - den letzten Zug zur\U000000FCcknehmen",
      "\\ oder # - zwischen dem Spiel- und Analysemodus wechseln",
      "\U00002190 und \U00002192  - einen Zug zur\U000000FCck / vorw\U000000E4rts gehen",
      "1 oder \U00002191 - zum Anfang der Partie springen",
      "2 oder \U00002193 - zum Ende der Partie springen",
      "0        - aktuelles Spiel als Hauptvariante sichern",
      "9        - zur Hauptvariante springen")

   }

   page1 <- c(txt.general)
   page2 <- c(txt.test, "", txt.add, "", txt.play)

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

   font.mono <- .get("font.mono")
   maxsw <- max(strwidth(page1, family=font.mono), strwidth(page2, family=font.mono))
   maxsh <- strheight("A", family=font.mono) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.85)

   ypos <- seq(8.5, 1.5, length.out=length(txt))

   while (TRUE) {

      rect(1.2, 1.2, 8.8, 8.8, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

      text(1+0.5, ypos, txt, pos=4, cex=cex, family=font.mono, font=ifelse(grepl(":", txt), 2, 1), col=.get("col.help"))
      text(8.8-0.1, 1.4, paste0(page, " / 2"), pos=2, cex=cex, family=font.mono, col=.get("col.help"))

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(click))
         break

      if (identical(click, "F1") || identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-["))
         break

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
      .showhelp(lwd=lwd, page=page)

   #.erase(1, 1, 9, 9)

   return()

}
