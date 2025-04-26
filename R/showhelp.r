.showhelp <- function(lwd) {

   .clearsideindicator()
   .drawtimer(clear=TRUE)

   lang <- .get("lang")

   if (lang == "en") {

      txt <- c(
      "General shortcuts:",
      "<space>  - switch mode (test/add)",
      "\\        - switch to play mode",
      "p        - select a player",
      "Ctrl-r   - remove a player",
      "n        - start a new sequence",
      "l        - list all sequences",
      "/ and .  - select sequences / select the last saved sequence",
      "* and 8  - select all sequences",
      "?        - find all sequences with the same moves",
      "m        - choose a sequence selection mode",
      "-+       - decrease/increase the time between moves",
      "w        - wait after completed sequences on/off",
      "x        - timed mode on/off",
      "[]       - decrease/increase the sound volume",
      "^        - specify the exponent value",
      "i        - toggle the language (English/German)",
      "v        - evaluation bar on/off",
      "()       - decrease/increase the line width",
      "<escape> - redraw the board",
      "F1       - show this help",
      "F2       - show the leaderboard and player statistics",
      "F3       - show current settings",
      "F4-F7    - settings for colors, text, miscellanea, and Stockfish",
      "F8       - add, remove, and select sequence directories",
      "F9       - open the current position on lichess.org",
      "F10      - show histograms/scatterplot of the sequence statistics",
      "Ctrl-c   - copy the FEN of the current position to the clipboard",
      "q        - quit the trainer",
      "",
      "Shortcuts for the test mode:",
      "h        - get a hint (can select twice)",
      "t        - take back a score increase",
      "o        - manually enter the score for the current sequence",
      "r and R  - repeat the last sequence / toggle repeat after mistake",
      "g and G  - show the progress graph / toggle always show at end",
      "a and A  - extend the current sequence/position with more moves",
      "Ctrl-d   - delete the current sequence",
      "u        - recalculate the evaluations",
      "",
      "Shortcuts for the add mode:",
      "f        - flip the board",
      "z        - show moves (play automatically) on/off",
      "c        - add a comment to the current move",
      "e and E  - edit the comments / edit the sequence file",
      "h and H  - show the best move according to Stockfish",
      "0        - make the current position the starting position",
      "t        - take back move",
      "b        - start the board editor",
      "s        - save the sequence"
      )

   }

   if (lang == "de") {

      txt <- c(
      "Allgemeine Tastenk\U000000FCrzel:",
      "<space>  - Modus wechseln (Test/Hinzuf\U000000FCgen)",
      "\\        - in den Spielmodus wechseln",
      "p        - Spieler ausw\U000000E4hlen",
      "Strg-r   - Spieler l\U000000F6schen",
      "n        - neue Sequenz starten",
      "l        - alle Sequenzen auflisten",
      "/ und .  - Sequenzen ausw\U000000E4hlen / zuletzt gespeicherte ausw\U000000E4hlen",
      "* und 8  - alle Sequenzen ausw\U000000E4hlen",
      "?        - alle Sequenzen mit den gleichen Z\U000000FCgen finden",
      "m        - Sequenzauswahlmodus w\U000000E4hlen",
      "-+       - Zeit zwischen den Z\U000000FCgen verringern/erh\U000000F6hen",
      "w        - Warten nach abgeschlossenen Sequenzen an/aus",
      "x        - Zeitgesteuerter Modus an/aus",
      "[]       - Lautst\U000000E4rke verringern/erh\U000000F6hen",
      "^        - Exponentenwert angeben",
      "i        - Sprache wechseln (Englisch/Deutsch)",
      "v        - Bewertungsbalken an/aus",
      "()       - Linienbreite verringern/erh\U000000F6hen",
      "<escape> - Brett neu zeichnen",
      "F1       - diese Hilfe anzeigen",
      "F2       - Rangliste und Spielerstatistiken anzeigen",
      "F3       - Einstellungen anzeigen",
      "F4-F7    - Einstellungen f\U000000FCr Farben, Text, Sonstiges, und Stockfish",
      "F8       - Sequenzverzeichnisse hinzuf\U000000FCgen, entfernen und ausw\U000000E4hlen",
      "F9       - die aktuelle Position auf lichess.org \U000000F6ffnen",
      "F10      - Histogramme/Streudiagramm der Sequenzstatistiken anzeigen",
      "Strg-c   - FEN der aktuellen Position in die Zwischenablage kopieren",
      "q        - Trainer beenden",
      "",
      "Tastenk\U000000FCrzel f\U000000FCr den Test Modus:",
      "h        - einen Tipp bekommen (kann zweimal gew\U000000E4hlt werden)",
      "t        - Punktekerh\U000000F6hung zur\U000000FCcknehmen",
      "o        - Punktewert f\U000000FCr die aktuelle Sequenz manuell eingeben",
      "r und R  - die letzte Sequenz wiederholen / nach Fehler wiederholen an/aus",
      "g und G  - Fortschrittsdiagramm anzeigen / immer am Ende anzeigen an/aus",
      "a und A  - die aktuelle Sequenz/Position mit Z\U000000FCgen erweitern",
      "Strg-d   - aktuelle Sequenz l\U000000F6schen",
      "u        - Bewertungen neu berechnen",
      "",
      "Tastenk\U000000FCrzel f\U000000FCr den Hinzuf\U000000FCgen Modus:",
      "f        - Brett umdrehen",
      "z        - Z\U000000FCge zeigen (automatisch spielen) an/aus",
      "c        - Kommentar zum aktuellen Zug hinzuf\U000000FCgen",
      "e und E  - Kommentar editieren / Sequenzfile editieren",
      "h und H  - den besten Zug laut Stockfish anzeigen",
      "0        - aktuelle Position zur Ausgangsposition machen",
      "t        - Zug zur\U000000FCcknehmen",
      "b        - Bretteditor starten",
      "s        - Sequenz abspeichern"
      )

   }

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
   maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
   cex <- (8.5 - 1.5) / max(maxsw, maxsh) * 0.90

   text(1+0.5, seq(8.5, 1.5, length.out=length(txt)), txt, pos=4, cex=cex,
        family=.get("font.mono"), font=ifelse(grepl(":", txt), 2, 1), col=.get("col.help"))

   .waitforclick()

}
