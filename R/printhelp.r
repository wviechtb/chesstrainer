.printhelp <- function(lwd) {

   lang <- .get("lang")

   if (lang == "en") {

      txt <- c(
      "General shortcuts:",
      "<space>  - switch mode (play/add)",
      "p        - select a player",
      "Ctrl-r   - remove a player",
      "n        - start a new sequence",
      "l        - list all sequences",
      "/ and .  - select sequences / select the last saved sequence",
      "* and 8  - select all sequences",
      "?        - find all sequences with the same moves",
      "e and E  - edit the comments / edit the sequence file",
      "-+       - decrease/increase the time between moves",
      "w        - pause after completing sequences on/off",
      "x        - timed mode on/off",
      "[]       - decrease/increase the sound volume",
      "^        - specify the exponent value",
      "i        - toggle the language (English/German)",
      "m        - choose a sequence selection mode",
      "v        - evaluation bar on/off",
      "()       - decrease/increase the line width",
      "<escape> - redraw the board",
      "F1       - show this help",
      "F2       - show the leaderboard and player statistics",
      "F3       - show current settings",
      "F4-F7    - adjust settings for colors, text sizes, miscellanea, and Stockfish",
      "F8       - add, remove, and select sequence directories",
      "F9       - print the FEN of the current position and open it on lichess.org",
      "F10      - show histograms/scatterplot of the sequence statistics",
      "Ctrl-c   - copy the FEN of the current position to the clipboard",
      "q        - quit the trainer",
      "",
      "Shortcuts in play mode:",
      "h        - get a hint (can select twice)",
      "t        - take back a score increase due to a wrong move",
      "o        - manually enter the score for the current sequence",
      "r        - repeat the last played sequence",
      "g and G  - show the progress graph for the sequence / always show at end",
      "a and A  - copy the current sequence/position and extend it with more moves",
      "Ctrl-d   - delete the current sequence",
      "u        - recalculate the evaluations",
      "",
      "Shortcuts in add mode:",
      "f        - flip the board",
      "z        - show moves (play automatically) on/off",
      "c        - add a comment to the current move",
      "h and H  - show the best move according to Stockfish (if Stockfish is running)",
      "0        - make the current position the starting position for the sequence",
      "t        - take back move",
      "b        - start the board editor",
      "s        - save the sequence"
      )

   }

   if (lang == "de") {

      txt <- c(
      "Allgemeine Tastenk\U000000FCrzel:",
      "<space>  - Modus wechseln (spielen/hinzuf\U000000FCgen)",
      "p        - Spieler ausw\U000000E4hlen",
      "Strg-r   - Spieler l\U000000F6schen",
      "n        - neue Sequenz starten",
      "l        - alle Sequenzen auflisten",
      "/ und .  - Sequenzen ausw\U000000E4hlen / zuletzt gespeicherte Sequenz ausw\U000000E4hlen",
      "* und 8  - alle Sequenzen ausw\U000000E4hlen",
      "?        - alle Sequenzen mit den gleichen Z\U000000FCgen finden",
      "e und E  - Kommentar editieren / Sequenzfile editieren",
      "-+       - Zeit zwischen den Z\U000000FCgen verringern/erh\U000000F6hen",
      "w        - Pause nach abgeschlossenen Sequenzen an/aus",
      "x        - Zeitgesteuerter Modus an/aus",
      "[]       - Lautst\U000000E4rke verringern/erh\U000000F6hen",
      "^        - Exponentenwert angeben",
      "i        - Sprache wechseln (Englisch/Deutsch)",
      "m        - Sequenzauswahlmodus w\U000000E4hlen",
      "v        - Bewertungsbalken an/aus",
      "()       - Linienbreite verringern/erh\U000000F6hen",
      "<escape> - Brett neu zeichnen",
      "F1       - diese Hilfe anzeigen",
      "F2       - Rangliste und Spielerstatistiken anzeigen",
      "F3       - Einstellungen anzeigen",
      "F4-F7    - Einstellungen f\U000000FCr Farben, Textgr\U000000F6\U000000DFen, Sonstiges, und Stockfish \U000000E4ndern",
      "F8       - Sequenzverzeichnisse hinzuf\U000000FCgen, entfernen und ausw\U000000E4hlen",
      "F9       - die FEN der aktuellen Position drucken und auf lichess.org \U000000F6ffnen",
      "F10      - Histogramme/Streudiagramm der Sequenzstatistiken anzeigen",
      "Strg-c   - die FEN der aktuellen Position in die Zwischenablage kopieren",
      "q        - Trainer beenden",
      "",
      "Tastenk\U000000FCrzel im Spiel Modus:",
      "h        - einen Tipp bekommen (kann zweimal gew\U000000E4hlt werden)",
      "t        - Punktekerh\U000000F6hung aufgrund eines falschen Zuges zur\U000000FCcknehmen",
      "o        - Punktewert f\U000000FCr die aktuelle Sequenz manuell eingeben",
      "r        - die zuletzt gespielte Sequenz wiederholen",
      "g und G  - Fortschrittsdiagramm f\U000000FCr die Sequenz anzeigen / immer am Ende anzeigen",
      "a und A  - aktuelle Sequenz/Position kopieren und mit Z\U000000FCgen erweitern",
      "Strg-d   - aktuelle Sequenz l\U000000F6schen",
      "u        - Bewertungen neu berechnen",
      "",
      "Tastenk\U000000FCrzel im Hinzuf\U000000FCgen Modus:",
      "f        - Brett umdrehen",
      "z        - Z\U000000FCge zeigen (automatisch spielen) an/aus",
      "c        - Kommentar zum aktuellen Zug hinzuf\U000000FCgen",
      "h und H  - den besten Zug laut Stockfish anzeigen (falls Stockfish l\U000000E4uft)",
      "0        - aktuelle Position zur Ausgangsposition der Sequenz machen",
      "t        - Zug zur\U000000FCcknehmen",
      "b        - Bretteditor starten",
      "s        - Sequenz abspeichern"
      )

   }

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
   maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
   cex <- (8.5 - 1.5) / max(maxsw, maxsh) * 0.95

   text(1+0.5, seq(8.5, 1.5, length.out=length(txt)), txt, pos=4, cex=cex,
        family=.get("font.mono"), font=ifelse(grepl(":", txt), 2, 1), col=.get("col.help"))

   getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button, x, y) return(""), onKeybd=function(key) return(""))

}
