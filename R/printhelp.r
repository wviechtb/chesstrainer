.printhelp <- function(...) {

   ddd <- list(...)

   if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))

   lang <- .get("lang")

   if (lang == "en") {

      cat("--------------------------------------------------------------\n")
      cat("<space> - switch mode (play/add)\n")
      cat("p       - select a player\n")
      cat("Ctrl-r  - remove a player\n")
      cat("l       - list all sequences\n")
      cat("/       - find/select sequences\n")
      cat("n       - start a new sequence\n")
      cat("e       - edit the current sequence\n")
      cat("-/+     - decrease/increase the time between moves\n")
      cat("w       - pause between completed sequences on/off\n")
      cat("m       - sound on/off\n")
      cat("[/]     - decrease/increase the sound volume\n")
      cat("u       - update/redraw the board\n")
      cat("^       - specify the exponent value\n")
      cat("i       - toggle the language (English/German)\n")
      cat("r       - toggle between random/sequential mode\n")
      cat("(/)     - decrease/increase the line width\n")
      cat("F1      - print help\n")
      cat("F2      - show the leaderboard and player statistics\n")
      cat("q       - quit the trainer\n")
      cat("\n")
      cat("Only in play mode:\n")
      cat("h       - get a hint (can select twice)\n")
      cat("t       - take back a score adjustment due to a wrong move\n")
      cat("o       - manually edit the score for the current sequence\n")
      cat("Ctrl-d  - delete the current sequence\n")
      cat("a       - copy the current sequence and extend with more moves\n")
      cat("\n")
      cat("Only in add mode:\n")
      cat("f       - flip the board\n")
      cat("z       - show moves on/off\n")
      cat("c       - add a comment to the current move\n")
      cat("0       - make the current position the starting position\n")
      cat("b       - take move back\n")
      cat("s       - save the sequence\n")
      cat("--------------------------------------------------------------\n")

   }

   if (lang == "de") {

      cat("--------------------------------------------------------------------\n")
      cat("<space> - Modus wechseln (spielen/hinzuf\U000000FCgen)\n")
      cat("p       - Spieler ausw\U000000E4hlen\n")
      cat("Strg-r  - Spieler l\U000000F6schen\n")
      cat("l       - alle Sequenzen auflisten\n")
      cat("/       - Sequenzen finden/ausw\U000000E4hlen\n")
      cat("n       - neue Sequenz starten\n")
      cat("e       - aktuelle Sequenz editieren\n")
      cat("-/+     - Zeit zwischen Z\U000000FCgen verringern/erh\U000000F6hen\n")
      cat("w       - Pausen zwischen abgeschlossenen Sequenzen an/aus\n")
      cat("m       - Ton an/aus\n")
      cat("[/]     - Lautst\U000000E4rke verringern/erh\U000000F6hen\n")
      cat("u       - Brett neu zeichnen\n")
      cat("^       - Exponentenwert angeben\n")
      cat("i       - wechsel die Sprache (Englisch/Deutsch)\n")
      cat("r       - wechsel zwischen Zufalls/sequenziellen Modus\n")
      cat("(/)     - Linienbreite verringern/erh\U000000F6hen\n")
      cat("F1      - Hilfe drucken\n")
      cat("F2      - Rangliste und Spielerstatistiken anzeigen\n")
      cat("q       - Trainer beenden\n")
      cat("\n")
      cat("Nur im Spiel Modus:\n")
      cat("h       - einen Tipp bekommen (kann zweimal gew\U000000E4hlt werden)\n")
      cat("t       - Punktekorrektur aufgrund eines falschen Zuges zur\U000000FCcknehmen\n")
      cat("o       - Punktewert f\U000000FCr die aktuelle Sequenz manuell eingeben\n")
      cat("Strg-d  - aktuelle Sequenz l\U000000F6schen\n")
      cat("a       - aktuelle Sequenz kopieren und mit weiteren Z\U000000FCgen erweitern\n")
      cat("\n")
      cat("Nur im Hinzuf\U000000FCgen Modus:\n")
      cat("f       - Brett umdrehen\n")
      cat("z       - Z\U000000FCge zeigen an/aus\n")
      cat("c       - Kommentar zum aktuellen Zug hinzuf\U000000FCgen\n")
      cat("0       - aktuelle Position zur Ausgangsposition machen\n")
      cat("b       - Zug zur\U000000FCck nehmen\n")
      cat("s       - Sequenz abspeichern\n")
      cat("--------------------------------------------------------------------\n")

   }

   if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))

}