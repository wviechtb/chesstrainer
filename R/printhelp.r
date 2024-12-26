.printhelp <- function(lwd) {

   lang <- .get("lang")

   if (lang == "en") {

      txt <- c(
      "General shortcuts:",
      "<space> - switch mode (play/add)",
      "p       - select a player",
      "Ctrl-r  - remove a player",
      "l       - list all sequences",
      "/       - find/select sequences",
      "n       - start a new sequence",
      "e       - edit the current sequence",
      "-/+     - decrease/increase the time between moves",
      "w       - pause between completed sequences on/off",
      "m       - sound on/off",
      "[/]     - decrease/increase the sound volume",
      "u       - update/redraw the board",
      "^       - specify the exponent value",
      "i       - toggle the language (English/German)",
      "r       - toggle between random/sequential mode",
      "(/)     - decrease/increase the line width",
      "F1      - show this help",
      "F2      - show the leaderboard and player statistics",
      "F3      - show current settings",
      "F4      - adjust colors",
      "F5      - adjust text sizes",
      "q       - quit the trainer",
      "",
      "Only in play mode:",
      "h       - get a hint (can select twice)",
      "t       - take back a score increase due to a wrong move",
      "o       - manually edit the score for the current sequence",
      "Ctrl-d  - delete the current sequence",
      "a       - copy the current sequence and extend with more moves",
      "",
      "Only in add mode:",
      "f       - flip the board",
      "z       - show moves on/off",
      "c       - add a comment to the current move",
      "0       - make the current position the starting position",
      "t       - take back move",
      "s       - save the sequence",
      "?       - find all sequences with the same moves"
      )

   }

   if (lang == "de") {

      txt <- c(
      "Allgemeine Tastenk\U000000FCrzel:",
      "<space> - Modus wechseln (spielen/hinzuf\U000000FCgen)",
      "p       - Spieler ausw\U000000E4hlen",
      "Strg-r  - Spieler l\U000000F6schen",
      "l       - alle Sequenzen auflisten",
      "/       - Sequenzen finden/ausw\U000000E4hlen",
      "n       - neue Sequenz starten",
      "e       - aktuelle Sequenz editieren",
      "-/+     - Zeit zwischen den Z\U000000FCgen verringern/erh\U000000F6hen",
      "w       - Pausen zwischen abgeschlossenen Sequenzen an/aus",
      "m       - Ton an/aus",
      "[/]     - Lautst\U000000E4rke verringern/erh\U000000F6hen",
      "u       - Brett neu zeichnen",
      "^       - Exponentenwert angeben",
      "i       - wechsel die Sprache (Englisch/Deutsch)",
      "r       - wechsel zwischen Zufalls/sequenziellen Modus",
      "(/)     - Linienbreite verringern/erh\U000000F6hen",
      "F1      - diese Hilfe anzeigen",
      "F2      - Rangliste und Spielerstatistiken anzeigen",
      "F3      - Einstellungen anzeigen",
      "F4      - Farben ausw\U000000E4hlen",
      "F5      - Textgr\U000000F6\U000000DFen ausw\U000000E4hlen",
      "q       - Trainer beenden",
      "",
      "Nur im Spiel Modus:",
      "h       - einen Tipp bekommen (kann zweimal gew\U000000E4hlt werden)",
      "t       - Punktekerh\U000000F6hung aufgrund eines falschen Zuges zur\U000000FCcknehmen",
      "o       - Punktewert f\U000000FCr die aktuelle Sequenz manuell eingeben",
      "Strg-d  - aktuelle Sequenz l\U000000F6schen",
      "a       - aktuelle Sequenz kopieren und mit weiteren Z\U000000FCgen erweitern",
      "",
      "Nur im Hinzuf\U000000FCgen Modus:",
      "f       - Brett umdrehen",
      "z       - Z\U000000FCge zeigen an/aus",
      "c       - Kommentar zum aktuellen Zug hinzuf\U000000FCgen",
      "0       - aktuelle Position zur Ausgangsposition machen",
      "t       - Zug zur\U000000FCcknehmen",
      "s       - Sequenz abspeichern",
      "?       - alle Sequenzen mit den gleichen Z\U000000FCgen finden"
      )

   }

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
   maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
   cex <- (8.5 - 1.5) / max(maxsw, maxsh) * 0.95

   text(1+0.5, seq(8.5, 1.5, length.out=length(txt)), txt, pos=4, cex=cex,
        family=.get("font.mono"), font=ifelse(grepl(":", txt), 2, 1), col=.get("col.help"))

   getGraphicsEvent(prompt="", onMouseDown=function(button, x, y) return(""), onKeybd=function(key) return(""))

}
