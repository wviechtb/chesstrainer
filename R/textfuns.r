.text <- function(x, arg) {

   lang <- .get("lang")

   if (x == "testdevice") {
      return(switch(lang,
                    de = "Das graphische Ausgabeger\U000000E4t unterst\U000000FCtzt keine Ereignisbehandlung.\nVielleicht klappt es mit x11() und dann play().",
                         "The graphics device does not support event handling.\nTry running x11() and then play()."
   ))}

   if (x == "modecheck") {
      return(switch(lang,
                    de = "Parameter 'mode' muss entweder 'add' oder 'play' sein.",
                         "Argument 'mode' must be either 'add' or 'play'."
   ))}

   if (x == "createconfigdir") {
      return(switch(lang,
                    de = paste0("\nErstelle Standardverzeichnis f\U000000FCr Einstellungen: ", arg, "\n"),
                         paste0("\nCreating default directory for settings: ", arg, "\n")
   ))}

   if (x == "createseqdir") {
      return(switch(lang,
                    de = paste0("\nErstelle Standardverzeichnis f\U000000FCr Sequenzen: ", arg, "\n"),
                         paste0("\nCreating default directory for sequences: ", arg, "\n")
   ))}

   if (x == "dircreateerror") {
      return(switch(lang,
                    de = "Verzeichnis f\U000000FCr kann nicht erstellt werden.",
                         "Cannot create directory."
   ))}

   if (x == "copyseqs") {
      return(switch(lang,
                    de = "Beispielsequenzen in das Sequenzverzeichnis kopieren? (j/N): ",
                         "Copy example sequences to the sequence directory? (y/N): "
   ))}

   if (x == "dirnotexists") {
      return(switch(lang,
                    de = "Das angegebene Sequenzverzeichnis existiert nicht.",
                         "Specified sequence directory does not exist."
   ))}

   if (x == "noreadaccess") {
      return(switch(lang,
                    de = "Keine Leseberechtigung f\U000000FCr das angegebene Sequenzverzeichnis.",
                         "No read permissions for the specified sequence directory."
   ))}

   if (x == "nowriteaccess") {
      return(switch(lang,
                    de = "Keine Schreibberechtigung f\U000000FCr das angegebene Sequenzverzeichnis.",
                         "No write permissions for the specified sequence directory."
   ))}

   if (x == "zeroseqsfound") {
      return(switch(lang,
                    de = "Keine Sequenzen gefunden ...\n",
                         "No sequences found ...\n"
   ))}

   if (x == "newplayername") {
      return(switch(lang,
                    de = "Gib einen neuen Spielernamen ein: ",
                         "Enter a new player name: "
   ))}

   if (x == "enterplayer") {
      return(switch(lang,
                    de = "Gib einen (neuen) Spielernamen ein oder die Spielernummer: ",
                         "Enter a (new) player name or a player number: "
   ))}

   if (x == "foundplayers") {
      return(switch(lang,
                    de = "Folgende Spieler gefunden:\n\n",
                         "Found the following players:\n\n"
   ))}

   if (x == "rlydelplayer") {
      return(switch(lang,
                    de = paste0("Wirlich Spieler '", arg, "' l\U000000F6schen? (j/N): "),
                         paste0("Really delete player '", arg, "'? (y/N): ")
   ))}

   if (x == "pauseon") {
      return(switch(lang,
                    de = "Pause an ...",
                         "Pause on ..."
   ))}

   if (x == "pauseoff") {
      return(switch(lang,
                    de = "Pause aus ...",
                         "Pause off ..."
   ))}

   if (x == "showmoveson") {
      return(switch(lang,
                    de = "Zug zeigen an ...",
                         "Show moves on ..."
   ))}

   if (x == "showmovesoff") {
      return(switch(lang,
                    de = "Zug zeigen aus ...",
                         "Show moves off ..."
   ))}

   if (x == "soundon") {
      return(switch(lang,
                    de = "Sound an ...",
                         "Sound on ..."
   ))}

   if (x == "soundoff") {
      return(switch(lang,
                    de = "Sound aus ...",
                         "Sound off ..."
   ))}

   if (x == "comment") {
      return(switch(lang,
                    de = "Kommentar zum Zug: ",
                         "Comment for the move: "
   ))}

   if (x == "saveseq") {
      return(switch(lang,
                    de = "Sequenz abspeichern ...",
                         "Save sequence ..."
   ))}

   if (x == "seqexists") {
      return(switch(lang,
                    de = paste0("Sequenz existiert bereits! (", arg, ")\n"),
                         paste0("Sequence exists already! (", arg, ")\n")
   ))}

   if (x == "addmoves") {
      return(switch(lang,
                    de = "F\U000000FCge der Sequenz weitere Z\U000000FCge hinzu.",
                         "Add more moves to the sequence."
   ))}

   if (x == "filename") {
      return(switch(lang,
                    de = "Dateiname: ",
                         "Filename: "
   ))}

   if (x == "rlyoverwrite") {
      return(switch(lang,
                    de = "Sequenz \U000000FCberschreiben? (j/N): ",
                         "Overwrite the sequence? (y/N): "
   ))}

   if (x == "overwrite") {
      return(switch(lang,
                    de = "Sequenz wird \U000000FCberschrieben ...\n",
                         "Sequence will be overwritten ...\n"
   ))}

   if (x == "played") {
      return(switch(lang,
                    de = "Gespielt",
                         "Played"
   ))}

   if (x == "score") {
      return(switch(lang,
                    de = "Punkte",
                         "Score"
   ))}

   if (x == "prob") {
      return(switch(lang,
                    de = "Wahrsch (%)",
                         "Prob (%)"
   ))}

   if (x == "player") {
      return(switch(lang,
                    de = "Spieler",
                         "Player"
   ))}

   if (x == "rlydelseq") {
      return(switch(lang,
                    de = "Die aktuelle Sequenz l\U000000F6schen (j/N): ",
                         "Delete the current sequence? (y/N): "
   ))}

   if (x == "delseq") {
      return(switch(lang,
                    de = "L\U000000F6sche die Sequenz ...\n",
                         "Deleting the sequence ...\n"
   ))}

   if (x == "seqsearch") {
      return(switch(lang,
                    de = "Sequenzsuche ('Begriff', 'Zahl1-Zahl2', 'Zahl', 'Punkte >|<(=)? Wert', oder 'Gespielt >|<(=)? Wert'): ",
                         "Sequence search ('String', 'Number1-Number2', 'Number', 'Score >|<(=)? Value', oder 'Played >|<(=)? Value'): "
   ))}

   if (x == "allseqselected") {
      return(switch(lang,
                    de = "Alle Sequenzen selektiert.\n",
                         "All sequences selected.\n"
   ))}

   if (x == "noseqsfound") {
      return(switch(lang,
                    de = "Keine passenden Sequenzen gefunden.\n",
                         "No matching sequences found.\n"
   ))}

   if (x == "noseqfound") {
      return(switch(lang,
                    de = "Keine passende Sequenz gefunden.\n",
                         "No matching sequence found.\n"
   ))}

   if (x == "selseq12") {
      return(switch(lang,
                    de = paste0("Selektiere Sequenzen ", arg[1], " bis ", arg[2], ".\n"),
                         paste0("Selecting sequences ", arg[1], " to ", arg[2], ".\n")
   ))}

   if (x == "selseq") {
      return(switch(lang,
                    de = paste0("Selektiere Sequenz ", arg, ".\n"),
                         paste0("Selecting sequence ", arg, ".\n")
   ))}

   if (x == "strcapscore") {
      return(switch(lang,
                    de = "^[P|p](unkte)?\\s*(>|<|>=|<=)\\s*([[:digit:]]+)$",
                         "^[S|s](core)?\\s*(>|<|>=|<=)\\s*([[:digit:]]+)$"
   ))}

   if (x == "selseqscore") {
      return(switch(lang,
                    de = paste0("Suche nach Sequenzen mit Punkten ", arg[[1]], " ", arg[[2]], ".\n"),
                         paste0("Searching for sequences with score ", arg[[1]], " ", arg[[2]], ".\n")
   ))}

   if (x == "numseqfound") {
      return(switch(lang,
                    de = paste0(arg, " passende Sequenzen gefunden.\n"),
                         paste0(arg, " matching sequences found.\n")
   ))}

   if (x == "strcapplayed") {
      return(switch(lang,
                    de = "^[G|g](espielt)?\\s*(>|<|>=|<=)\\s*([[:digit:]]+)$",
                         "^[P|p](layed)?\\s*(>|<|>=|<=)\\s*([[:digit:]]+)$"
   ))}

   if (x == "selseqplayed") {
      return(switch(lang,
                    de = paste0("Suche nach Sequenzen die ", arg[[1]], " ", arg[[2]], " Mal gespielt wurden.\n"),
                         paste0("Searching for sequences that were played ", arg[[1]], " ", arg[[2]], " times.\n")
   ))}

   if (x == "seqsearchterm") {
      return(switch(lang,
                    de = paste0("Suche nach Sequenzen mit dem Begriff '", arg, "' ...\n"),
                         paste0("Searching for sequences with string '", arg, "' ...\n")
   ))}

   if (x == "waittime") {
      return(switch(lang,
                    de = paste0("Zeit zwischen den Z\U000000FCgen: ", arg),
                         paste0("Time between moves: ", arg)
   ))}

   if (x == "volume") {
      return(switch(lang,
                    de = paste0("Lautst\U000000E4rke : ", arg),
                         paste0("Sound volume: ", arg)
   ))}

   if (x == "newscore") {
      return(switch(lang,
                    de = paste0("Neuer Punktestand (jetztiger Wert ist ", arg, "): "),
                         paste0("New score (current value is ", arg, "): ")
   ))}

   if (x == "setnewscore") {
      return(switch(lang,
                    de = paste0("Setze den Punktestand auf ", arg, ".\n"),
                         paste0("Setting the score to ", arg, ".\n")
   ))}

   if (x == "setscoreback") {
      return(switch(lang,
                    de = paste0("Setze den Punktestand zur\U000000FCck auf ", arg, ".\n"),
                         paste0("Setting the score back to ", arg, ".")
   ))}

   if (x == "setposstart") {
      return(switch(lang,
                    de = "Setze die jetzige Stellung als Startposition.",
                         "Setting the current position as the start position."
   ))}

   if (x == "waslastmove") {
      return(switch(lang,
                    de = "Das war der letzte Zug!",
                         "That was the last move!"
   ))}

   if (x == "newexpval") {
      return(switch(lang,
                    de = paste0("Exponenten Wert: (jetztiger Wert ist ", arg, "): "),
                         paste0("Exponent value: (current value is ", arg, "): ")
   ))}

   if (x == "expvalinf") {
      return(switch(lang,
                    de = "Der gew\U000000E4hlte Wert ist zu gro\U000000DF und f\U000000FChrt zu unendlichen Wahrscheinlichkeiten.\n",
                         "Chosen value is too large and leads to infinite probabilities.\n"
   ))}

   if (x == "setnewexpval") {
      return(switch(lang,
                    de = paste0("Setze den Exponenten Wert auf ", arg, ".\n"),
                         paste0("Setting the exponent value to ", arg, ".\n")
   ))}

   if (x == "welldone") {
      return(switch(lang,
                    de = "Gut gemacht! Die n\U000000E4chste Sequenz ...",
                         "Well done! Next sequence ..."
   ))}

   if (x == "noleader") {
      return(switch(lang,
                    de = "Keine Rangliste verf\U000000FCgbar.\n",
                         "No leaderboard available.\n"
   ))}

   if (x == "lang") {
      return(switch(lang,
                    de = "Sprache: Deutsch",
                         "Language: Englisch"
   ))}

   if (x == "randomon") {
      return(switch(lang,
                    de = "Schalte in den Zufallsmodus ...",
                         "Switching to random mode ..."
   ))}

   if (x == "randomoff") {
      return(switch(lang,
                    de = "Schalte in den sequenziellen Modus ...",
                         "Switching to sequential mode ..."
   ))}

   if (x == "lwdadj") {
      return(switch(lang,
                    de = paste0("Linienbreite: ", arg),
                         paste0("Line Width: ", arg)
   ))}

   if (x == "colcurrent") {
      return(switch(lang,
                    de = "Aktuelle Farbeinstellungen:\n",
                         "Current color settings:\n"
   ))}

   if (x == "colwhich") {
      return(switch(lang,
                    de = "Farbe \U000000E4ndern (<Zahl> w\U000000E4hlen oder <Enter> zum Verlassen): ",
                         "Change color (choose a <number> or <enter> to quit): "
   ))}

   if (x == "colval") {
      return(switch(lang,
                    de = paste0("Farbe eingeben (jetztiger Wert ist '", arg, "'): "),
                         paste0("Enter color (current value is '", arg, "'): ")
   ))}



   if (x == "cexcurrent") {
      return(switch(lang,
                    de = "Aktuelle Einstellungen:\n",
                         "Current settings:\n"
   ))}

   if (x == "cexwhich") {
      return(switch(lang,
                    de = "Gr\U000000F6\U000000DFe \U000000E4ndern (<Zahl> w\U000000E4hlen oder <Enter> zum Verlassen): ",
                         "Change size (choose a <number> or <enter> to quit): "
   ))}

   if (x == "cexval") {
      return(switch(lang,
                    de = paste0("Gr\U000000F6\U000000DFe eingeben (jetztiger Wert ist ", arg, "): "),
                         paste0("Enter size (current value is ", arg, "): ")
   ))}

}

.confirm <- function(x) {

   lang <- .get("lang")

   if (lang == "en")
      return(identical(x, "y"))

   if (lang == "de")
      return(identical(x, "j"))

   return(FALSE)

}

.fileprefix <- function(flip) {

   lang <- .get("lang")

   if (flip) {
      return(switch(lang,
                    de = "s_",
                         "b_"
   ))} else {
      return(switch(lang,
                    de = "w_",
                         "w_"

   ))}

}
