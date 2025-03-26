.text <- function(x, arg) {

   lang <- .get("lang")

   if (x == "interactive") {
      return(switch(lang,
                    de = "R l\U000000E4uft nicht im interaktiven Modus.",
                         "R is not running in interactive mode."
   ))}

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
                    de = paste0("Erstelle Standardverzeichnis f\U000000FCr Einstellungen: ", arg, "\n"),
                         paste0("Creating default directory for settings: ", arg, "\n")
   ))}

   if (x == "loadsettings") {
      return(switch(lang,
                    de = paste0("Lade Einstellungen ...\n"),
                         paste0("Loading settings ...\n")
   ))}

   if (x == "createseqdir") {
      return(switch(lang,
                    de = paste0("Erstelle Standardverzeichnis f\U000000FCr Sequenzen: ", arg, "\n"),
                         paste0("Creating default directory for sequences: ", arg, "\n")
   ))}

   if (x == "dircreateerror") {
      return(switch(lang,
                    de = "Verzeichnis f\U000000FCr kann nicht erstellt werden.",
                         "Cannot create directory."
   ))}

   if (x == "copyseqs") {
      return(switch(lang,
                    de = "Beispielsequenzen in das Sequenzverzeichnis kopieren? (J/n): ",
                         "Copy example sequences to the sequence directory? (Y/n): "
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
                    de = "Keine Sequenzen gefunden.\n",
                         "No sequences found.\n"
   ))}

   if (x == "seqdir") {
      return(switch(lang,
                    de = paste0("Benutze Sequenzverzeichnis: ", arg, "\n"),
                         paste0("Using sequence directory: ", arg, "\n")
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

   if (x == "pause") {
      return(switch(lang,
                    de = paste0("Pause: ", ifelse(arg, "an", "aus")),
                         paste0("Pause: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "showmoves") {
      return(switch(lang,
                    de = paste0("Zug zeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show moves: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "sound") {
      return(switch(lang,
                    de = paste0("Sound: ", ifelse(arg, "an", "aus")),
                         paste0("Sound: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "showgraph") {
      return(switch(lang,
                    de = paste0("Fortschrittsdiagramm am Ende von Sequenzen anzeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show progress graph at the end of the sequences: ", ifelse(arg, "on", "off"))
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

   if (x == "days") {
      return(switch(lang,
                    de = "Tage",
                         "Days"
   ))}

   if (x == "score") {
      return(switch(lang,
                    de = "Punkte",
                         "Score"
   ))}

   if (x == "prob") {
      return(switch(lang,
                    de = "Wahrsch",
                         "Prob"
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
                    de = "Sequenzsuche (Text, Nummer1-Nummer2, Nummer, Punkte > Wert, Gespielt < Wert, Tage > Wert, FEN, K: Text, *):\n",
                         "Sequence search (string, number1-number2, number, score > value, played < value, days > value, FEN, c: string, *):\n"
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
                    de = paste0("\nSuche nach Sequenzen mit Punkten ", arg[[1]], " ", arg[[2]], ".\n"),
                         paste0("\nSearching for sequences with score ", arg[[1]], " ", arg[[2]], ".\n")
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
                    de = paste0("\nSuche nach Sequenzen die ", arg[[1]], " ", arg[[2]], " Mal gespielt wurden.\n"),
                         paste0("\nSearching for sequences that were played ", arg[[1]], " ", arg[[2]], " times.\n")
   ))}

   if (x == "strcapdays") {
      return(switch(lang,
                    de = "^[T|t](age)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$",
                         "^[D|d](ays)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$"
   ))}

   if (x == "selseqdays") {
      return(switch(lang,
                    de = paste0("Suche nach Sequenzen die vor ", arg[[1]], " als ", arg[[2]], " Tagen gespielt wurden.\n"),
                         paste0("Searching for sequences that were played ", arg[[1]], " than ", arg[[2]], " days ago.\n")
   ))}



   if (x == "seqsearchterm") {
      return(switch(lang,
                    de = paste0("\nSuche nach Sequenzen mit dem Begriff '", arg, "' ...\n"),
                         paste0("\nSearching for sequences with string '", arg, "' ...\n")
   ))}

   if (x == "seqsmatchstart") {
      return(switch(lang,
                    de = "Sequenzen mit dem gleichen Anfang: \n",
                         "Sequences with the same beginning: \n"
   ))}

   if (x == "seqsmatchfen") {
      return(switch(lang,
                    de = "\nSequenzen mit der gleichen FEN: \n",
                         "\nSequences with the same FEN: \n"
   ))}

   if (x == "seqsmatchcomment") {
      return(switch(lang,
                    de = "\nSequenzen mit dem Kommentar: \n",
                         "\nSequences with the comment: \n"
   ))}

   if (x == "selmatches") {
      return(switch(lang,
                    de = "\nDiese Sequenzen selektieren? (J/n): ",
                         "\nSelect these sequences? (Y/n): "
   ))}

   if (x == "selmatchesconfirm") {
      return(switch(lang,
                    de = "Sequenzen selektiert.\n",
                         "Sequences selected.\n"
   ))}

   if (x == "waittime") {
      return(switch(lang,
                    de = paste0("Zeit zwischen den Z\U000000FCgen: ", arg),
                         paste0("Time between moves: ", arg)
   ))}

   if (x == "volume") {
      return(switch(lang,
                    de = paste0("Lautst\U000000E4rke: ", arg),
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
                    de = paste0("Exponentenwert: (jetztiger Wert ist ", arg, "): "),
                         paste0("Exponent value: (current value is ", arg, "): ")
   ))}

   if (x == "expvalinf") {
      return(switch(lang,
                    de = "Der gew\U000000E4hlte Wert ist zu gro\U000000DF und f\U000000FChrt zu unendlichen Wahrscheinlichkeiten.\n",
                         "Chosen value is too large and leads to infinite probabilities.\n"
   ))}

   if (x == "setnewexpval") {
      return(switch(lang,
                    de = paste0("Setze den Exponentenwert auf ", arg, ".\n"),
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
                         "Language: English"
   ))}

   if (x == "lwdadj") {
      return(switch(lang,
                    de = paste0("Linienbreite: ", arg),
                         paste0("Line Width: ", arg)
   ))}

   if (x == "colwhich") {
      return(switch(lang,
                    de = "Farbe \U000000E4ndern (<Nummer> oder <Enter> zum Verlassen): ",
                         "Change color (<number> or <enter> to quit): "
   ))}

   if (x == "colval") {
      return(switch(lang,
                    de = paste0("Farbe eingeben (jetztiger Wert ist '", arg, "'): "),
                         paste0("Enter color (current value is '", arg, "'): ")
   ))}

   if (x == "currentsettings") {
      return(switch(lang,
                    de = "Aktuelle Einstellungen:\n",
                         "Current settings:\n"
   ))}

   if (x == "cexwhich") {
      return(switch(lang,
                    de = "Gr\U000000F6\U000000DFe \U000000E4ndern (<Nummer> oder <Enter> zum Verlassen): ",
                         "Change size (<number> or <enter> to quit): "
   ))}

   if (x == "cexval") {
      return(switch(lang,
                    de = paste0("Gr\U000000F6\U000000DFe eingeben (jetztiger Wert ist ", arg, "): "),
                         paste0("Enter size (current value is ", arg, "): ")
   ))}

   if (x == "settingwhich") {
      return(switch(lang,
                    de = "Einstellung \U000000E4ndern (<Nummer> oder <Enter> zum Verlassen): ",
                         "Change setting (<number> or <enter> to quit): "
   ))}

   if (x == "settingval") {
      return(switch(lang,
                    de = paste0("Wert eingeben (jetztiger Wert ist ", arg, "): "),
                         paste0("Enter value (current value is ", arg, "): ")
   ))}

   if (x == "verbose") {
      return(switch(lang,
                    de = paste0("Verbose: ", ifelse(arg, "An", "Aus")),
                         paste0("Verbose: ", ifelse(arg, "On", "Off"))
   ))}

   if (x == "eval") {
      return(switch(lang,
                    de = paste0("Bewertungsleiste: ", ifelse(arg, "An", "Aus")),
                         paste0("Evaluation bar: ", ifelse(arg, "On", "Off"))
   ))}

   if (x == "sfstart") {
      return(switch(lang,
                    de = "Starte Stockfish ...\n",
                         "Starting Stockfish ...\n"
   ))}

   if (x == "sfstop") {
      return(switch(lang,
                    de = "Stockfish beenden ...\n",
                         "Stopping Stockfish ...\n"
   ))}

   if (x == "sfstarterror") {
      return(switch(lang,
                    de = "Stockfish konnte nicht gestartet werden.\n",
                         "Could not start Stockfish.\n"
   ))}

   if (x == "sfstartsuccess") {
      return(switch(lang,
                    de = "Stockfish wurde erfolgreich gestartet.\n",
                         "Successfully started Stockfish.\n"
   ))}

   if (x == "sfstoperror") {
      return(switch(lang,
                    de = "Stockfish konnte nicht beendet werden.\n",
                         "Could not stop Stockfish.\n"
   ))}

   if (x == "sfstopsuccess") {
      return(switch(lang,
                    de = "Stockfish wurde erfolgreich beendet.\n",
                         "Successfully stopped Stockfish.\n"
   ))}

   if (x == "sfrunning") {
      return(switch(lang,
                    de = paste0("Stockfish Status:     ", ifelse(arg, "an", "aus"), "\n"),
                         paste0("Stockfish status:      ", ifelse(arg, "on", "off"), "\n")
   ))}

   if (x == "sfpath") {
      return(switch(lang,
                    de = paste0("Stockfish Pfad:       ", arg, "\n"),
                         paste0("Stockfish Path:        ", arg, "\n")
   ))}

   if (x == "sfgo") {
      return(switch(lang,
                    de = paste0("Berechnungsparameter: ", arg, "\n"),
                         paste0("Calculation parameter: ", arg, "\n")
   ))}

   if (x == "sfoptions") {
      return(switch(lang,
                    de = "1 - Stockfisch (neu) starten\n2 - Stockfisch beenden\n3 - Pfad \U000000E4ndern\n4 - Berechnungsparameter \U000000E4ndern\n5 - Einstellungen anzeigen\n",
                         "1 - (Re)start Stockfisch\n2 - Quit Stockfisch\n3 - Change path\n4 - Change calculation parameter\n5 - Show settings"
   ))}

   if (x == "sfoptionwhich") {
      return(switch(lang,
                    de = "Option (<Nummer> oder <Enter> zum Verlassen): ",
                         "Option (<number> or <enter> to quit): "
   ))}

   if (x == "sfenterpath") {
      return(switch(lang,
                    de = "Pfad (einschlie\U000000DFlich der ausf\U000000FChrbaren Datei): ",
                         "Path (including the executable): "
   ))}

   if (x == "sfpathsuccess") {
      return(switch(lang,
                    de = "Neuer Pfad wurde erfolgreich gesetzt.\n",
                         "New path was set successfully.\n"
   ))}

   if (x == "sfpathfail") {
      return(switch(lang,
                    de = "Pfad konnte nicht gefunden/eingestellt werden.\n",
                         "Could not find/set path.\n"
   ))}

   if (x == "sfentergo") {
      return(switch(lang,
                    de = "Berechnungsparameter ('depth <Nummer>' oder 'movetime <Millisekunden>'): ",
                         "Calculation parameter ('depth <number>' or 'movetime <milliseconds>'): "
   ))}

   if (x == "sfgosuccess") {
      return(switch(lang,
                    de = "Neuer Berechnungsparameter wurde erfolgreich gesetzt.\n",
                         "New calculation parameter was set successfully.\n"
   ))}

   if (x == "sfgofail") {
      return(switch(lang,
                    de = "Neuer Berechnungsparameter konnte nicht gesetzt werden.\n",
                         "New calculation parameter could not be set.\n"
   ))}

   if (x == "sfsegfault") {
      return(switch(lang,
                    de = "Stockfish ist abgest\U000000FCrzt ('segmentation fault').\n",
                         "Stockfish crashed ('segmentation fault').\n"
   ))}

   if (x == "nobestmove") {
      return(switch(lang,
                    de = "Kann den besten Zug nicht anzeigen (l\U000000E4uft Stockfish?).\n",
                         "Cannot show best move (is Stockfish running?)."
   ))}

   if (x == "sfdeepeval") {
      return(switch(lang,
                    de = "Beginne mit tiefer Analyse ...",
                         "Starting deep evaluation ..."
   ))}

   if (x == "quit") {
      return(switch(lang,
                    de = "Schachtrainer wird beendet ...\n",
                         "Quitting the chess trainer ...\n"
   ))}

   if (x == "evalupdateold") {
      return(switch(lang,
                    de = "Aktuelle Bewertungen:\n\n",
                         "Current evaluations:\n\n"
   ))}

   if (x == "evalupdatestart") {
      return(switch(lang,
                    de = "\nStarte Neuberechnung der Bewertungen ...\n\n",
                         "\nStarting recalculation of the evaluations ...\n\n"
   ))}

   if (x == "evalupdatenew") {
      return(switch(lang,
                    de = "Neue Bewertungen:\n\n",
                         "New evaluations:\n\n"
   ))}

   if (x == "evalupdatenosf") {
      return(switch(lang,
                    de = "Neuberechnung der Bewertungen ohne Stockfish nicht m\U000000F6glich.\n",
                         "Cannot recalculate evaluations without Stockfish.\n"
   ))}

   if (x == "bestmove") {
      return(switch(lang,
                    de = paste0("Bester Zug: ", arg),
                         paste0("Best move: ", arg)
   ))}

   if (x == "explsettings") {
      return(switch(lang,
                    de = c("Sprache", "Spielername", "Modus", "Selektionsmodus f\U000000FCr Sequenzen", "Exponentenwert", "Multiplikator f\U000000FCr abgeschlossene Sequenzen", "Strafpunkte f\U000000FCr falsche Z\U000000FCge", "Strafpunkte pro Hinweis", "Bewertungsleiste anzeigen", "Animationsschritte f\U000000FCr die Bewertungsleiste", "Pause zwischen Sequenzen", "Zeit zwischen den Z\U000000FCgen (Sekunden)", "Linienbreite", "Lautst\U000000E4rke (%)", "Fortschrittsdiagramm nach Sequenzen anzeigen", "Gr\U000000F6\U000000DFe f\U000000FCr den Text am oberen Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text am unteren Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text in der Bewertungsleiste", "Stockfish Pfad", "Berechnungsparameter f\U000000FCr Stockfish"),
                         c("Language", "Player name", "Mode", "Selection mode for sequences", "Exponent value", "Multiplier for completed sequences", "Score penalty for wrong moves", "Score penalty per hint", "Show evaluation bar", "Animation steps for the evaluation bar", "Pause between sequences", "Time between moves (seconds)", "Line width", "Sound volume (%)", "Show progress graph after sequences", "Size of text at the top", "Size of text at the bottom", "Size of the text in the evaluation bar", "Stockfish path", "Calculation parameter for Stockfish")
   ))}

   if (x == "comment") {
      return(switch(lang,
                    de = "Kommentar zum Zug: ",
                         "Comment for the move: "
   ))}

   if (x == "commentedit") {
      return(switch(lang,
                    de = paste0("Kommentar bearbeiten (<Nummer>, 's' (Startkommentar) 'e' (Endkommentar), 'l' (l\U000000F6schen), oder <Enter> zum Verlassen): "),
                         paste0("Edit a comment (<number>, 's' (start comment), 'e' (end comment), 'd' (delete), or <enter> to quit): ")
   ))}

   if (x == "commentnew") {
      return(switch(lang,
                    de = paste0("Neuer Kommentar: "),
                         paste0("New comment: ")
   ))}

   if (x == "commentdelete") {
      return(switch(lang,
                    de = paste0("Zu l\U000000F6schender Kommentar (<Nummer>, 's' f\U000000FCr Startkommentar, 'e' f\U000000FCr Endkommentar): "),
                         paste0("Comment to delete (<number>, 's' for start comment, or 'e' for endcomment): ")
   ))}

   if (x == "commentend") {
      return(switch(lang,
                    de = paste0("\nEndkommentar: ", arg, "\n"),
                         paste0("\nEnd comment: ", arg, "\n")
   ))}

   if (x == "commentendnow") {
      return(switch(lang,
                    de = paste0("Aktueller Endkommentar: ", arg, "\n"),
                         paste0("Current end comment: ", arg, "\n")
   ))}

   if (x == "commentendnew") {
      return(switch(lang,
                    de = paste0("Neuer Endkommentar: "),
                         paste0("New end comment: ")
   ))}

   if (x == "commentenddeleted") {
      return(switch(lang,
                    de = paste0("Endkommentar gel\U000000F6scht.\n"),
                         paste0("Deleted end comment.\n")
   ))}

   if (x == "commentstart") {
      return(switch(lang,
                    de = paste0("\nStartkommentar: ", arg, "\n"),
                         paste0("\nStart comment: ", arg, "\n")
   ))}

   if (x == "commentstartnow") {
      return(switch(lang,
                    de = paste0("Aktueller Startkommentar: ", arg, "\n"),
                         paste0("Current start comment: ", arg, "\n")
   ))}

   if (x == "commentstartnew") {
      return(switch(lang,
                    de = paste0("Neuer Startkommentar: "),
                         paste0("New start comment: ")
   ))}

   if (x == "commentstartdeleted") {
      return(switch(lang,
                    de = paste0("Startkommentar gel\U000000F6scht.\n"),
                         paste0("Deleted start comment.\n")
   ))}

   if (x == "copyfen") {
      return(switch(lang,
                    de = paste0("FEN in die Zwischenablage kopiert."),
                         paste0("Copied FEN to the clipboard.")
   ))}

   if (x == "bookmarked") {
      return(switch(lang,
                    de = paste0("Sequenz vorgemerkt: ", arg, "\n"),
                         paste0("Sequence bookmarked: ", arg, "\n")
   ))}

   if (x == "allmovesshown") {
      return(switch(lang,
                    de = paste0("Alle Z\U000000FCge der Sequenz werden automatisch angezeigt.\nEs muss mindestens einen Zug geben, der gespielt werden muss."),
                         paste0("All moves in the sequence are automatically shown.\nThere must be at least one move that has to be played.")
   ))}

}

.confirm <- function(x) {

   lang <- .get("lang")

   x <- tolower(x)

   if (lang == "en")
      return(identical(x, "y"))

   if (lang == "de")
      return(identical(x, "j"))

   return(FALSE)

}
