.text <- function(x, arg, arg2, arg3) {

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
                    de = "Parameter 'mode' muss entweder 'add', 'test', oder 'play' sein.",
                         "Argument 'mode' must be either 'add', 'test', or 'play'."
   ))}

   if (x == "createconfigdir") {
      return(switch(lang,
                    de = paste0("Erstelle Standardverzeichnis f\U000000FCr Einstellungen: ", arg, "\n"),
                         paste0("Creating default directory for settings: ", arg, "\n")
   ))}

   if (x == "loadsettings") {
      return(switch(lang,
                    de = "Lade Einstellungen ...\n",
                         "Loading settings ...\n"
   ))}

   if (x == "createseqdir") {
      return(switch(lang,
                    de = paste0("Erstelle Standardverzeichnis f\U000000FCr Sequenzen: ", arg, "\n"),
                         paste0("Creating default directory for sequences: ", arg, "\n")
   ))}

   if (x == "dircreateerror") {
      return(switch(lang,
                    de = "Das Verzeichnis konnte nicht erstellt werden.\n",
                         "The directory could not be created.\n"
   ))}

   if (x == "copyseqs") {
      return(switch(lang,
                    de = "Beispielsequenzen in das Sequenzverzeichnis kopieren? (J/n): ",
                         "Copy example sequences to the sequence directory? (Y/n): "
   ))}

   if (x == "zeroseqsfound") {
      return(switch(lang,
                    de = "Keine Sequenzen gefunden.\n",
                         "No sequences found.\n"
   ))}

   if (x == "useseqdir") {
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

   if (x == "wait") {
      return(switch(lang,
                    de = paste0("Warten: ", ifelse(arg, "an", "aus")),
                         paste0("Wait: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "timed") {
      return(switch(lang,
                    de = paste0("Zeitgesteuerter Modus: ", ifelse(arg, "an", "aus")),
                         paste0("Timed mode: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "tooslow") {
      return(switch(lang,
                    de = paste0("Gespielte Zeit (", arg, " Sekunden) ist langsamer als erlaubte Zeit (", arg2, " Sekunden)."),
                         paste0("Play time (", arg, " seconds) is slower than permitted time (", arg2, " seconds).")
   ))}

   if (x == "showmoves") {
      return(switch(lang,
                    de = paste0("Zug zeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show moves: ", ifelse(arg, "on", "off"))
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

   if (x == "sleeptime") {
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

   if (x == "setscoreback100") {
      return(switch(lang,
                    de = "Der Punktestand lag bereits bei 100.",
                         "The score was already at 100."
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

   if (x == "nextseq") {
      return(switch(lang,
                    de = "Weiter \U000000FCben! Die n\U000000E4chste Sequenz ...",
                         "Keep practicing! Next sequence ..."
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
                    de = paste0("Stockfish Status:                         ", ifelse(arg, "an", "aus"), "\n"),
                         paste0("Stockfish status:                    ", ifelse(arg, "on", "off"), "\n")
   ))}

   if (x == "sfpath") {
      return(switch(lang,
                    de = paste0("Stockfish Pfad:                           ", arg, "\n"),
                         paste0("Stockfish path:                      ", arg, "\n")
   ))}

   if (x == "depths") {
      return(switch(lang,
                    de = paste0("Berechnungstiefen (schnell,tief,spielen): ", arg, ",", arg2, ",", arg3, "\n"),
                         paste0("Calculation depths (fast,deep,play): ", arg, ",", arg2, ",", arg3, "\n")
   ))}

   if (x == "multipvs") {
      return(switch(lang,
                    de = paste0("Anzahl der Zugvarianten (schnell,tief):   ", arg, ",", arg2, "\n"),
                         paste0("Number of variations (fast,deep):    ", arg, ",", arg2, "\n")
   ))}

   if (x == "threads") {
      return(switch(lang,
                    de = paste0("Anzahl der Threads:                       ", arg, "\n"),
                         paste0("Number of threads:                   ", arg, "\n")
   ))}

   if (x == "hash") {
      return(switch(lang,
                    de = paste0("Hashgr\U000000F6\U000000DFe:                                ", arg, "\n"),
                         paste0("Hash size:                           ", arg, "\n")
   ))}

   if (x == "sfoptions") {
      return(switch(lang,
                    de = "1 - Stockfisch (neu) starten\n2 - Stockfisch beenden\n3 - Pfad f\U000000FCr Stockfish \U000000E4ndern\n4 - Berechnungstiefen \U000000E4ndern\n5 - Anzahl der Zugvarianten \U000000E4ndern\n6 - Anzahl der Threads \U000000E4ndern\n7 - Hashgr\U000000F6\U000000DFe \U000000E4ndern",
                         "1 - (Re)start Stockfisch\n2 - Quit Stockfisch\n3 - Change path for Stockfish\n4 - Change calculation depths\n5 - Change number of principal variations\n6 - Change number of threads\n7 - Change hash size"
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
                    de = "Pfad konnte nicht gefunden/gesetzt werden.\n",
                         "Could not find/set path.\n"
   ))}

   if (x == "depthenter") {
      return(switch(lang,
                    de = "Berechnungstiefen (<schnell>,<tief>,<spielen>): ",
                         "Calculation depths (<fast>,<deep>,<play>): "
   ))}

   if (x == "depthsetsuccess") {
      return(switch(lang,
                    de = "Neue Berechnungstiefen wurden erfolgreich gesetzt.\n",
                         "New calculation depths were set successfully.\n"
   ))}

   if (x == "depthsetfail") {
      return(switch(lang,
                    de = "Neue Berechnungstiefen konnten nicht gesetzt werden.\n",
                         "New calculation depths could not be set.\n"
   ))}

   if (x == "multipventer") {
      return(switch(lang,
                    de = "Anzahl der besten Zugvarianten (<schnell>,<tief>): ",
                         "Number of principal variations (<fast>,<deep>): "
   ))}

   if (x == "multipvsetsuccess") {
      return(switch(lang,
                    de = "Neue Werte wurden erfolgreich gesetzt.\n",
                         "New numbers were set successfully.\n"
   ))}

   if (x == "multipvsetfail") {
      return(switch(lang,
                    de = "Neue Werte konnten nicht gesetzt werden.\n",
                         "New numbers could not be set.\n"
   ))}

   if (x == "threadsenter") {
      return(switch(lang,
                    de = "Anzahl der Threads: ",
                         "Number of threads: "
   ))}

   if (x == "threadssetsuccess") {
      return(switch(lang,
                    de = "Neue Anzahl der Threads wurde erfolgreich gesetzt.\n",
                         "New number of threads was set successfully.\n"
   ))}

   if (x == "threadssetfail") {
      return(switch(lang,
                    de = "Neue Anzahl der Threads konnte nicht gesetzt werden.\n",
                         "New number of threads could not be set.\n"
   ))}

   if (x == "hashenter") {
      return(switch(lang,
                    de = "Hashgr\U000000F6\U000000DFe (MB): ",
                         "Hash size (MB): "
   ))}

   if (x == "hashsetsuccess") {
      return(switch(lang,
                    de = "Neue Hashgr\U000000F6\U000000DFe wurde erfolgreich gesetzt.\n",
                         "New hash size was set successfully.\n"
   ))}

   if (x == "hashsetfail") {
      return(switch(lang,
                    de = "Neue Hashgr\U000000F6\U000000DFe konnte nicht gesetzt werden.\n",
                         "New hash size could not be set.\n"
   ))}

   if (x == "sfsegfault") {
      return(switch(lang,
                    de = "Stockfish ist abgest\U000000FCrzt ('segmentation fault').\n",
                         "Stockfish crashed ('segmentation fault').\n"
   ))}

   if (x == "noplaymode") {
      return(switch(lang,
                    de = "Kann nicht in den Spielmodus wechseln ohne dass Stockfish l\U000000E4uft.\n",
                         "Cannot switch to play mode without Stockfish running."
   ))}

   if (x == "nomove") {
      return(switch(lang,
                    de = "Stockfish hat keinen Zug angezeigt.\n",
                         "Stockfish did not return a move.\n"
   ))}

   if (x == "nobestmove") {
      return(switch(lang,
                    de = "Kann den besten Zug nicht anzeigen.\n",
                         "Cannot show best move."
   ))}

   if (x == "sfdeepeval") {
      return(switch(lang,
                    de = "Beginne mit tiefer Analyse ...",
                         "Starting deep evaluation ..."
   ))}

   if (x == "curdepth") {
      return(switch(lang,
                    de = paste0("Tiefe: ", arg),
                         paste0("Depth: ", arg)
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

   if (x == "explsettings") {
      return(switch(lang,
                    de = c("Sprache", "Spielername", "Modus", "Aktuelles Sequenzverzeichnis", "Selektionsmodus f\U000000FCr Sequenzen", "Zeitgesteuerter Modus", "Zeit pro Zug im zeitgesteuerten Modus (in Sekunden)", "Exponentenwert", "Multiplikator f\U000000FCr abgeschlossene Sequenzen", "Strafpunkte f\U000000FCr falsche Z\U000000FCge", "Strafpunkte pro Hinweis", "Bewertungsleiste anzeigen", "Animationsschritte f\U000000FCr die Bewertungsleiste", "Warten zwischen Sequenzen", "Zeit zwischen den Z\U000000FCgen (in Sekunden)", "Linienbreite", "Lautst\U000000E4rke (in %)", "Fortschrittsdiagramm nach Sequenzen anzeigen", "Sequenzen nach Fehler wiederholen", "Gr\U000000F6\U000000DFe f\U000000FCr den Text am oberen Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text am unteren Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text in der Bewertungsleiste", "Stockfish Pfad", "Berechnungstiefe (schnell)", "Berechnungstiefe (tief)", "Berechnungstiefe (spielen)", "Anzahl der besten Zugvarianten (schnell)", "Anzahl der besten Zugvarianten (tief)", "Anzahl der Threads f\U000000FCr Stockfish", "Hashgr\U000000F6\U000000DFe f\U000000FCr Stockfish"),
                         c("Language", "Player name", "Mode", "Current sequence directory", "Selection mode for sequences", "Timed mode", "Time per move in timed mode (in seconds)", "Exponent value", "Multiplier for completed sequences", "Score penalty for wrong moves", "Score penalty per hint", "Show evaluation bar", "Animation steps for the evaluation bar", "Wait between sequences", "Time between moves (in seconds)", "Line width", "Sound volume (in %)", "Show progress graph after sequences", "Repeat sequences after mistake", "Size of text at the top", "Size of text at the bottom", "Size of the text in the evaluation bar", "Stockfish path", "Calculation depth (fast)", "Calculation depth (deep)", "Calculation depth (play)", "Number of principal variations (fast)", "Number of principal variations (deep)", "Number of threads for Stockfish", "Hash size for Stockfish")
   ))}

   if (x == "comment") {
      return(switch(lang,
                    de = "Kommentar zum Zug: ",
                         "Comment for the move: "
   ))}

   if (x == "commentedit") {
      return(switch(lang,
                    de = "Kommentar bearbeiten (<Nummer>, 's' (Startkommentar) 'e' (Endkommentar), 'l' (l\U000000F6schen), oder <Enter> zum Verlassen): ",
                         "Edit a comment (<number>, 's' (start comment), 'e' (end comment), 'd' (delete), or <enter> to quit): "
   ))}

   if (x == "commentnew") {
      return(switch(lang,
                    de = "Neuer Kommentar: ",
                         "New comment: "
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
                    de = "Neuer Endkommentar: ",
                         "New end comment: "
   ))}

   if (x == "commentenddeleted") {
      return(switch(lang,
                    de = "Endkommentar gel\U000000F6scht.\n",
                         "Deleted end comment.\n"
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
                    de = "Neuer Startkommentar: ",
                         "New start comment: "
   ))}

   if (x == "commentstartdeleted") {
      return(switch(lang,
                    de = "Startkommentar gel\U000000F6scht.\n",
                         "Deleted start comment.\n"
   ))}

   if (x == "copyfen") {
      return(switch(lang,
                    de = "FEN in die Zwischenablage kopiert.",
                         "Copied FEN to the clipboard."
   ))}

   if (x == "bookmarked") {
      return(switch(lang,
                    de = paste0("Sequenz vorgemerkt: ", arg, "\n"),
                         paste0("Sequence bookmarked: ", arg, "\n")
   ))}

   if (x == "allmovesshown") {
      return(switch(lang,
                    de = "Alle Z\U000000FCge der Sequenz werden automatisch angezeigt.\nEs muss mindestens einen Zug geben, der gespielt werden muss.",
                         "All moves in the sequence are automatically shown.\nThere must be at least one move that has to be played."
   ))}

   if (x == "seqdiroptionwhich") {
      return(switch(lang,
                    de = "Option (<Nummer>, 'h' (hinzuf\U000000FCgen), 'e' (entfernen), 'p' (Position ver\U000000E4ndern), oder <Enter> zum Verlassen): ",
                         "Option (<number>, 'a' (add), 'r' (remove), 'p' (change position), or <enter> to quit): "
   ))}

   if (x == "selectedseqdir") {
      return(switch(lang,
                    de = "ausgew\U000000E4hlt",
                         "selected"
   ))}

   if (x == "addseqdir") {
      return(switch(lang,
                    de = "\nSequenzverzeichnis hinzuf\U000000FCgen: ",
                         "\nSequence directory to add: "
   ))}

   if (x == "dirdoesnotexist") {
      return(switch(lang,
                    de = "Das angegebene Sequenzverzeichnis existiert nicht.\n",
                         "Specified sequence directory does not exist.\n"
   ))}

   if (x == "createdir") {
      return(switch(lang,
                    de = "Soll das angegebene Sequenzverzeichnis erstellt werden? (J/n): ",
                         "Should the specified sequence directory be created? (Y/n): "
   ))}

   if (x == "seqdirtoremove") {
      return(switch(lang,
                    de = "\nSequenzverzeichnis zum Entfernen (<Nummer> oder <Enter> zum Verlassen): ",
                         "\nSequence directory to remove (<number> or <enter> to quit)): "
   ))}

   if (x == "cannotremovesingleseqdir") {
      return(switch(lang,
                    de = "\nEin Verzeichnis kann nicht entfernt werden, wenn es nur ein einziges Sequenzverzeichnis gibt.\n",
                         "\nCannot remove a directory when there is only a single sequence directory.\n"
   ))}

   if (x == "seqdirtomove") {
      return(switch(lang,
                    de = "\nSequenzverzeichnis dessen Position ver\U000000E4ndert werden soll (<Nummer> oder <Enter> zum Verlassen): ",
                         "\nSequence directory whose position should be changed (<number> or <enter> to quit)): "
   ))}

   if (x == "seqdirnewpos") {
      return(switch(lang,
                    de = "Neue Position f\U000000FCr das Sequenzverzeichnis (<Nummer> oder <Enter> zum Verlassen): ",
                         "New position for the sequence directory (<number> or <enter> to quit)): "
   ))}

   if (x == "plotlegend") {
      return(switch(lang,
                    de = c("bestm\U000000F6gliche Leistung", "tats\U000000E4chliche Leistung"),
                         c("best possible performance", "actual performance")
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

   if (x == "replast") {
      return(switch(lang,
                    de = "Wiederhole die letzte Sequenz ...",
                         "Repeating the last sequence ..."
   ))}

   if (x == "repmistake") {
      return(switch(lang,
                    de = paste0("Sequenzen nach Fehler wiederholen: ", ifelse(arg, "an", "aus")),
                         paste0("Repeat sequences after mistake: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "nolastseq") {
      return(switch(lang,
                    de = "Keine vorherige Sequenz gefunden ...",
                         "Could not find a previous sequence ..."
   ))}

   if (x == "sequential") {
      return(switch(lang,
                    de = "Schalte in den sequenziellen Selektionsmodus ...",
                         "Switching to the sequential selection mode ..."
   ))}

   if (x == "fliponlyatstart") {
      return(switch(lang,
                    de = "Das Brett kann nur am Anfang einer Sequenz gedreht werden.",
                         "Can only flip the board at the beginning of a sequence."
   ))}

   if (x == "mate") {
      return(switch(lang,
                    de = "Matt!",
                         "Mate!"
   ))}

   if (x == "stalemate") {
      return(switch(lang,
                    de = "Patt!",
                         "Stalemate!"
   ))}

   if (x == "seqdirsettings") {
      return(switch(lang,
                    de = "Sequenzverzeichnis:",
                         "Sequence directory: "
   ))}

   if (x == "sfpathsettings") {
      return(switch(lang,
                    de = "Stockfish Pfad:",
                         "Stockfish path:"
   ))}

   if (x == "enterrochade") {
      return(switch(lang,
                    de = "Rochadem\U000000F6glichkeiten eingeben in FEN Notation (z.B., Kq): ",
                         "Enter castling availability in FEN notation (e.g., Kq): "
   ))}

   if (x == "notcorrectrochade") {
      return(switch(lang,
                    de = "Falsche Rochade Angabe.\n",
                         "Incorrect castling specification.\n"
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
