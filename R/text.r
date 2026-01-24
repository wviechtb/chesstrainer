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

   if (x == "createconfigdir") {
      return(switch(lang,
                    de = paste0("Erstelle Verzeichnis f\U000000FCr Einstellungen: ", arg, "\n"),
                         paste0("Creating directory for settings: ", arg, "\n")
   ))}

   if (x == "createseqdir") {
      return(switch(lang,
                    de = paste0("Erstelle Standardverzeichnis f\U000000FCr Sequenzen: ", arg, "\n"),
                         paste0("Creating default directory for sequences: ", arg, "\n")
   ))}

   if (x == "createsessionsdir") {
      return(switch(lang,
                    de = paste0("Erstelle Verzeichnis f\U000000FCr Session Infos: ", arg, "\n"),
                         paste0("Creating directory for session infos: ", arg, "\n")
   ))}

   if (x == "loadsettings") {
      return(switch(lang,
                    de = "Lade Einstellungen ...\n",
                         "Loading settings ...\n"
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

   if (x == "seqsnomoves") {
      return(switch(lang,
                    de = "Alle (ausgew\U000000E4hlten) Sequenzen haben keine Z\U000000FCge.\n",
                         "All (selected) sequences have no moves.\n"
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
                    de = "Gib einen (neuen) Namen oder die Nummer ein: ",
                         "Enter a (new) name or a number: "
   ))}

   if (x == "foundplayers") {
      return(switch(lang,
                    de = "Folgende Spieler gefunden:\n\n",
                         "Found the following players:\n\n"
   ))}

   if (x == "rlydelplayer") {
      return(switch(lang,
                    de = paste0("Wirklich Spieler '", arg, "' l\U000000F6schen? (j/N)"),
                         paste0("Really delete player '", arg, "'? (y/N)")
   ))}

   if (x == "rlyrlydelplayer") {
      return(switch(lang,
                    de = paste0("Ganz sicher, dass Spieler '", arg, "' gel\U000000F6scht werden soll? (j/N)"),
                         paste0("Really sure that player '", arg, "' should be deleted? (y/N)")
   ))}

   if (x == "delplayer") {
      return(switch(lang,
                    de = paste0("L\U000000F6sche Spieler '", arg, "' ..."),
                         paste0("Deleting the player '", arg, "' ...")
   ))}

   if (x == "coords") {
      return(switch(lang,
                    de = paste0("Koordinaten: ", ifelse(arg, "an", "aus")),
                         paste0("coordinates: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "matdiff") {
      return(switch(lang,
                    de = paste0("Materialunterschied anzeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show material difference: ", ifelse(arg, "on", "off"))
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
                    de = paste0("Z\U000000FCge zeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show moves: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "showmovescomp") {
      return(switch(lang,
                    de = paste0("Computer Z\U000000FCge zeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show computer moves: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "zenmode") {
      return(switch(lang,
                    de = paste0("Zen Modus: ", ifelse(arg, "an", "aus")),
                         paste0("Zen mode: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "showgraph") {
      return(switch(lang,
                    de = paste0("Fortschrittsdiagramm am Ende von Sequenzen anzeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show progress graph at the end of sequences: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "saveseq") {
      return(switch(lang,
                    de = "Sequenz abspeichern ...",
                         "Save sequence ..."
   ))}

   if (x == "seqexists") {
      return(switch(lang,
                    de = paste0("Sequenz mit den gleichen Z\U000000FCgen existiert bereits!\n(", arg, ")\n"),
                         paste0("Sequence with the same moves exists already!\n(", arg, ")\n")
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

   if (x == "round") {
      return(switch(lang,
                    de = "Runde",
                         "Round"
   ))}

   if (x == "rounds") {
      return(switch(lang,
                    de = "Runden",
                         "Rounds"
   ))}

   if (x == "age") {
      return(switch(lang,
                    de = "Alter",
                         "age"
   ))}

   if (x == "lastsession") {
      return(switch(lang,
                    de = "Letzte Sitzung",
                         "last session"
   ))}

   if (x == "day") {
      return(switch(lang,
                    de = paste0("Tag", ifelse(arg, "e", "")),
                         paste0("day", ifelse(arg, "s", ""))
   ))}

   if (x == "week") {
      return(switch(lang,
                    de = paste0("Woche", ifelse(arg, "n", "")),
                         paste0("week", ifelse(arg, "s", ""))
   ))}

   if (x == "month") {
      return(switch(lang,
                    de = paste0("Monat", ifelse(arg, "e", "")),
                         paste0("month", ifelse(arg, "s", ""))
   ))}

   if (x == "hour") {
      return(switch(lang,
                    de = paste0("Stunde", ifelse(arg, "n", "")),
                         paste0("hour", ifelse(arg, "s", ""))
   ))}

   if (x == "minute") {
      return(switch(lang,
                    de = paste0("Minute", ifelse(arg, "n", "")),
                         paste0("minute", ifelse(arg, "s", ""))
   ))}

   if (x == "second") {
      return(switch(lang,
                    de = paste0("Sekunde", ifelse(arg, "n", "")),
                         paste0("second", ifelse(arg, "s", ""))
   ))}

   if (x == "score") {
      return(switch(lang,
                    de = "Punkte",
                         "Score"
   ))}

   if (x == "diff") {
      return(switch(lang,
                    de = "Diff",
                         "Diff"
   ))}


   if (x == "difficulty") {
      return(switch(lang,
                    de = "Schwierigkeit",
                         "Difficulty"
   ))}

   if (x == "prob") {
      return(switch(lang,
                    de = "%",
                         "%"
   ))}

   if (x == "player") {
      return(switch(lang,
                    de = "Spieler",
                         "Player"
   ))}

   if (x == "rlydelseq") {
      return(switch(lang,
                    de = "Die aktuelle Sequenz l\U000000F6schen? (j/N)",
                         "Delete the current sequence? (y/N)"
   ))}

   if (x == "delseq") {
      return(switch(lang,
                    de = "L\U000000F6sche die Sequenz ...",
                         "Deleting the sequence ..."
   ))}

   if (x == "seqsearch") {
      return(switch(lang,
                    de = "Sequenzsuche (Text, Nummer1-Nummer2, Nummer, Punkte > Wert, Runden < Wert, Alter > Wert, Schwierigkeit > Wert, FEN, K: Text, *):\n",
                         "Sequence search (string, number1-number2, number, score > value, rounds < value, age > value, difficulty > value, FEN, c: string, *):\n"
   ))}

   if (x == "allseqselected") {
      return(switch(lang,
                    de = "Alle Sequenzen selektiert.\n",
                         "All sequences selected.\n"
   ))}

   if (x == "allseqalreadyselected") {
      return(switch(lang,
                    de = "Alle Sequenzen sind bereits selektiert.\n",
                         "All sequences are already selected.\n"
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

   if (x == "strcaprounds") {
      return(switch(lang,
                    de = "^[R|r](unden)?\\s*(>|<|>=|<=)\\s*([[:digit:]]+)$",
                         "^[R|r](rounds)?\\s*(>|<|>=|<=)\\s*([[:digit:]]+)$"
   ))}

   if (x == "selseqrounds") {
      return(switch(lang,
                    de = paste0("\nSuche nach Sequenzen die ", arg[[1]], " ", arg[[2]], " Mal gespielt wurden.\n"),
                         paste0("\nSearching for sequences that were played ", arg[[1]], " ", arg[[2]], " times.\n")
   ))}

   if (x == "strcapage") {
      return(switch(lang,
                    de = "^[A|a](lter)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$",
                         "^[A|a](ge)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$"
   ))}

   if (x == "selseqage") {
      return(switch(lang,
                    de = paste0("Suche nach Sequenzen die vor ", arg[[1]], " als ", arg[[2]], " Tagen gespielt wurden.\n"),
                         paste0("Searching for sequences that were played ", arg[[1]], " than ", arg[[2]], " days ago.\n")
   ))}

   if (x == "strcapdiff") {
      return(switch(lang,
                    de = "^[S|s](chwierigkeit)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$",
                         "^[D|d](ifficulty)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$"
   ))}

   if (x == "selseqdiff") {
      return(switch(lang,
                    de = paste0("\nSuche nach Sequenzen mit Schwierigkeit ", arg[[1]], " ", arg[[2]], ".\n"),
                         paste0("\nSearching for sequences with difficulty ", arg[[1]], " ", arg[[2]], ".\n")
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
                    de = paste0(arg, " Sequenz", ifelse(arg > 1, "en", ""), " selektiert.\n"),
                         paste0(arg, " sequence", ifelse(arg > 1, "s", ""), " selected.\n")
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

   if (x == "currentscore") {
      return(switch(lang,
                    de = "Aktueller Punktesstand: ",
                         "Current score: "
   ))}

   if (x == "newscore") {
      return(switch(lang,
                    de = paste0("Neuer Punktestand: "),
                         paste0("New score: ")
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
                    de = "Setze die jetzige Stellung als Ausgangsstellung.",
                         "Setting the current position as the start position."
   ))}

   if (x == "waslastmove") {
      return(switch(lang,
                    de = "Das war der letzte Zug!",
                         "That was the last move!"
   ))}

   if (x == "notlastmove") {
      return(switch(lang,
                    de = "Das war nicht der letzte Zug!",
                         "That was not the last move!"
   ))}

   if (x == "currentexpval") {
      return(switch(lang,
                    de = "Aktueller Exponentenwert: ",
                         "Current exponent value: "
   ))}

   if (x == "newexpval") {
      return(switch(lang,
                    de = paste0("Neuer Exponentenwert: "),
                         paste0("New exponent value: ")
   ))}

   if (x == "currenttarget") {
      return(switch(lang,
                    de = "Aktueller Zielwert: ",
                         "Current target score: "
   ))}

   if (x == "newtarget") {
      return(switch(lang,
                    de = paste0("Neuer Zielwert: "),
                         paste0("New target score: ")
   ))}

   if (x == "welldone") {
      return(switch(lang,
                    de = "Gut gemacht! Die n\U000000E4chste Sequenz ...",
                         "Well done! Next sequence ..."
   ))}

   if (x == "keeppracticing") {
      return(switch(lang,
                    de = "Weiter \U000000FCben!",
                         "Keep practicing!"
   ))}

   if (x == "noleader") {
      return(switch(lang,
                    de = "Keine Rangliste verf\U000000FCgbar.\n",
                         "No leaderboard available.\n"
   ))}

   if (x == "noleader_selected") {
      return(switch(lang,
                    de = "Keine Rangliste verf\U000000FCgbar f\U000000FCr die ausgew\U000000E4hlte(n) Sequenz(en).\n",
                         "No leaderboard available for the selected sequence(s).\n"
   ))}

   if (x == "lang") {
      return(switch(lang,
                    de = "Sprache: Deutsch",
                         "Language: English"
   ))}

   if (x == "maradj") {
      return(switch(lang,
                    de = paste0("Randbreite: ", paste0(arg, collapse="/")),
                         paste0("Margin Width: ", paste0(arg, collapse="/"))
   ))}

   if (x == "lwd") {
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

   if (x == "contanalysis") {
      return(switch(lang,
                    de = paste0("Kontinuierliche Analyse: ", ifelse(arg, "An", "Aus")),
                         paste0("Continuous analysis: ", ifelse(arg, "On", "Off"))
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

   if (x == "sflim") {
      return(switch(lang,
                    de = paste0("Spielmodus-St\U000000E4rke-Limit:                  ", arg, ifelse(is.na(arg), "", ifelse(arg <= 20, " (level)", " (Elo)")), "\n"),
                         paste0("Play mode strength limit:            ", arg, ifelse(is.na(arg), "", ifelse(arg <= 20, " (level)", " (Elo)")), "\n")
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

   if (x == "hintdepth") {
      return(switch(lang,
                    de = paste0("Hinweistiefe:                             ", arg, "\n"),
                         paste0("Hint depth:                          ", arg, "\n")
   ))}

   if (x == "sfoptions") {
      return(switch(lang,
                    de = "1 - Stockfisch (neu) starten\n2 - Stockfisch beenden\n3 - Pfad f\U000000FCr Stockfish \U000000E4ndern\n4 - Berechnungstiefen \U000000E4ndern\n5 - Spielmodus-St\U000000E4rke-Limit \U000000E4ndern\n6 - Anzahl der Zugvarianten \U000000E4ndern\n7 - Anzahl der Threads \U000000E4ndern\n8 - Hashgr\U000000F6\U000000DFe \U000000E4ndern\n9 - Hinweistiefe \U000000E4ndern",
                         "1 - (Re)start Stockfisch\n2 - Quit Stockfisch\n3 - Change the path for Stockfish\n4 - Change the calculation depths\n5 - Change the play mode strength limit\n6 - Change the number of principal variations\n7 - Change the number of threads\n8 - Change the hash size\n9 - Change the hint depth"
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

   if (x == "sflimenter") {
      return(switch(lang,
                    de = "St\U000000E4rke-Limit (0 bis 20 f\U000000FCr level, 1320 bis 3190 f\U000000FCr Elo; NA f\U000000FCr kein Limit): ",
                         "Strength limit (0 to 20 for level; 1320 to 3190 for Elo; NA for no limit): "
   ))}

   if (x == "sflimsetsuccess") {
      return(switch(lang,
                    de = "Neues St\U000000E4rke-Limit wurde erfolgreich gesetzt.\n",
                         "New strength limit was set successfully.\n"
   ))}

   if (x == "sflimsetfail") {
      return(switch(lang,
                    de = "Neues St\U000000E4rke-Limit konnte nicht gesetzt werden.\n",
                         "New strength limit could not be set.\n"
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

   if (x == "hintdepthenter") {
      return(switch(lang,
                    de = "Hinweistiefe: ",
                         "Hint depth: "
   ))}

   if (x == "hintdepthsetsuccess") {
      return(switch(lang,
                    de = "Neue Hinweistiefe wurde erfolgreich gesetzt.\n",
                         "New hint depth was set successfully.\n"
   ))}

   if (x == "hintdepthsetfail") {
      return(switch(lang,
                    de = "Neue Hinweistiefe konnte nicht gesetzt werden.\n",
                         "New hint depth could not be set.\n"
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
                    de = "Stockfish hat keinen Zug vorgeschlagen.\n",
                         "Stockfish did not suggest a move.\n"
   ))}

   if (x == "nobestmove") {
      return(switch(lang,
                    de = "Kann den besten Zug nicht anzeigen (vielleicht ist es Matt/Patt?).\n",
                         "Cannot show best move (maybe it is (stale)mate?)."
   ))}

   if (x == "nomovewoutsf") {
      return(switch(lang,
                    de = "Kann nur die besten Z\U000000FCge zeigen, wenn Stockfish l\U000000E4uft.\n",
                         "Can only show best moves when Stockfish is running."
   ))}

   if (x == "noevalgraph") {
      return(switch(lang,
                    de = "Bewertungsdiagram ist nicht verf\U000000FCgbar.",
                         "Evaluation graph is not available."
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
                    de = c("Sprache", "Spielername", "Modus", "Aktuelles Sequenzverzeichnis", "Selektionsmodus f\U000000FCr Sequenzen", "Zen Modus", "Zeitgesteuerter Modus", "Zeit pro Zug im zeitgesteuerten Modus (Sekunden)", "Exponentenwert", "Multiplikator f\U000000FCr abgeschlossene Sequenzen", "Strafpunkte f\U000000FCr falsche Z\U000000FCge", "Strafpunkte pro Hinweis", "Bewertungsleiste (Hinzuf\U000000FCgen/Test/Spielen/Analyse)", "Animationsschritte f\U000000FCr die Bewertungsleiste", "Koordinaten anzeigen", "Materialunterschied anzeigen", "Warten zwischen Sequenzen", "Zeit zwischen den Z\U000000FCgen (Sekunden)", "Leerlaufzeit (Sekunden)", "Randbreite (unten/links/oben/rechts)", "Randbreite f\U000000FCr Inline-Diagramme", "Linienbreite", "Lautst\U000000E4rke (%)", "Fortschrittsdiagramm nach Sequenzen anzeigen", "Sequenzen nach Fehler wiederholen", "Zielwert", "Gr\U000000F6\U000000DFe f\U000000FCr den Text am oberen Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text am unteren Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text in der Bewertungsleiste", "Gr\U000000F6\U000000DFe f\U000000FCr die Koordinaten", "Gr\U000000F6\U000000DFe f\U000000FCr den Materialunterschied", "Gr\U000000F6\U000000DFe f\U000000FCr Diagramme", "Schwierigkeitsberechnung (Methode)", "Schwierigkeitsberechnung (Anzahl der letzten Werte)", "Schwierigkeitsberechnung (Mindestanzahl der Werte)", "Stockfish Pfad", "Berechnungstiefe (schnell)", "Berechnungstiefe (tief)", "Berechnungstiefe (spielen)", "Spielmodus-St\U000000E4rke-Limit", "Anzahl der besten Zugvarianten (schnell)", "Anzahl der besten Zugvarianten (tief)", "Anzahl der Threads f\U000000FCr Stockfish", "Hashgr\U000000F6\U000000DFe f\U000000FCr Stockfish", "Hinweistiefe", "Kontinuierliche Analyse"),
                         c("Language", "Player name", "Mode", "Current sequence directory", "Selection mode for sequences", "Zen mode", "Timed mode", "Time per move in timed mode (seconds)", "Exponent value", "Multiplier for completed sequences", "Score penalty for wrong moves", "Score penalty per hint", "Evaluation bar (add/test/play/analysis)", "Animation steps for the evaluation bar", "Show coordinates", "Show material difference", "Wait between sequences", "Time between moves (seconds)", "Idle time (seconds)", "Margin width (bottom/left/top/right)", "Margin width for inline graphs", "Line width", "Sound volume (%)", "Show progress graph after sequences", "Repeat sequences after mistake", "Target score", "Size of text at the top", "Size of text at the bottom", "Size of the text in the evaluation bar", "Size of the coordinates", "Size of the material difference", "Size of plots", "Difficulty calculation (method)", "Difficulty calculation (number of recent values)", "Difficulty calculation (minimum number of values)", "Stockfish path", "Calculation depth (fast)", "Calculation depth (deep)", "Calculation depth (play)", "Play mode strength limit", "Number of principal variations (fast)", "Number of principal variations (deep)", "Number of threads for Stockfish", "Hash size for Stockfish", "Hint depth", "Continuous analysis")
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

   if (x == "bookmark") {
      return(switch(lang,
                    de = "Lesezeichen",
                         "Bookmarks"
   ))}

   if (x == "bookmarked") {
      return(switch(lang,
                    de = paste0("Lesezeichen f\U000000FCr Sequenz gesetzt:\n", arg),
                         paste0("Sequence bookmarked:\n", arg)
   ))}

   if (x == "nobookmarks") {
      return(switch(lang,
                    de = "Keine Lesezeichen gefunden.",
                         "No bookmarks found."
   ))}

   if (x == "selbookmark") {
      return(switch(lang,
                    de = paste0("Lesezeichen ausw\U000000E4hlen (<F1> f\U000000FCr Hilfe): ", arg),
                         paste0("Select bookmark (<F1> for help): ", arg)
   ))}

   if (x == "bookmarktoremove") {
      return(switch(lang,
                    de = paste0("Lesezeichen zum Entfernen: ", arg),
                         paste0("Bookmark to remove: ", arg)
   ))}

   if (x == "bookmarktomove") {
      return(switch(lang,
                    de = paste0("Lesezeichen dessen Position ver\U000000E4ndert werden soll: ", arg),
                         paste0("Bookmark whose position should be changed: ", arg)
   ))}

   if (x == "bookmarknewpos") {
      return(switch(lang,
                    de = paste0("Neue Position f\U000000FCr das Lesezeichen: ", arg),
                         paste0("New position for the bookmark: ", arg)
   ))}

   if (x == "rlydelallbookmarks") {
      return(switch(lang,
                    de = paste0("Wirklich alle Lesezeichen l\U000000F6schen? (j/N): "),
                         paste0("Really delete all bookmarks? (y/N): ")
   ))}

   if (x == "allmovesshown") {
      return(switch(lang,
                    de = "Alle Z\U000000FCge der Sequenz werden automatisch angezeigt.\nEs muss mindestens einen Zug geben, der gespielt werden muss.",
                         "All moves in the sequence are automatically shown.\nThere must be at least one move that has to be played."
   ))}

   if (x == "lastmoveplayer") {
      return(switch(lang,
                    de = "Sequenzen m\U000000FCssen mit einem Zug des Spielers enden.",
                         "Sequences must end with a move by the player."
   ))}

   if (x == "notatend") {
      return(switch(lang,
                    de = "Nicht am Ende der Sequenz.",
                         "Not at the end of the sequence."
   ))}

   if (x == "seqdiroptionwhich") {
      return(switch(lang,
                    de = "Option (<Nummer>, 'h' (hinzuf\U000000FCgen), 'e' (entfernen), 'p' (Position ver\U000000E4ndern), oder <Enter> zum Verlassen): ",
                         "Option (<number>, 'a' (add), 'r' (remove), 'p' (change position), or <enter> to quit): "
   ))}

   if (x == "selectedseqdir") {
      return(switch(lang,
                    de = c("Verzeichnis", "ausgew\U000000E4hlt"),
                         c("directory", "selected")
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

   if (x == "selmodeinfo") {
      return(switch(lang,
                    de = "W\U000000E4hle mit den Zifferntasten 1-9, F1-F11, oder per Mausklick aus.",
                         "Select via the number keys 1-9, F1-F11, or via mouse click."
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

   if (x == "generalsettings") {
      return(switch(lang,
                    de = "Allgemeine Einstellungen:",
                         "General settings: "
   ))}

   if (x == "sfsettings") {
      return(switch(lang,
                    de = "Stockfish Einstellungen:",
                         "Stockfish settings: "
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

   if (x == "enterfen") {
      return(switch(lang,
                    de = "FEN: ",
                         "FEN: "
   ))}

   if (x == "notvalidfen") {
      return(switch(lang,
                    de = "Das ist keine g\U000000FCltige FEN.",
                         "Not a valid FEN."
   ))}

   if (x == "evalgraph-x") {
      return(switch(lang,
                    de = "Zug",
                         "Move"
   ))}

   if (x == "evalgraph-y-cp") {
      return(switch(lang,
                    de = "Bewertung",
                         "Evaluation"
   ))}

   if (x == "evalgraph-y-wp") {
      return(switch(lang,
                    de = "Gewinnwahrscheinlichkeit",
                         "Win Percentage"
   ))}

   if (x == "toofewscores") {
      return(switch(lang,
                    de = "Diagramme k\U000000F6nnen nicht angezeigt werden (zu wenige Sequenzen).",
                         "Cannot show graphs (too few sequences)."
   ))}

   if (x == "toofewplays") {
      return(switch(lang,
                    de = "Diagramme k\U000000F6nnen nicht angezeigt werden (keine gespielten Sequenzen).",
                         "Cannot show graphs (no played sequences)."
   ))}

   if (x == "toofewseqsplayed") {
      return(switch(lang,
                    de = "Das Diagramm kann nicht angezeigt werden (zu wenige Sequenzen gespielt).",
                         "Cannot show graph (too few sequences played)."
   ))}

   if (x == "meanscore") {
      return(switch(lang,
                    de = "Durchschnittliche Punktzahl",
                         "Average Score"
   ))}

   if (x == "nosessionhistory") {
      return(switch(lang,
                    de = "Kein Sitzungsverlauf f\U000000FCr den aktuellen Spieler verf\U000000FCgbar.",
                         "No session history available for the current player."
   ))}

   if (x == "playtime") {
      return(switch(lang,
                    de = paste0("Spielzeit: ", arg),
                         paste0("play time: ", arg)
   ))}

   if (x == "historyplaytime_mins") {
      return(switch(lang,
                    de = "Spielzeit (Minuten)",
                         "play time (minutes)"
   ))}

   if (x == "historyplaytime_hours") {
      return(switch(lang,
                    de = "Spielzeit (Stunden)",
                         "play time (hours)"
   ))}

   if (x == "historyseqsplayed") {
      return(switch(lang,
                    de = "Anzahl der Sequenzen",
                         "number of sequences"
   ))}

   if (x == "totalplaytime") {
      return(switch(lang,
                    de = paste0("Gesamte Spielzeit: ", arg),
                         paste0("total play time: ", arg)
   ))}

   if (x == "totalseqsplayed") {
      return(switch(lang,
                    de = paste0("Gesamte Anzahl der Sequenzen: ", arg),
                         paste0("total number of sequences: ", arg)
   ))}

   if (x == "difflencur") {
      return(switch(lang,
                    de = "Aktueller Wert f\U000000FCr n: ",
                         "Current value for n: "
   ))}

   if (x == "difflennew") {
      return(switch(lang,
                    de = "Neuer Wert f\U000000FCr n: ",
                         "New value for n: "
   ))}

   if (x == "diffmincur") {
      return(switch(lang,
                    de = "Aktueller Wert f\U000000FCr m: ",
                         "Current value for m: "
   ))}

   if (x == "diffminnew") {
      return(switch(lang,
                    de = "Neuer Wert f\U000000FCr m: ",
                         "New value for m: "
   ))}

   if (x == "selmodeswitch") {
      return(switch(lang,
                    de = paste0("Wechsele zum Selektionsmodus: ", arg),
                         paste0("Switching to selection mode: ", arg)
   ))}

   if (x == "storegame") {
      return(switch(lang,
                    de = "Aktuelles Spiel als Hauptvariante sichern.",
                         "Storing current game as the main variation."
   ))}

   if (x == "retstoregame") {
      return(switch(lang,
                    de = "Springe zur Hauptvariante zur\U000000FCck.",
                         "Jumping back to the main variation."
   ))}

   if (x == "missingkings") {
      return(switch(lang,
                    de = "Illegale Stellung\n(es muss ein wei\U000000DFer und schwarzer K\U000000F6nig auf dem Brett sein)",
                         "Illegal position\n(there must be one white and black king on the board)"
   ))}

   if (x == "doublecheck") {
      return(switch(lang,
                    de = "Illegale Stellung\n(beide K\U000000F6nige stehen im Schach)",
                         "Illegal position\n(both kings are in check)"
   ))}

   if (x == "wrongsidecheck") {
      return(switch(lang,
                    de = "Illegale Stellung\n(K\U000000F6nige im Schach, aber die andere Seite zieht)",
                         "Illegal position\n(king in check but other side moves)"
   ))}

   if (x == "threefold") {
      return(switch(lang,
                    de = "Remis durch dreifache Stellungswiederholung.",
                         "Draw by threefold repetition."
   ))}

   if (x == "fifty") {
      return(switch(lang,
                    de = "Remis durch die 50-Z\U000000FCge-Regel.",
                         "Draw by the fifty-move rule."
   ))}

   if (x == "finishedround") {
      return(switch(lang,
                    de = "Eine Runde aller Sequenzen abgeschlossen.",
                         "Finished one round of all sequences."
   ))}

   if (x == "belowtarget") {
      return(switch(lang,
                    de = paste0("Die Punktzahlen aller (ausgew\U000000E4hlten) Sequenzen liegen jetzt unter ", arg, "."),
                         paste0("The scores of all (selected) sequences are now below ", arg, ".")
   ))}

   if (x == "scatterplot") {
      return(switch(lang,
                    de = "Streudiagramm",
                         "Scatterplot"
   ))}

   if (x == "edithistory") {
      return(switch(lang,
                    de = "\nZeilen l\U000000F6schen (<Nummer>, <Nummer1-Nummer2>, oder <Enter> zum Verlassen): ",
                         "\nDelete rows (<number>, <number1-number2>, or <enter> to exit): "
   ))}

   if (x == "currentseqno") {
      return(switch(lang,
                    de = "Aktuelle Sequenznummer: ",
                         "Current sequence number: "
   ))}

   if (x == "newseqno") {
      return(switch(lang,
                    de = paste0("Neue Sequenznummer (1-", arg, "): "),
                         paste0("New sequence number (1-", arg, "): ")
   ))}

   if (x == "jumponlymodes") {
      return(switch(lang,
                    de = "Das Springen zu einer Sequenznummer geht nur in sequenziellen Auswahlmodi.",
                         "Jumping to a sequence number is only possible in sequential selection modes."
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
