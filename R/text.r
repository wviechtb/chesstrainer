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

   if (x == "createcachedir") {
      return(switch(lang,
                    de = paste0("Erstelle Verzeichnis f\U000000FCr den Cache: ", arg, "\n"),
                         paste0("Creating directory for the cache: ", arg, "\n")
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

   if (x == "san") {
      return(switch(lang,
                    de = paste0("Kurze algebraische Notation: ", ifelse(arg, "an", "aus")),
                         paste0("Short algebraic notation: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "piecesymbols") {
      return(switch(lang,
                    de = paste0("Figurensymbole: ", ifelse(arg == 1, "\U0000265A\U0000265B\U0000265C\U0000265D\U0000265E", ifelse(arg == 2, "KQRBN", "KDTLS"))),
                         paste0("Piece symbols: ",  ifelse(arg == 1, "\U0000265A\U0000265B\U0000265C\U0000265D\U0000265E", "KQRBN"))
   ))}

   if (x == "showtransp") {
      return(switch(lang,
                    de = paste0("Zugumstellungen anzeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Show move transpositions: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "showmatdiff") {
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

   if (x == "sequence") {
      return(switch(lang,
                    de = "Sequenz",
                         "Sequence"
   ))}

   if (x == "rounds") {
      return(switch(lang,
                    de = "Runden",
                         "Rounds"
   ))}

   if (x == "age") {
      return(switch(lang,
                    de = "Alter",
                         "Age"
   ))}

   if (x == "lastsession") {
      return(switch(lang,
                    de = "Letzte Sitzung",
                         "last session"
   ))}

   if (x == "daysago") {
      return(switch(lang,
                    de = "Vor x Tagen",
                         "Days Ago"
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
                    de = "Sequenzsuche (Text, Nummer, Nummer1-Nummer2, Punkte/Runden/Alter/Schwierigkeit/Fehler >/< Wert, FEN, K: Text):\n",
                    #de = "Sequenzsuche (Text, Nummer, Nummer1-Nummer2, Punkte > Wert, Runden < Wert, Alter > Wert, Schwierigkeit > Wert, FEN, K: Text):\n",
                         "Sequence search (string, number, number1-number2, score/rounds/age/difficulty/mistake >/< value, FEN, c: string):\n"
                         #"Sequence search (string, number, number1-number2, score > value, rounds < value, age > value, difficulty > value, FEN, c: string):\n"
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

   if (x == "noseqsfurthermoves") {
      return(switch(lang,
                    de = "Keine Sequenzen mit weiteren Z\U000000FCgen gefunden.\n",
                         "No sequences with further moves found.\n"
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

   if (x == "strcapmistake") {
      return(switch(lang,
                    de = "^[F|f|M|m](ehler)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$",
                         "^[M|m](istake)?\\s*(>|<|>=|<=)\\s*(\\d+\\.?\\d*)$"
   ))}

   if (x == "selseqmistake") {
      return(switch(lang,
                    de = paste0("Suche nach Sequenzen mit dem letzten Fehler vor ", arg[[1]], " als ", arg[[2]], ifelse(arg[[2]]==1, " Tag", " Tagen"), ".\n"),
                         paste0("Searching for sequences with the last mistake ", arg[[1]], " than ", arg[[2]], ifelse(arg[[2]]==1, " day", " days"), " ago.\n")
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
                    de = paste0(arg, " passende ", ifelse(arg==1, "Sequenz", "Sequenzen"), " gefunden.\n"),
                         paste0(arg, " matching ", ifelse(arg==1, "sequence", "sequences"), " found.\n")
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
                    de = paste0("Suche nach Sequenzen die vor ", arg[[1]], " als ", arg[[2]], ifelse(arg[[2]]==1, " Tag", " Tagen"), " gespielt wurden.\n"),
                         paste0("Searching for sequences that were played ", arg[[1]], " than ", arg[[2]], ifelse(arg[[2]]==1, " day", " days"), " ago.\n")
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
                    de = "Sequenzen, die mit den gleichen Z\U000000FCgen anfangen: \n\n",
                         "Sequences that start with the same moves: \n\n"
   ))}

   if (x == "seqsinclpos") {
      return(switch(lang,
                    de = "Sequenzen, die die gleiche Stellung beinhalten: \n\n",
                         "Sequences that contain the same position: \n\n"
   ))}

   if (x == "seqsendpos") {
      return(switch(lang,
                    de = "Sequenzen, die mit der gleichen Stellung enden: \n\n",
                         "Sequences that end on the same position: \n\n"
   ))}

   if (x == "seqsmatchfen") {
      return(switch(lang,
                    de = "\nSequenzen mit der gleichen FEN: \n\n",
                         "\nSequences with the same FEN: \n\n"
   ))}

   if (x == "seqsmatchcomment") {
      return(switch(lang,
                    de = "\nSequenzen mit dem Kommentar: \n\n",
                         "\nSequences with the comment: \n\n"
   ))}

   if (x == "seqsmatchpossquare") {
      return(switch(lang,
                    de = "Sequenzen mit der gleichen Figur auf dem gleichen Feld: \n\n",
                         "Sequences with the same piece on the same square: \n\n"
   ))}

   if (x == "selmatches") {
      return(switch(lang,
                    de = "\nDiese Sequenzen selektieren? (J/n): ",
                         "\nSelect these sequences? (Y/n): "
   ))}

   if (x == "selmatchestop") {
      return(switch(lang,
                    de = "\nDiese Sequenzen selektieren? (J/n)",
                         "\nSelect these sequences? (Y/n)"
   ))}

   if (x == "selmatchesconfirm") {
      return(switch(lang,
                    de = paste0(arg, " Sequenz", ifelse(arg > 1, "en", ""), " selektiert.\n"),
                         paste0(arg, " sequence", ifelse(arg > 1, "s", ""), " selected.\n")
   ))}

   if (x == "delaytime") {
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

   if (x == "whichsetting") {
      return(switch(lang,
                    de = "Einstellung w\U000000E4hlen (<Nummer> oder <Enter> zum Verlassen): ",
                         "Choose setting (<number> or <enter> to quit): "
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

   if (x == "colsetexpl") {
      return(switch(lang,
                    de = c("Hintergrundfarbe", "Vordergrundfarbe", "Farbe f\U000000FCr helle Felder", "Farbe f\U000000FCr dunkle Felder", "Farbe der zus\U000000E4tzlichen Felder im Brett-Editor", "Farbe f\U000000FCr Text oben", "Farbe f\U000000FCr Text unten", "Farbe des Hilfetext", "Farbe der Fensterrahmen", "Farbe f\U000000FCr Hinweise", "Farbe f\U000000FCr Pfeile f\U000000FCr den besten Zug", "Farbe f\U000000FCr falsche Z\U000000FCge", "Farbe f\U000000FCr die Felder Rechtecke", "Farbe f\U000000FCr Annotationen", "Farbe f\U000000FCr den Seitenindikator (wei\U000000DF)", "Farbe f\U000000FCr den Seitenindikator (schwarz)", "Farbe f\U000000FCr Remis", "Farbe f\U000000FCr schnelle Zeiten im Zeitmodus", "Farbe f\U000000FCr langsame Zeiten im Zeitmodus"),
                         c("Background color", "Foreground color", "Color of light squares", "Color of dark squares", "Color of the extra squares in the board editor", "Color for text at the top", "Color for text at the bottom", "Color of help text", "Color of window borders", "Color for hints", "Color for best move arrows", "Color for wrong moves", "Color for the square rectangles", "Color for annotations", "Color for the side indicator (white)", "Color for the side indicator (black)", "Color for draws", "Color for fast times in timed mode", "Color for slow times in timed mode")
   ))}

   if (x == "cexsetexpl") {
      return(switch(lang,
                    de = c("Gr\U000000F6\U000000DFe f\U000000FCr den Text am oberen Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text am unteren Rand", "Gr\U000000F6\U000000DFe f\U000000FCr den Text in der Bewertungsleiste", "Gr\U000000F6\U000000DFe f\U000000FCr die Brettkoordinaten", "Gr\U000000F6\U000000DFe f\U000000FCr den Materialunterschied", "Gr\U000000F6\U000000DFe f\U000000FCr Diagramme", "Gr\U000000F6\U000000DFe f\U000000FCr Text in Glyphen"),
                         c("Size of text at the top", "Size of text at the bottom", "Size of the text in the evaluation bar", "Size of the board coordinates", "Size of the material difference", "Size of plots", "Size of text in glyphs")
   ))}

   if (x == "miscsetexpl") {
      return(switch(lang,
                    de = c("Punktemultiplikator f\U000000FCr fehlerfrei abgeschlossene Sequenzen", "Strafpunkte f\U000000FCr falsche Z\U000000FCge", "Strafpunkte pro Hinweis", "Animationsschritte f\U000000FCr die Bewertungsleiste", "Anzahl der anzuzeigenden n\U000000E4chsten Z\U000000FCge nach Suchen", "Zeit pro Zug im zeitgesteuerten Modus (Sekunden)", "Leerlaufzeit (Sekunden)", "Mindestdauer der Sitzung (Sekunden)", "Pausenanpassung (Sekunden)"),
                         c("Score multiplier for completed sequences without mistakes", "Score penalty for wrong moves", "Score penalty per hint", "Animation steps for the evaluation bar", "Number of next moves to show after searches", "Time per move in timed mode (seconds)", "Idle time (seconds)", "Minimum session time (seconds)", "Pause adjustment (seconds)")
   ))}

   if (x == "showcoordsexpl") {
      return(switch(lang,
                    de = "Brettkoordinaten anzeigen",
                         "Show board coordinates"
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

   if (x == "usesfcache") {
      return(switch(lang,
                    de = paste0("Positionsbewertungen aus dem Cache abrufen: ", ifelse(arg, "An", "Aus")),
                         paste0("Get position evaluations from the cache: ", ifelse(arg, "On", "Off"))
   ))}

   if (x == "uselicache") {
      return(switch(lang,
                    de = paste0("Positionsdaten aus dem Cache abrufen: ", ifelse(arg, "An", "Aus")),
                         paste0("Get position information from the cache: ", ifelse(arg, "On", "Off"))
   ))}

   if (x == "contliquery") {
      return(switch(lang,
                    de = paste0("Kontinuierliche Abfrage der Lichess Spieldatenbank: ", ifelse(arg, "An", "Aus")),
                         paste0("Continuous query of the Lichess games database: ", ifelse(arg, "On", "Off"))
   ))}

   if (x == "eval") {
      return(switch(lang,
                    de = paste0("Stockfish Bewertungsleiste: ", ifelse(arg, "An", "Aus")),
                         paste0("Stockfish evaluation bar: ", ifelse(arg, "On", "Off"))
   ))}

   if (x == "libar") {
      return(switch(lang,
                    de = paste0("Lichess Bewertungsleiste: ", ifelse(arg, "An", "Aus")),
                         paste0("Lichess evaluation bar: ", ifelse(arg, "On", "Off"))
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

   if (x == "sfpath") {
      return(switch(lang,
                    de = paste0("Stockfish Pfad: ", arg, "\n"),
                         paste0("Stockfish path: ", arg, "\n")
   ))}

   if (x == "sfenterpath") {
      return(switch(lang,
                    de = "Pfad (einschlie\U000000DFlich der ausf\U000000FChrbaren Datei): ",
                         "Path (including the executable): "
   ))}

   if (x == "sfrunning") {
      return(switch(lang,
                    de = "Stockfish Status: ",
                         "Stockfish status: "
   ))}

   if (x == "on") {
      return(switch(lang,
                    de = "An",
                         "On"
   ))}

   if (x == "off") {
      return(switch(lang,
                    de = "Aus",
                         "Off"
   ))}

   if (x == "depth1") {
      return(switch(lang,
                    de = "Berechnungstiefe (schnell)",
                         "Calculation depths (fast)"
   ))}

   if (x == "depth2") {
      return(switch(lang,
                    de = "Berechnungstiefe (tief)",
                         "Calculation depths (deep)"
   ))}

   if (x == "depth3") {
      return(switch(lang,
                    de = "Berechnungstiefe (spielen)",
                         "Calculation depths (play)"
   ))}

   if (x == "multipv1") {
      return(switch(lang,
                    de = "Anzahl der Zugvarianten (schnell)",
                         "Number of variations (fast)"
   ))}

   if (x == "multipv2") {
      return(switch(lang,
                    de = "Anzahl der Zugvarianten (tief)",
                         "Number of variations (deep)"
   ))}

   if (x == "sflim_level") {
      return(switch(lang,
                    de = "Spielmodus-St\U000000E4rke-Limit (Level)",
                         "Play mode strength limit (level)"
   ))}

   if (x == "sflim_elo") {
      return(switch(lang,
                    de = "Spielmodus-St\U000000E4rke-Limit (Elo)",
                         "Play mode strength limit (Elo)"
   ))}

   if (x == "threads") {
      return(switch(lang,
                    de = "Anzahl der Threads",
                         "Number of threads"
   ))}

   if (x == "hash") {
      return(switch(lang,
                    de = "Hashgr\U000000F6\U000000DFe (MB)",
                         "Hash size (MB)"
   ))}

   if (x == "hintdepth") {
      return(switch(lang,
                    de = "Hinweistiefe",
                         "Hint depth"
   ))}

   if (x == "multiplier") {
      return(switch(lang,
                    de = "Punktemultiplikator f\U000000FCr fehlerfrei abgeschlossene Sequenzen",
                         "Score multiplier for completed sequences without mistakes"
   ))}

   if (x == "adjustwrong") {
      return(switch(lang,
                    de = "Strafpunkte f\U000000FCr falsche Z\U000000FCge",
                         "Score penalty for wrong moves"
   ))}

   if (x == "adjusthint") {
      return(switch(lang,
                    de = "Strafpunkte pro Hinweis",
                         "Score penalty per hint"
   ))}

   if (x == "timepermove") {
      return(switch(lang,
                    de = "Zeit pro Zug (Sekunden)",
                         "Time per move (seconds)"
   ))}

   if (x == "movestoshow") {
      return(switch(lang,
                    de = "Z\U000000FCge zeigen nach Suche",
                         "Moves to show after search"
   ))}

   if (x == "idletime") {
      return(switch(lang,
                    de = "Leerlaufzeit (Sekunden)",
                         "Idle time (seconds)"
   ))}

   if (x == "mintime") {
      return(switch(lang,
                    de = "Mindestdauer der Sitzung (Sekunden)",
                         "Minimum session time (seconds)"
   ))}

   if (x == "evalsteps") {
      return(switch(lang,
                    de = "Balken Animationsschritte",
                         "Animation steps for the bar"
   ))}

   if (x == "sleepadj") {
      return(switch(lang,
                    de = "Pausenanpassung (Sekunden)",
                         "Pause adjustment (seconds)"
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

   if (x == "sfsegfault") {
      return(switch(lang,
                    de = "Stockfish ist abgest\U000000FCrzt ('segmentation fault').\n",
                         "Stockfish crashed ('segmentation fault').\n"
   ))}

   if (x == "noplaymodewoutsf") {
      return(switch(lang,
                    de = "Kann nicht in den Spielmodus wechseln ohne dass Stockfish l\U000000E4uft.",
                         "Cannot switch to play mode without Stockfish running."
   ))}

   if (x == "nomovesf") {
      return(switch(lang,
                    de = "Stockfish hat keinen Zug vorgeschlagen.",
                         "Stockfish did not suggest a move."
   ))}

   if (x == "nomoveli") {
      return(switch(lang,
                    de = "Die Lichess Datenbank hat keinen Zug vorgeschlagen.",
                         "The Lichess database did not suggest a move."
   ))}

   if (x == "nobestmove") {
      return(switch(lang,
                    de = "Kann den besten Zug nicht anzeigen (vielleicht ist es Matt/Patt?).",
                         "Cannot show best move (maybe it is (stale)mate?)."
   ))}

   if (x == "nomovewoutsf") {
      return(switch(lang,
                    de = "Kann nur die besten Z\U000000FCge zeigen, wenn Stockfish l\U000000E4uft.",
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
                    de = "Neuberechnung der Bewertungen ohne Stockfish nicht m\U000000F6glich.",
                         "Cannot recalculate evaluations without Stockfish."
   ))}

   if (x == "rlyevalupdateall") {
      return(switch(lang,
                    de = "Wirklich die Bewertungen aller (selektierten) Sequenzen neu berechnen? (j/N): ",
                         "Really recalculate the evaluations for all (selected) sequences? (y/N): "
   ))}

   if (x == "explsettings") {
      return(switch(lang,
                    de = c("Sprache", "Spielername", "Modus", "Aktuelles Sequenzverzeichnis", "Selektionsmodus f\U000000FCr Sequenzen", "Zen Modus", "Zeitgesteuerter Modus", "Zeit pro Zug im zeitgesteuerten Modus (Sekunden)", "Exponentenwert", "Multiplikator f\U000000FCr abgeschlossene Sequenzen", "Strafpunkte f\U000000FCr falsche Z\U000000FCge", "Strafpunkte pro Hinweis", "Bewertungsleiste anzuzeigen (Hinzuf\U000000FCgen/Test/Spielen/Analyse)", "Animationsschritte f\U000000FCr die Bewertungsleiste", "Anzahl der anzuzeigenden n\U000000E4chsten Z\U000000FCge nach Suchen", "Brettkoordinaten anzeigen", "Zugumstellungen anzeigen", "Materialunterschied anzeigen", "Kurze algebraische Notation", "Figurensymbole", "Warten zwischen Sequenzen", "Verz\U000000F6gerung zwischen den Z\U000000FCgen (Sekunden)", "Leerlaufzeit (Sekunden)", "Mindestdauer der Sitzung (Sekunden)", "Pausenanpassung (Sekunden)", "Randbreite (unten/links/oben/rechts)", "Randbreite f\U000000FCr Inline-Diagramme", "Linienbreite", "Lautst\U000000E4rke (%)", "Fortschrittsdiagramm nach Sequenzen anzeigen", "Sequenzen nach Fehler wiederholen", "Zielwert f\U000000FCr Punkte", "Schwierigkeitsberechnung (Methode)", "Schwierigkeitsberechnung (Anzahl der letzten Werte)", "Schwierigkeitsberechnung (Mindestanzahl der Werte)", "Stockfish Pfad", "Berechnungstiefe (schnell)", "Berechnungstiefe (tief)", "Berechnungstiefe (spielen)", "Spielmodus-St\U000000E4rke-Limit", "Anzahl der besten Zugvarianten (schnell)", "Anzahl der besten Zugvarianten (tief)", "Anzahl der Threads f\U000000FCr Stockfish", "Hashgr\U000000F6\U000000DFe f\U000000FCr Stockfish", "Hinweistiefe", "Maximales Alter von Cache-Eintr\U000000E4gen (in Monaten)", "Kontinuierliche Analyse", "Positionsbewertungen aus dem Cache abrufen"),
                         c("Language", "Player name", "Mode", "Current sequence directory", "Selection mode for sequences", "Zen mode", "Timed mode", "Time per move in timed mode (seconds)", "Exponent value", "Multiplier for completed sequences", "Score penalty for wrong moves", "Score penalty per hint", "Show evaluation bar (add/test/play/analysis)", "Animation steps for the evaluation bar", "Number of next moves to show after searches", "Show board coordinates", "Show move transpositions", "Show material difference", "Short algebraic notation", "Piece symbols", "Wait between sequences", "Delay between moves (seconds)", "Idle time (seconds)", "Minimum session duration (seconds)", "Pause adjustment (seconds)", "Margin width (bottom/left/top/right)", "Margin width for inline graphs", "Line width", "Sound volume (%)", "Show progress graph after sequences", "Repeat sequences after mistake", "Target value for scores", "Difficulty calculation (method)", "Difficulty calculation (number of recent values)", "Difficulty calculation (minimum number of values)", "Stockfish path", "Calculation depth (fast)", "Calculation depth (deep)", "Calculation depth (play)", "Play mode strength limit", "Number of principal variations (fast)", "Number of principal variations (deep)", "Number of threads for Stockfish", "Hash size for Stockfish", "Hint depth", "Maximum age of cache entries (in months)", "Continuous analysis", "Get position evaluations from the cache")
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
                    de = "Wirklich alle Lesezeichen l\U000000F6schen? (j/N): ",
                         "Really delete all bookmarks? (y/N): "
   ))}

   if (x == "allmovesshown") {
      return(switch(lang,
                    de = "Alle Z\U000000FCge der Sequenz werden automatisch angezeigt.\nEs muss mindestens einen Zug geben, der gespielt werden muss.",
                         "All moves in the sequence are automatically shown.\nThere must be at least one move that has to be played."
   ))}

   if (x == "notatend") {
      return(switch(lang,
                    de = "Nicht am Ende der Sequenz.",
                         "Not at the end of the sequence."
   ))}

   if (x == "directory") {
      return(switch(lang,
                    de = "Verzeichnis",
                         "Directory"
   ))}

   if (x == "selseqdir") {
      return(switch(lang,
                    de = paste0("Verzeichnis ausw\U000000E4hlen (<F1> f\U000000FCr Hilfe): ", arg),
                         paste0("Select directory (<F1> for help): ", arg)
   ))}

   if (x == "seqdirtoremove") {
      return(switch(lang,
                    de = paste0("Verzeichnis zum Entfernen: ", arg),
                         paste0("Directory to remove: ", arg)
   ))}

   if (x == "seqdirtomove") {
      return(switch(lang,
                    de = paste0("Verzeichnis dessen Position ver\U000000E4ndert werden soll: ", arg),
                         paste0("Directory whose position should be changed: ", arg)
   ))}

   if (x == "seqdirnewpos") {
      return(switch(lang,
                    de = paste0("Neue Position f\U000000FCr das Verzeichnis: ", arg),
                         paste0("New position for the directory: ", arg)
   ))}

   if (x == "cannotremovesingleseqdir") {
      return(switch(lang,
                    de = "Ein Verzeichnis kann nicht entfernt werden,\nwenn es nur ein einziges Sequenzverzeichnis gibt.",
                         "Cannot remove a directory when there\nis only a single sequence directory."
   ))}

   if (x == "dirdoesnotexistcreate") {
      return(switch(lang,
                    de = "Das angegebene Sequenzverzeichnis existiert nicht.\nSoll das angegebene Sequenzverzeichnis erstellt werden? (J/n)",
                         "Specified sequence directory does not exist.\rShould the specified sequence directory be created? (Y/n)"
   ))}

   if (x == "addseqdir") {
      return(switch(lang,
                    de = "Sequenzverzeichnis hinzuf\U000000FCgen: ",
                         "Sequence directory to add: "
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
                         "Sequence directory:"
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

   if (x == "move") {
      return(switch(lang,
                    de = "Zug",
                         "Move"
   ))}

   if (x == "evaluation") {
      return(switch(lang,
                    de = "Bewertung",
                         "Evaluation"
   ))}

   if (x == "winperc") {
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

   if (x == "kingswrong") {
      return(switch(lang,
                    de = "Illegale Stellung (zu wenige oder zu viele K\U000000F6nige auf dem Brett)",
                         "Illegal position (too few or many kings on the board)"
   ))}

   if (x == "doublecheck") {
      return(switch(lang,
                    de = "Illegale Stellung\ n(beide K\U000000F6nige stehen im Schach)",
                         "Illegal position (both kings are in check)"
   ))}

   if (x == "wrongsidecheck") {
      return(switch(lang,
                    de = "Illegale Stellung (K\U000000F6nig im Schach, aber die andere Seite zieht)",
                         "Illegal position (king in check but the other side moves)"
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
                    de = "Das Springen zu einer Sequenznummer ist nur in sequenziellen Auswahlmodi m\U000000F6glich.",
                         "Jumping to a sequence number is only possible in sequential selection modes."
   ))}

   if (x == "transpositions") {
      return(switch(lang,
                    de = paste0("Es gibt ", ifelse(arg, "eine Sequenz", "mehrere Sequenzen"), " mit Zugumstellungen die zu dieser Stellung ", ifelse(arg, "f\U000000FChrt.", "f\U000000FChren.")),
                         paste0("There ", ifelse(arg, "is one sequence", "are multiple sequences"), " with move transpositions that ", ifelse(arg, "leads", "lead"), "to this position.")
   ))}

   if (x == "transposseqs") {
      return(switch(lang,
                    de = paste0(ifelse(arg, "Sequenz", "Sequenzen"), " mit Zugumstellungen:\n\n"),
                         paste0(ifelse(arg, "Sequence", "Sequences"), " with move transpositions:\n\n")
   ))}

   if (x == "posnotfound") {
      return(switch(lang,
                    de = "Position konnte nicht in der Lichess Datenbank gefunden werden.",
                         "Position could not be found in the Lichess database."
   ))}

   if (x == "ratelimit") {
      return(switch(lang,
                    de = "Die Durchsatzbegrenzung von Lichess wurde erreicht.\nBitte eine Minute vor dem n\U000000E4chsten Versuch warten.",
                         "Hit the rate limit for Lichess.\nPlease wait a minute before trying again."
   ))}

   if (x == "noconnect") {
      return(switch(lang,
                    de = "Verbindung zur Lichess Datenbank konnte nicht hergestellt werden.",
                         "Could not connect to the Lichess database."
   ))}

   if (x == "nointqueryli") {
      return(switch(lang,
                    de = "Ohne Internetverbindung kann die Lichess Datenbank nicht abgefragt werden.",
                         "Cannot query the Lichess database without an internet connection."
   ))}

   if (x == "speed") {
      return(switch(lang,
                    de = "Bedenkzeit",
                         "Time control"
   ))}

   if (x == "rating") {
      return(switch(lang,
                    de = "Durchschnittswertung",
                         "Average Rating"
   ))}

   if (x == "lichessdb") {
      return(switch(lang,
                    de = "Datenbank",
                         "Database"
   ))}

   if (x == "delcache") {
      return(switch(lang,
                    de = "Cache l\U000000F6schen",
                         "Delete cache"
   ))}

   if (x == "usecacheshort") {
      return(switch(lang,
                    de = "Cache nutzen",
                         "Use cache"
   ))}

   if (x == "usecacheshort:") {
      return(switch(lang,
                    de = "Cache nutzen: ",
                         "Use cache:    "
   ))}

   if (x == "yes") {
      return(switch(lang,
                    de = "Ja",
                         "Yes"
   ))}

   if (x == "no") {
      return(switch(lang,
                    de = "Nein",
                         "No"
   ))}

   if (x == "monthscache") {
      return(switch(lang,
                    de = "Maximales Alter von Cache-Eintr\U000000E4gen (in Monaten)",
                         "Maximum age of cache entries (in months)"
   ))}

   if (x == "barlen") {
      return(switch(lang,
                    de = "Balkenl\U000000E4nge",
                         "Bar length"
   ))}

   if (x == "invertbar") {
      return(switch(lang,
                    de = "Balken invertieren",
                         "Invert bar"
   ))}

   if (x == "entertoken") {
      return(switch(lang,
                    de = "API Token",
                         "API token"
   ))}

   if (x == "token") {
      return(switch(lang,
                    de = "API Token: ",
                         "API token: "
   ))}

   if (x == "needtoken") {
      return(switch(lang,
                    de = "F\U000000FCr diese Funktion muss ein API Token festgelegt werden (via F8).",
                         "Need to set API token for this function (via F8)."
   ))}

   if (x == "tokenonline") {
      return(switch(lang,
                    de = "Ein API Token kann nur bei bestehender Internetverbindung festgelegt werden.",
                         "Can only set an API token with an active internet connection."
   ))}

   if (x == "tokensetfail") {
      return(switch(lang,
                    de = "API Token konnte nicht festgelegt werden (falscher Token?).",
                         "API token could not be set (incorrect token?)."
   ))}

   if (x == "tokensetsuccess") {
      return(switch(lang,
                    de = "API Token wurde eingerichtet.",
                         "API token successfully set."
   ))}

   if (x == "rlydelcache") {
      return(switch(lang,
                    de = paste0("Wirklich den ", arg, " Cache l\U000000F6schen? (j/N)"),
                         paste0("Really delete the ", arg, " cache? (y/N)")
   ))}

   if (x == "deletingcache") {
      return(switch(lang,
                    de = "L\U000000F6sche den Cache ...",
                         "Deleting the cache ..."
   ))}

   if (x == "mpg123onlywin") {
      return(switch(lang,
                    de = "Die Installation von mpg123 mit dieser Funktion ist nur unter Windows m\U000000F6glich.\n",
                         "Installating mpg123 with this function is only possible under Windows.\n"
   ))}

   if (x == "priormpg123") {
      return(switch(lang,
                    de = paste0("Verzeichnis einer vorherigen Installation von mpg123 gefunden unter: ", arg, "\n"),
                         paste0("Directory of prior installation of mpg123 found at: ", arg, "\n")
   ))}

   if (x == "overwriteinst") {
      return(switch(lang,
                    de = "Soll die vorherige Installation \U000000FCberschrieben werden? (j/N): ",
                         "Should the prior installation be overwritten? (y/N): "
   ))}

   if (x == "installedmpg123") {
      return(switch(lang,
                    de = paste0("Verzeichnis, in das mpg123 installiert wurde: ", arg, "\n"),
                         paste0("Directory into which mpg123 was installed: ", arg, "\n")
   ))}

   if (x == "sfonlywin") {
      return(switch(lang,
                    de = "Die Installation von Stockfish mit dieser Funktion ist nur unter Windows m\U000000F6glich.\n",
                         "Installing Stockfish with this function is only possible under Windows.\n"
   ))}

   if (x == "priorsf") {
      return(switch(lang,
                    de = paste0("Verzeichnis einer vorherigen Installation von Stockfish gefunden unter: ", arg, "\n"),
                         paste0("Directory of prior installation of Stockfish found at: ", arg, "\n")
   ))}

   if (x == "installedsf") {
      return(switch(lang,
                    de = paste0("Verzeichnis, in das Stockfish installiert wurde: ", arg, "\n"),
                         paste0("Directory into which Stockfish was installed: ", arg, "\n")
   ))}

   if (x == "scheme_brown") {
      return(switch(lang,
                    de = "Braune Farbpalette ausw\U000000E4hlen",
                         "Select brown color scheme"
   ))}

   if (x == "scheme_green") {
      return(switch(lang,
                    de = "Gr\U000000FCne Farbpalette ausw\U000000E4hlen",
                         "Select green color scheme"
   ))}

   if (x == "scheme_blue") {
      return(switch(lang,
                    de = "Blaue Farbpalette ausw\U000000E4hlen",
                         "Select blue color scheme"
   ))}

   if (x == "scheme_gray") {
      return(switch(lang,
                    de = "Graue Farbpalette ausw\U000000E4hlen",
                         "Select gray color scheme"
   ))}

   if (x == "none") {
      return(switch(lang,
                    de = "keins",
                         "none"
   ))}

   if (x == "notseqmove") {
      return(switch(lang,
                    de = "Nicht einer der Sequenzez\U000000FCge.",
                         "Not one of the sequence moves."
   ))}

   if (x == "existingseqmoves") {
      return(switch(lang,
                    de = paste0("Z\U000000FCge aus bereits erstellten Sequenzen:\n", arg),
                         paste0("Moves from already existing sequences:\n", arg)
   ))}

   if (x == "compseq") {
      return(switch(lang,
                    de = paste0("Vergleiche Z\U000000FCge mit den bestehenden Sequenzen: ", ifelse(arg, "an", "aus")),
                         paste0("Compare moves to the existing sequences: ", ifelse(arg, "on", "off"))
   ))}

   if (x == "upsidedown") {
      return(switch(lang,
                    de = paste0("Schachbrett verkehrt herum anzeigen: ", ifelse(arg, "an", "aus")),
                         paste0("Display the chessboard upside down: ", ifelse(arg, "on", "off"))
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
