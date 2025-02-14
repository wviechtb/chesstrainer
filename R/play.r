play <- function(player="", lang="en", seqdir="", sfpath="", sfgo="depth 20", ...) {

   if (!interactive())
      return(.text("interactive"))

   if (!is.element(lang, c("en","de")))
      stop("Argument 'lang' must be either 'en' or 'de'.", call.=FALSE)

   assign("lang", lang, envir=.chesstrainer)

   ddd <- list(...)

   mode        <- ifelse(is.null(ddd$mode),        "add",          ddd$mode)
   selmode     <- ifelse(is.null(ddd$selmode),     "score_random", ddd$selmode)
   expval      <- ifelse(is.null(ddd$expval),      2,              ddd$expval)
   multiplier  <- ifelse(is.null(ddd$multiplier),  0.8,            ddd$multiplier)
   adjustwrong <- ifelse(is.null(ddd$adjustwrong), 40,             ddd$adjustwrong)
   adjusthint  <- ifelse(is.null(ddd$adjusthint),  20,             ddd$adjusthint)
   eval        <- ifelse(is.null(ddd$eval),        TRUE,           isTRUE(ddd$eval))
   evalsteps   <- ifelse(is.null(ddd$evalsteps),   10,             ddd$evalsteps)
   pause       <- ifelse(is.null(ddd$pause),       TRUE,           isTRUE(ddd$pause))
   sleep       <- ifelse(is.null(ddd$sleep),       0.5,            ddd$sleep)
   lwd         <- ifelse(is.null(ddd$lwd),         2,              ddd$lwd)
   volume      <- ifelse(is.null(ddd$volume),      50,             ddd$volume)
   showgraph   <- ifelse(is.null(ddd$showgraph),   FALSE,          ddd$showgraph)
   cex.top     <- ifelse(is.null(ddd$cex.top),     1.4,            ddd$cex.top)
   cex.bot     <- ifelse(is.null(ddd$cex.bot),     0.7,            ddd$cex.bot)
   cex.eval    <- ifelse(is.null(ddd$cex.eval),    0.5,            ddd$cex.eval)

   # ensure that some arguments are sensible

   if (!is.element(mode, c("add","play")))
      stop(.text("modecheck"), call.=FALSE)

   expval[expval < 0] <- 0
   multiplier[multiplier < 0] <- 0
   multiplier[multiplier > 1] <- 1
   adjustwrong[adjustwrong < 0] <- 0
   adjusthint[adjusthint < 0] <- 0
   evalsteps[evalsteps < 2] <- 2
   evalsteps <- round(evalsteps)
   sleep[sleep < 0] <- 0
   lwd[lwd < 1] <- 1
   volume[volume < 0] <- 0
   volume[volume > 100] <- 100
   cex.top[cex.top < 0.1] <- 0.1
   cex.bot[cex.bot < 0.1] <- 0.1
   cex.eval[cex.eval < 0.1] <- 0.1

   verbose <- isTRUE(ddd$verbose)

   cols.all <- c("col.bg", "col.fg", "col.square.l", "col.square.d", "col.square.be",
                 "col.top", "col.bot", "col.help", "col.help.border",
                 "col.hint", "col.wrong", "col.rect", "col.annot", "col.side.w", "col.side.b")

   # create config directory and read/save settings and colors

   configdir <- tools::R_user_dir(package="chesstrainer", which="config")

   if (!dir.exists(configdir)) {
      cat(.text("createconfigdir", configdir))
      success <- dir.create(configdir, recursive=TRUE)
      if (!success)
         stop(.text("dircreateerror"), call.=FALSE)
      settings <- data.frame(lang, player, mode, selmode, expval, multiplier, adjustwrong, adjusthint, eval, evalsteps, pause, sleep, lwd, volume, showgraph, cex.top, cex.bot, cex.eval, sfpath, sfgo)
      saveRDS(settings, file=file.path(configdir, "settings.rds"))
      cols <- sapply(cols.all, function(x) .get(x))
      saveRDS(cols, file=file.path(configdir, "colors.rds"))
   } else {
      if (file.exists(file.path(configdir, "settings.rds"))) {
         cat(.text("loadsettings"))
         # apply settings, but only for those that are not set via play()
         settings <- readRDS(file.path(configdir, "settings.rds"))
         mc <- as.list(match.call())
         if (is.null(mc$lang))
            lang <- settings$lang
         if (is.null(mc$player))
            player <- settings$player
         if (is.null(mc$mode))
            mode <- settings$mode
         if (is.null(mc$selmode))
            selmode <- settings$selmode
         if (is.null(mc$expval))
            expval <- settings$expval
         if (is.null(mc$multiplier))
            multiplier <- settings$multiplier
         if (is.null(mc$adjustwrong))
            adjustwrong <- settings$adjustwrong
         if (is.null(mc$adjusthint))
            adjusthint <- settings$adjusthint
         if (is.null(mc$eval))
            eval <- settings$eval
         if (is.null(mc$evalsteps))
            evalsteps <- settings$evalsteps
         if (is.null(mc$pause))
            pause <- settings$pause
         if (is.null(mc$sleep))
            sleep <- settings$sleep
         if (is.null(mc$lwd))
            lwd <- settings$lwd
         if (is.null(mc$volume))
            volume <- settings$volume
         if (is.null(mc$showgraph))
            showgraph <- settings$showgraph
         if (is.null(mc$cex.top))
            cex.top <- settings$cex.top
         if (is.null(mc$cex.bot))
            cex.bot <- settings$cex.bot
         if (is.null(mc$cex.eval))
            cex.eval <- settings$cex.eval
         if (is.null(mc$sfpath))
            sfpath <- settings$sfpath
         if (is.null(mc$sfgo))
            sfgo <- settings$sfgo
      }
      sfpath <- suppressWarnings(normalizePath(sfpath))
      settings <- data.frame(lang, player, mode, selmode, expval, multiplier, adjustwrong, adjusthint, eval, evalsteps, pause, sleep, lwd, volume, showgraph, cex.top, cex.bot, cex.eval, sfpath, sfgo)
      saveRDS(settings, file=file.path(configdir, "settings.rds"))
      if (file.exists(file.path(configdir, "colors.rds"))) {
         cols <- readRDS(file.path(configdir, "colors.rds"))
         for (j in 1:length(cols.all)) {
            assign(cols.all[j], cols[cols.all[j]], envir=.chesstrainer)
         }
      } else {
         cols <- sapply(cols.all, function(x) .get(x))
         saveRDS(cols, file=file.path(configdir, "colors.rds"))
      }
   }

   assign("lang", lang, envir=.chesstrainer)
   assign("cex.top", cex.top, envir=.chesstrainer)
   assign("cex.bot", cex.bot, envir=.chesstrainer)
   assign("cex.eval", cex.eval, envir=.chesstrainer)

   # create / check sequence directory

   if (seqdir == "") {
      seqdir <- tools::R_user_dir(package="chesstrainer", which="data")
      if (!dir.exists(seqdir)) {
         cat(.text("createseqdir", seqdir))
         success <- dir.create(seqdir, recursive=TRUE)
         if (!success)
            stop(.text("dircreateerror"), call.=FALSE)
         copyseqs <- readline(prompt=.text("copyseqs"))
         if (identical(copyseqs, "") || .confirm(copyseqs))
            tmp <- file.copy(list.files(system.file("sequences", package="chesstrainer"), full.names=TRUE, pattern=".rds$"), seqdir)
      }
   }

   if (!dir.exists(seqdir))
      stop(.text("dirnotexists"), call.=FALSE)
   if (file.access(seqdir, mode=4L) != 0L)
      stop(.text("noreadaccess"), call.=FALSE)
   if (file.access(seqdir, mode=2L) != 0L)
      stop(.text("nowriteaccess"), call.=FALSE)

   cat(.text("seqdir", seqdir))

   # .sequential file in sequence directory overrides selmode setting

   if (file.exists(file.path(seqdir, ".sequential")))
      selmode <- "sequential"

   # start Stockfish

   tmp <- .sf.start(sfpath=sfpath)
   sfproc <- tmp$sfproc
   sfrun  <- tmp$sfrun

   if (sfrun) {
      sfproc$write_input("uci\n")
      .sf.ready(sfproc)
   }

   # get starting position matrix 'pos'

   start.pos <- .get("pos")

   # some defaults

   selected <- NULL
   oldvolume <- volume
   seqno <- 1

   # create the getGraphicsEvent() functions

   mousedown <- function(buttons, x, y) {
      squares <- .calcsquare(x,y,plt)
      pos.x <- squares[1]
      pos.y <- squares[2]
      click1.x <<- pos.x
      click1.y <<- pos.y
      click2.x <<- pos.x
      click2.y <<- pos.y
      button <<- buttons
      if (identical(buttons, 0L)) {
         if (flip && pos[9-pos.x, 9-pos.y] == "") {
            empty.square <<- TRUE
            return(NULL)
         }
         if (!flip && pos[pos.x, pos.y] == "") {
            empty.square <<- TRUE
            return(NULL)
         }
      }
      empty.square <<- FALSE
      if (identical(buttons, 0L))
         .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd)
      return(NULL)
   }

   dragmousemove <- function(buttons, x, y) {

      if (identical(buttons, 0L) && !empty.square) {

         squares <- .calcsquare(x,y,plt)
         pos.x <- squares[1]
         pos.y <- squares[2]

         .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd)

         if (isTRUE(pos.x != click2.x) || isTRUE(pos.y != click2.y))
            .rmrect(click2.x, click2.y, lwd=lwd)

         click2.x <<- pos.x
         click2.y <<- pos.y

      }

      if (identical(buttons, 2L)) {

         squares <- .calcsquare(x,y,plt)
         pos.x <- squares[1]
         pos.y <- squares[2]

         click2.x <<- pos.x
         click2.y <<- pos.y

      }

      return(NULL)

   }

   mouseup <- function(buttons, x, y) {
      if (identical(buttons, 0L)) {
         .rmrect(click1.x, click1.y, lwd=lwd)
         .rmrect(click2.x, click2.y, lwd=lwd)
      }
      if (givehint1) {
         .rmrect(hintx1, hinty1, lwd=lwd)
         givehint1 <<- FALSE
      }
      if (givehint2) {
         .rmrect(hintx2, hinty2, lwd=lwd)
         givehint2 <<- FALSE
      }
      return(1)
   }

   run.all <- TRUE

   while (run.all) {

      ###### [a] ######

      pos <- start.pos

      # some defaults for a particular round / sequence

      seqname <- ""
      seqnum  <- NA_integer_
      score <- 100
      played <- 0
      totalmoves <- 0
      show <- TRUE
      comment <- ""
      evalval <- NA_real_
      i <- 1
      texttop <- ""
      flip <- FALSE
      hasarrows <- FALSE
      circles <- matrix(c(0,0), nrow=1, ncol=2)
      scoreadd <- 0
      sidetoplay <- "w"
      givehint1 <- FALSE
      givehint2 <- FALSE
      mistake <- FALSE

      .sf.newgame(sfproc, sfrun)

      # select a player if no player is currently selected

      if (player == "") {
         player <- .selectplayer(player, seqdir, mustselect=TRUE)
         settings$player <- player
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
      }

      # load all sequences into 'dat.all'

      files.all <- list.files(seqdir, pattern=".rds$")
      dat.all <- lapply(file.path(seqdir, files.all), readRDS)

      k.all <- length(files.all)

      if (mode == "play" && k.all == 0L) {
         cat(.text("zeroseqsfound"))
         mode <- "add"
      }

      scores.all <- lapply(dat.all, function(x) tail(x$player[[player]]$score, 1))
      scores.all[is.na(scores.all) | .is.null(scores.all)] <- 100
      scores.all <- unlist(scores.all)
      played.all <- lapply(dat.all, function(x) tail(x$player[[player]]$played, 1))
      played.all[is.na(played.all) | .is.null(played.all)] <- 0
      played.all <- unlist(played.all)
      date.all <- lapply(dat.all, function(x) tail(x$player[[player]]$date, 1))
      date.all[is.na(date.all) | .is.null(date.all)] <- NA_real_
      date.all <- unlist(date.all)
      dayslp.all <- as.numeric(Sys.time() - as.POSIXct(date.all), units="days")

      # apply selection to sequences

      if (is.null(selected)) {
        files <- files.all
        dat <- dat.all
      } else {
        files <- files.all[files.all %in% selected]
        dat <- dat.all[files.all %in% selected]
      }

      k <- length(files)

      # if a single sequence is selected and it is deleted, then k will be 0;
      # in this case select all sequences and start over (but only if there is
      # at least one sequence, as otherwise this goes into an infinite loop)

      if (k.all > 0L && k == 0L) {
         cat(.text("zeroseqsfound"))
         cat(.text("allseqselected"))
         selected <- NULL
         next
      }

      scores.selected <- lapply(dat, function(x) tail(x$player[[player]]$score, 1))
      scores.selected[is.na(scores.selected) | .is.null(scores.selected)] <- 100
      scores.selected <- unlist(scores.selected)
      played.selected <- lapply(dat, function(x) tail(x$player[[player]]$played, 1))
      played.selected[is.na(played.selected) | .is.null(played.selected)] <- 0
      played.selected <- unlist(played.selected)
      date.selected <- lapply(dat, function(x) tail(x$player[[player]]$date, 1))
      date.selected[is.na(date.selected) | .is.null(date.selected)] <- NA_real_
      date.selected <- unlist(date.selected)
      dayslp.selected <- as.numeric(Sys.time() - as.POSIXct(date.selected), units="days")

      if (all(scores.selected == 0)) # in case all sequences have a score of 0
         scores.selected <- rep(1, length(scores.selected))

      if (selmode == "score_random") {
         probvals.selected <- scores.selected^expval
         probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
         probvals.selected <- 100 * probvals.selected / sum(probvals.selected)
      }

      if (selmode == "score_highest") {
         probvals.selected <- rep(0, k)
         probvals.selected[which(scores.selected == max(scores.selected[scores.selected != 0]))[1]] <- 100
      }

      if (selmode == "played_random") {
         probvals.selected <- (max(played.selected) + 1 - played.selected) / (max(played.selected) + 1)
         probvals.selected <- probvals.selected^expval
         probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
         probvals.selected <- 100 * probvals.selected / sum(probvals.selected)
      }

      if (selmode == "played_lowest") {
         probvals.selected <- rep(0, k)
         probvals.selected[which(played.selected == min(played.selected[scores.selected != 0]))[1]] <- 100
      }

      if (selmode == "days_random") {
         probvals.selected <- dayslp.selected / max(dayslp.selected)
         probvals.selected[is.na(probvals.selected)] <- 1 # if NA, set prob to 1
         probvals.selected <- probvals.selected^expval
         probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
         probvals.selected <- 100 * probvals.selected / sum(probvals.selected)
      }

      if (selmode == "days_oldest") {
         probvals.selected <- rep(0, k)
         if (any(is.na(dayslp.selected))) {
            probvals.selected[which(is.na(dayslp.selected))[1]] <- 100
         } else {
            probvals.selected[which(dayslp.selected == max(dayslp.selected[scores.selected != 0]))[1]] <- 100
         }
      }

      if (selmode == "sequential") {
         while (scores.selected[seqno] == 0) {
            seqno <- seqno + 1
         }
         probvals.selected <- rep(0, k)
         probvals.selected[seqno] <- 100
      }

      if (mode == "add") {

         # set up the data frame for a new sequence

         sub <- list(flip = flip, moves = data.frame(x1=numeric(), y1=numeric(), x2=numeric(), y2=numeric(), show=logical(), move=character(), eval=numeric(), comment=character()))

      } else {

         # select a sequence

         sel <- sample(seq_len(k), 1L, prob=probvals.selected)

         if (selmode == "sequential") {
            seqno <- seqno + 1
            if (seqno > k)
               seqno <- 1
         }

         sub <- dat[[sel]]

         # overwrite defaults based on the current sequence

         seqname <- files[sel]
         seqnum  <- which(seqname == files.all)

         score <- tail(sub$player[[player]]$score, 1)
         if (is.null(score) || is.na(score))
            score <- 100

         played <- tail(sub$player[[player]]$played, 1)
         if (is.null(played) || is.na(played))
            played <- 0

         totalmoves <- nrow(sub$moves)
         flip <- sub$flip

         if (!is.null(sub$pos))
            pos <- sub$pos

      }

      # draw board and add info at the bottom

      .drawboard(pos, flip=flip)
      .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
      #.draweval(clear=TRUE)

      # check for getGraphicsEvent() capabilities of the current plotting device

      if (any(!is.element(c("MouseDown", "MouseMove", "MouseUp", "Keybd"), dev.capabilities()$events))) {
         dev.off()
         stop(.text("testdevice"), call.=FALSE)
      }

      run.rnd <- TRUE

      while (run.rnd) {

         ###### [b] ######

         if (mode == "play") {

            # play shown moves

            if (!identical(sub$moves$comment[i], "")) {
               texttop <- .texttop(sub$moves$comment[i])
               Sys.sleep(sleep)
            }

            while (isTRUE(sub$moves$show[i])) {
               pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
               sub$moves$move[i] <- attr(pos,"move")
               .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
               i <- i + 1
               sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
               if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                  texttop <- .texttop(sub$moves$comment[i-1])
               } else {
                  texttop <- .texttop(sub$moves$comment[i])
               }
               .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               Sys.sleep(sleep)
            }

            show <- FALSE
            .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)

         } else {

            .drawsideindicator(sidetoplay, flip)

         }

         wait <- TRUE

         while (wait) {

            ###### [c] ######

            ##################################################################

            click1.x <- NULL
            click1.y <- NULL
            click2.x <- NULL
            click2.y <- NULL
            empty.square <- FALSE
            button <- 0L

            plt <- par("plt")

            click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=function(key) return(key))

            keys      <- c("q", " ", "n", "p", "e", "E", "l", "-", "=", "+", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F9", "F12", "m", "/", ".", "w", "t", "h", "ctrl-R", "^", "[", "]", "i", "r", "(", ")", "ctrl-[", "\033", "v", "a", "G", "ctrl-C")
            keys.add  <- c("f", "z", "c", "s", "0", "?", "b")
            keys.play <- c("z", "c", "s", "\b", "ctrl-D", "Right", "Left", "o", "u", "A", "g")

            if (mode == "add" && is.character(click) && !is.element(click, c(keys, keys.add)))
               next
            if (mode == "play" && is.character(click) && !is.element(click, c(keys, keys.play)))
               next

            ##################################################################
            ##################################################################
            ##################################################################

            # q to quit the program

            if (identical(click, "q")) {
               run.all <- FALSE
               run.rnd <- FALSE
               wait <- FALSE
               tmp <- .sf.stop(sfproc, sfrun)
               cat(.text("quit"))
               .quit()
               dev.off()
               next
            }

            # <space> to select the mode (play versus add) (starts a new round)

            if (identical(click, " ")) {
               mode <- ifelse(mode == "add", "play", "add")
               settings$mode <- mode
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               run.rnd <- FALSE
               wait <- FALSE
               next
            }

            # n to start a new sequence (starts a new round)

            if (identical(click, "n")) {
               run.rnd <- FALSE
               wait <- FALSE
               next
            }

            # p to select player (starts a new round)

            if (identical(click, "p")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               player <- .selectplayer(player, seqdir)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               settings$player <- player
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               run.rnd <- FALSE
               wait <- FALSE
               next
            }

            # ctrl-r to remove a player (starts a new round)

            if (identical(click, "ctrl-R")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               rmplayer <- readline(prompt=.text("rlydelplayer", player))
               if (.confirm(rmplayer)) {
                  .removeplayer(player, seqdir)
                  player <- .selectplayer(player, seqdir, mustselect=TRUE)
                  run.rnd <- FALSE
                  wait <- FALSE
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # f to flip the board (only in add mode)

            if (mode == "add" && identical(click, "f")) {
               flip <- !flip
               sub$flip <- flip
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               next
            }

            # z to switch show moves on/off (only in add mode)

            if (mode == "add" && identical(click, "z")) {
               show <- !show
               .texttop(.text("showmoves", show))
               Sys.sleep(1)
               .texttop(texttop)
               .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               next
            }

            # c to add a comment to the current move (only in add mode)

            if (mode == "add" && identical(click, "c")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               comment <- readline(prompt=.text("comment"))
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # s to save the sequence (only in add mode)

            if (mode == "add" && identical(click, "s")) {
               .texttop(.text("saveseq"))
               seqident <- sapply(dat.all, function(x) identical(sub$moves[1:5], x$moves[1:5]))
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               if (any(seqident)) {
                  cat(.text("seqexists", files.all[which(seqident)[1]]))
                  cat(.text("addmoves"))
                  next
               }
               filename <- readline(prompt=.text("filename"))
               if (identical(filename, "")) {
                  if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  next
               }
               filename <- file.path(seqdir, paste0(.fileprefix(flip), filename, ".rds"))
               dosave <- TRUE
               if (file.exists(filename)) {
                  dosave <- FALSE
                  overwrite <- readline(prompt=.text("rlyoverwrite"))
                  if (.confirm(overwrite)) {
                     cat(.text("overwrite"))
                     dosave <- TRUE
                  }
               }
               if (dosave) {
                  saveRDS(sub, file=filename)
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  run.rnd <- FALSE
                  wait <- FALSE
                  seqno <- 1
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # e to edit a sequence using edit()

            if (identical(click, "e")) {
               sub$moves <- edit(sub$moves)
               sub$moves$comment[is.na(sub$moves$comment)] <- ""
               if (mode == "play")
                  saveRDS(sub, file=file.path(seqdir, seqname))
               next
            }

            # E to edit the comments using prompts

            if (identical(click, "E")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               dosave <- FALSE
               print(sub$moves)
               cat("\n")
               while (TRUE) {
                  comnum <- readline(prompt=.text("commenttoedit"))
                  if (identical(comnum, ""))
                     break
                  if (grepl("^[0-9]+$", comnum)) {
                     comnum <- round(as.numeric(comnum))
                     if (comnum < 1 || comnum > nrow(sub$moves))
                        next
                     newcom <- readline(prompt=.text("newcomment"))
                     newcom <- gsub("\\n", "\n", newcom, fixed=TRUE)
                     sub$moves$comment[comnum] <- newcom
                     dosave <- TRUE
                     cat("\n")
                     print(sub$moves)
                     cat("\n")
                  }
               }
               if (dosave && mode == "play")
                  saveRDS(sub, file=file.path(seqdir, seqname))
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # g to show the progress graph (only in play mode)

            if (mode == "play" && identical(click, "g")) {
               if (!is.null(sub$player[[player]]$score)) {
                  .scoregraph(sub$player[[player]], lwd=lwd)
                  .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
                  .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
                  hasarrows <- FALSE
                  circles <- matrix(c(0,0), nrow=1, ncol=2)
               }
               next
            }

            # G to toggle showing the progress graph after completed sequences

            if (identical(click, "G")) {
               showgraph <- !showgraph
               .texttop(.text("showgraph", showgraph))
               Sys.sleep(1)
               .texttop(texttop)
               settings$showgraph <- showgraph
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # l to list all (selected) sequences

            if (identical(click, "l")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               if (k > 0L) {
                  if (max(probvals.selected) == min(probvals.selected)) {
                     bars <- rep(5, length(probvals.selected))
                  } else {
                     bars <- round(5 * (probvals.selected - min(probvals.selected)) / (max(probvals.selected) - min(probvals.selected)))
                  }
                  bars <- sapply(bars, function(x) paste0(rep("*", x), collapse=""))
                  tab <- data.frame(files, played.selected, formatC(dayslp.selected, format="f", digits=1), scores.selected, formatC(probvals.selected, format="f", digits=1), bars)
                  tab$bars <- format(tab$bars, justify="left")
                  names(tab) <- c("Name", .text("played"), .text("days"), .text("score"), .text("prob"), "")
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  if (!is.null(selected))
                     rownames(tab) <- which(files.all %in% selected)
                  print(tab, print.gap=2)
               } else {
                  cat(.text("zeroseqsfound"))
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # / (or .) to enter a search term or sequence range

            if (identical(click, "/") || identical(click, ".")) {

               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))

               searchterm <- readline(prompt=.text("seqsearch"))

               if (identical(searchterm , "")) {
                  cat(.text("allseqselected"))
                  selected <- NULL
                  run.rnd <- FALSE
                  wait <- FALSE
                  mode <- "add"
                  seqno <- 1
                  if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  next
               }

               tmp <- strcapture("^([[:digit:]]+)\\s*-\\s*([[:digit:]]+)$", searchterm, data.frame(seq.lo=integer(), seq.hi=integer()))

               if (!is.na(tmp$seq.lo)) {
                  if (tmp$seq.lo < 1L || tmp$seq.lo > k.all || tmp$seq.hi < tmp$seq.lo || tmp$seq.hi > k.all) {
                     cat(.text("noseqsfound"))
                  } else {
                     cat(.text("selseq12", c(tmp$seq.lo, tmp$seq.hi)))
                     selected <- list.files(seqdir, pattern=".rds$")[tmp$seq.lo:tmp$seq.hi]
                     run.rnd <- FALSE
                     wait <- FALSE
                     mode <- "add"
                     seqno <- 1
                  }
                  if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  next
               }

               if (grepl("^[0-9]+$", searchterm)) {
                  seqno <- as.numeric(searchterm)
                  if (seqno < 1 || seqno > k.all) {
                     cat(.text("noseqfound"))
                  } else {
                     cat(.text("selseq", seqno))
                     selected <- list.files(seqdir, pattern=".rds$")[seqno]
                     run.rnd <- FALSE
                     wait <- FALSE
                     mode <- "add"
                     seqno <- 1
                  }
                  if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  next
               }

               tmp <- strcapture(.text("strcapscore"), searchterm, data.frame(text=character(), sign=character(), cutoff=integer()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqscore", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("scores.all", tmp$sign, tmp$cutoff)))
                  selected <- list.files(seqdir, pattern=".rds$")[selected]
                  if (length(selected) == 0L) {
                     cat(.text("noseqsfound"))
                     selected <- NULL
                  } else {
                     cat(.text("numseqfound", length(selected)))
                     run.rnd <- FALSE
                     wait <- FALSE
                     mode <- "add"
                     seqno <- 1
                  }
                  if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  next
               }

               tmp <- strcapture(.text("strcapplayed"), searchterm, data.frame(text=character(), sign=character(), cutoff=integer()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqplayed", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("played.all", tmp$sign, tmp$cutoff)))
                  selected <- list.files(seqdir, pattern=".rds$")[selected]
                  if (length(selected) == 0L) {
                     cat(.text("noseqsfound"))
                     selected <- NULL
                  } else {
                     cat(.text("numseqfound", length(selected)))
                     run.rnd <- FALSE
                     wait <- FALSE
                     mode <- "add"
                     seqno <- 1
                  }
                  if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  next
               }

               tmp <- strcapture(.text("strcapdays"), searchterm, data.frame(text=character(), sign=character(), cutoff=numeric()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqdays", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("dayslp.all", tmp$sign, tmp$cutoff)))
                  selected[is.na(selected)] <- FALSE
                  selected <- list.files(seqdir, pattern=".rds$")[selected]
                  if (length(selected) == 0L) {
                     cat(.text("noseqsfound"))
                     selected <- NULL
                  } else {
                     cat(.text("numseqfound", length(selected)))
                     run.rnd <- FALSE
                     wait <- FALSE
                     mode <- "add"
                     seqno <- 1
                  }
                  if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  next
               }

               cat(.text("seqsearchterm", searchterm))

               if (length(grep(searchterm, files.all)) == 0L) {
                  cat(.text("noseqsfound"))
               } else {
                  selected <- grepl(searchterm, files.all)
                  selected <- list.files(seqdir, pattern=".rds$")[selected]
                  cat(.text("numseqfound", length(selected)))
                  run.rnd <- FALSE
                  wait <- FALSE
                  mode <- "add"
                  seqno <- 1
               }

               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next

            }

            # ? to find all sequences that start in the same way (only in add mode)

            if (mode == "add" && identical(click, "?")) {
               seqident <- sapply(dat.all, function(x) identical(sub$moves[1:(i-1),1:4], x$moves[1:(i-1),1:4]))
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               if (any(seqident)) {
                  cat(.text("seqsmatch"))
                  tab <- data.frame(Name=files.all[seqident])
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  rownames(tab) <- which(seqident)
                  print(tab, print.gap=2)
                  cat("\n")
                  selmatches <- readline(prompt=.text("selmatches"))
                  if (identical(selmatches, "") || .confirm(selmatches)) {
                     cat(.text("selmatchesconfirm"))
                     selected <- files.all[seqident]
                     run.rnd <- FALSE
                     wait <- FALSE
                     seqno <- 1
                  }
               } else {
                  cat(.text("noseqsfound"))
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # ctrl-d to delete the current sequence (only in play mode)

            if (mode == "play" && identical(click, "ctrl-D")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               answer <- readline(prompt=.text("rlydelseq"))
               if (.confirm(answer)) {
                  cat(.text("delseq"))
                  file.remove(file.path(seqdir, seqname))
                  run.rnd <- FALSE
                  wait <- FALSE
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # h to get a hint

            if (identical(click, "h")) {
               if (mode == "play") {
                  if (!givehint1) {
                     hintx1 <- sub$moves$x1[i]
                     hinty1 <- sub$moves$y1[i]
                     .addrect(hintx1, hinty1, col=.get("col.hint"), lwd=lwd)
                     score <- min(100, score + adjusthint)
                     givehint1 <- TRUE
                     mistake <- TRUE
                  } else {
                     if (!givehint2) {
                        hintx2 <- sub$moves$x2[i]
                        hinty2 <- sub$moves$y2[i]
                        .addrect(hintx2, hinty2, col=.get("col.hint"), lwd=lwd)
                        score <- min(100, score + adjusthint)
                        givehint2 <- TRUE
                     }
                  }
                  .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               } else {
                  if (!identical(bestmove, "")) {
                     .texttop(.text("bestmove", bestmove))
                     hintx1 <- as.numeric(substr(bestmove, 2, 2))
                     hinty1 <- which(letters[1:8] == substr(bestmove, 1, 1))
                     hintx2 <- as.numeric(substr(bestmove, 4, 4))
                     hinty2 <- which(letters[1:8] == substr(bestmove, 3, 3))
                     if (flip) {
                        hintx1 <- 9 - hintx1
                        hinty1 <- 9 - hinty1
                        hintx2 <- 9 - hintx2
                        hinty2 <- 9 - hinty2
                     }
                     .addrect(hintx1, hinty1, col=.get("col.hint"), lwd=lwd)
                     .addrect(hintx2, hinty2, col=.get("col.hint"), lwd=lwd)
                     givehint1 <- TRUE
                     givehint2 <- TRUE
                  }
               }
               next
            }

            # o to edit the (last) score for the current sequence (only in play mode)

            if (mode == "play" && identical(click, "o")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               newscore <- readline(prompt=.text("newscore", score))
               if (grepl("^[0-9]+$", newscore)) {
                  newscore <- round(as.numeric(newscore))
                  newscore[newscore < 0] <- 0
                  cat(.text("setnewscore", newscore))
                  score <- newscore
                  .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # t to take back a score increase (in play mode) or take back a move (in add mode)

            if (identical(click, "t")) {
               if (mode == "play") {
                  if (scoreadd > 0) {
                     score <- score - scoreadd
                     .texttop(.text("setscoreback", score))
                     Sys.sleep(1)
                     .texttop(texttop)
                     .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                     scoreadd <- 0
                  }
               } else {
                  if (i > 1) {
                     if (is.null(sub$pos)) {
                        pos <- start.pos
                     } else {
                        pos <- sub$pos
                     }
                     .drawboard(pos, flip=flip)
                     hasarrows <- FALSE
                     circles <- matrix(c(0,0), nrow=1, ncol=2)
                     i <- i - 1
                     sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]
                     sidetoplay <- "w"
                     for (j in seq_len(i-1)) {
                        pos <- .updateboard(pos, move=sub$moves[j,1:4], flip=flip, volume=0, verbose=verbose)
                        sub$moves$move[j] <- attr(pos,"move")
                        sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     }
                     comment <- ""
                     .draweval(sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
                     .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                     .drawsideindicator(sidetoplay, flip)
                     fen <- .genfen(pos, flip, sidetoplay, i)
                     tmp <- .sf.eval(sfproc, sfrun, sfgo, fen, sidetoplay, verbose)
                     evalval  <- tmp$eval
                     bestmove <- tmp$bestmove
                     sfproc   <- tmp$sfproc
                     sfrun    <- tmp$sfrun
                  }
               }
               next
            }

            # a to copy the current sequence to add new moves as a new sequence (only in play mode or if only a single sequence is selected)
            # A does the same but only goes to the position when A is pressed

            if (identical(click, "a") || identical(click, "A")) {

               if (mode == "add") {
                  if (k == 1) {
                     sel <- 1
                     sub <- dat[[sel]]
                     seqname <- files[sel]
                     seqnum <- which(seqname == files.all)
                     score <- 100
                     played <- 0
                     totalmoves <- nrow(sub$moves)
                     flip <- sub$flip
                     if (!is.null(sub$pos))
                        pos <- sub$pos
                  } else {
                     next
                  }
               }

               mode <- "add"

               sub$player <- NULL

               if (!identical(sub$moves$comment[i], "")) {
                  texttop <- .texttop(sub$moves$comment[i])
                  if (identical(click, "a"))
                     Sys.sleep(sleep)
               }

               if (is.null(sub$pos)) {
                  pos <- start.pos
               } else {
                  pos <- sub$pos
               }

               .drawboard(pos, flip=flip)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)

               if (identical(click, "A"))
                  sub$moves <- sub$moves[1:(i-1),,drop=FALSE]

               for (i in 1:nrow(sub$moves)) {
                  pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
                  sub$moves$move[i] <- attr(pos,"move")
                  if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                     texttop <- .texttop(sub$moves$comment[i-1])
                  } else {
                     texttop <- .texttop(sub$moves$comment[i])
                  }
                  .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                  .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
                  if (identical(click, "a"))
                     Sys.sleep(sleep)
               }

               i <- i + 1
               if (identical(click, "A")) {
                  sidetoplay <- ifelse(flip, "b", "w")
               } else {
                  sidetoplay <- ifelse(flip, "w", "b")
               }
               show <- FALSE
               .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               .drawsideindicator(sidetoplay, flip)
               fen <- .genfen(pos, flip, sidetoplay, i)
               tmp <- .sf.eval(sfproc, sfrun, sfgo, fen, sidetoplay, verbose)
               evalval  <- tmp$eval
               bestmove <- tmp$bestmove
               sfproc   <- tmp$sfproc
               sfrun    <- tmp$sfrun
               next

            }

            # 0 to make the current position the starting position (only in add mode)

            if (mode == "add" && identical(click, "0")) {
               .texttop(.text("setposstart"))
               Sys.sleep(1)
               .texttop(texttop)
               i <- 1
               comment <- ""
               sub$moves <- sub$moves[numeric(0),]
               sub$pos <- pos
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               sidetoplay <- ifelse(flip, "b", "w")
               .drawsideindicator(sidetoplay, flip)
               .draweval(evalval, flip=flip, eval=eval, evalsteps=evalsteps)
               next
            }

            # right arrow to play the next move (only in play mode)

            if (mode == "play" && identical(click, "Right")) {
               if (i > nrow(sub$moves)) {
                  .texttop(.text("waslastmove"))
                  next
               }
               pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
               sub$moves$move[i] <- attr(pos,"move")
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               i <- i + 1
               .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                  texttop <- .texttop(sub$moves$comment[i-1])
               } else {
                  texttop <- .texttop(sub$moves$comment[i])
               }
               givehint1 <- FALSE
               givehint2 <- FALSE
               next
            }

            # left arrow to go back one move (only in play mode)

            if (mode == "play" && identical(click, "Left")) {

               if (i > 1) {
                  if (is.null(sub$pos)) {
                     pos <- start.pos
                  } else {
                     pos <- sub$pos
                  }
                  .drawboard(pos, flip=flip)
                  hasarrows <- FALSE
                  circles <- matrix(c(0,0), nrow=1, ncol=2)
                  i <- i - 1
                  sidetoplay <- "w"
                  for (j in seq_len(i-1)) {
                     pos <- .updateboard(pos, move=sub$moves[j,1:4], flip=flip, volume=0, verbose=verbose)
                     sub$moves$move[j] <- attr(pos,"move")
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                  }
                  comment <- ""
                  .draweval(sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
                  .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                  #.drawsideindicator(sidetoplay, flip)
               }
               givehint1 <- FALSE
               givehint2 <- FALSE
               next

            }

            # Escape to update the board

            if (identical(click, "\033") || identical(click, "ctrl-[")) {
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               next
            }

            # w to switch wait on/off

            if (identical(click, "w")) {
               pause <- !pause
               .texttop(.text("pause", pause))
               Sys.sleep(1)
               .texttop(texttop)
               settings$pause <- pause
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # -/+ (also =) to decrease/increase the time between moves

            if (identical(click, "-") || identical(click, "=") || identical(click, "+")) {
               if (identical(click, "-")) {
                  sleep <- max(0, sleep - 0.25)
               } else {
                  sleep <- min(2, sleep + 0.25)
               }
               .texttop(.text("waittime", formatC(sleep, format="f", digits=2)))
               Sys.sleep(1)
               .texttop(texttop)
               settings$sleep <- sleep
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # m to switch mute on/off

            if (identical(click, "m")) {
               if (volume == 0) {
                  volume <- oldvolume
               } else {
                  oldvolume <- volume
                  volume <- 0
               }
               .texttop(.text("sound", volume > 0))
               Sys.sleep(1)
               .texttop(texttop)
               settings$volume <- volume
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # [/] to decrease/increase the volume

            if (identical(click, "[") || identical(click, "]")) {
               if (identical(click, "[")) {
                  volume <- max(0, volume - 25)
               } else {
                  volume <- min(100, volume + 25)
               }
               oldvolume <- volume
               .texttop(paste0(.text("volume", round(volume)), "%"))
               Sys.sleep(1)
               .texttop(texttop)
               settings$volume <- volume
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # ^ to edit the exponent value

            if (identical(click, "^")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               newexpval <- readline(prompt=.text("newexpval", expval))
               if (grepl("^[0-9]+(.)?([0-9]+)?$", newexpval)) {
                  newexpval <- as.numeric(newexpval)
                  newexpval[newexpval < 0] <- 0
                  if (any(is.infinite(scores.all^newexpval))) {
                     cat(.text("expvalinf"))
                  } else {
                     cat(.text("setnewexpval", newexpval))
                     expval <- newexpval
                     settings$expval <- expval
                     saveRDS(settings, file=file.path(configdir, "settings.rds"))
                     run.rnd <- FALSE
                     wait <- FALSE
                  }
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # i to toggle the language

            if (identical(click, "i")) {
               if (lang == "de") {
                  lang <- "en"
               } else {
                  lang <- "de"
               }
               assign("lang", lang, envir=.chesstrainer)
               .texttop(.text("lang"))
               .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               Sys.sleep(1)
               .texttop(texttop)
               settings$lang <- lang
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # r to choose a selection mode

            if (identical(click, "r")) {
               selmodeold <- selmode
               selmode <- .selmodesetting(selmode, lwd)
               .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               settings$selmode <- selmode
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               if (selmodeold != selmode) {
                  seqno <- 1
                  run.rnd <- FALSE
                  wait <- FALSE
               }
               next
            }

            # (/) to decrease/increase the line width

            if (identical(click, "(") || identical(click, ")")) {
               if (identical(click, "(")) {
                  lwd <- max(1, lwd - 1)
               } else {
                  lwd <- lwd + 1
               }
               .texttop(.text("lwdadj", lwd))
               Sys.sleep(0.5)
               .texttop(texttop)
               settings$lwd <- lwd
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # v to toggle the evaluation bar on/off

            if (identical(click, "v")) {
               eval <- !eval
               .texttop(.text("eval", eval))
               if (eval) {
                  if (mode == "add") {
                     .draweval(evalval, flip=flip, eval=eval, evalsteps=evalsteps)
                  } else {
                     .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
                  }
               } else {
                  .draweval(clear=TRUE)
               }
               Sys.sleep(1)
               .texttop(texttop)
               settings$eval <- eval
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # u to update the evaluations (only in play mode)

            if (mode == "play" && identical(click, "u")) {

               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))

               if (!is.null(sfproc) && sfrun) {

                  if (is.null(sub$pos)) {
                     pos <- start.pos
                  } else {
                     pos <- sub$pos
                  }

                  .drawboard(pos, flip=flip)
                  hasarrows <- FALSE
                  circles <- matrix(c(0,0), nrow=1, ncol=2)
                  sidetoplay <- "w"

                  cat(.text("evalupdateold"))
                  print(sub$moves)
                  cat("\n")
                  cat(.text("evalupdatestart"))
                  cat("\n")

                  for (i in 1:nrow(sub$moves)) {
                     pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
                     sub$moves$move[i] <- attr(pos,"move")
                     if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                        texttop <- .texttop(sub$moves$comment[i-1])
                     } else {
                        texttop <- .texttop(sub$moves$comment[i])
                     }
                     .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     .drawsideindicator(sidetoplay, flip)
                     fen <- .genfen(pos, flip, sidetoplay, i)
                     tmp <- .sf.eval(sfproc, sfrun, sfgo, fen, sidetoplay, verbose)
                     evalval  <- tmp$eval
                     bestmove <- tmp$bestmove
                     sfproc   <- tmp$sfproc
                     sfrun    <- tmp$sfrun
                     .draweval(evalval, flip=flip, eval=eval, evalsteps=evalsteps)
                     sub$moves$eval[i] <- evalval
                  }
                  .printinfo(mode, show, player, seqname, seqnum, score, played, i+1, totalmoves, selmode)
                  .drawsideindicator(sidetoplay, flip)
                  cat(.text("evalupdatenew"))
                  print(sub$moves)
                  saveRDS(sub, file=file.path(seqdir, seqname))

               } else {

                  cat(.text("evalupdatenosf"))

               }

               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next

            }

            # F1 to show the help

            if (identical(click, "F1")) {
               .printhelp(lwd=lwd)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               next
            }

            # F2 to show the leaderboard and player statistics

            if (identical(click, "F2")) {
               .leaderboard(seqdir, files, lwd)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               next
            }

            # F3 to print the settings

            if (identical(click, "F3")) {
               settings <- data.frame(lang, player, mode, selmode, expval, multiplier, adjustwrong, adjusthint, eval, evalsteps, pause, sleep, lwd, volume, showgraph, cex.top, cex.bot, cex.eval, sfpath, sfgo)
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               tab <- t(settings)
               tab <- cbind(tab, .text("explsettings"))
               colnames(tab) <- c("", "")
               print(tab, quote=FALSE, print.gap=3)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # F4 to adjust the colors

            if (identical(click, "F4")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               .colorsettings(cols.all, pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, selmode)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               cols <- sapply(cols.all, function(x) .get(x))
               saveRDS(cols, file=file.path(configdir, "colors.rds"))
               next
            }

            # F5 to adjust the cex values

            if (identical(click, "F5")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               .cexsettings(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, selmode)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               cex.top <- .get("cex.top")
               cex.bot <- .get("cex.bot")
               cex.eval <- .get("cex.eval")
               settings$cex.top <- cex.top
               settings$cex.bot <- cex.bot
               settings$cex.eval <- cex.eval
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F6 to adjust the miscellaneous settings

            if (identical(click, "F6")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               tmp <- .miscsettings(multiplier, adjustwrong, adjusthint, evalsteps)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               multiplier  <- tmp$multiplier
               adjustwrong <- tmp$adjustwrong
               adjusthint  <- tmp$adjusthint
               evalsteps   <- tmp$evalsteps
               settings$multiplier  <- multiplier
               settings$adjustwrong <- adjustwrong
               settings$adjusthint  <- adjusthint
               settings$evalsteps   <- evalsteps
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F7 to adjust the Stockfish settings

            if (identical(click, "F7")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               tmp <- .sfsettings(sfproc, sfrun, sfpath, sfgo)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               sfproc <- tmp$sfproc
               sfrun  <- tmp$sfrun
               sfpath <- tmp$sfpath
               sfgo   <- tmp$sfgo
               settings$sfpath <- sfpath
               settings$sfgo   <- sfgo
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F9 to print the FEN and open the position on lichess

            if (identical(click, "F9")) {
               fen <- .genfen(pos, flip, sidetoplay, i)
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               cat(fen, "\n")
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               #clipr::write_clip(fen)
               fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE))
               browseURL(fen)
               next
            }

            # ctrl-c to copy the FEN to the clipboard

            if (identical(click, "Ctrl-C")) {
               fen <- .genfen(pos, flip, sidetoplay, i)
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               cat(fen, "\n")
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               clipr::write_clip(fen, object_type="character")
               next
            }

            # F12 to toggle verbose on/off

            if (identical(click, "F12")) {
               verbose <- !verbose
               .texttop(.text("verbose", verbose))
               Sys.sleep(0.5)
               .texttop(texttop)
               next
            }

            # b to start the board editor (only in add mode)

            if (mode == "add" && identical(click, "b")) {
               out <- .boardeditor(pos, flip, lwd, verbose)
               pos <- out$pos
               colnames(pos) <- LETTERS[1:8]
               rownames(pos) <- 1:8
               flip <- out$flip
               i <- 1
               show <- FALSE
               comment <- ""
               evalval <- NA_real_
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               sub$moves <- sub$moves[numeric(0),]
               sub$pos <- pos
               sub$flip <- flip
               attr(sub$pos, "move") <- NULL
               attr(sub$pos, "ispp") <- NULL
               attr(sub$pos, "y1") <- NULL
               sidetoplay <- ifelse(flip, "b", "w")
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               next
            }

            ##################################################################

            # if click is an actual click (and drag) on the board

            if (verbose) {
               cat("Click 1: ", click1.x, ", ", click1.y, sep="")
               cat("\n")
               cat("Click 2: ", click2.x, ", ", click2.y, sep="")
               cat("\n")
            }

            # when clicking too fast, click may not be registered

            if (is.null(click1.x) || is.null(click2.x) || is.null(click1.y) || is.null(click2.y))
               next

            if (is.na(click1.x) || is.na(click2.x) || is.na(click1.y) || is.na(click2.y))
               next

            # when start and end positions are the same

            if (click1.x == click2.x && click1.y == click2.y) {

               if (identical(button, 2L)) {

                  # if the right button was pressed, then draw a circle on the square
                  # (or if the square already has a circle, remove the circle)

                  hascircle <- apply(circles, 1, function(pos) isTRUE(pos[1] == click1.x && pos[2] == click1.y))

                  if (any(hascircle)) {
                     .drawsquare(click1.x, click1.y)
                     .drawpiece(click1.x, click1.y, ifelse(flip, pos[9-click1.x,9-click1.y], pos[click1.x,click1.y]))
                     circles <- circles[!hascircle,,drop=FALSE]
                  } else {
                     .addcircle(click1.x, click1.y, lwd=lwd)
                     circles <- rbind(circles, c(click1.x, click1.y))
                  }

               } else {

                  # if other buttons were pressed, clear arrows/circles (if there are any)

                  if (hasarrows || nrow(circles) >= 2L) {
                     .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
                     .draweval(sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
                     hasarrows <- FALSE
                     circles <- matrix(c(0,0), nrow=1, ncol=2)
                  }

               }

               next

            }

            wait <- FALSE

         }

         if (!run.rnd)
            next # jumps to [b] but since run.rnd is FALSE then jumps to [a]

         # start and end positions of the mouse move are not the same

         # if button 2 was used for the move, draw an arrow and go back to [b]

         if (identical(button, 2L)) {
            shape::Arrows(click1.y+0.5, click1.x+0.5, click2.y+0.5, click2.x+0.5, lwd=lwd*4, col=.get("col.annot"), arr.type="triangle", arr.length=lwd*0.1, arr.width=lwd*0.1, ljoin=1)
            hasarrows <- TRUE
            next
         }

         # if button 0 was used for the move and there are arrows/circles, redraw the board before making the move

         if (identical(button, 0L) && (hasarrows || nrow(circles) >= 2L)) {
            .drawboard(pos, flip)
            hasarrows <- FALSE
            circles <- matrix(c(0,0), nrow=1, ncol=2)
            if (mode == "add")
               .drawsideindicator(sidetoplay, flip)
         }

         if (mode == "add" || all(c(click1.x==sub$moves$x1[i], click1.y==sub$moves$y1[i], click2.x==sub$moves$x2[i], click2.y==sub$moves$y2[i]))) {

            # if in add mode or if the move is correct, make the move

            pos <- .updateboard(pos, move=c(click1.x, click1.y, click2.x, click2.y), flip=flip, volume=volume, verbose=verbose)
            .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)

            if (mode == "play") {
               .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
               sub$moves$move[i] <- attr(pos,"move")
            } else {
               .texttop(" ")
            }

         } else {

            # if in play mode and the move was incorrect, adjust the score, and show that it was the wrong move

            mistake <- TRUE

            if (score >= 1) {
               scoreadd <- min(adjustwrong, 100-score)
               score <- score + scoreadd
            }

            .rmrect(click1.x, click1.y, lwd=lwd)
            .addrect(click2.x, click2.y, col=.get("col.wrong"), lwd=lwd)
            playsound(system.file("sounds", "error.ogg", package="chesstrainer"), volume=volume)
            Sys.sleep(sleep/2)
            .rmrect(click2.x, click2.y, lwd=lwd)
            next

         }

         if (mode == "play" && i == nrow(sub$moves)) {

            # end of the sequence in play mode

            .texttop(.text("welldone"))
            playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)

            # adjust the score (but only if no mistake was made)

            if (!mistake && score >= 1)
               score <- ceiling(score * multiplier)

            played <- played + 1

            .printinfo(mode, show, player, seqname, seqnum, score, played, i+1, totalmoves, selmode)

            tmp <- data.frame(date=as.numeric(Sys.time()), played=played, score=score)

            if (is.null(sub$player[[player]])) {
               sub$player[[player]] <- tmp
            } else {
               sub$player[[player]] <- rbind(sub$player[[player]], tmp)
            }

            if (showgraph) {
               .scoregraph(sub$player[[player]], lwd=lwd)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
            }

            if (pause) {

               dobreak <- FALSE
               donext  <- FALSE

               while (TRUE) {

                  click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))

                  if (is.numeric(click))
                     break

                  if (identical(click, "n")) {
                     dobreak <- TRUE
                     break
                  }

                  if (identical(click, "a")) {
                     mode <- "add"
                     sub$player <- NULL
                     show <- FALSE
                     .texttop(" ")
                     .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     .drawsideindicator(sidetoplay, flip)
                     donext <- TRUE
                     break
                  }

                  if (identical(click, "o")) {
                     if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
                     newscore <- readline(prompt=.text("newscore", score))
                     if (grepl("^[0-9]+$", newscore)) {
                        newscore <- round(as.numeric(newscore))
                        newscore[newscore < 0] <- 0
                        cat(.text("setnewscore", newscore))
                        score <- newscore
                        .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                        sub$player[[player]]$score[length(sub$player[[player]]$score)] <- score
                     }
                     if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                  }

                  if (identical(click, "F9")) {
                     fen <- .genfen(pos, flip, ifelse(sidetoplay == "w", "b", "w"), i+1)
                     if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
                     cat(fen, "\n")
                     if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
                     clipr::write_clip(fen)
                     fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE))
                     browseURL(fen)
                  }

                  if (identical(click, "g")) {
                     .scoregraph(sub$player[[player]], lwd=lwd)
                     .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode)
                     .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
                  }

               }

               if (dobreak)
                  break

               if (donext)
                  next

            }

            saveRDS(sub, file=file.path(seqdir, seqname))
            Sys.sleep(2*sleep)
            run.rnd <- FALSE
            next

            # plot(0:25, 100 * 0.80^(0:25), type="o", pch=19, xlab="Played Correctly", ylab="Score", bty="l", ylim=c(0,100))

         }

         i <- i + 1
         sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

         if (mode == "add") {

            fen <- .genfen(pos, flip, sidetoplay, i)
            evalvallast <- evalval
            tmp <- .sf.eval(sfproc, sfrun, sfgo, fen, sidetoplay, verbose)
            evalval  <- tmp$eval
            bestmove <- tmp$bestmove
            sfproc   <- tmp$sfproc
            sfrun    <- tmp$sfrun
            .draweval(evalval, evalvallast, flip=flip, eval=eval, evalsteps=evalsteps)

            # in add move, add the current move to sub

            sub$moves <- rbind(sub$moves, data.frame(x1=click1.x, y1=click1.y, x2=click2.x, y2=click2.y, show=show, move=attr(pos,"move"), eval=evalval, comment=comment))
            comment <- ""

         } else {

            # in play mode, let the trainer play the next move and increase i

            texttop <- .texttop(sub$moves$comment[i])
            .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
            Sys.sleep(sleep)
            pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
            sub$moves$move[i] <- attr(pos,"move")
            .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
            texttop <- .texttop(sub$moves$comment[i])
            i <- i + 1
            sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

         }

         .printinfo(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
         givehint1 <- FALSE
         givehint2 <- FALSE

         # go to [b]

      }

   }

   invisible()

}
