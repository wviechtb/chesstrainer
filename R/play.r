play <- function(lang="en", sfpath="", ...) {

   if (!interactive())
      return(.text("interactive"))

   if (!is.element(lang, c("en","de")))
      stop("Argument 'lang' must be either 'en' or 'de'.", call.=FALSE)

   assign("lang", lang, envir=.chesstrainer)

   # get arguments passed via ...

   ddd <- list(...)

   if (is.null(ddd[["seqdir"]])) { # seqdir can be a vector, so not using ifelse()
      seqdir <- ""
   } else {
      seqdir <- ddd[["seqdir"]]
   }

   player      <- ifelse(is.null(ddd[["player"]]),      "",             ddd[["player"]])
   seqdirpos   <- ifelse(is.null(ddd[["seqdirpos"]]),   1,              ddd[["seqdirpos"]])
   mode        <- ifelse(is.null(ddd[["mode"]]),        "add",          ddd[["mode"]])
   selmode     <- ifelse(is.null(ddd[["selmode"]]),     "score_random", ddd[["selmode"]])
   timed       <- ifelse(is.null(ddd[["timed"]]),       FALSE,          ddd[["timed"]])
   timepermove <- ifelse(is.null(ddd[["timepermove"]]), 5,              ddd[["timepermove"]])
   expval      <- ifelse(is.null(ddd[["expval"]]),      2,              ddd[["expval"]])
   multiplier  <- ifelse(is.null(ddd[["multiplier"]]),  0.8,            ddd[["multiplier"]])
   adjustwrong <- ifelse(is.null(ddd[["adjustwrong"]]), 40,             ddd[["adjustwrong"]])
   adjusthint  <- ifelse(is.null(ddd[["adjusthint"]]),  20,             ddd[["adjusthint"]])
   eval        <- ifelse(is.null(ddd[["eval"]]),        TRUE,           isTRUE(ddd[["eval"]]))
   evalsteps   <- ifelse(is.null(ddd[["evalsteps"]]),   5,              ddd[["evalsteps"]])
   wait        <- ifelse(is.null(ddd[["wait"]]),        TRUE,           isTRUE(ddd[["wait"]]))
   sleep       <- ifelse(is.null(ddd[["sleep"]]),       0.5,            ddd[["sleep"]])
   lwd         <- ifelse(is.null(ddd[["lwd"]]),         2,              ddd[["lwd"]])
   volume      <- ifelse(is.null(ddd[["volume"]]),      50,             ddd[["volume"]])
   showgraph   <- ifelse(is.null(ddd[["showgraph"]]),   FALSE,          ddd[["showgraph"]])
   repmistake  <- ifelse(is.null(ddd[["repmistake"]]),  FALSE,          ddd[["repmistake"]])
   cex.top     <- ifelse(is.null(ddd[["cex.top"]]),     1.4,            ddd[["cex.top"]])
   cex.bot     <- ifelse(is.null(ddd[["cex.bot"]]),     0.7,            ddd[["cex.bot"]])
   cex.eval    <- ifelse(is.null(ddd[["cex.eval"]]),    0.5,            ddd[["cex.eval"]])
   depth1      <- ifelse(is.null(ddd[["depth1"]]),      20,             ddd[["depth1"]])
   depth2      <- ifelse(is.null(ddd[["depth2"]]),      30,             ddd[["depth2"]])
   depth3      <- ifelse(is.null(ddd[["depth3"]]),      10,             ddd[["depth3"]])
   multipv1    <- ifelse(is.null(ddd[["multipv1"]]),    1,              ddd[["multipv1"]])
   multipv2    <- ifelse(is.null(ddd[["multipv2"]]),    1,              ddd[["multipv2"]])
   threads     <- ifelse(is.null(ddd[["threads"]]),     1,              ddd[["threads"]])
   hash        <- ifelse(is.null(ddd[["hash"]]),        256,            ddd[["hash"]])
   quitanim    <- ifelse(is.null(ddd[["quitanim"]]),    TRUE,           ddd[["quitanim"]])

   # get switch1/switch2 functions if they are specified via ...

   if (is.null(ddd[["switch1"]])) {
      switch1 <- parse(text="invisible(NULL)")
   } else {
      switch1 <- parse(text = ddd[["switch1"]])
   }

   if (is.null(ddd[["switch2"]])) {
      switch2 <- parse(text="invisible(NULL)")
   } else {
      switch2 <- parse(text = ddd[["switch2"]])
   }

   # ensure that some arguments are sensible

   if (!is.element(mode, c("add","test","play")))
      stop(.text("modecheck"), call.=FALSE)

   seqdirpos <- round(seqdirpos)
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
   depth1[depth1 < 1] <- 1
   depth2[depth2 < 1] <- 1
   depth3[depth3 < 1] <- 1
   multipv1[multipv1 < 1] <- 1
   multipv2[multipv2 < 1] <- 1
   hash[hash < 16] <- 16

   verbose <- isTRUE(ddd$verbose)

   cols.all <- c("col.bg", "col.fg", "col.square.l", "col.square.d", "col.square.be",
                 "col.top", "col.bot", "col.help", "col.help.border",
                 "col.hint", "col.best", "col.wrong", "col.rect", "col.annot",
                 "col.side.w", "col.side.b", "col.time.fast", "col.time.slow")

   # create config directory and read/save settings and colors

   configdir <- tools::R_user_dir(package="chesstrainer", which="config")

   if (!dir.exists(configdir)) {
      cat(.text("createconfigdir", configdir))
      success <- dir.create(configdir, recursive=TRUE)
      if (!success)
         stop(.text("dircreateerror"), call.=FALSE)
      settings <- list(lang=lang, player=player, mode=mode, seqdir=seqdir, seqdirpos=seqdirpos,
                       selmode=selmode, timed=timed, timepermove=timepermove, expval=expval, multiplier=multiplier, adjustwrong=adjustwrong, adjusthint=adjusthint,
                       eval=eval, evalsteps=evalsteps, wait=wait, sleep=sleep, lwd=lwd, volume=volume, showgraph=showgraph, repmistake=repmistake,
                       cex.top=cex.top, cex.bot=cex.bot, cex.eval=cex.eval,
                       sfpath=sfpath, depth1=depth1, depth2=depth2, depth3=depth3, multipv1=multipv1, multipv2=multipv2, threads=threads, hash=hash)
      saveRDS(settings, file=file.path(configdir, "settings.rds"))
      cols <- sapply(cols.all, function(x) .get(x))
      saveRDS(cols, file=file.path(configdir, "colors.rds"))
   } else {
      if (file.exists(file.path(configdir, "settings.rds"))) {
         cat(.text("loadsettings"))
         # apply settings, but only for those that are not set via play()
         settings <- readRDS(file.path(configdir, "settings.rds"))
         mc <- as.list(match.call())
         if (is.null(mc[["lang"]]))
            lang <- settings[["lang"]]
         if (is.null(mc[["player"]]))
            player <- settings[["player"]]
         if (is.null(mc[["mode"]]))
            mode <- settings[["mode"]]
         if (is.null(mc[["seqdir"]]))
            seqdir <- settings[["seqdir"]]
         if (is.null(mc[["seqdirpos"]]))
            seqdirpos <- settings[["seqdirpos"]]
         if (is.null(mc[["selmode"]]))
            selmode <- settings[["selmode"]]
         if (is.null(mc[["timed"]]))
            timed <- settings[["timed"]]
         if (is.null(mc[["timepermove"]]))
            timepermove <- settings[["timepermove"]]
         if (is.null(mc[["expval"]]))
            expval <- settings[["expval"]]
         if (is.null(mc[["multiplier"]]))
            multiplier <- settings[["multiplier"]]
         if (is.null(mc[["adjustwrong"]]))
            adjustwrong <- settings[["adjustwrong"]]
         if (is.null(mc[["adjusthint"]]))
            adjusthint <- settings[["adjusthint"]]
         if (is.null(mc[["eval"]]))
            eval <- settings[["eval"]]
         if (is.null(mc[["evalsteps"]]))
            evalsteps <- settings[["evalsteps"]]
         if (is.null(mc[["wait"]]))
            wait <- settings[["wait"]]
         if (is.null(mc[["sleep"]]))
            sleep <- settings[["sleep"]]
         if (is.null(mc[["lwd"]]))
            lwd <- settings[["lwd"]]
         if (is.null(mc[["volume"]]))
            volume <- settings[["volume"]]
         if (is.null(mc[["showgraph"]]))
            showgraph <- settings[["showgraph"]]
         if (is.null(mc[["repmistake"]]))
            repmistake <- settings[["repmistake"]]
         if (is.null(mc[["cex.top"]]))
            cex.top <- settings[["cex.top"]]
         if (is.null(mc[["cex.bot"]]))
            cex.bot <- settings[["cex.bot"]]
         if (is.null(mc[["cex.eval"]]))
            cex.eval <- settings[["cex.eval"]]
         if (is.null(mc[["sfpath"]]))
            sfpath <- settings[["sfpath"]]
         if (is.null(mc[["depth1"]]))
            depth1 <- settings[["depth1"]]
         if (is.null(mc[["depth2"]]))
            depth2 <- settings[["depth2"]]
         if (is.null(mc[["depth3"]]))
            depth3 <- settings[["depth3"]]
         if (is.null(mc[["multipv1"]]))
            multipv1 <- settings[["multipv1"]]
         if (is.null(mc[["multipv2"]]))
            multipv2 <- settings[["multipv2"]]
         if (is.null(mc[["threads"]]))
            threads <- settings[["threads"]]
         if (is.null(mc[["hash"]]))
            hash <- settings[["hash"]]
      }
      sfpath <- suppressWarnings(normalizePath(sfpath))
      settings <- list(lang=lang, player=player, mode=mode, seqdir=seqdir, seqdirpos=seqdirpos,
                       selmode=selmode, timed=timed, timepermove=timepermove, expval=expval, multiplier=multiplier, adjustwrong=adjustwrong, adjusthint=adjusthint,
                       eval=eval, evalsteps=evalsteps, wait=wait, sleep=sleep, lwd=lwd, volume=volume, showgraph=showgraph, repmistake=repmistake,
                       cex.top=cex.top, cex.bot=cex.bot, cex.eval=cex.eval,
                       sfpath=sfpath, depth1=depth1, depth2=depth2, depth3=depth3, multipv1=multipv1, multipv2=multipv2, threads=threads, hash=hash)
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

   # if seqdir is not an empty string, remove any directories from seqdir that do not exist

   if (!identical(seqdir, "")) {

      direxists <- dir.exists(seqdir)

      if (any(!direxists)) {
         seqdir <- seqdir[!direxists]
         if (length(seqdir) == 0L)
            seqdir <- ""
         seqdirpos <- 1
         settings$seqdir <- seqdir
         settings$seqdirpos <- seqdirpos
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
      }

      if (seqdirpos < 1 || seqdirpos > length(seqdir)) {
         seqdirpos <- 1
         settings$seqdirpos <- 1
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
      }

   }

   # if seqdir is an empty string, create the default sequence directory and (prompt to) copy the example sequences to the directory

   if (identical(seqdir, "")) {

      seqdir <- tools::R_user_dir(package="chesstrainer", which="data")

      if (!dir.exists(seqdir)) {
         cat(.text("createseqdir", seqdir))
         success <- dir.create(seqdir, recursive=TRUE)
         if (!success)
            stop(.text("dircreateerror"), call.=FALSE)
         copyseqs <- readline(prompt=.text("copyseqs"))
         if (identical(copyseqs, "") || .confirm(copyseqs))
            file.copy(list.files(system.file("sequences", package="chesstrainer"), full.names=TRUE, pattern=".rds$"), seqdir)
      }
      seqdirpos <- 1
      settings$seqdir <- seqdir
      settings$seqdirpos <- seqdirpos
      saveRDS(settings, file=file.path(configdir, "settings.rds"))

   }

   # TODO: what to do with this?
   #if (file.access(seqdir, mode=4L) != 0L)
   #   stop(.text("noreadaccess"), call.=FALSE)
   #if (file.access(seqdir, mode=2L) != 0L)
   #   stop(.text("nowriteaccess"), call.=FALSE)

   cat(.text("useseqdir", seqdir[seqdirpos]))

   # .sequential file in sequence directory overrides selmode setting

   if (file.exists(file.path(seqdir[seqdirpos], ".sequential")) && selmode != "sequential") {
      cat(.text("sequential"))
      cat("\n")
      selmode <- "sequential"
   }

   # start Stockfish

   tmp <- .sf.start(sfpath=sfpath, threads=threads, hash=hash)
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
   seqno <- 1
   filename <- ""
   bookmark <- ""
   lastseq  <- ""
   bestmove <- ""
   useflip  <- TRUE
   replast  <- FALSE
   oldmode  <- ifelse(mode == "play", "add", mode)

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

   keys      <- c("q", " ", "n", "p", "e", "E", "l", "-", "=", "+",
                  "m", "/", "*", "8", "?", "'", ",", ".", "b", "B", "w", "t", "h", "ctrl-R",
                  "^", "6", "[", "]", "i", "(", ")", "ctrl-[", "\033", "v", "a", "G", "R", "ctrl-C", "x", "<",
                  "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F12", "\\", "#")
   keys.add  <- c("f", "z", "c", "H", "0", "s")
   keys.test <- c("o", "r", "g", "A", "ctrl-D", "Right", "Left", "u")
   keys.play <- c("H")
   keys.add  <- c(keys, keys.add)
   keys.test <- c(keys, keys.test)
   keys.play <- c(keys, keys.play)

   run.all <- TRUE

   while (run.all) {

      ###### [a] ######

      pos <- start.pos

      # some defaults for a particular round / sequence

      i          <- 1 # move counter
      seqname    <- ""
      seqnum     <- NA_integer_
      score      <- 100
      played     <- 0
      totalmoves <- 0
      show       <- TRUE
      comment    <- ""
      evalval    <- NA_real_
      texttop    <- ""
      if (useflip) {
         flip <- FALSE
         useflip <- TRUE
      }
      scoreadd     <- 0
      sidetoplay   <- "w"
      givehint1    <- FALSE
      givehint2    <- FALSE
      mistake      <- FALSE
      timetotal    <- 0
      movesplayed  <- 0
      movestoplay  <- 1
      drawcircles  <- TRUE
      drawarrows   <- TRUE
      showstartcom <- TRUE
      matetype     <- "none"

      circles <- matrix(nrow=0, ncol=2) # to store circles
      arrows  <- matrix(nrow=0, ncol=4) # to store arrows
      harrows <- matrix(nrow=0, ncol=4) # to store hint arrows

      # start new game for Stockfish

      .sf.newgame(sfproc, sfrun)

      # select a player if no player is currently selected

      if (player == "") {
         player <- .selectplayer(player, seqdir[seqdirpos], mustselect=TRUE)
         settings$player <- player
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
      }

      # load all sequences into 'dat.all'

      files.all <- list.files(seqdir[seqdirpos], pattern=".rds$")
      dat.all <- lapply(file.path(seqdir[seqdirpos], files.all), readRDS)

      k.all <- length(files.all)

      if (mode == "test" && k.all == 0L) {
         cat(.text("zeroseqsfound"))
         mode <- oldmode <- "add"
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

      if (selmode == "sequential" && length(scores.selected) >= 1L) {
         while (scores.selected[seqno] == 0) {
            seqno <- seqno + 1
         }
         probvals.selected <- rep(0, k)
         probvals.selected[seqno] <- 100
      }

      if (mode %in% c("add","play")) {

         # set up the data frame for a new sequence

         sub <- list(flip = flip, moves = data.frame(x1=numeric(), y1=numeric(), x2=numeric(), y2=numeric(), show=logical(), move=character(),
                                                     eval=numeric(), comment=character(), circles=character(), arrows=character(), fen=character()))

      }

      if (mode == "test") {

         # select a sequence

         if (replast && filename != "") {

            sel <- grep(filename, files)
            replast <- FALSE

         } else {

            sel <- sample(seq_len(k), 1L, prob=probvals.selected)

            if (selmode == "sequential") {
               seqno <- seqno + 1
               if (seqno > k)
                  seqno <- 1
            }

         }

         sub <- dat[[sel]]
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

         if (!is.null(sub$pos)) {
            pos <- sub$pos
            if (nrow(sub$moves) > 0L) {
               # determine sidetoplay based on the first piece moved
               if (flip) {
                  piece <- pos[9-sub$moves[1,1], 9-sub$moves[1,2]]
               } else {
                  piece <- pos[sub$moves[1,1], sub$moves[1,2]]
               }
               sidetoplay <- ifelse(startsWith(piece, "W"), "w", "b")
            }
         }

         # compute number of moves to be made by the player
         movestoplay <- (sum(!sub$moves$show) + 1) / 2

      }

      sidetoplaystart <- sidetoplay

      # draw board and add info at the bottom

      .drawboard(pos, flip=flip)
      .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)

      # check for getGraphicsEvent() capabilities of the current plotting device

      if (any(!is.element(c("MouseDown", "MouseMove", "MouseUp", "Keybd"), dev.capabilities()$events))) {
         dev.off()
         stop(.text("testdevice"), call.=FALSE)
      }

      run.rnd <- TRUE

      while (run.rnd) {

         ###### [b] ######

         if (mode == "test") {

            # show the start comment if there one at move 1 (and showstartcom is TRUE)
            if (i == 1 && !is.null(sub$commentstart) && showstartcom) {
               .startcomment(sub$commentstart, lwd=lwd)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
            }

            if (nrow(sub$moves) == 0L) {
               # can have a sequence that is just a start comment
               .texttop(" ")
               run.rnd <- FALSE
               next
            }

            if (!identical(sub$moves$comment[i], "")) {
               texttop <- .texttop(sub$moves$comment[i])
               if (isTRUE(show[i+1]))
                  Sys.sleep(sleep)
            }

            if (drawcircles) {
               circles <- .parseannot(sub$moves$circles[i], cols=2)
               .drawcircles(circles, lwd=lwd)
            }

            if (drawarrows) {
               arrows <- .parseannot(sub$moves$arrows[i], cols=4)
               .drawarrows(arrows, lwd=lwd)
            }

            # play shown moves

            while (isTRUE(sub$moves$show[i])) {
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                  Sys.sleep(sleep)
                  .rmannot(pos, circles, arrows, flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
               }
               pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
               #sub$moves$move[i] <- attr(pos,"move")
               .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
               i <- i + 1
               sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
               if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                  texttop <- .texttop(sub$moves$comment[i-1])
               } else {
                  texttop <- .texttop(sub$moves$comment[i])
               }
               circles <- .parseannot(sub$moves$circles[i], cols=2)
               arrows <- .parseannot(sub$moves$arrows[i], cols=4)
               .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               Sys.sleep(sleep)
            }

            show <- FALSE
            #.textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)

         }

         if (mode == "test" && timed) {
            .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
         } else {
            .drawsideindicator(sidetoplay, flip)
         }

         input <- TRUE

         while (input) {

            ###### [c] ######

            ##################################################################

            if (mode == "play") {

               # in play mode, if it is the computer to move, make the move now

               .drawsideindicator(sidetoplay, flip)

               # if Stockfish is not running anymore, then it must have crashed, so wait for click and switch to add mode

               if (!sfrun) {
                  .waitforclick()
                  mode <- oldmode <- "add"
                  next
               }

               if (flip) {
                  domove <- sidetoplay == "w"
               } else {
                  domove <- sidetoplay == "b"
               }

               if (domove) {

                  tmp <- .parsesfmove(bestmove[1], pos, flip, NA, rename=FALSE)

                  if (length(tmp$x1) == 0L) {
                     .texttop(.text("nomove"))
                     .waitforclick()
                     run.rnd <- FALSE
                     input <- FALSE
                     next
                  }

                  sub$moves <- rbind(sub$moves, data.frame(x1=tmp$x1, y1=tmp$y1, x2=tmp$x2, y2=tmp$y2, show=FALSE, move=tmp$txt, eval=evalval[1], comment="", circles="", arrows="", fen=fen))
                  pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)

                  i <- i + 1
                  sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

                  .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)

                  fen <- .genfen(pos, flip, sidetoplay, i)
                  evalvallast <- evalval[1]
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, fen, sidetoplay, verbose)
                  evalval   <- res.sf$eval
                  bestmove  <- res.sf$bestmove
                  matetype  <- res.sf$matetype
                  sfproc    <- res.sf$sfproc
                  sfrun     <- res.sf$sfrun

                  sub$moves$eval[i-1] <- evalval[1]
                  sub$moves$fen[i-1] <- fen

                  .drawsideindicator(sidetoplay, flip)
                  .draweval(sub$moves$eval[i-1], sub$moves$eval[i-2], flip=flip, eval=eval, evalsteps=evalsteps)

                  if (!identical(matetype, "none")) {
                     .texttop(.text(matetype))
                     .waitforclick()
                     run.rnd <- FALSE
                     input <- FALSE
                     next
                  }

               }

            }

            ##################################################################

            click1.x <- NULL
            click1.y <- NULL
            click2.x <- NULL
            click2.y <- NULL
            empty.square <- FALSE
            button <- 0L

            plt <- par("plt")

            timestart <- proc.time()[[3]]

            click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=function(key) return(key))

            #if (mode == "test") {
            #   if (seqname == "<lastsequence>.rds") {
            #      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=function(key) return(key))
            #   } else {
            #      click <- "u"
            #   }
            #} else {
            #   click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=function(key) return(key))
            #}

            if (mode == "add" && is.character(click) && !is.element(click, keys.add))
               next
            if (mode == "test" && is.character(click) && !is.element(click, keys.test))
               next
            if (mode == "play" && is.character(click) && !is.element(click, keys.play))
               next

            ##################################################################

            # q to quit the trainer

            if (identical(click, "q")) {
               run.all <- FALSE
               run.rnd <- FALSE
               input <- FALSE
               .sf.stop(sfproc, sfrun)
               cat(.text("quit"))
               if (quitanim)
                  .quit()
               dev.off()
               next
            }

            # <space> to select the mode (add versus test) (starts a new round)

            if (identical(click, " ")) {
               if (mode == "play") {
                  mode <- oldmode
               } else {
                  mode <- ifelse(mode == "add", "test", "add")
                  oldmode <- mode
               }
               settings$mode <- mode
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               run.rnd <- FALSE
               input <- FALSE
               next
            }

            # n to start a new sequence / round (from play mode, jumps back to oldmode)

            if (identical(click, "n")) {
               if (mode == "play")
                  mode <- oldmode
               useflip <- FALSE
               run.rnd <- FALSE
               input <- FALSE
               next
            }

            # p to select a player (starts a new round if a new player is selected)

            if (identical(click, "p")) {
               eval(expr=switch1)
               oldplayer <- player
               player <- .selectplayer(player, seqdir[seqdirpos])
               eval(expr=switch2)
               settings$player <- player
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               if (player != oldplayer) {
                  run.rnd <- FALSE
                  input <- FALSE
               }
               next
            }

            # ctrl-r to remove the current player (starts a new round)

            if (identical(click, "ctrl-R")) {
               eval(expr=switch1)
               rmplayer <- readline(prompt=.text("rlydelplayer", player))
               if (.confirm(rmplayer)) {
                  .removeplayer(player, seqdir[seqdirpos])
                  player <- .selectplayer(player, seqdir[seqdirpos], mustselect=TRUE)
                  run.rnd <- FALSE
                  input <- FALSE
               }
               eval(expr=switch2)
               next
            }

            # f to flip the board (only in add mode)

            if (mode == "add" && identical(click, "f")) {
               if (i > 1) {
                  .texttop(.text("fliponlyatstart", show), sleep=1)
                  .texttop(texttop)
                  next
               }
               flip <- !flip
               sub$flip <- flip
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
               harrows <- matrix(nrow=0, ncol=4)
               next
            }

            # z to switch show moves on/off (only in add mode)

            if (mode == "add" && identical(click, "z")) {
               show <- !show
               .texttop(.text("showmoves", show), sleep=0.75)
               .texttop(texttop)
               .textbot(mode, show=show, onlyshow=TRUE)
               next
            }

            # c to add a comment to the current move (only in add mode)

            if (mode == "add" && identical(click, "c")) {
               eval(expr=switch1)
               comment <- readline(prompt=.text("comment"))
               eval(expr=switch2)
               next
            }

            # s to save the sequence (only in add mode)

            if (mode == "add" && identical(click, "s")) {
               if (all(sub$moves$show)) {
                  .texttop(.text("allmovesshown"))
                  next
               }
               eval(expr=switch1)
               .texttop(.text("saveseq"))
               seqident <- sapply(dat.all, function(x) identical(sub$moves[1:5], x$moves[1:5]))
               if (any(seqident)) {
                  cat(.text("seqexists", files.all[which(seqident)[1]]))
                  cat(.text("addmoves"))
                  next
               }
               filename <- readline(prompt=.text("filename"))
               if (identical(filename, "")) {
                  eval(expr=switch2)
                  next
               }
               filename <- sub("\\.rds$", "", filename) # strip .rds from end of filename
               filenamefull <- file.path(seqdir[seqdirpos], paste0(filename, ".rds"))
               dosave <- TRUE
               if (file.exists(filenamefull)) {
                  dosave <- FALSE
                  overwrite <- readline(prompt=.text("rlyoverwrite"))
                  if (.confirm(overwrite)) {
                     cat(.text("overwrite"))
                     dosave <- TRUE
                  }
               }
               if (dosave) {
                  saveRDS(sub, file=filenamefull)
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  run.rnd <- FALSE
                  input <- FALSE
                  seqno <- 1
               }
               eval(expr=switch2)
               next
            }

            # e to edit the comments using prompts (only in add or test mode)

            if (mode %in% c("add","test") && identical(click, "e")) {
               eval(expr=switch1)
               sub <- .editcomments(sub, seqdir[seqdirpos], seqname, mode) # note: saves after edit only in test mode
               eval(expr=switch2)
               next
            }

            # E to edit a sequence using edit() (only in add or test mode)

            if (mode %in% c("add","test") && identical(click, "E")) {
               sub$moves <- edit(sub$moves)
               sub$moves$comment[is.na(sub$moves$comment)] <- ""
               if (!is.null(sub$moves$circles))
                  sub$moves$circles[is.na(sub$moves$circles)] <- ""
               if (!is.null(sub$moves$arrows))
               sub$moves$arrows[is.na(sub$moves$arrows)] <- ""
               if (mode == "test") # note: saves after edit only in test mode
                  saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
               next
            }

            # g to show the progress graph (only in test mode)

            if (mode == "test" && identical(click, "g")) {
               if (!is.null(sub$player[[player]]$score)) {
                  .scoregraph(sub$player[[player]], lwd=lwd)
                  .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
                  .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
                  .drawcircles(circles, lwd=lwd)
                  .drawarrows(arrows, lwd=lwd)
                  .drawarrows(harrows, lwd=lwd, hint=TRUE)
               }
               next
            }

            # G to toggle showing the progress graph after each completed sequence

            if (identical(click, "G")) {
               showgraph <- !showgraph
               .texttop(.text("showgraph", showgraph), sleep=1)
               .texttop(texttop)
               settings$showgraph <- showgraph
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # r to repeat the last played sequence (only in test mode)

            if (mode == "test" && identical(click, "r")) {
               if (lastseq == "") {
                  .texttop(.text("nolastseq"), sleep=1)
                  .texttop(texttop)
                  next
               }
               .texttop(.text("replast"), sleep=0.75)
               replast <- TRUE
               filename <- lastseq
               run.rnd <- FALSE
               input <- FALSE
               next
            }

            # R to toggle repeating a sequence if a mistake was made

            if (identical(click, "R")) {
               repmistake <- !repmistake
               .texttop(.text("repmistake", repmistake), sleep=1)
               .texttop(texttop)
               settings$repmistake <- repmistake
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # l to list all (selected) sequences

            if (identical(click, "l")) {
               eval(expr=switch1)
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
                  tab$Name <- substr(tab$Name, 1, nchar(tab$Name)-4)
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  if (!is.null(selected))
                     rownames(tab) <- which(files.all %in% selected)
                  print(tab, print.gap=2)
               } else {
                  cat(.text("zeroseqsfound"))
               }
               eval(expr=switch2)
               next
            }

            # < to bookmark a sequence (in add mode, last saved sequence; in test mode, current sequence)

            if (mode %in% c("add","test") && identical(click, "<")) {
               if (mode == "add") {
                  if (filename != "")
                     bookmark <- filename
               }
               if (mode == "test") {
                  if (seqname != "")
                     bookmark <- seqname
               }
               if (bookmark != "") {
                  eval(expr=switch1)
                  cat(.text("bookmarked", bookmark))
                  eval(expr=switch2)
               }
               next
            }

            # / or , to select one or more sequences, . to select last saved sequence, B to select the bookmarked sequence

            if (identical(click, "/") || identical(click, ",") || identical(click, ".") || identical(click, "B")) {

               doprompt <- TRUE

               if (identical(click, ".")) {
                  if (filename != "") {
                     searchterm <- filename
                     doprompt <- FALSE
                  } else {
                     next
                  }
               }

               if (identical(click, "B")) {
                  if (bookmark != "") {
                     searchterm <- bookmark
                     doprompt <- FALSE
                  } else {
                     next
                  }
               }

               eval(expr=switch1)

               if (doprompt) {
                  cat(.text("seqsearch"))
                  searchterm <- readline(prompt="")
               }

               # empty prompt = do nothing

               if (identical(searchterm , "")) {
                  eval(expr=switch2)
                  next
               }

               # * = select all sequences

               if (identical(searchterm , "*")) {
                  cat(.text("allseqselected"))
                  selected <- NULL
                  run.rnd <- FALSE
                  input <- FALSE
                  mode <- oldmode <- "add"
                  seqno <- 1
                  eval(expr=switch2)
                  next
               }

               # FEN entered (use piece placement, active color, and castling availability for matching)

               if (grepl("^([rnbqkpRNBQKP1-8]+/){7}[rnbqkpRNBQKP1-8]+ [wb] (-|[KQkq]{1,4}) (-|[a-h][36]) \\d+ \\d+$", searchterm)) {
                  searchterm <- paste(strsplit(searchterm, " ", fixed=TRUE)[[1]][1:3], collapse=" ")
                  seqident <- sapply(dat.all, function(x) any(grepl(searchterm, x$moves$fen, fixed=TRUE)))
                  if (any(seqident)) {
                     cat(.text("seqsmatchfen"))
                     tab <- data.frame(Name=files.all[seqident])
                     tab$Name <- format(tab$Name, justify="left")
                     names(tab)[1] <- ""
                     rownames(tab) <- which(seqident)
                     print(tab, print.gap=2)
                     selmatches <- readline(prompt=.text("selmatches"))
                     if (identical(selmatches, "") || .confirm(selmatches)) {
                        cat(.text("selmatchesconfirm"))
                        selected <- files.all[seqident]
                        run.rnd <- FALSE
                        input <- FALSE
                        mode <- oldmode <- "add"
                        seqno <- 1
                     }
                  } else {
                     cat(.text("noseqsfound"))
                  }
                  eval(expr=switch2)
                  next
               }

               # C: ... (or c: or K: or k:) entered (search among the comments)

               if (grepl("^[CcKk]:\\s.*$", searchterm)) {
                  searchterm <- tolower(trimws(strsplit(searchterm, ":")[[1]][2]))
                  seqident <- sapply(dat.all, function(x) any(grepl(searchterm, tolower(x$moves$comment), fixed=TRUE)))
                  if (any(seqident)) {
                     cat(.text("seqsmatchcomment"))
                     tab <- data.frame(Name=files.all[seqident])
                     tab$Name <- format(tab$Name, justify="left")
                     names(tab)[1] <- ""
                     rownames(tab) <- which(seqident)
                     print(tab, print.gap=2)
                     selmatches <- readline(prompt=.text("selmatches"))
                     if (identical(selmatches, "") || .confirm(selmatches)) {
                        cat(.text("selmatchesconfirm"))
                        selected <- files.all[seqident]
                        run.rnd <- FALSE
                        input <- FALSE
                        mode <- oldmode <- "add"
                        seqno <- 1
                     }
                  } else {
                     cat(.text("noseqsfound"))
                  }
                  eval(expr=switch2)
                  next
               }

               # 'number - number' entered

               tmp <- strcapture("^([[:digit:]]+)\\s*-\\s*([[:digit:]]+)$", searchterm, data.frame(seq.lo=integer(), seq.hi=integer()))

               if (!is.na(tmp$seq.lo)) {
                  if (tmp$seq.lo < 1L || tmp$seq.lo > k.all || tmp$seq.hi < tmp$seq.lo || tmp$seq.hi > k.all) {
                     cat(.text("noseqsfound"))
                  } else {
                     cat(.text("selseq12", c(tmp$seq.lo, tmp$seq.hi)))
                     selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[tmp$seq.lo:tmp$seq.hi]
                     run.rnd <- FALSE
                     input <- FALSE
                     mode <- oldmode <- "add"
                     seqno <- 1
                  }
                  eval(expr=switch2)
                  next
               }

               # 'number' entered

               if (grepl("^[0-9]+$", searchterm)) {
                  seqno <- as.numeric(searchterm)
                  if (seqno < 1 || seqno > k.all) {
                     cat(.text("noseqfound"))
                  } else {
                     cat(.text("selseq", seqno))
                     selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[seqno]
                     run.rnd <- FALSE
                     input <- FALSE
                     mode <- oldmode <- "add"
                     seqno <- 1
                  }
                  eval(expr=switch2)
                  next
               }

               # 'score >/</>=/<= value' entered

               tmp <- strcapture(.text("strcapscore"), searchterm, data.frame(text=character(), sign=character(), cutoff=integer()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqscore", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("scores.all", tmp$sign, tmp$cutoff)))
                  selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[selected]
                  if (length(selected) == 0L) {
                     cat(.text("noseqsfound"))
                     selected <- NULL
                  } else {
                     cat(.text("numseqfound", length(selected)))
                     run.rnd <- FALSE
                     input <- FALSE
                     mode <- oldmode <- "add"
                     seqno <- 1
                  }
                  eval(expr=switch2)
                  next
               }

               # 'played >/</>=/<= value' entered

               tmp <- strcapture(.text("strcapplayed"), searchterm, data.frame(text=character(), sign=character(), cutoff=integer()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqplayed", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("played.all", tmp$sign, tmp$cutoff)))
                  selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[selected]
                  if (length(selected) == 0L) {
                     cat(.text("noseqsfound"))
                     selected <- NULL
                  } else {
                     cat(.text("numseqfound", length(selected)))
                     run.rnd <- FALSE
                     input <- FALSE
                     mode <- oldmode <- "add"
                     seqno <- 1
                  }
                  eval(expr=switch2)
                  next
               }

               # 'days >/</>=/<= value' entered

               tmp <- strcapture(.text("strcapdays"), searchterm, data.frame(text=character(), sign=character(), cutoff=numeric()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqdays", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("dayslp.all", tmp$sign, tmp$cutoff)))
                  selected[is.na(selected)] <- FALSE
                  selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[selected]
                  if (length(selected) == 0L) {
                     cat(.text("noseqsfound"))
                     selected <- NULL
                  } else {
                     cat(.text("numseqfound", length(selected)))
                     run.rnd <- FALSE
                     input <- FALSE
                     mode <- oldmode <- "add"
                     seqno <- 1
                  }
                  eval(expr=switch2)
                  next
               }

               # if nothing above applies, then a string was entered for matching against the file names

               cat(.text("seqsearchterm", searchterm))

               if (length(grep(searchterm, files.all)) == 0L) {
                  cat(.text("noseqsfound"))
               } else {
                  selected <- grepl(searchterm, files.all)
                  selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[selected]
                  cat(.text("numseqfound", length(selected)))
                  run.rnd <- FALSE
                  input <- FALSE
                  mode <- oldmode <- "add"
                  seqno <- 1
               }

               eval(expr=switch2)
               next

            }

            # * (or 8) to select all sequences (starts a new round in add mode)

            if (identical(click, "*") || identical(click, "8")) {
               eval(expr=switch1)
               cat(.text("allseqselected"))
               selected <- NULL
               run.rnd <- FALSE
               input <- FALSE
               mode <- oldmode <- "add"
               seqno <- 1
               eval(expr=switch2)
               next
            }

            # ' to find all sequences with the same FEN

            if (identical(click, "'")) {
               if (i == 1)
                  next
               eval(expr=switch1)
               searchterm <- .genfen(pos, flip, sidetoplay, i)
               searchterm <- paste(strsplit(searchterm, " ", fixed=TRUE)[[1]][1:3], collapse=" ")
               seqident <- sapply(dat.all, function(x) any(grepl(searchterm, x$moves$fen, fixed=TRUE)))
               if (any(seqident)) {
                  cat(.text("seqsmatchfen"))
                  tab <- data.frame(Name=files.all[seqident])
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  rownames(tab) <- which(seqident)
                  print(tab, print.gap=2)
                  selmatches <- readline(prompt=.text("selmatches"))
                  if (identical(selmatches, "") || .confirm(selmatches)) {
                     cat(.text("selmatchesconfirm"))
                     selected <- files.all[seqident]
                     run.rnd <- FALSE
                     input <- FALSE
                     seqno <- 1
                  }
               } else {
                  cat(.text("noseqsfound"))
               }
               eval(expr=switch2)
               next
            }

            # ? to find all sequences that start in the same way

            if (identical(click, "?")) {
               if (i == 1)
                  next
               eval(expr=switch1)
               seqident <- sapply(dat.all, function(x) identical(sub$moves[1:(i-1),1:4], x$moves[1:(i-1),1:4]))
               if (any(seqident)) {
                  cat(.text("seqsmatchstart"))
                  tab <- data.frame(Name=files.all[seqident])
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  rownames(tab) <- which(seqident)
                  print(tab, print.gap=2)
                  selmatches <- readline(prompt=.text("selmatches"))
                  if (identical(selmatches, "") || .confirm(selmatches)) {
                     cat(.text("selmatchesconfirm"))
                     selected <- files.all[seqident]
                     run.rnd <- FALSE
                     input <- FALSE
                     seqno <- 1
                  }
               } else {
                  cat(.text("noseqsfound"))
               }
               eval(expr=switch2)
               next
            }

            # ctrl-d to delete the current sequence (only in test mode)

            if (mode == "test" && identical(click, "ctrl-D")) {
               eval(expr=switch1)
               answer <- readline(prompt=.text("rlydelseq"))
               if (.confirm(answer)) {
                  cat(.text("delseq"))
                  file.remove(file.path(seqdir[seqdirpos], seqname))
                  run.rnd <- FALSE
                  input <- FALSE
               }
               eval(expr=switch2)
               next
            }

            # H to do a deep evaluation in add or play mode and show the best move(s)

            if (mode %in% c("add","play") && identical(click, "H") && !is.null(sfproc) && sfrun) {
               fen <- .genfen(pos, flip, sidetoplay, i)
               .texttop(.text("sfdeepeval"))
               res.sf <- .sf.eval(sfproc, sfrun, depth2, multipv2, fen, sidetoplay, verbose, progbar=TRUE)
               evalval  <- res.sf$eval
               bestmove <- res.sf$bestmove
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
               click <- "h"
            }

            # h to get a hint in test mode or show the best move in add or play mode

            if (identical(click, "h")) {
               if (mode == "test") {
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
                  .textbot(mode, score=score, onlyscore=TRUE)
               } else {
                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                     .rmannot(pos, circles, rbind(arrows, harrows), flip)
                     harrows <- matrix(nrow=0, ncol=4)
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                  }
                  if (i == 1 && is.na(evalval[1])) {
                     fen <- .genfen(pos, flip, sidetoplay, i)
                     res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, fen, sidetoplay, verbose)
                     evalval  <- res.sf$eval
                     bestmove <- res.sf$bestmove
                     matetype <- res.sf$matetype
                     sfproc   <- res.sf$sfproc
                     sfrun    <- res.sf$sfrun
                  }
                  if (any(bestmove != "")) {
                     nmoves <- length(bestmove)
                     bestmovetxt <- c()
                     for (j in 1:nmoves) {
                        if (bestmove[j] == "")
                           next
                        bestmovetxt <- c(bestmovetxt, .parsesfmove(bestmove[j], pos, flip, evalval[j])$txt)
                        bestx1 <- as.numeric(substr(bestmove[j], 2, 2))
                        besty1 <- which(letters[1:8] == substr(bestmove[j], 1, 1))
                        bestx2 <- as.numeric(substr(bestmove[j], 4, 4))
                        besty2 <- which(letters[1:8] == substr(bestmove[j], 3, 3))
                        if (flip) {
                           bestx1 <- 9 - bestx1
                           besty1 <- 9 - besty1
                           bestx2 <- 9 - bestx2
                           besty2 <- 9 - besty2
                        }
                        harrows <- rbind(harrows, c(bestx1, besty1, bestx2, besty2))
                     }
                     .drawarrows(harrows, lwd=lwd, hint=TRUE)
                     if (nmoves > 1L)
                        bestmovetxt <- paste0(seq_along(bestmovetxt), ": ", bestmovetxt)
                     .texttop(paste0(bestmovetxt, collapse="\n"), pos=4, xpos=4.1)
                  } else {
                     .texttop(.text("nobestmove"), sleep=1)
                     .texttop(" ")
                  }
               }
               next
            }

            # o to edit the (last) score for the current sequence (only in test mode)

            if (mode == "test" && identical(click, "o")) {
               eval(expr=switch1)
               newscore <- readline(prompt=.text("newscore", score))
               if (grepl("^[0-9]+$", newscore)) {
                  newscore <- round(as.numeric(newscore))
                  newscore[newscore < 0] <- 0
                  cat(.text("setnewscore", newscore))
                  score <- newscore
                  .textbot(mode, score=score, onlyscore=TRUE)
                  saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
               }
               eval(expr=switch2)
               next
            }

            # t to take back a score increase (in test mode) or take back a move (in add or play mode)

            if (identical(click, "t")) {
               if (mode == "test") {
                  if (scoreadd > 0) {
                     score <- score - scoreadd
                     .texttop(.text("setscoreback", score), sleep=1)
                     .texttop(texttop)
                     .textbot(mode, score=score, onlyscore=TRUE)
                     scoreadd <- 0
                  } else {
                     if (score == 100) {
                        .texttop(.text("setscoreback100"), sleep=1)
                        .texttop(texttop)
                     }
                  }
                  mistake <- FALSE
               }
               if (mode %in% c("add","play")) {
                  if (i > 1) {
                     oldeval <- sub$moves$eval[i-1]
                     posold <- pos
                     if (is.null(sub$pos)) {
                        pos <- start.pos
                     } else {
                        pos <- sub$pos
                     }
                     i <- i - ifelse(mode == "play", 2, 1)
                     sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]
                     sidetoplay <- sidetoplaystart
                     for (j in seq_len(nrow(sub$moves))) {
                        pos <- .updateboard(pos, move=sub$moves[j,1:6], flip=flip, autoprom=TRUE, volume=0, verbose=verbose, draw=FALSE)
                        #sub$moves$move[j] <- attr(pos,"move")
                        sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     }
                     comment <- ""
                     playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
                     if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                        .rmannot(pos, circles, rbind(arrows, harrows), flip)
                        circles <- matrix(nrow=0, ncol=2)
                        arrows  <- matrix(nrow=0, ncol=4)
                        harrows <- matrix(nrow=0, ncol=4)
                     }
                     .redrawpos(pos, posold, flip=flip)
                     .draweval(sub$moves$eval[i-1], oldeval, flip=flip, eval=eval, evalsteps=evalsteps)
                     .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
                     .drawsideindicator(sidetoplay, flip)
                     fen <- .genfen(pos, flip, sidetoplay, i)
                     res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, fen, sidetoplay, verbose)
                     evalval  <- res.sf$eval[1]
                     bestmove <- res.sf$bestmove[1]
                     matetype <- res.sf$matetype
                     sfproc   <- res.sf$sfproc
                     sfrun    <- res.sf$sfrun
                  }
               }
               next
            }

            # a to copy the current sequence to add new moves as a new sequence
            # (only in test mode or if only a single sequence is selected in add mode)
            # A does the same but starts at the position when A is pressed

            if (mode %in% c("add","test") && (identical(click, "a") || identical(click, "A"))) {

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
                     click <- "a"
                  } else {
                     next
                  }
               }

               mode <- oldmode <- "add"

               sub$player <- NULL

               if (identical(click, "A")) {

                  # when 'A' is pressed at the start position sub$pos, then i is 1 and sub$moves has 0 rows

                  if (nrow(sub$moves) > 0L)
                     sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]

                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                     .rmannot(pos, circles, rbind(arrows, harrows), flip)
                     circles <- matrix(nrow=0, ncol=2)
                     arrows  <- matrix(nrow=0, ncol=4)
                     harrows <- matrix(nrow=0, ncol=4)
                  }

               } else {

                  if (is.null(sub$pos)) {
                     pos <- start.pos
                     sidetoplay <- "w"
                  } else {
                     pos <- sub$pos
                     if (flip) {
                        piece <- pos[9-sub$moves[1,1], 9-sub$moves[1,2]]
                     } else {
                        piece <- pos[sub$moves[1,1], sub$moves[1,2]]
                     }
                     sidetoplay <- ifelse(startsWith(piece, "W"), "w", "b")
                  }

                  .drawboard(pos, flip=flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)

                  if (!identical(sub$moves$comment[1], "")) {
                     texttop <- .texttop(sub$moves$comment[1])
                     Sys.sleep(sleep)
                  }

                  sidetoplaystart <- sidetoplay

                  for (i in 1:nrow(sub$moves)) {
                     pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
                     #sub$moves$move[i] <- attr(pos,"move")
                     if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                        texttop <- .texttop(sub$moves$comment[i-1])
                     } else {
                        texttop <- .texttop(sub$moves$comment[i])
                     }
                     .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
                     .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     Sys.sleep(sleep)
                  }

                  i <- i + 1
                  show <- FALSE

               }

               .textbot(mode, show=show, i=i, totalmoves=totalmoves, onlyshow=TRUE, onlyi=TRUE)
               .drawsideindicator(sidetoplay, flip)
               fen <- .genfen(pos, flip, sidetoplay, i)
               res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, fen, sidetoplay, verbose)
               evalval  <- res.sf$eval[1]
               bestmove <- res.sf$bestmove[1]
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               next

            }

            # 0 to make the current position the starting position (only in add mode)

            if (mode == "add" && identical(click, "0")) {
               .texttop(.text("setposstart"), sleep=1)
               .texttop(texttop)
               i <- 1
               comment <- ""
               sub$moves <- sub$moves[numeric(0),]
               sub$pos <- pos
               #.redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               #.draweval(evalval, flip=flip, eval=eval, evalsteps=evalsteps)
               next
            }

            # right arrow to play the next move (only in test mode)

            if (mode == "test" && identical(click, "Right")) {
               if (i > nrow(sub$moves)) {
                  .texttop(.text("waslastmove"))
                  next
               }
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                  .rmannot(pos, circles, arrows, flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
               }
               pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
               #sub$moves$move[i] <- attr(pos,"move")
               .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
               sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
               if (timed) {
                  .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
               } else {
                  .drawsideindicator(sidetoplay, flip)
               }
               i <- i + 1
               .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                  texttop <- .texttop(sub$moves$comment[i-1])
               } else {
                  texttop <- .texttop(sub$moves$comment[i])
               }
               givehint1 <- FALSE
               givehint2 <- FALSE
               next
            }

            # left arrow to go back one move (only in test mode)

            if (mode == "test" && identical(click, "Left")) {

               if (i > 1) {
                  posold <- pos
                  if (is.null(sub$pos)) {
                     pos <- start.pos
                  } else {
                     pos <- sub$pos
                  }
                  sidetoplay <- sidetoplaystart
                  i <- i - 1
                  for (j in seq_len(i-1)) {
                     pos <- .updateboard(pos, move=sub$moves[j,1:6], flip=flip, autoprom=TRUE, volume=0, verbose=verbose, draw=FALSE)
                     #sub$moves$move[j] <- attr(pos,"move")
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                  }
                  comment <- ""
                  playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                     .rmannot(pos, circles, arrows, flip)
                     circles <- matrix(nrow=0, ncol=2)
                     arrows  <- matrix(nrow=0, ncol=4)
                  }
                  .redrawpos(pos, posold, flip=flip)
                  .draweval(sub$moves$eval[i-1], sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
                  .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
                  if (timed) {
                     .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
                  } else {
                     .drawsideindicator(sidetoplay, flip)
                  }
               }
               givehint1 <- FALSE
               givehint2 <- FALSE
               next

            }

            # Escape to update the board

            if (identical(click, "\033") || identical(click, "ctrl-[")) {
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
               harrows <- matrix(nrow=0, ncol=4)
               next
            }

            # w to switch wait on/off

            if (identical(click, "w")) {
               wait <- !wait
               .texttop(.text("wait", wait), sleep=0.75)
               .texttop(texttop)
               settings$wait <- wait
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
               .texttop(.text("sleeptime", formatC(sleep, format="f", digits=2)), sleep=0.75)
               .texttop(texttop)
               settings$sleep <- sleep
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # [/] to decrease/increase the sound volume

            if (identical(click, "[") || identical(click, "]")) {
               if (identical(click, "[")) {
                  volume <- max(0, volume - 25)
               } else {
                  volume <- min(100, volume + 25)
               }
               playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
               .texttop(paste0(.text("volume", round(volume)), "%"), sleep=0.5)
               .texttop(texttop)
               settings$volume <- volume
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # ^ (or 6) to edit the exponent value

            if (identical(click, "^") || identical(click, "6")) {
               eval(expr=switch1)
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
                     input <- FALSE
                  }
               }
               eval(expr=switch2)
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
               .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
               Sys.sleep(0.75)
               .texttop(texttop)
               settings$lang <- lang
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # m to choose a sequence selection mode

            if (identical(click, "m")) {
               selmodeold <- selmode
               selmode <- .selmodesetting(selmode, lwd)
               if (selmodeold != selmode) {
                  seqno <- 1
                  run.rnd <- FALSE
                  input <- FALSE
               } else {
                  .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                  settings$selmode <- selmode
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
                  .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
                  .drawcircles(circles, lwd=lwd)
                  .drawarrows(arrows, lwd=lwd)
                  .drawarrows(harrows, lwd=lwd, hint=TRUE)
               }
               next
            }

            # x to switch the timed mode on/off (only in add or test mode)

            if (mode %in% c("add","test") && identical(click, "x")) {
               timed <- !timed
               .texttop(.text("timed", timed), sleep=0.75)
               .texttop(texttop)
               settings$timed <- timed
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               if (timed) {
                  run.rnd <- FALSE
                  input <- FALSE
               } else {
                  .drawtimer(clear=TRUE)
                  .drawsideindicator(sidetoplay, flip)
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
               .texttop(.text("lwdadj", lwd), sleep=0.5)
               .texttop(texttop)
               settings$lwd <- lwd
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # v to toggle the evaluation bar on/off

            if (identical(click, "v")) {
               eval <- !eval
               .texttop(.text("eval", eval), sleep=0.75)
               if (eval) {
                  .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               } else {
                  .draweval(clear=TRUE)
               }
               .texttop(texttop)
               settings$eval <- eval
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # u to update the evaluations (only in test mode)

            if (mode == "test" && identical(click, "u")) {

               eval(expr=switch1)

               if (!is.null(sfproc) && sfrun) {

                  if (is.null(sub$pos)) {
                     pos <- start.pos
                  } else {
                     pos <- sub$pos
                  }

                  .drawboard(pos, flip=flip)
                  .drawtimer(clear=TRUE)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  sidetoplay <- sidetoplaystart

                  if (is.null(sub$moves$circles))
                     sub$moves$circles <- ""
                  if (is.null(sub$moves$arrows))
                     sub$moves$arrows <- ""
                  if (is.null(sub$moves$fen))
                     sub$moves$fen <- ""

                  cat(.text("evalupdateold"))
                  print(sub$moves[-11])
                  cat(.text("evalupdatestart"))

                  for (i in 1:nrow(sub$moves)) {
                     pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
                     #sub$moves$move[i] <- attr(pos,"move")
                     if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                        texttop <- .texttop(sub$moves$comment[i-1])
                     } else {
                        texttop <- .texttop(sub$moves$comment[i])
                     }
                     .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     .drawsideindicator(sidetoplay, flip)
                     fen <- .genfen(pos, flip, sidetoplay, i)
                     res.sf <- .sf.eval(sfproc, sfrun, depth2, multipv1, fen, sidetoplay, verbose, progbar=TRUE)
                     evalval  <- res.sf$eval
                     bestmove <- res.sf$bestmove
                     matetype <- res.sf$matetype
                     sfproc   <- res.sf$sfproc
                     sfrun    <- res.sf$sfrun
                     sub$moves$eval[i] <- evalval[1]
                     sub$moves$fen[i] <- fen
                     .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
                  }
                  .textbot(mode, i=i+1, totalmoves=totalmoves, onlyi=TRUE)
                  .drawsideindicator(sidetoplay, flip)
                  cat(.text("evalupdatenew"))
                  print(sub$moves[-11])
                  saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  if (!wait) {
                     run.rnd <- FALSE
                     input <- FALSE
                  }

               } else {

                  cat(.text("evalupdatenosf"))

               }

               eval(expr=switch2)
               next

            }

            # F1 to show the help overlay

            if (identical(click, "F1")) {
               .showhelp(lwd=lwd)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE)
               next
            }

            # F2 to show the leaderboard and player statistics

            if (identical(click, "F2")) {
               .leaderboard(seqdir[seqdirpos], files, lwd)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE)
               next
            }

            # F3 to print the settings

            if (identical(click, "F3")) {
               tab <- data.frame(lang, player, mode, seqdir=seqdir[seqdirpos], selmode, timed, timepermove, expval, multiplier, adjustwrong, adjusthint, eval, evalsteps, wait, sleep, lwd, volume, showgraph, repmistake, cex.top, cex.bot, cex.eval, sfpath, depth1, depth2, depth3, multipv1, multipv2, threads, hash)
               .showsettings(tab, lwd)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE)
               next
            }

            # F4 to adjust the colors

            if (identical(click, "F4")) {
               eval(expr=switch1)
               .colorsettings(cols.all, pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               eval(expr=switch2)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE)
               cols <- sapply(cols.all, function(x) .get(x))
               saveRDS(cols, file=file.path(configdir, "colors.rds"))
               next
            }

            # F5 to adjust the cex values

            if (identical(click, "F5")) {
               eval(expr=switch1)
               .cexsettings(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, lwd, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               eval(expr=switch2)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE)
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
               eval(expr=switch1)
               tmp <- .miscsettings(multiplier, adjustwrong, adjusthint, evalsteps, timepermove)
               eval(expr=switch2)
               multiplier  <- tmp$multiplier
               adjustwrong <- tmp$adjustwrong
               adjusthint  <- tmp$adjusthint
               evalsteps   <- tmp$evalsteps
               timepermove <- tmp$timepermove
               settings$multiplier  <- multiplier
               settings$adjustwrong <- adjustwrong
               settings$adjusthint  <- adjusthint
               settings$evalsteps   <- evalsteps
               settings$timepermove <- timepermove
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F7 to adjust the Stockfish settings

            if (identical(click, "F7")) {
               eval(expr=switch1)
               tmp <- .sfsettings(sfproc, sfrun, sfpath, depth1, depth2, depth3, multipv1, multipv2, threads, hash)
               eval(expr=switch2)
               sfproc   <- tmp$sfproc
               sfrun    <- tmp$sfrun
               sfpath   <- tmp$sfpath
               depth1   <- tmp$depth1
               depth2   <- tmp$depth2
               depth3   <- tmp$depth3
               multipv1 <- tmp$multipv1
               multipv2 <- tmp$multipv2
               threads  <- tmp$threads
               hash     <- tmp$hash
               settings$sfpath   <- sfpath
               settings$depth1   <- depth1
               settings$depth2   <- depth2
               settings$depth3   <- depth3
               settings$multipv1 <- multipv1
               settings$multipv2 <- multipv2
               settings$threads  <- threads
               settings$hash     <- hash
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F8 to manage / select sequence directories

            if (identical(click, "F8")) {
               eval(expr=switch1)
               tmp <- .seqdirsettings(seqdir, seqdirpos)
               eval(expr=switch2)
               seqdirold <- seqdir
               seqdirposold <- seqdirpos
               seqdir <- tmp$seqdir
               seqdirpos <- tmp$seqdirpos
               settings$seqdir <- seqdir
               settings$seqdirpos <- seqdirpos
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               if (!identical(seqdir, seqdirold) || !identical(seqdirpos, seqdirposold)) {
                  seqno <- 1
                  if (file.exists(file.path(seqdir[seqdirpos], ".sequential")) && selmode != "sequential") {
                     .texttop(.text("sequential"), sleep=1.5)
                     selmode <- "sequential"
                  }
                  run.rnd <- FALSE
                  input <- FALSE
               }
               next
            }

            # F9 to print the FEN and open the position on lichess.org

            if (identical(click, "F9")) {
               eval(expr=switch1)
               fen <- .genfen(pos, flip, sidetoplay, i)
               cat(fen, "\n")
               eval(expr=switch2)
               fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE))
               browseURL(fen)
               next
            }

            # F10 to show histograms / scatterplot

            if (identical(click, "F10")) {
               .distributions(scores.selected, played.selected, dayslp.selected, lwd, multiplier)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               .draweval(sub$moves$eval[i-1], 0, flip=flip, eval=eval, evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE)
               next
            }

            # ctrl-c to copy the FEN to the clipboard

            if (identical(click, "ctrl-C")) {
               eval(expr=switch1)
               fen <- .genfen(pos, flip, sidetoplay, i)
               cat(fen, "\n")
               eval(expr=switch2)
               clipr::write_clip(fen, object_type="character")
               .texttop(.text("copyfen"), sleep=0.75)
               .texttop(texttop)
               next
            }

            # F12 to toggle verbose mode on/off

            if (identical(click, "F12")) {
               verbose <- !verbose
               if (verbose) {
                  eval(expr=switch1)
                  .printverbose(selected, seqno, filename, bookmark, lastseq, flip, useflip, replast, oldmode, i, seqname, seqnum, score, played, totalmoves, show, comment, bestmove, evalval, texttop, scoreadd, sidetoplay, givehint1, givehint2, mistake, timetotal, movesplayed, movestoplay, drawcircles, drawarrows, showstartcom, pos)
                  eval(expr=switch2)
               }
               .texttop(.text("verbose", verbose), sleep=0.5)
               .texttop(texttop)
               next
            }

            # b to start the board editor (only in add or play mode)

            if (mode %in% c("add","play") && identical(click, "b")) {
               out <- .boardeditor(pos, flip, sidetoplay, lwd, verbose, switch1, switch2)
               pos <- out$pos
               colnames(pos) <- LETTERS[1:8]
               rownames(pos) <- 1:8
               flip <- out$flip
               sidetoplay <- out$sidetoplay
               i <- 1
               # show is FALSE unless the next move is made by the opposite side
               if (flip) {
                  show <- sidetoplay == "w"
               } else {
                  show <- sidetoplay == "s"
               }
               sidetoplaystart <- sidetoplay
               comment <- ""
               evalval <- NA_real_
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
               harrows <- matrix(nrow=0, ncol=4)
               sub$moves <- sub$moves[numeric(0),]
               sub$flip <- flip
               attr(pos, "move") <- NULL
               attr(pos, "ispp") <- NULL
               attr(pos, "y1") <- NULL
               attr(pos,"moves50") <- 0
               sub$pos <- pos
               if (verbose)
                  print(pos)
               .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
               next
            }

            # \ (or #) to switch into play mode (or back to add mode)

            if (identical(click, "\\") || identical(click, "#")) {
               sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]
               if (mode == "play") {
                  mode <- "add"
                  .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                  next
               }
               if (!sfrun) {
                  .texttop(.text("noplaymode"), sleep=1)
                  .texttop(texttop)
                  next
               }
               mode <- "play"
               timed <- FALSE
               fen <- .genfen(pos, flip, sidetoplay, i)
               res.sf <- .sf.eval(sfproc, sfrun, ifelse(flip, ifelse(sidetoplay=="b", depth1, depth3), ifelse(sidetoplay=="w", depth1, depth3)), multipv1, fen, sidetoplay, verbose)
               evalval  <- res.sf$eval[1]
               bestmove <- res.sf$bestmove[1]
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
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

                  # if the right button was pressed, then draw a circle on the square (or if the square already has a circle, remove the circle)

                  hascircle <- apply(circles, 1, function(x) isTRUE(x[1] == click1.x && x[2] == click1.y))

                  if (any(hascircle)) {
                     .drawsquare(click1.x, click1.y)
                     .drawpiece(click1.x, click1.y, ifelse(flip, pos[9-click1.x,9-click1.y], pos[click1.x,click1.y]))
                     circles <- circles[!hascircle,,drop=FALSE]
                  } else {
                     .drawcircle(click1.x, click1.y, lwd=lwd)
                     circles <- rbind(circles, c(click1.x, click1.y))
                  }

               } else {

                  # if other buttons were pressed, remove the annotations (if there are any)

                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                     .rmannot(pos, circles, rbind(arrows, harrows), flip)
                     circles <- matrix(nrow=0, ncol=2)
                     arrows  <- matrix(nrow=0, ncol=4)
                     harrows <- matrix(nrow=0, ncol=4)
                     next
                  }

               }

               next

            }

            input <- FALSE

         }

         if (!run.rnd)
            next # jumps to [b] but since run.rnd is FALSE then jumps to [a]

         # if we are here, then the start and end positions of the mouse move are not the same

         # if button 2 was used for the move, draw an arrow and go back to [b]

         if (identical(button, 2L)) {
            .drawarrow(click1.x, click1.y, click2.x, click2.y, lwd=lwd)
            arrows <- rbind(arrows, c(click1.x, click1.y, click2.x, click2.y))
            drawcircles  <- FALSE # to prevent circles from being redrawn
            drawarrows   <- FALSE # to prevent arrows from being redrawn
            showstartcom <- FALSE # to prevent the start comment from being shown again
            next
         }

         drawcircles  <- TRUE
         drawarrows   <- TRUE
         showstartcom <- TRUE

         # if button 0 was used for the move and there are arrows/circles, remove the annotations before making the move

         circlesvar <- ""
         arrowsvar  <- ""

         if (identical(button, 0L) && (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L)) {
            .rmannot(pos, circles, rbind(arrows, harrows), flip)
            if (nrow(circles) >= 1L)
               circlesvar <- paste0(apply(circles, 1, function(x) paste0("(",x[1],",",x[2],")")), collapse=";")
            if (nrow(arrows) >= 1L)
               arrowsvar <- paste0(apply(arrows, 1, function(x) paste0("(",x[1],",",x[2],",",x[3],",",x[4],")")), collapse=";")
            circles <- matrix(nrow=0, ncol=2)
            arrows  <- matrix(nrow=0, ncol=4)
            harrows <- matrix(nrow=0, ncol=4)
         }

         if (mode %in% c("add","play")) {

            # if in add or play mode, make the move

            pos <- .updateboard(pos, move=data.frame(click1.x, click1.y, click2.x, click2.y, NA, NA), flip=flip, autoprom=FALSE, volume=volume, verbose=verbose)
            .texttop(" ")

         }

         if (mode == "test") {

            domistake <- TRUE

            # if in test mode, check that the move is correct (and also check that a promotion is made correctly)

            if (all(c(click1.x==sub$moves$x1[i], click1.y==sub$moves$y1[i], click2.x==sub$moves$x2[i], click2.y==sub$moves$y2[i]))) {
               tmp <- .updateboard(pos, move=data.frame(click1.x, click1.y, click2.x, click2.y, sub$moves[i,5:6]), flip=flip, autoprom=FALSE, volume=volume, verbose=verbose)
               if (!identical(tmp, "prommistake")) {
                  movesplayed <- movesplayed + 1
                  timenow <- proc.time()[[3]]
                  timetotal <- timetotal + (timenow - timestart)
                  if (timed) .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
                  domistake <- FALSE
                  pos <- tmp
                  .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
                  #sub$moves$move[i] <- attr(pos,"move")
               }
            }

            # if the move was incorrect, adjust the score, and show that it was the wrong move

            if (domistake) {
               mistake <- TRUE
               if (score >= 1) {
                  scoreadd <- min(adjustwrong, 100-score)
                  score <- score + scoreadd
               }
               .textbot(mode, score=score, onlyscore=TRUE)
               .rmrect(click1.x, click1.y, lwd=lwd)
               .addrect(click2.x, click2.y, col=.get("col.wrong"), lwd=lwd)
               playsound(system.file("sounds", "error.ogg", package="chesstrainer"), volume=volume)
               Sys.sleep(sleep/2)
               .rmrect(click2.x, click2.y, lwd=lwd)
               next
            }

            if (i == nrow(sub$moves)) {

               # end of the sequence in test mode

               if (timed) {

                  # check that time was not exceeded in timed mode

                  timepermitted <- timepermove * movestoplay

                  if (verbose) {
                     cat("Moves played:   ", movestoplay, "\n")
                     cat("Time permitted: ", timepermitted, "\n")
                     cat("Time total:     ", timetotal, "\n")
                  }

                  if (timetotal > timepermitted) {
                     mistake <- TRUE
                     if (score >= 1) {
                        scoreadd <- min(adjustwrong, 100-score)
                        score <- score + scoreadd
                     }
                     .texttop(.text("tooslow", round(timetotal, digits=2), round(timepermitted, digits=2)))
                     .waitforclick()
                  }

               }

               if (is.null(sub$commentend)) {
                  if (mistake) {
                     .texttop(.text("nextseq"))
                  } else {
                     .texttop(.text("welldone"))
                  }
               } else {
                  .texttop(sub$commentend)
                  if (!wait)
                     .waitforclick()
               }

               playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)

               lastseq <- seqname

               if (mistake && repmistake) {
                  replast <- TRUE
                  filename <- seqname
               }

               # adjust the score (but only if no mistake was made)

               if (!mistake && score > 1)
                  score <- round(score * multiplier)

               played <- played + 1

               .textbot(mode, show, player, seqname, seqnum, score, played, i+1, totalmoves, selmode)

               tmp <- data.frame(date=as.numeric(Sys.time()), played=played, score=score)

               if (is.null(sub$player[[player]])) {
                  sub$player[[player]] <- tmp
               } else {
                  sub$player[[player]] <- rbind(sub$player[[player]], tmp)
               }

               if (showgraph) {
                  .scoregraph(sub$player[[player]], lwd=lwd)
                  .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
                  .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
               }

               if (wait) {

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

                     if (identical(click, "a") || identical(click, "A")) {
                        saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                        oldmode <- "test"
                        mode <- "add"
                        sub$player <- NULL
                        show <- FALSE
                        .texttop(" ")
                        .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                        sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                        .drawsideindicator(sidetoplay, flip)
                        donext <- TRUE
                        break
                     }

                     if (identical(click, "\\") || identical(click, "#")) {
                        saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                        sub$moves <- sub$moves[seq_len(i),,drop=FALSE]
                        oldmode <- "test"
                        mode <- "play"
                        timed <- FALSE
                        .texttop(" ")
                        sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                        .drawsideindicator(sidetoplay, flip)
                        i <- i + 1
                        fen <- .genfen(pos, flip, sidetoplay, i)
                        res.sf <- .sf.eval(sfproc, sfrun, ifelse(flip, ifelse(sidetoplay=="b", depth1, depth3), ifelse(sidetoplay=="w", depth1, depth3)), multipv1, fen, sidetoplay, verbose)
                        evalval  <- res.sf$eval[1]
                        bestmove <- res.sf$bestmove[1]
                        matetype <- res.sf$matetype
                        sfproc   <- res.sf$sfproc
                        sfrun    <- res.sf$sfrun
                        .textbot(mode, show, player, seqname, seqnum, score, played, i, totalmoves, selmode)
                        donext <- TRUE
                        break
                     }

                     if (identical(click, "o")) {
                        eval(expr=switch1)
                        newscore <- readline(prompt=.text("newscore", score))
                        if (grepl("^[0-9]+$", newscore)) {
                           newscore <- round(as.numeric(newscore))
                           newscore[newscore < 0] <- 0
                           cat(.text("setnewscore", newscore))
                           score <- newscore
                           .textbot(mode, score=score, onlyscore=TRUE)
                           sub$player[[player]]$score[length(sub$player[[player]]$score)] <- score
                        }
                        eval(expr=switch2)
                     }

                     if (identical(click, "F9")) {
                        fen <- .genfen(pos, flip, ifelse(sidetoplay == "w", "b", "w"), i+1)
                        fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE))
                        browseURL(fen)
                     }

                     if (identical(click, "ctrl-C")) {
                        fen <- .genfen(pos, flip, sidetoplay, i)
                        clipr::write_clip(fen, object_type="character")
                     }

                     if (identical(click, "g")) {
                        .scoregraph(sub$player[[player]], lwd=lwd)
                        .redrawall(pos, flip, mode, show, player, seqname, seqnum, score, played, i, totalmoves, texttop, sidetoplay, selmode, timed, movestoplay, movesplayed, timetotal, timepermove)
                        .draweval(sub$moves$eval[i], flip=flip, eval=eval, evalsteps=evalsteps)
                     }

                     if (identical(click, "F12")) {
                        eval(expr=switch1)
                        .printverbose(selected, seqno, filename, bookmark, lastseq, flip, useflip, replast, oldmode, i, seqname, seqnum, score, played, totalmoves, show, comment, bestmove, evalval, texttop, scoreadd, sidetoplay, givehint1, givehint2, mistake, timetotal, movesplayed, movestoplay, drawcircles, drawarrows, showstartcom, pos)
                        eval(expr=switch2)
                     }

                  }

                  if (dobreak) # to skip saving when 'n' was pressed
                     break

                  if (donext)
                     next

               }

               saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
               Sys.sleep(2*sleep)
               run.rnd <- FALSE
               next

            }

         }

         i <- i + 1
         sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

         if (mode %in% c("add","play")) {

            # get the evaluation after the move has been made

            fen <- .genfen(pos, flip, sidetoplay, i)
            evalvallast <- evalval[1]
            res.sf <- .sf.eval(sfproc, sfrun, ifelse(mode=="add", depth1, depth3), multipv1, fen, sidetoplay, verbose)
            evalval  <- res.sf$eval
            bestmove <- res.sf$bestmove
            matetype <- res.sf$matetype
            sfproc   <- res.sf$sfproc
            sfrun    <- res.sf$sfrun

            .draweval(evalval[1], evalvallast, flip=flip, eval=eval, evalsteps=evalsteps)

            # add the current move to sub

            if (is.null(sub$moves$circles))
               sub$moves$circles <- ""
            if (is.null(sub$moves$arrows))
               sub$moves$arrows <- ""
            if (is.null(sub$moves$fen))
               sub$moves$fen <- ""

            sub$moves <- rbind(sub$moves, data.frame(x1=click1.x, y1=click1.y, x2=click2.x, y2=click2.y, show=show, move=attr(pos,"move"), eval=evalval[1], comment=comment, circles=circlesvar, arrows=arrowsvar, fen=fen))
            comment <- ""

            if (mode == "play" && !identical(matetype, "none")) {
               playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
               .texttop(.text(matetype))
               .waitforclick()
               run.rnd <- FALSE
               input <- FALSE
               next
            }

         }

         if (mode == "test") {

            # in test mode, let the trainer play the next move and increase i

            .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
            if (timed) {
               .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
            } else {
               .drawsideindicator(sidetoplay, flip)
            }
            texttop <- .texttop(sub$moves$comment[i])
            circles <- .parseannot(sub$moves$circles[i], cols=2)
            arrows  <- .parseannot(sub$moves$arrows[i], cols=4)
            if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .waitforclick()
            } else {
               Sys.sleep(sleep)
            }
            pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
            #sub$moves$move[i] <- attr(pos,"move")
            .draweval(sub$moves$eval[i], sub$moves$eval[i-1], flip=flip, eval=eval, evalsteps=evalsteps)
            texttop <- .texttop(sub$moves$comment[i])
            if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
               .rmannot(pos, circles, arrows, flip)
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
            }
            i <- i + 1
            sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

         }

         .textbot(mode, i=i, totalmoves=totalmoves, onlyi=TRUE)
         givehint1 <- FALSE
         givehint2 <- FALSE

         # go to [b]

      }

   }

   invisible()

}
