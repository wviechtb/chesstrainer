play <- function(player="", mode="add", sleep=0.5, volume=0.5, lwd=2, cex.top=1.4, cex.bot=0.7, cex.eval=0.5, expval=2, pause=TRUE, random=TRUE, eval=TRUE, lang="en", ...) {

   if (!is.element(lang, c("en","de")))
      stop("Argument 'lang' must be either 'en' or 'de'.", call.=FALSE)

   assign("lang", lang, envir=.chesstrainer)

   # ensure that some arguments are sensible

   if (!is.element(mode, c("add","play")))
      stop(.text("modecheck"), call.=FALSE)

   sleep[sleep < 0] <- 0
   volume[volume < 0] <- 0
   volume[volume > 1] <- 1
   lwd[lwd < 1] <- 1
   cex.top[cex.top < 0.1] <- 0.1
   cex.bot[cex.bot < 0.1] <- 0.1
   cex.eval[cex.eval < 0.1] <- 0.1
   expval[expval < 0] <- 0

   ddd <- list(...)

   verbose <- isTRUE(ddd$verbose)

   # create config directory and read/save settings and colors

   configdir <- tools::R_user_dir(package="chesstrainer", which="config")

   if (!dir.exists(configdir)) {
      cat(.text("createconfigdir", configdir))
      success <- dir.create(configdir, recursive=TRUE)
      if (!success)
         stop(.text("dircreateerror"), call.=FALSE)
      settings <- data.frame(player, mode, sleep, volume, lwd, cex.top, cex.bot, cex.eval, expval, pause, random, eval, lang)
      saveRDS(settings, file=file.path(configdir, "settings.rds"))
      cols <- c("col.bg"=.get("col.bg"), "col.fg"=.get("col.fg"),
                "col.square.l"=.get("col.square.l"), "col.square.d"=.get("col.square.d"), "col.square.be"=.get("col.square.be"),
                "col.top"=.get("col.top"), "col.bot"=.get("col.bot"), "col.help"=.get("col.help"), "col.help.border"=.get("col.help.border"),
                "col.hint"=.get("col.hint"), "col.wrong"=.get("col.wrong"), "col.rect"=.get("col.rect"), "col.annot"=.get("col.annot"),
                "col.side.w"=.get("col.side.w"), "col.side.b"=.get("col.side.b"))
      saveRDS(cols, file=file.path(configdir, "colors.rds"))
   } else {
      if (file.exists(file.path(configdir, "settings.rds"))) {
         # apply settings, but only for those that are not set via play()
         settings <- readRDS(file.path(configdir, "settings.rds"))
         mc <- as.list(match.call())
         if (is.null(mc$player))
            player <- settings$player
         if (is.null(mc$mode))
            mode <- settings$mode
         if (is.null(mc$sleep))
            sleep <- settings$sleep
         if (is.null(mc$volume))
            volume <- settings$volume
         if (is.null(mc$lwd))
            lwd <- settings$lwd
         if (is.null(mc$cex.top))
            cex.top <- settings$cex.top
         if (is.null(mc$cex.bot))
            cex.bot <- settings$cex.bot
         if (is.null(mc$cex.eval))
            cex.eval <- settings$cex.eval
         if (is.null(mc$expval))
            expval <- settings$expval
         if (is.null(mc$pause))
            pause <- settings$pause
         if (is.null(mc$random))
            random <- settings$random
         if (is.null(mc$eval))
            eval <- settings$eval
         if (is.null(mc$lang))
            lang <- settings$lang
         settings <- data.frame(player, mode, sleep, volume, lwd, cex.top, cex.bot, cex.eval, expval, pause, random, eval, lang)
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
      } else {
         settings <- data.frame(player, mode, sleep, volume, lwd, cex.top, cex.bot, cex.eval, expval, pause, random, eval, lang)
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
      }
      if (file.exists(file.path(configdir, "colors.rds"))) {
         cols <- readRDS(file.path(configdir, "colors.rds"))
         assign("col.bg",          cols["col.bg"],          envir=.chesstrainer)
         assign("col.fg",          cols["col.fg"],          envir=.chesstrainer)
         assign("col.square.l",    cols["col.square.l"],    envir=.chesstrainer)
         assign("col.square.d",    cols["col.square.d"],    envir=.chesstrainer)
         assign("col.square.be",   cols["col.square.be"],   envir=.chesstrainer)
         assign("col.top",         cols["col.top"],         envir=.chesstrainer)
         assign("col.bot",         cols["col.bot"],         envir=.chesstrainer)
         assign("col.help",        cols["col.help"],        envir=.chesstrainer)
         assign("col.help.border", cols["col.help.border"], envir=.chesstrainer)
         assign("col.hint",        cols["col.hint"],        envir=.chesstrainer)
         assign("col.wrong",       cols["col.wrong"],       envir=.chesstrainer)
         assign("col.rect",        cols["col.rect"],        envir=.chesstrainer)
         assign("col.annot",       cols["col.annot"],       envir=.chesstrainer)
         assign("col.side.w",      cols["col.side.w"],      envir=.chesstrainer)
         assign("col.side.b",      cols["col.side.b"],      envir=.chesstrainer)
      } else {
         cols <- c("col.bg"=.get("col.bg"), "col.fg"=.get("col.fg"),
                   "col.square.l"=.get("col.square.l"), "col.square.d"=.get("col.square.d"), "col.square.be"=.get("col.square.be"),
                   "col.top"=.get("col.top"), "col.bot"=.get("col.bot"), "col.help"=.get("col.help"), "col.help.border"=.get("col.help.border"),
                   "col.hint"=.get("col.hint"), "col.wrong"=.get("col.wrong"), "col.rect"=.get("col.rect"), "col.annot"=.get("col.annot"),
                   "col.side.w"=.get("col.side.w"), "col.side.b"=.get("col.side.b"))
         saveRDS(cols, file=file.path(configdir, "colors.rds"))
      }
   }

   assign("lang", lang, envir=.chesstrainer)
   assign("cex.top", cex.top, envir=.chesstrainer)
   assign("cex.bot", cex.bot, envir=.chesstrainer)
   assign("cex.eval", cex.eval, envir=.chesstrainer)

   # create / check sequence directory

   if (is.null(ddd$seqdir)) {
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
   } else {
      seqdir <- ddd$seqdir
   }

   if (!dir.exists(seqdir))
      stop(.text("dirnotexists"), call.=FALSE)
   if (file.access(seqdir, mode=4L) != 0L)
      stop(.text("noreadaccess"), call.=FALSE)
   if (file.access(seqdir, mode=2L) != 0L)
      stop(.text("nowriteaccess"), call.=FALSE)

   # .random/.sequential files in sequence directory override random setting

   if (file.exists(file.path(seqdir, ".sequential")))
      random <- FALSE
   if (file.exists(file.path(seqdir, ".random")))
      random <- TRUE

   # get starting position matrix 'pos'

   start.pos <- .get("pos")

   # some defaults

   selected <- list.files(seqdir, pattern=".rds$")
   oldvolume <- volume
   seqno <- 1

   run.all <- TRUE

   while (run.all) {

      ###### [a] ######

      pos <- start.pos

      # some defaults for a particular round / sequence

      seqname <- ""
      score <- 100
      played <- 0
      totalmoves <- 0
      show <- TRUE
      comment <- ""
      i <- 1
      hintval <- 0
      texttop <- ""
      flip <- FALSE
      hasarrows <- FALSE
      circles <- matrix(c(0,0), nrow=1, ncol=2)
      scoreadd <- 0
      sidetoplay <- "w"

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

      scores.all <- sapply(dat.all, function(x) x$score[player])
      scores.all[is.na(scores.all) | .is.null(scores.all)] <- 100
      scores.all <- unname(unlist(scores.all))
      played.all <- sapply(dat.all, function(x) x$played[player])
      played.all[is.na(played.all) | .is.null(played.all)] <- 0
      played.all <- unname(unlist(played.all))

      # apply selection to sequences

      files <- files.all[files.all %in% selected]
      dat <- dat.all[files.all %in% selected]

      k <- length(files)

      # if a single sequence is selected and it is deleted, then k will be 0;
      # in this case select all sequences and start over

      if (k == 0L) {
         cat(.text("zeroseqsfound"))
         cat(.text("allseqselected"))
         selected <- list.files(seqdir, pattern=".rds$")
         next
      }

      scores.selected <- sapply(dat, function(x) x$score[player])
      scores.selected[is.na(scores.selected) | .is.null(scores.selected)] <- 100
      scores.selected <- unname(unlist(scores.selected))
      played.selected <- sapply(dat, function(x) x$played[player])
      played.selected[is.na(played.selected) | .is.null(played.selected)] <- 0
      played.selected <- unname(unlist(played.selected))

      if (all(scores.selected == 0)) # in case all sequences have a score of 0
         scores.selected <- rep(1, length(scores.selected))

      probvals.selected <- scores.selected^expval
      probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
      probvals.selected <- 100 * probvals.selected / sum(probvals.selected)

      if (mode == "add") {

         # set up the data frame for a new sequence

         sub <- list(flip=flip, score=setNames(100, player), played=setNames(0, player),
                     moves=data.frame(x1=numeric(), y1=numeric(), x2=numeric(), y2=numeric(), show=logical(), move=character(), eval=numeric(), comment=character()))

      } else {

         # select a sequence

         if (random) {
            sel <- sample(seq_len(k), 1L, prob=probvals.selected)
         } else {
            sel <- seqno
            seqno <- seqno + 1
            if (seqno > k)
               seqno <- 1
         }

         sub <- dat[[sel]]

         # overwrite defaults based on the current sequence

         seqname <- files[sel]
         if (is.null(sub$score))
            sub$score <- setNames(score, player)
         if (is.na(sub$score[player]))
            sub$score[player] <- score
         score <- sub$score[player]
         if (is.null(sub$played))
            sub$played <- setNames(played, player)
         if (is.na(sub$played[player]))
            sub$played[player] <- played
         played <- sub$played[player]
         totalmoves <- nrow(sub$moves)
         flip <- sub$flip

         if (!is.null(sub$pos))
            pos <- sub$pos

      }

      # draw board and add info at the bottom

      .drawboard(pos, flip=flip)
      .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
      .draweval(clear=TRUE)

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
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
               i <- i + 1
               texttop <- .texttop(sub$moves$comment[i])
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
               Sys.sleep(sleep)
            }

            show <- FALSE
            .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)

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

            .calcsquare <- function(x, y) {
               square.x <- floor((y - plt[3]) / (plt[4] - plt[3]) * 8 + 1)
               square.y <- floor((x - plt[1]) / (plt[2] - plt[1]) * 8 + 1)
               square.x[square.x < 1] <- 1
               square.x[square.x > 8] <- 8
               square.y[square.y < 1] <- 1
               square.y[square.y > 8] <- 8
               return(c(square.x, square.y))
            }

            mousedown <- function(buttons, x, y) {
               squares <- .calcsquare(x,y)
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

                  squares <- .calcsquare(x,y)
                  pos.x <- squares[1]
                  pos.y <- squares[2]

                  .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd)

                  if (isTRUE(pos.x != click2.x) || isTRUE(pos.y != click2.y))
                     .rmrect(click2.x, click2.y, lwd=lwd)

                  click2.x <<- pos.x
                  click2.y <<- pos.y

               }

               if (identical(buttons, 2L)) {

                  squares <- .calcsquare(x,y)
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
               return(1)
            }

            click <- getGraphicsEvent(prompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=function(key) return(key))

            keys      <- c("q", " ", "n", "p", "e", "l", "-", "=", "+", "F1", "F2", "F3", "m", "/", ".", "w", "t", "ctrl-R", "u", "^", "[", "]", "i", "r", "(", ")", "ctrl-[", "\033", "F12", "F4", "F5", "v")
            keys.add  <- c("f", "z", "c", "s", "0", "?", "b")
            keys.play <- c("z", "c", "s", "\b", "ctrl-D", "h", "a", "Right", "o")

            if (mode == "add" && is.character(click) && !is.element(click, c(keys, keys.add)))
               next
            if (mode == "play" && is.character(click) && !is.element(click, c(keys, keys.play)))
               next

            ##################################################################

            # q to quit the program

            if (identical(click, "q")) {
               run.all <- FALSE
               run.rnd <- FALSE
               wait <- FALSE
               #sfproc$kill()
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
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
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

            # z to switch show moves on/off (only in add mode)

            if (mode == "add" && identical(click, "z")) {
               show <- !show
               .texttop(.text("showmoves", show))
               Sys.sleep(1)
               .texttop(texttop)
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
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

            # e to edit a sequence

            if (identical(click, "e")) {
               sub$moves <- edit(sub$moves)
               sub$moves$comment[is.na(sub$moves$comment)] <- ""
               if (mode == "play")
                  saveRDS(sub, file=file.path(seqdir, seqname))
               next
            }

            # l to list all (selected) sequences

            if (identical(click, "l")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               if (k > 0L) {
                  tab <- data.frame(files, played.selected, scores.selected, formatC(probvals.selected, format="f", digits=1))
                  names(tab) <- c("Name", .text("played"), .text("score"), .text("prob"))
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  rownames(tab) <- which(files.all %in% selected)
                  print(tab, print.gap=2)
               } else {
                  cat(.text("zeroseqsfound"))
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

            # h to get a hint (only in play mode)

            if (mode == "play" && identical(click, "h")) {
               hintval <- hintval + 1
               if (hintval >= 1)
                  .addrect(sub$moves$x1[i], sub$moves$y1[i], col=.get("col.hint"), lwd=lwd)
               if (hintval >= 2)
                  .addrect(sub$moves$x2[i], sub$moves$y2[i], col=.get("col.hint"), lwd=lwd)
               if (hintval <= 2)
                  score <- min(100, score + 25)
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
               next
            }

            # -/+ to decrease/increase the time between moves

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
                  volume <- max(0, volume - 0.25)
               } else {
                  volume <- min(1, volume + 0.25)
               }
               oldvolume <- volume
               .texttop(.text("volume", formatC(volume, format="f", digits=2)))
               Sys.sleep(1)
               .texttop(texttop)
               settings$volume <- volume
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # / (or .) to enter a search term or sequence range

            if (identical(click, "/") || identical(click, ".")) {

               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))

               searchterm <- readline(prompt=.text("seqsearch"))

               if (identical(searchterm , "")) {
                  cat(.text("allseqselected"))
                  selected <- list.files(seqdir, pattern=".rds$")
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
                  sign <- strsplit(searchterm, " ")[[1]][2]
                  cutoff <- as.numeric(strsplit(searchterm, sign)[[1]][2])
                  cat(.text("selseqplayed", list(tmp$sign, cutoff)))
                  selected <- eval(parse(text = paste("played.all", tmp$sign, tmp$cutoff)))
                  selected <- list.files(seqdir, pattern=".rds$")[selected]
                  if (length(selected) == 0L) {
                     cat(.text("noseqsfound"))
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

            # o to edit the score for the current sequence (only in play mode)

            if (mode == "play" && identical(click, "o")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               newscore <- readline(prompt=.text("newscore", score))
               if (grepl("^[0-9]+$", newscore)) {
                  newscore <- round(as.numeric(newscore))
                  newscore[newscore < 0] <- 0
                  cat(.text("setnewscore", newscore))
                  sub$score[player] <- newscore
                  saveRDS(sub, file=file.path(seqdir, seqname))
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # t to take back a score adjustment due to a wrong move (in play mode) or take back a move (in add mode)

            if (identical(click, "t")) {
               if (mode == "play") {
                  if (scoreadd > 0) {
                     score <- score - scoreadd
                     .texttop(.text("setscoreback", score))
                     Sys.sleep(1)
                     .texttop(texttop)
                     .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
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
                        sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     }
                     comment <- ""
                     .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
                     .drawsideindicator(sidetoplay, flip)
                  }
               }
               next
            }

            # a to copy the current sequence to add new moves as a new sequence (only in play mode)

            if (mode == "play" && identical(click, "a")) {

               mode <- "add"

               sub$score <- setNames(100, player)
               sub$played <- setNames(0, player)

               if (!identical(sub$moves$comment[i], "")) {
                  texttop <- .texttop(sub$moves$comment[i])
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

               for (i in 1:nrow(sub$moves)) {
                  pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
                  texttop <- .texttop(sub$moves$comment[i])
                  .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
                  .draweval(sub$moves$eval[i], flip=flip, eval=eval)
                  Sys.sleep(sleep)
               }

               i <- i + 1
               sidetoplay <- ifelse(flip, "w", "b")
               show <- FALSE
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
               .drawsideindicator(sidetoplay, flip)
               next

            }

            # 0 to make current position the starting position (only in add mode)

            if (mode == "add" && identical(click, "0")) {
               .texttop(.text("setposstart"))
               Sys.sleep(1)
               .texttop(texttop)
               i <- 1
               comment <- ""
               sub$moves <- sub$moves[numeric(0),]
               sub$pos <- pos
               attr(sub$pos, "move") <- NULL
               attr(sub$pos, "ispp") <- NULL
               attr(sub$pos, "y1") <- NULL
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               sidetoplay <- ifelse(flip, "b", "w")
               .drawsideindicator(sidetoplay, flip)
               .draweval(clear=TRUE)
               next
            }

            # right arrow to play the next move (only in play mode)

            if (mode == "play" && identical(click, "Right")) {
               if (i > nrow(sub$moves)) {
                  .texttop(.text("waslastmove"))
                  next
               }
               pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
               i <- i + 1
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
               texttop <- .texttop(sub$moves$comment[i])
               hintval <- 0
               next
            }

            # u (and Escape) to update board

            if (identical(click, "u") || identical(click, "\033") || identical(click, "ctrl-[")) {
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               next
            }

            # u to edit the exponent value

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
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
               Sys.sleep(1)
               .texttop(texttop)
               settings$lang <- lang
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # r to toggle between random/sequential mode

            if (identical(click, "r")) {
               random <- !random
               .texttop(.text("random", random))
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
               Sys.sleep(1)
               .texttop(texttop)
               settings$random <- random
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # (/) to decrease/increase the line width value

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

            # v to toggle evaluation bar on.off

            if (identical(click, "v")) {
               eval <- !eval
               .texttop(.text("eval", eval))
               if (eval) {
                  .draweval(sub$moves$eval[i], flip=flip, eval=eval)
               } else {
                  .draweval(clear=TRUE)
               }
               Sys.sleep(1)
               .texttop(texttop)
               settings$eval <- eval
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F1 to show the help

            if (identical(click, "F1")) {
               .printhelp(lwd=lwd)
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               next
            }

            # F2 to show the leaderboard and player statistics

            if (identical(click, "F2")) {
               .leaderboard(seqdir, files, lwd)
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               next
            }

            # F3 to print settings

            if (identical(click, "F3")) {
               settings <- data.frame(player, mode, sleep, volume, lwd, cex.top, cex.bot, cex.eval, expval, pause, random, eval, lang)
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               tab <- t(settings)
               colnames(tab) <- ""
               print(tab, quote=FALSE, print.gap=3)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # F4 to adjust colors

            if (identical(click, "F4")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               .colorpick(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, lwd, sidetoplay, random)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               cols <- c("col.bg"=.get("col.bg"), "col.fg"=.get("col.fg"),
                         "col.square.l"=.get("col.square.l"), "col.square.d"=.get("col.square.d"), "col.square.be"=.get("col.square.be"),
                         "col.top"=.get("col.top"), "col.bot"=.get("col.bot"), "col.help"=.get("col.help"), "col.help.border"=.get("col.help.border"),
                         "col.hint"=.get("col.hint"), "col.wrong"=.get("col.wrong"), "col.rect"=.get("col.rect"), "col.annot"=.get("col.annot"),
                         "col.side.w"=.get("col.side.w"), "col.side.b"=.get("col.side.b"))
               saveRDS(cols, file=file.path(configdir, "colors.rds"))
               next
            }

            # F5 to adjust cex values

            if (identical(click, "F5")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               .cexpick(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, lwd, sidetoplay, random)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
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
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               sub$moves <- sub$moves[numeric(0),]
               sub$pos <- pos
               sub$flip <- flip
               attr(sub$pos, "move") <- NULL
               attr(sub$pos, "ispp") <- NULL
               attr(sub$pos, "y1") <- NULL
               sidetoplay <- ifelse(flip, "b", "w")
               .redrawall(pos, flip, mode, show, player, seqname, score, played, i, totalmoves, texttop, sidetoplay, random)
               .draweval(sub$moves$eval[i], flip=flip, eval=eval)
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
                     .drawboard(pos, flip)
                     hasarrows <- FALSE
                     circles <- matrix(c(0,0), nrow=1, ncol=2)
                     if (mode == "add")
                        .drawsideindicator(sidetoplay, flip)
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
            .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
            .draweval(sub$moves$eval[i], flip=flip, eval=eval)

         } else {

            # if in play mode and the move was incorrect, adjust the score, and show that it was the wrong move

            if (score >= 1) {
               scoreadd <- min(50, 100-score)
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

            # adjust the score

            if (score >= 1)
               score <- ceiling(score * 0.80)

            played <- played + 1

            .printinfo(mode, show, player, seqname, score, played, i+1, totalmoves, random)

            if (pause) {

               click <- getGraphicsEvent(prompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))

               if (identical(click, "n"))
                  break

               if (identical(click, "a")) {
                  mode <- "add"
                  sub$score <- setNames(100, player)
                  sub$played <- setNames(0, player)
                  show <- FALSE
                  .texttop(" ")
                  .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
                  sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                  .drawsideindicator(sidetoplay, flip)
                  next
               }

            }

            sub$score[player] <- score
            sub$played[player] <- played
            saveRDS(sub, file=file.path(seqdir, seqname))
            Sys.sleep(2*sleep)
            run.rnd <- FALSE
            next

            # plot(0:25, 100 * 0.80^(0:25), type="o", pch=19, xlab="Played Correctly", ylab="Score", bty="l", ylim=c(0,100))

         }

         i <- i + 1
         sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

         fen <- .genfen(pos, flip, sidetoplay, i)
         #print(pos)
         #cat(fen, "\n")

         if (mode == "add") {

            # in add move, add the current move to sub

            sub$moves <- rbind(sub$moves, data.frame(x1=click1.x, y1=click1.y, x2=click2.x, y2=click2.y, show=show, move=attr(pos,"move"), eval=NA_real_, comment=comment))
            comment <- ""

         } else {

            # in play mode, let the trainer play the next move and increase i

            texttop <- .texttop(sub$moves$comment[i])
            .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
            Sys.sleep(sleep)
            .draweval(sub$moves$eval[i], flip=flip, eval=eval)
            pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
            texttop <- .texttop(sub$moves$comment[i])
            i <- i + 1
            sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

         }

         .printinfo(mode, show, player, seqname, score, played, i, totalmoves, random)
         hintval <- 0

         # go to [b]

      }

   }

   invisible()

}
