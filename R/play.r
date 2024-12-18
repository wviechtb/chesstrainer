play <- function(player="", mode="add", sleep=0.5, volume=0.5, lwd=2, expval=2, pause=TRUE, lang="en", random=TRUE, ...) {

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
   expval[expval < 0] <- 0

   ddd <- list(...)

   verbose <- isTRUE(ddd$verbose)

   # create config directory and read/save settings

   configdir <- tools::R_user_dir(package="chesstrainer", which="config")

   if (!dir.exists(configdir)) {
      cat(.text("createconfigdir", configdir))
      success <- dir.create(configdir, recursive=TRUE)
      if (!success)
         stop(.text("dircreateerror"), call.=FALSE)
      settings <- data.frame(player, mode, sleep, volume, lwd, expval, pause, lang, random)
      saveRDS(settings, file=file.path(configdir, "settings.rds"))
   } else {
      if (file.exists(file.path(configdir, "settings.rds"))) {
         # apply settings, but only for those that are equal to their defaults
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
         if (is.null(mc$expval))
            expval <- settings$expval
         if (is.null(mc$pause))
            pause <- settings$pause
         if (is.null(mc$lang))
            lang <- settings$lang
         if (is.null(mc$random))
            random <- settings$random
         settings <- data.frame(player, mode, sleep, volume, lwd, expval, pause, lang, random)
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
         assign("lang", lang, envir=.chesstrainer)
      } else {
         settings <- data.frame(player, mode, sleep, volume, lwd, expval, pause, lang, random)
         saveRDS(settings, file=file.path(configdir, "settings.rds"))
      }
   }

   # create / check sequence directory

   if (is.null(ddd$seqdir)) {
      seqdir <- tools::R_user_dir(package="chesstrainer", which="data")
      if (!dir.exists(seqdir)) {
         cat(.text("createseqdir", seqdir))
         success <- dir.create(seqdir, recursive=TRUE)
         if (!success)
            stop(.text("dircreateerror"), call.=FALSE)
         copyseqs <- readline(prompt=.text("copyseqs"))
         if (.confirm(copyseqs))
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

   # create starting position matrix 'pos'

   pos <- matrix("", 8, 8)
   pos[2,] <- "WP"
   pos[1,1] <- pos[1,8] <- "WR"
   pos[1,2] <- pos[1,7] <- "WN"
   pos[1,3] <- pos[1,6] <- "WB"
   pos[1,4] <- "WQ"
   pos[1,5] <- "WK"
   pos[7,] <- "BP"
   pos[8,1] <- pos[8,8] <- "BR"
   pos[8,2] <- pos[8,7] <- "BN"
   pos[8,3] <- pos[8,6] <- "BB"
   pos[8,4] <- "BQ"
   pos[8,5] <- "BK"
   colnames(pos) <- LETTERS[1:8]
   rownames(pos) <- 1:8

   start.pos <- pos

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

         newdat <- list(flip=flip, score=setNames(100, player), played=setNames(0, player),
                        moves=data.frame(x1=numeric(), y1=numeric(), x2=numeric(), y2=numeric(), show=logical(), move=character(), comment=character()))

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
      .printinfo(mode, show, player, seqname, score, played, i, totalmoves)

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
               i <- i + 1
               texttop <- .texttop(sub$moves$comment[i])
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
               Sys.sleep(sleep)
            }

            show <- FALSE
            .printinfo(mode, show, player, seqname, score, played, i, totalmoves)

         } else {

            .drawsideindicator(i, flip)

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
               move <- c(click1.x, click1.y, click2.x, click2.y, buttons)
               empty.square <- FALSE
               if (identical(buttons, 0L)) {
                  .rmrect(click1.x, click1.y, lwd=lwd)
                  .rmrect(click2.x, click2.y, lwd=lwd)
               }
               click1.x <<- NULL
               click1.y <<- NULL
               click2.x <<- NULL
               click2.y <<- NULL
               button <- 0L
               return(move)
            }

            click <- getGraphicsEvent(prompt="", onMouseDown=mousedown, onMouseMove=dragmousemove, onMouseUp=mouseup, onKeybd=function(key) return(key))

            keys      <- c("q", " ", "n", "p", "e", "l", "-", "=", "+", "F1", "F2", "F3", "m", "/", ".", "w", "ctrl-R", "u", "^", "[", "]", "i", "r", "(", ")", "ctrl-[", "\033", "F12")
            keys.add  <- c("f", "z", "c", "s", "b", "0") #, "???")
            keys.play <- c("z", "c", "s", "\b", "ctrl-D", "h", "a", "Right", "o", "t")

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
               .quit()
               dev.off()
               next
            }

            # <space> to select the mode (play versus add) (starts a new round)

            if (identical(click, " ")) {
               if (mode == "add") {
                  mode <- "play"
               } else {
                  mode <- "add"
               }
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
               newdat$flip <- flip
               .drawboard(pos, flip=flip)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
               .drawsideindicator(i, flip)
               next
            }

            # w to switch wait on/off

            if (identical(click, "w")) {
               pause <- !pause
               if (pause) {
                  .texttop(.text("pauseon"))
                  Sys.sleep(1)
                  .texttop(texttop)
               } else {
                  .texttop(.text("pauseoff"))
                  Sys.sleep(1)
                  .texttop(texttop)
               }
               settings$pause <- pause
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # z to switch show moves on/off (only in add mode)

            if (mode == "add" && identical(click, "z")) {
               show <- !show
               if (show) {
                  .texttop(.text("showmoveson"))
                  Sys.sleep(1)
                  .texttop(texttop)
               } else {
                  .texttop(.text("showmovesoff"))
                  Sys.sleep(1)
                  .texttop(texttop)
               }
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
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
               seqident <- sapply(dat.all, function(x) identical(newdat$moves[1:5], x$moves[1:5]))
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
                  saveRDS(newdat, file=filename)
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  run.rnd <- FALSE
                  wait <- FALSE
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # e to edit a sequence

            if (identical(click, "e")) {
               if (mode == "add") {
                  newdat$moves <- edit(newdat$moves)
                  newdat$moves$comment[is.na(newdat$moves$comment)] <- ""
               } else {
                 sub$moves <- edit(sub$moves)
                 sub$moves$comment[is.na(sub$moves$comment)] <- ""
                 saveRDS(sub, file=file.path(seqdir, seqname))
               }
               next
            }

            # l to list all (selected) sequences

            if (identical(click, "l")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               if (k > 0L) {
                  tmp <- data.frame(files, played.selected, scores.selected, formatC(probvals.selected, format="f", digits=1))
                  names(tmp) <- c("Name", .text("played"), .text("score"), .text("prob"))
                  tmp$Name <- format(tmp$Name, justify="left")
                  names(tmp)[1] <- ""
                  rownames(tmp) <- which(files.all %in% selected)
                  print(tmp, print.gap=2)
               } else {
                  cat(.text("zeroseqsfound"))
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # F2 to get leaderboard and player statistics

            if (identical(click, "F2")) {
               tmp <- lapply(file.path(seqdir, files.all), readRDS)
               tmp.scores <- lapply(tmp, function(x) x$score)
               players <- unique(unlist(lapply(tmp.scores, function(x) names(x))))
               nplayers <- length(players)
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               if (nplayers >= 1) {
                  tmp.scores <- lapply(players, function(player) {
                     x <- lapply(tmp, function(x) x$score[player])
                     x[sapply(x, is.null)] <- 100
                     unlist(x)
                  })
                  tmp.scores <- do.call(cbind, tmp.scores)
                  tmp.scores[is.na(tmp.scores)] <- 100
                  rownames(tmp.scores) <- files
                  tmp.scores[tmp.scores == 0] <- NA_real_
                  mean.scores <- round(apply(tmp.scores, 2, mean, na.rm=TRUE))
                  sd.scores   <- round(apply(tmp.scores, 2, sd, na.rm=TRUE))
                  min.scores  <- apply(tmp.scores, 2, min, na.rm=TRUE)
                  max.scores  <- apply(tmp.scores, 2, max, na.rm=TRUE)
                  tmp.played <- lapply(players, function(player) {
                     x <- lapply(tmp, function(x) x$played[player])
                     x[sapply(x, is.null)] <- 0
                     unlist(x)
                  })
                  tmp.played <- do.call(cbind, tmp.played)
                  tmp.played[is.na(tmp.played)] <- 0
                  total.played <- round(apply(tmp.played, 2, sum))
                  tmp <- data.frame(players, mean.scores, sd.scores, min.scores, max.scores, total.played)
                  names(tmp) <- c(.text("player"), .text("score"), "SD", "Min", "Max", .text("played"))
                  tmp <- tmp[order(tmp[[2]]),]
                  rownames(tmp) <- NULL
                  print(tmp)
               } else {
                  cat(.text("noleader"))
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # ctrl-d to delete the current sequence (only in play mode)

            if (mode == "play" && identical(click, "ctrl-D")) {
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               answer <- readline(prompt=.text("rlydelseq"))
               if (identical(answer, "j")) {
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
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
               next
            }

            # -+ to decrease/increase the time between moves

            if (identical(click, "-")) {
               sleep <- max(0, sleep - 0.25)
               .texttop(.text("waittime", formatC(sleep, format="f", digits=2)))
               Sys.sleep(1)
               .texttop(texttop)
               settings$sleep <- sleep
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            if (identical(click, "=") || identical(click, "+")) {
               sleep <- min(2, sleep + 0.25)
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
                  .texttop(.text("soundon"))
                  Sys.sleep(1)
                  .texttop(texttop)
                  settings$volume <- volume
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
               } else {
                  oldvolume <- volume
                  volume <- 0
                  .texttop(.text("soundoff"))
                  Sys.sleep(1)
                  .texttop(texttop)
                  settings$volume <- volume
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
               }
               next
            }

            # [] to decrease/increase the volume

            if (identical(click, "[")) {
               volume <- max(0, volume - 0.25)
               oldvolume <- volume
               .texttop(.text("volume", formatC(volume, format="f", digits=2)))
               Sys.sleep(1)
               .texttop(texttop)
               settings$volume <- volume
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }
            if (identical(click, "]")) {
               volume <- min(1, volume + 0.25)
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
                  run.rnd <- FALSE
                  wait <- FALSE
               }
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # t to take back a score adjustment due to a wrong move (only in play mode)

            if (mode == "play" && identical(click, "t")) {
               if (scoreadd > 0) {
                  score <- score - scoreadd
                  .texttop(.text("setscoreback", score))
                  Sys.sleep(1)
                  .texttop(texttop)
                  .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
                  scoreadd <- 0
               }
               next
            }

            # a to copy the current sequence to add new moves as a new sequence (only in play mode)

            if (mode == "play" && identical(click, "a")) {

               mode <- "add"

               newdat <- sub
               newdat$score <- setNames(100, player)
               newdat$played <- setNames(0, player)

               if (!identical(newdat$moves$comment[i], "")) {
                  texttop <- .texttop(newdat$moves$comment[i])
                  Sys.sleep(sleep)
               }

               if (is.null(newdat$pos)) {
                  pos <- start.pos
               } else {
                  pos <- newdat$pos
               }

               .drawboard(pos, flip=flip)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)

               for (i in 1:nrow(newdat$moves)) {
                  pos <- .updateboard(pos, move=newdat$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
                  texttop <- .texttop(newdat$moves$comment[i])
                  .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
                  Sys.sleep(sleep)
               }

               i <- i + 1
               show <- FALSE
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
               .drawsideindicator(i, flip)
               next

            }

            # b key to take back a move (only in add mode)

            if (mode == "add" && identical(click, "b")) {
               if (i > 1) {
                  if (is.null(newdat$pos)) {
                     pos <- start.pos
                  } else {
                     pos <- newdat$pos
                  }
                  .drawboard(pos, flip=flip)
                  hasarrows <- FALSE
                  circles <- matrix(c(0,0), nrow=1, ncol=2)
                  i <- i - 1
                  newdat$moves <- newdat$moves[seq_len(i-1),,drop=FALSE]
                  for (j in seq_len(i-1)) {
                     pos <- .updateboard(pos, move=newdat$moves[j,1:4], flip=flip, volume=0, verbose=verbose)
                  }
                  comment <- ""
                  .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
                  .drawsideindicator(i, flip)
               }
               next
            }

            # 0 to make current position the starting position (only in add mode)

            if (mode == "add" && identical(click, "0")) {
               .texttop(.text("setposstart"))
               Sys.sleep(1)
               .texttop(texttop)
               i <- 1
               comment <- ""
               newdat$moves <- newdat$moves[numeric(0),]
               newdat$pos <- pos
               attr(newdat$pos, "move") <- NULL
               attr(newdat$pos, "ispp") <- NULL
               attr(newdat$pos, "y1") <- NULL
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
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
               texttop <- .texttop(sub$moves$comment[i])
               hintval <- 0
               next
            }

            # u (and Escape) to update board

            if (identical(click, "u") || identical(click, "\033") || identical(click, "ctrl-[")) {
               .drawboard(pos, flip=flip)
               hasarrows <- FALSE
               circles <- matrix(c(0,0), nrow=1, ncol=2)
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
               .texttop(texttop)
               if (mode == "add")
                  .drawsideindicator(i, flip)
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
               .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
               Sys.sleep(1)
               .texttop(texttop)
               settings$lang <- lang
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # r to toggle between random/sequential mode

            if (identical(click, "r")) {
               if (random) {
                  random <- FALSE
                  .texttop(.text("randomoff"))
                  Sys.sleep(1)
                  .texttop(texttop)
               } else {
                  random <- TRUE
                  .texttop(.text("randomon"))
                  Sys.sleep(1)
                  .texttop(texttop)
               }
               settings$random <- random
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # (/) to decrease/increase the line width value

            if (identical(click, "(")) {
               lwd <- max(1, lwd - 1)
               .texttop(.text("lwdadj", lwd))
               Sys.sleep(0.5)
               .texttop(texttop)
               settings$lwd <- lwd
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            if (identical(click, ")")) {
               lwd <- lwd + 1
               .texttop(.text("lwdadj", lwd))
               Sys.sleep(0.5)
               .texttop(texttop)
               settings$lwd <- lwd
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            #if (mode == "add" && identical(click, "b")) {
            #   .texttop("Board Editor")
            #   i <- 1
            #   comment <- ""
            #   newdat$moves <- newdat$moves[numeric(0),]
            #   #click <- getGraphicsEvent(prompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))
            #   next
            #}

            # F1 to print the help

            if (identical(click, "F1")) {
               .printhelp(lwd=lwd)
               next
            }

            # F3 to print settings

            if (identical(click, "F3")) {
               settings <- data.frame(player, mode, sleep, volume, lwd, expval, pause, lang, random)
               if (!is.null(ddd[["switch1"]])) eval(expr = parse(text = ddd[["switch1"]]))
               tab <- t(settings)
               colnames(tab) <- ""
               print(tab, quote=FALSE, print.gap=3)
               if (!is.null(ddd[["switch2"]])) eval(expr = parse(text = ddd[["switch2"]]))
               next
            }

            # F12 to toggle verbose on/off

            if (identical(click, "F12")) {
               verbose <- !verbose
               next
            }

            ##################################################################

            # if click is an actual click (and drag) on the board

            click1.x <- click[1]
            click1.y <- click[2]
            click2.x <- click[3]
            click2.y <- click[4]
            button   <- click[5]

            if (verbose) {
               cat("Click 1: ", click1.x, ", ", click1.y, sep="")
               cat("\n")
               cat("Click 2: ", click2.x, ", ", click2.y, sep="")
               cat("\n")
            }

            # when clicking too fast, click may not be registered

            if (is.na(click1.x) || is.na(click2.x) || is.na(click1.y) || is.na(click2.y))
               next

            # when start and end positions are the same

            if (click1.x == click2.x && click1.y == click2.y) {

               if (identical(button, 2)) {

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
                     .drawboard(pos, flip=flip)
                     hasarrows <- FALSE
                     circles <- matrix(c(0,0), nrow=1, ncol=2)
                     if (mode == "add")
                        .drawsideindicator(i, flip)
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

         if (identical(button, 2)) {
            shape::Arrows(click1.y+0.5, click1.x+0.5, click2.y+0.5, click2.x+0.5, lwd=lwd*4, col=.get("col.annot"), arr.type="triangle", arr.length=lwd*0.1, arr.width=lwd*0.1, ljoin=1)
            hasarrows <- TRUE
            next
         }

         # if button 0 was used for the move and there are arrows/circles, redraw the board before making the move

         if (identical(button, 0) && (hasarrows || nrow(circles) >= 2L)) {
            .drawboard(pos, flip=flip)
            hasarrows <- FALSE
            circles <- matrix(c(0,0), nrow=1, ncol=2)
            if (mode == "add")
               .drawsideindicator(i, flip)
         }

         if (mode == "add" || all(c(click1.x==sub$moves$x1[i], click1.y==sub$moves$y1[i], click2.x==sub$moves$x2[i], click2.y==sub$moves$y2[i]))) {

            # if in add mode or if the move is correct, make the move

            if (mode == "add") {
               pos <- .updateboard(pos, move=c(click1.x, click1.y, click2.x, click2.y), flip=flip, volume=volume, verbose=verbose)
            } else {
               pos <- .updateboard(pos, move=c(click1.x, click1.y, click2.x, click2.y), flip=flip, volume=volume, verbose=verbose)
            }

            .printinfo(mode, show, player, seqname, score, played, i, totalmoves)

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

            if (pause) {

               click <- getGraphicsEvent(prompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=function(key) return(key))

               if (identical(click, "n"))
                  next

               if (identical(click, "a")) {
                  mode <- "add"
                  newdat <- sub
                  newdat$score <- setNames(100, player)
                  newdat$played <- setNames(0, player)
                  show <- FALSE
                  .texttop(" ")
                  .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
                  next
               }

            }

            # adjust the score

            if (score >= 1)
               sub$score[player] <- ceiling(score * 0.80)
            sub$played[player] <- played + 1
            saveRDS(sub, file=file.path(seqdir, seqname))
            Sys.sleep(2*sleep)
            run.rnd <- FALSE
            next

            # plot(0:25, 100 * 0.80^(0:25), type="o", pch=19, xlab="Played Correctly", ylab="Score", bty="l", ylim=c(0,100))

         }

         i <- i + 1

         if (mode == "add") {

            # in add move, add the current move to newdat

            newdat$moves <- rbind(newdat$moves, data.frame(x1=click1.x, y1=click1.y, x2=click2.x, y2=click2.y, show=show, move=attr(pos,"move"), comment=comment))
            comment <- ""

         } else {

            # in play mode, let the trainer play the next move and increase i

            texttop <- .texttop(sub$moves$comment[i])
            .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
            Sys.sleep(sleep)
            pos <- .updateboard(pos, move=sub$moves[i,1:4], flip=flip, volume=volume, verbose=verbose)
            texttop <- .texttop(sub$moves$comment[i])
            i <- i + 1

         }

         .printinfo(mode, show, player, seqname, score, played, i, totalmoves)
         hintval <- 0

         # go to [b]

      }

   }

   invisible()

}
