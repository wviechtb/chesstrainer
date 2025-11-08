play <- function(lang="en", sfpath="", ...) {

   if (!interactive())
      return(.text("interactive"))

   if (!is.element(lang, c("en","de")))
      stop("Argument 'lang' must be either 'en' or 'de'.", call.=FALSE)

   # set warn=1 for easier debugging

   owarn <- options()$warn
   options(warn=1)
   on.exit(options(warn=owarn))

   assign("lang", lang, envir=.chesstrainer)

   # get arguments passed via ...

   ddd <- list(...)

   if (is.null(ddd[["seqdir"]])) { # seqdir can be a vector, so not using ifelse()
      seqdir <- ""
   } else {
      seqdir <- ddd[["seqdir"]]
   }

   if (is.null(ddd[["eval"]])) { # eval can be a vector, so not using ifelse()
      eval <- TRUE
   } else {
      eval <- ddd[["eval"]]
   }

   if (is.null(ddd[["mar"]])) { # mar can be a vector, so not using ifelse()
      mar <- c(5.5, 5.5, 5.5, 5.5)
   } else {
      mar <- ddd[["mar"]]
   }

   if (is.null(ddd[["mar2"]])) { # mar2 can be a vector, so not using ifelse()
      mar2 <- c(11, 11, 9, 9)
   } else {
      mar2 <- ddd[["mar2"]]
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
   evalsteps   <- ifelse(is.null(ddd[["evalsteps"]]),   5,              ddd[["evalsteps"]])
   wait        <- ifelse(is.null(ddd[["wait"]]),        TRUE,           ddd[["wait"]])
   sleep       <- ifelse(is.null(ddd[["sleep"]]),       0.5,            ddd[["sleep"]])
   idletime    <- ifelse(is.null(ddd[["idletime"]]),    120,            ddd[["idletime"]])
   lwd         <- ifelse(is.null(ddd[["lwd"]]),         2,              ddd[["lwd"]])
   volume      <- ifelse(is.null(ddd[["volume"]]),      50,             ddd[["volume"]])
   showgraph   <- ifelse(is.null(ddd[["showgraph"]]),   FALSE,          ddd[["showgraph"]])
   repmistake  <- ifelse(is.null(ddd[["repmistake"]]),  FALSE,          ddd[["repmistake"]])
   cex.top     <- ifelse(is.null(ddd[["cex.top"]]),     1.4,            ddd[["cex.top"]])
   cex.bot     <- ifelse(is.null(ddd[["cex.bot"]]),     0.7,            ddd[["cex.bot"]])
   cex.eval    <- ifelse(is.null(ddd[["cex.eval"]]),    0.5,            ddd[["cex.eval"]])
   depth1      <- ifelse(is.null(ddd[["depth1"]]),      14,             ddd[["depth1"]])
   depth2      <- ifelse(is.null(ddd[["depth2"]]),      24,             ddd[["depth2"]])
   depth3      <- ifelse(is.null(ddd[["depth3"]]),      6,              ddd[["depth3"]])
   sflim       <- ifelse(is.null(ddd[["sflim"]]),       NA,             ddd[["sflim"]])
   multipv1    <- ifelse(is.null(ddd[["multipv1"]]),    1,              ddd[["multipv1"]])
   multipv2    <- ifelse(is.null(ddd[["multipv2"]]),    1,              ddd[["multipv2"]])
   threads     <- ifelse(is.null(ddd[["threads"]]),     1,              ddd[["threads"]])
   hash        <- ifelse(is.null(ddd[["hash"]]),        256,            ddd[["hash"]])
   hintdepth   <- ifelse(is.null(ddd[["hintdepth"]]),   10,             ddd[["hintdepth"]])
   difffun     <- ifelse(is.null(ddd[["difffun"]]),     1,              ddd[["difffun"]])
   difflen     <- ifelse(is.null(ddd[["difflen"]]),     15,             ddd[["difflen"]])
   diffmin     <- ifelse(is.null(ddd[["diffmin"]]),     5,              ddd[["diffmin"]])
   zenmode     <- ifelse(is.null(ddd[["zenmode"]]),     FALSE,          ddd[["zenmode"]])
   quitanim    <- ifelse(is.null(ddd[["quitanim"]]),    TRUE,           ddd[["quitanim"]])
   inhibit     <- ifelse(is.null(ddd[["inhibit"]]),     FALSE,          ddd[["inhibit"]])

   # get switch1/switch2 functions if they are specified via ...

   if (is.null(ddd[["switch1"]])) {
      switch1 <- parse(text="invisible()")
   } else {
      switch1 <- parse(text = ddd[["switch1"]])
   }

   if (is.null(ddd[["switch2"]])) {
      switch2 <- parse(text="invisible()")
   } else {
      switch2 <- parse(text = ddd[["switch2"]])
   }

   # ensure that some arguments are sensible

   if (!is.element(mode, c("add","test","play")))
      mode <- "add"

   seqdirpos <- round(seqdirpos)
   expval[expval < 0] <- 0
   multiplier[multiplier < 0] <- 0
   multiplier[multiplier > 1] <- 1
   adjustwrong[adjustwrong < 0] <- 0
   adjusthint[adjusthint < 0] <- 0
   if (length(eval) == 1L)
      eval <- c(add=eval, test=eval, play=eval, analysis=eval)
   evalsteps[evalsteps < 2] <- 2
   evalsteps <- round(evalsteps)
   sleep[sleep < 0] <- 0
   idletime[idletime < 1] <- 1
   lwd[lwd < 1] <- 1
   volume[volume < 0] <- 0
   volume[volume > 100] <- 100
   volume <- round(volume)
   cex.top[cex.top < 0.1] <- 0.1
   cex.bot[cex.bot < 0.1] <- 0.1
   cex.eval[cex.eval < 0.1] <- 0.1
   depth1[depth1 < 1] <- 1
   depth2[depth2 < 1] <- 1
   depth3[depth3 < 1] <- 1
   depth1 <- round(depth1)
   depth2 <- round(depth2)
   depth3 <- round(depth3)
   sflim <- max(0, round(sflim))
   if (!is.na(sflim) && (sflim > 20 && sflim < 1320))
      sflim <- NA
   sflim[sflim > 3190] <- 3190
   multipv1[multipv1 < 1] <- 1
   multipv2[multipv2 < 1] <- 1
   multipv1 <- round(multipv1)
   multipv2 <- round(multipv2)
   hash[hash < 16] <- 16
   hash <- round(hash)
   hintdepth[hintdepth < 2] <- 2
   hintdepth <- round(hintdepth)
   difflen[difflen < 2] <- 2
   difflen <- round(difflen)
   diffmin[diffmin < 2] <- 2
   diffmin <- round(diffmin)
   if (length(mar) == 1L)
      mar <- rep(mar,4)
   mar <- pmax(mar,1)
   if (length(mar2) == 1L)
      mar2 <- rep(mar2,4)
   mar2 <- pmax(mar2,1)

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
                       eval=eval, evalsteps=evalsteps, wait=wait, sleep=sleep, idletime=idletime, lwd=lwd, volume=volume, showgraph=showgraph, repmistake=repmistake,
                       cex.top=cex.top, cex.bot=cex.bot, cex.eval=cex.eval,
                       sfpath=sfpath, depth1=depth1, depth2=depth2, depth3=depth3, sflim=sflim, multipv1=multipv1, multipv2=multipv2, threads=threads, hash=hash, hintdepth=hintdepth,
                       difffun=difffun, difflen=difflen, diffmin=diffmin, zenmode=zenmode, mar=mar, mar2=mar2)
      saveRDS(settings, file=file.path(configdir, "settings.rds"))
      cols <- sapply(cols.all, function(x) .get(x))
      saveRDS(cols, file=file.path(configdir, "colors.rds"))
   } else {
      if (file.exists(file.path(configdir, "settings.rds"))) {
         # apply settings, but only for those that are not set via play()
         settings <- readRDS(file.path(configdir, "settings.rds"))
         mc <- as.list(match.call())
         if (is.null(mc[["lang"]]))
            lang <- settings[["lang"]]
         assign("lang", lang, envir=.chesstrainer)
         cat(.text("loadsettings"))
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
         if (is.null(mc[["idletime"]]))
            idletime <- settings[["idletime"]]
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
         if (is.null(mc[["sflim"]]))
            sflim <- settings[["sflim"]]
         if (is.null(mc[["multipv1"]]))
            multipv1 <- settings[["multipv1"]]
         if (is.null(mc[["multipv2"]]))
            multipv2 <- settings[["multipv2"]]
         if (is.null(mc[["threads"]]))
            threads <- settings[["threads"]]
         if (is.null(mc[["hash"]]))
            hash <- settings[["hash"]]
         if (is.null(mc[["hintdepth"]]))
            hintdepth <- settings[["hintdepth"]]
         if (is.null(mc[["difffun"]]))
            difffun <- settings[["difffun"]]
         if (is.null(mc[["difflen"]]))
            difflen <- settings[["difflen"]]
         if (is.null(mc[["diffmin"]]))
            diffmin <- settings[["diffmin"]]
         if (is.null(mc[["zenmode"]]))
            zenmode <- settings[["zenmode"]]
         if (is.null(mc[["mar"]]))
            mar <- settings[["mar"]]
         if (is.null(mc[["mar2"]]))
            mar2 <- settings[["mar2"]]
      }
      sfpath <- suppressWarnings(normalizePath(sfpath))
      settings <- list(lang=lang, player=player, mode=mode, seqdir=seqdir, seqdirpos=seqdirpos,
                       selmode=selmode, timed=timed, timepermove=timepermove, expval=expval, multiplier=multiplier, adjustwrong=adjustwrong, adjusthint=adjusthint,
                       eval=eval, evalsteps=evalsteps, wait=wait, sleep=sleep, idletime=idletime, lwd=lwd, volume=volume, showgraph=showgraph, repmistake=repmistake,
                       cex.top=cex.top, cex.bot=cex.bot, cex.eval=cex.eval,
                       sfpath=sfpath, depth1=depth1, depth2=depth2, depth3=depth3, sflim=sflim, multipv1=multipv1, multipv2=multipv2, threads=threads, hash=hash, hintdepth=hintdepth,
                       difffun=difffun, difflen=difflen, diffmin=diffmin, zenmode=zenmode, mar=mar, mar2=mar2)
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
   assign("upsidedown", FALSE, envir=.chesstrainer)

   # if seqdir is not an empty string, remove any directories from seqdir that do not exist

   if (!identical(seqdir, "")) {

      direxists <- dir.exists(seqdir)

      if (any(!direxists)) {
         seqdir <- seqdir[direxists]
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

      seqdir <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sequences")

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

   # create sessions directory if it doesn't already exist

   sessionsdir <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions")

   if (!dir.exists(sessionsdir)) {
      cat(.text("createsessionsdir", sessionsdir))
      dir.create(sessionsdir, recursive=TRUE)
   }

   # TODO: what to do with this?
   #if (file.access(seqdir, mode=4L) != 0L)
   #   stop(.text("noreadaccess"), call.=FALSE)
   #if (file.access(seqdir, mode=2L) != 0L)
   #   stop(.text("nowriteaccess"), call.=FALSE)

   cat(.text("useseqdir", seqdir[seqdirpos]))

   # load selmode from .selmode file in sequence directory (if it exist; otherwise create it)

   selmode <- .loadselmode(seqdir, seqdirpos, selmode)

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
   seqno    <- 1
   filename <- ""
   lastseq  <- ""
   bestmove <- list("")
   useflip  <- TRUE
   replast  <- FALSE
   oldmode  <- ifelse(mode == "play", "add", mode)
   .difffun <- eval(parse(text=paste0(".difffun", difffun)))

   # session variables

   session.seqsplayed <- 0
   session.mean.scores <- list(NULL)
   session.length <- 1
   session.date.start <- Sys.time()
   session.time.start <- proc.time()[[3]]

   # save par() if a device is already open

   if (dev.cur() != 1L) {
      opar <- par(no.readonly=TRUE)
      on.exit(par(opar), add=TRUE)
   }

   # create the getGraphicsEvent() functions

   mousedown <- function(buttons, x, y) {
      click.num <<- ifelse(click.num == 0, 1, click.num)
      squares <- .calcsquare(x, y, plt)
      pos.x <- squares[1]
      pos.y <- squares[2]
      square.sel <- ifelse(flip, pos[9-pos.x,9-pos.y], pos[pos.x,pos.y]) # get the value of the clicked square
      piece.color <- tolower(substr(square.sel, 1, 1)) # get the color of the piece on the clicked square (either w, b, or "" if the clicked square is empty)
      empty.square <- piece.color == ""
      button <<- buttons
      if (click.num == 1) {
         # if this is the first click
         click1.x <<- pos.x
         click1.y <<- pos.y
         click2.x <<- pos.x
         click2.y <<- pos.y
         if (identical(buttons, 0L)) { # if the click is made with the left mouse button
            if (givehint1) {
               .rmrect(hintx1, hinty1, lwd=lwd)
               givehint1 <<- FALSE
            }
            if (givehint2) {
               .rmrect(hintx2, hinty2, lwd=lwd)
               givehint2 <<- FALSE
            }
            if (!empty.square && piece.color == sidetoplay) { # then check that the square is not empty and has a piece of the correct color
               .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd) # if so, highlight the corresponding square (this is the starting square)
               return(NULL) # note: if the mouse button is released right away, then mouseup() increases click.num to 2
            } else {
               # otherwise exit start over
               remove.annotations <<- TRUE
               return(0)
            }
         }
      } else {
         # if we are here, then (click1.x, click1.y) is the selected square, is not empty, and has a piece of the correct color
         click2.x <<- pos.x
         click2.y <<- pos.y
         if (identical(buttons, 0L)) {
            # if the second click was with the left mouse button
            if (empty.square || piece.color != sidetoplay) { # if the second clicked square is empty or has a piece of the opposite color
               .rmrect(click1.x, click1.y, lwd=lwd) # remove the highlight from the starting square
               .rmrect(click2.x, click2.y, lwd=lwd) # remove the highlight from the target square
               return(1) # exit and then try to move the selected piece to this square
            }
            if (isTRUE(click1.x == click2.x) && isTRUE(click1.y == click2.y)) { # if the second clicked square is the same as (click1.x, click1.y)
               return(NULL)
            }
            if (piece.color == sidetoplay) { # if another piece of the same color is clicked
               .rmrect(click1.x, click1.y, lwd=lwd) # remove the highlight from the original square
               .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd) # highlight the new square
               click1.x <<- pos.x
               click1.y <<- pos.y
               new.piece <<- TRUE
               return(NULL)
            }
         } else {
            # if the second click was with the right mouse button (could either be the same as the starting square or a different one)
            .rmrect(click1.x, click1.y, lwd=lwd) # remove the highlight from the starting square
            .rmrect(click2.x, click2.y, lwd=lwd) # remove the highlight from the target square
            return(1) # exit and either draw/remove a circle or draw an arrow from the starting to the target square
         }
      }
   }

   mousemove <- function(buttons, x, y) {
      if (click.num == 0) # need this in case we start over but the mouse button is still pressed and dragged, so nothing happens
         return(NULL)
      squares <- .calcsquare(x, y, plt)
      pos.x <- squares[1]
      pos.y <- squares[2]
      new.square <- isTRUE(pos.x != click2.x) || isTRUE(pos.y != click2.y)
      if (new.square) {
         switched.square <<- TRUE
         if (identical(buttons, 0L) || click.num == 2) {
            if (isTRUE(pos.x == click1.x) && isTRUE(pos.y == click1.y)) {
               # if the new square is the starting square, remove the highlight from the previous square
               .rmrect(click2.x, click2.y, lwd=lwd)
            } else {
               # if the new square is not the starting square, then highlight the new square and remove the highlight from the previous square but only if the previous square was not the starting square
               .addrect(pos.x, pos.y, col=.get("col.rect"), lwd=lwd)
               if (isTRUE(click1.x != click2.x) || isTRUE(click1.y != click2.y))
                  .rmrect(click2.x, click2.y, lwd=lwd)
            }
         }
      }
      click2.x <<- pos.x
      click2.y <<- pos.y
      return(NULL)
   }

   mouseup <- function(buttons, x, y) {
      if (click.num == 0) # need this in case we start over but the mouse button is still pressed and then released, so nothing happens
         return(NULL)
      button <<- buttons
      if (click.num == 1) {
         if (isTRUE(click1.x != click2.x) || isTRUE(click1.y != click2.y)) {
            # if we are here, then a starting square was clicked, the button was held, the mouse was moved to another square, and the button was released
            if (identical(buttons, 0L)) { # if the dragging was done with the left mouse button
               .rmrect(click1.x, click1.y, lwd=lwd) # remove the highlight from the starting square
               .rmrect(click2.x, click2.y, lwd=lwd) # remove the highlight from the target square
            }
            return(1) # exit and try to make the corresponding move or draw the corresponding arrow; this also covers dragging a piece over another piece of the same color (an illegal move)
         } else {
            if (switched.square) {
               # if we are here, then a starting square was clicked, the button was held, the mouse moved to another square and then back to the starting square, and the button was released
               .rmrect(click1.x, click1.y, lwd=lwd) # remove the highlight from the starting square
               return(0) # exit and start over
            } else {
               # if we are here, then a starting square was clicked and the mouse button was released on the same square without moving the mouse in-between to a different square;
               if (identical(buttons, 0L)) {
                  click.num <<- 2
                  return(NULL)
               } else {
                  return(1) # if the square was clicked with the right mouse button, then exit and draw a circle on the square or remove it if there already is a circle
               }
            }
         }
      } else {
         if (isTRUE(click1.x == click2.x) && isTRUE(click1.y == click2.y)) {
            if (new.piece) {
               # here another piece of the same color was clicked
               new.piece <<- FALSE
               return(NULL)
            } else {
               # if we are here, then the second click was on the same square as the starting square with the left mouse button
               # (cannot have been with the right mouse button, since then we would have already exited via mousedown())
               .rmrect(click1.x, click1.y, lwd=lwd) # remove the highlight from the starting square
               return(0) # exit and start over
            }
         } else {
            # if we are here, then the second left mouse button *release* was on a different square than the starting square
            # (if the second *clicked* square is empty or has a piece of the opposite color, then we already exited via mousedown())
            square.sel <- ifelse(flip, pos[9-click2.x,9-click2.y], pos[click2.x,click2.y]) # get the value of the square on which the mouse button was released
            piece.color <- tolower(substr(square.sel, 1, 1)) # get the color of the piece on this square (either w, b, or "")
            empty.square <- piece.color == ""
            .rmrect(click1.x, click1.y, lwd=lwd)
            .rmrect(click2.x, click2.y, lwd=lwd)
            if (empty.square || piece.color != sidetoplay) {
               return(1) # exit and then try to move the selected piece to this square
            } else {
               # here the piece is dragged onto another piece of the same color
               return(0) # exit and start over
            }
         }
      }
   }

   # define keys

   keys <- c("q", "ctrl-Q", "\033", "ctrl-[", " ", "m", "d", "\\", "#", "n", "N", "p", "ctrl-R",
             "g", "H", "h", "Left", "Right", "Up", "Down", "t", "0", "1", "2", "3", "4", "5", "9",
             "r", "o", "u", "M", "B", "U", "j",
             "a", "A", "f", "z", "c", "e", "E", "s", "b",
             "^", "6", "R", "G", "w", "-", "=", "+", "[", "]", "{", "}", "(", ")", "i", "x", "v", "ctrl-V",
             "l", "<", ">", "ctrl-F", "ctrl-C", "ctrl-D", "/", ",", ".", "|", "*", "8", "?", "'", "\"",
             "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "ctrl-!", "ctrl-@", "ctrl-E")

   run.all <- TRUE

   while (run.all) {

      ###### [a] ######

      pos <- start.pos

      # some defaults for a particular round / sequence

      i          <- 1 # move counter
      seqname    <- ""
      seqnum     <- NA_integer_
      score      <- 100
      rounds     <- 0
      age        <- NA
      difficulty <- NA
      totalmoves <- 0
      show       <- TRUE
      comment    <- ""
      evalval    <- NA_real_
      starteval  <- 0.2
      texttop    <- ""
      if (useflip) {
         flip <- FALSE
         useflip <- TRUE
      }
      scoreadd      <- 0
      sidetoplay    <- "w"
      sideindicator <- NA
      givehint1     <- FALSE
      givehint2     <- FALSE
      mistake       <- FALSE
      timetotal     <- 0
      movesplayed   <- 0
      movestoplay   <- 1
      drawcircles   <- TRUE
      drawarrows    <- TRUE
      showstartcom  <- TRUE
      matetype      <- "none"
      savgame       <- NULL
      assign("checkpos", c(NA,NA), envir=.chesstrainer)

      circles <- matrix(nrow=0, ncol=2) # to store circles
      arrows  <- matrix(nrow=0, ncol=4) # to store arrows
      harrows <- matrix(nrow=0, ncol=4) # to store hint arrows
      evalvals <- c()

      # start new game for Stockfish

      .sf.newgame(sfproc, sfrun)

      # select a player if no player is currently selected

      if (player == "") {
         player <- .selectplayerconsole(player, seqdir[seqdirpos], mustselect=TRUE)
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
      rounds.all <- lapply(dat.all, function(x) tail(x$player[[player]]$round, 1))
      rounds.all[is.na(rounds.all) | .is.null(rounds.all)] <- 0
      rounds.all <- unlist(rounds.all)
      date.all <- lapply(dat.all, function(x) tail(x$player[[player]]$date, 1))
      date.all[is.na(date.all) | .is.null(date.all)] <- NA_real_
      date.all <- unlist(date.all)
      age.all <- as.numeric(Sys.time() - as.POSIXct(date.all), units="days")
      difficulty.all <- lapply(dat.all, function(x) .difffun(x$player[[player]]$score, difflen, diffmin, adjusthint, multiplier))
      difficulty.all[is.na(difficulty.all) | .is.null(difficulty.all)] <- NA_real_
      difficulty.all <- unlist(difficulty.all)

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

      # if all selected sequences have no moves, then select all sequences and start over

      if (all(sapply(dat, function(x) nrow(x$moves) == 0L))) {
         cat(.text("seqsnomoves"))
         cat(.text("allseqselected"))
         selected <- NULL
         next
      }

      scores.selected <- lapply(dat, function(x) tail(x$player[[player]]$score, 1))
      scores.selected[is.na(scores.selected) | .is.null(scores.selected)] <- 100
      scores.selected <- unlist(scores.selected)
      rounds.selected <- lapply(dat, function(x) tail(x$player[[player]]$round, 1))
      rounds.selected[is.na(rounds.selected) | .is.null(rounds.selected)] <- 0
      rounds.selected <- unlist(rounds.selected)
      date.selected <- lapply(dat, function(x) tail(x$player[[player]]$date, 1))
      date.selected[is.na(date.selected) | .is.null(date.selected)] <- NA_real_
      date.selected <- unlist(date.selected)
      age.selected <- as.numeric(Sys.time() - as.POSIXct(date.selected), units="days")
      difficulty.selected <- lapply(dat, function(x) .difffun(x$player[[player]]$score, difflen, diffmin, adjusthint, multiplier))
      difficulty.selected[is.na(difficulty.selected) | .is.null(difficulty.selected)] <- NA_real_
      difficulty.selected <- unlist(difficulty.selected)
      length.selected <- sapply(dat, function(x) sum(!x$moves$show))
      moves.selected <- sapply(dat, function(x) paste0(x$moves$move, collapse=", "))
      moves.selected <- gsub("[NBRKQ+=]", "", moves.selected)
      moves.selected <- gsub("x", "-", moves.selected)

      if (all(scores.selected == 0)) # in case all selected sequences have a score of 0
         scores.selected <- rep(1, k)

      if (selmode == "score_random") {
         probvals.selected <- scores.selected^expval
         probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
         if (any(is.infinite(probvals.selected))) {
            probvals.selected <- rep(0, k)
            probvals.selected[which(is.infinite(probvals.selected))[1]] <- 100
         } else {
            probvals.selected <- 100 * probvals.selected / sum(probvals.selected)
         }
      }

      if (selmode == "score_highest") {
         probvals.selected <- rep(0, k)
         probvals.selected[which(scores.selected == max(scores.selected[scores.selected != 0]))[1]] <- 100
      }

      if (selmode == "rounds_random") {
         probvals.selected <- (max(rounds.selected) + 1 - rounds.selected) / (max(rounds.selected) + 1)
         probvals.selected <- probvals.selected^expval
         probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
         if (any(is.infinite(probvals.selected))) {
            probvals.selected <- rep(0, k)
            probvals.selected[which(is.infinite(probvals.selected))[1]] <- 100
         } else {
            probvals.selected <- 100 * probvals.selected / sum(probvals.selected)
         }
      }

      if (selmode == "rounds_lowest") {
         probvals.selected <- rep(0, k)
         probvals.selected[which(rounds.selected == min(rounds.selected[scores.selected != 0]))[1]] <- 100
      }

      if (selmode == "age_random") {
         probvals.selected <- age.selected / max(age.selected, na.rm=TRUE)
         probvals.selected[is.na(probvals.selected)] <- 1 # if NA, set prob to 1
         probvals.selected <- probvals.selected^expval
         probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
         if (any(is.infinite(probvals.selected))) {
            probvals.selected <- rep(0, k)
            probvals.selected[which(is.infinite(probvals.selected))[1]] <- 100
         } else {
            probvals.selected <- 100 * probvals.selected / sum(probvals.selected)
         }
      }

      if (selmode == "age_oldest") {
         probvals.selected <- rep(0, k)
         if (any(is.na(age.selected))) {
            probvals.selected[which(is.na(age.selected))[1]] <- 100
         } else {
            probvals.selected[which(age.selected == max(age.selected[scores.selected != 0]))[1]] <- 100
         }
      }

      if (selmode == "diff_random") {
         probvals.selected <- difficulty.selected / max(difficulty.selected, na.rm=TRUE)
         probvals.selected[is.na(probvals.selected)] <- 0 # if NA, set prob to 0
         probvals.selected <- probvals.selected^expval
         probvals.selected[scores.selected == 0] <- 0 # in case of 0^0
         if (any(is.infinite(probvals.selected))) {
            probvals.selected <- rep(0, k)
            probvals.selected[which(is.infinite(probvals.selected))[1]] <- 100
         } else {
            probvals.selected <- 100 * probvals.selected / sum(probvals.selected)
         }
      }

      if (selmode == "diff_highest") {
         tmp <- difficulty.selected
         tmp[is.na(tmp)] <- 0
         probvals.selected <- rep(0, k)
         probvals.selected[which(tmp == max(tmp[scores.selected != 0]))[1]] <- 100
      }

      if (selmode %in% c("sequential","sequential_len","sequential_mov") && k >= 1L && !replast) {
         if (seqno > k)
            seqno <- 1
         while (scores.selected[seqno] == 0) {
            seqno <- seqno + 1
            if (seqno > k) {
               seqno <- 1
               if (k > 1L) {
                  playsound(system.file("sounds", "finished.ogg", package="chesstrainer"), volume=volume)
                  .texttop(.text("finishedround"), sleep=2)
               }
            }
         }
         probvals.selected <- rep(0, k)
         if (selmode == "sequential")
            probvals.selected[seqno] <- 100
         if (selmode == "sequential_len")
            probvals.selected[order(length.selected)[seqno]] <- 100
         if (selmode == "sequential_mov")
            probvals.selected[order(moves.selected)[seqno]] <- 100
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

            if (all(is.na(probvals.selected))) {
               sel <- sample(k, 1L)
            } else {
               sel <- sample(seq_len(k), 1L, prob=probvals.selected)
            }

         }

         if (is.null(sel) || sel == 0) {
            # just in case ('attempt to select less than one element in get1index'
            # error did occur before, although not sure how this could arise)
            #print(probvals.selected)
            sel <- 1
         }

         sub <- dat[[sel]]
         seqname <- files[sel]
         seqnum  <- which(seqname == files.all)

         score <- tail(sub$player[[player]]$score, 1)
         if (is.null(score) || is.na(score))
            score <- 100

         rounds <- tail(sub$player[[player]]$round, 1)
         if (is.null(rounds) || is.na(rounds))
            rounds <- 0

         age <- tail(sub$player[[player]]$date, 1)
         if (is.null(age))
            age <- NA
         age <- as.numeric(Sys.time() - as.POSIXct(age), units="days")

         difficulty <- .difffun(sub$player[[player]]$score, difflen, diffmin, adjusthint, multiplier)
         if (is.null(difficulty) || is.na(difficulty))
            difficulty <- NA

         totalmoves <- nrow(sub$moves)
         flip <- sub$flip

         if (!is.null(sub$pos)) {

            pos <- sub$pos

            # determine sidetoplay based on the first piece to be moved

            if (nrow(sub$moves) > 0L) {
               if (flip) {
                  piece <- pos[9-sub$moves[1,1], 9-sub$moves[1,2]]
               } else {
                  piece <- pos[sub$moves[1,1], sub$moves[1,2]]
               }
               sidetoplay <- ifelse(startsWith(piece, "W"), "w", "b")
            }

            # calculate starteval (in case this is missing) or get it

            if (is.null(attr(pos, "starteval"))) {
               fen <- .genfen(pos, flip, sidetoplay, i)
               res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
               evalval  <- res.sf$eval
               bestmove <- res.sf$bestmove
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               starteval <- evalval[1]
               attr(sub$pos, "starteval") <- starteval
            } else {
               starteval <- attr(sub$pos, "starteval")
            }

         }

         # compute number of moves to be made by the player
         movestoplay <- sum(!sub$moves$show)

      }

      sidetoplaystart <- sidetoplay

      # draw board and add info at the bottom

      .drawboard(pos, flip=flip, inhibit=inhibit, mar=mar)
      .drawcheck(pos, flip=flip)
      .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
      .draweval(starteval, NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)

      # check for getGraphicsEvent() capabilities of the current plotting device

      if (any(!is.element(c("MouseDown", "MouseMove", "MouseUp", "Keybd"), dev.capabilities()$events))) {
         dev.off()
         stop(.text("testdevice"), call.=FALSE)
      }

      run.rnd <- TRUE

      while (run.rnd) {

         ###### [b] ######

         if (mode == "test" && i <= nrow(sub$moves)) {

            # show the start comment if there is one at move 1 (and showstartcom is TRUE)
            if (i == 1 && !is.null(sub$commentstart) && showstartcom) {
               .startcomment(sub$commentstart, lwd=lwd) # waits for click
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(starteval, NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
            }

            if (nrow(sub$moves) == 0L) {
               # can have a sequence that is just a start comment
               .texttop("")
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
                  .waitforclick()
                  .rmannot(pos, circles, arrows, flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
               }
               pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
               sub$moves$move[i] <- attr(pos,"move")
               .draweval(sub$moves$eval[i], sub$moves$eval[i-1], i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               i <- i + 1
               sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
               if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                  texttop <- .texttop(sub$moves$comment[i-1])
               } else {
                  texttop <- .texttop(sub$moves$comment[i])
               }
               .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               circles <- .parseannot(sub$moves$circles[i], cols=2)
               arrows <- .parseannot(sub$moves$arrows[i], cols=4)
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                  .drawcircles(circles, lwd=lwd)
                  .drawarrows(arrows, lwd=lwd)
               }
               Sys.sleep(sleep)
            }

            show <- FALSE
            #.textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)

         }

         if (mode == "test" && timed) {
            .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
         } else {
            sideindicator <- .drawsideindicator(sidetoplay, flip=flip, clear=!identical(sideindicator, sidetoplay))
         }

         input <- TRUE

         while (input) {

            ###### [c] ######

            ##################################################################

            if (mode == "play") {

               # if Stockfish is not running anymore, then it must have crashed, so switch to analysis mode

               if (!sfrun) {
                  mode <- "analysis"
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
                  .draweval(clear=TRUE)
                  next
               }

               if (flip) {
                  domove <- sidetoplay == "w"
               } else {
                  domove <- sidetoplay == "b"
               }

               if (domove) {

                  # in play mode, if it is the computer to move, make the move now

                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)

                  # note: bestmove comes from [d]

                  if (!nzchar(bestmove[[1]][1])) {
                     .texttop(.text("nomove"))
                     mode <- "analysis"
                     .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                     .draweval(evalval[1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                     next
                  }

                  tmp <- .parsebestmove(bestmove[[1]][1], pos=pos, flip=flip, evalval=NA, i=i, sidetoplay=sidetoplay, rename=FALSE, returnline=FALSE, hintdepth=1)

                  sub$moves <- rbind(sub$moves, data.frame(x1=tmp$x1, y1=tmp$y1, x2=tmp$x2, y2=tmp$y2, show=TRUE, move=tmp$txt, eval=evalval[1], comment="", circles="", arrows="", fen=fen))
                  pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)

                  i <- i + 1
                  sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

                  .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)

                  evalvallast <- evalval[1]
                  fen <- .genfen(pos, flip, sidetoplay, i)
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                  evalval   <- res.sf$eval
                  bestmove  <- res.sf$bestmove
                  matetype  <- res.sf$matetype
                  sfproc    <- res.sf$sfproc
                  sfrun     <- res.sf$sfrun

                  sub$moves$eval[i-1] <- evalval[1]
                  sub$moves$fen[i-1] <- fen

                  .draweval(sub$moves$eval[i-1], sub$moves$eval[i-2], i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)

                  if (!identical(matetype, "none")) {
                     .texttop(.text(matetype))
                     mode <- "analysis"
                  }

                  threefold <- any(table(sapply(strsplit(sub$moves$fen, " "), function(x) paste(x[1:4], collapse = " "))) == 3L)

                  if (threefold) {
                     .texttop(.text("threefold"))
                     evalval <- 0
                     mode <- "analysis"
                  }

                  fifty <- identical(strsplit(fen, " ")[[1]][5], "100")

                  if (fifty) {
                     .texttop(.text("fifty"))
                     evalval <- 0
                     mode <- "analysis"
                  }

                  if (mode == "analysis") {
                     playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                     .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                     .draweval(evalval[1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
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
            switched.square <- FALSE
            new.piece <- FALSE
            button <- 0L
            click.num <- 0
            remove.annotations <- FALSE

            plt <- par("plt")

            timestart <- proc.time()[[3]]

            click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=mousemove, onMouseUp=mouseup, onKeybd=.keyfun)

            idle.time <- proc.time()[[3]] - timestart

            if (idle.time > idletime)
               session.time.start <- session.time.start + idle.time

            #if (mode == "test") {
            #   if (seqname == "<lastsequence>.rds") {
            #      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=mousemove, onMouseUp=mouseup, onKeybd=.keyfun)
            #   } else {
            #      click <- "u"
            #   }
            #} else {
            #   click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=mousedown, onMouseMove=mousemove, onMouseUp=mouseup, onKeybd=.keyfun)
            #}

            if (remove.annotations) {

               if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                  .rmannot(pos, circles, rbind(arrows, harrows), flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
                  evalvals <- c()
               }

            }

            if (is.numeric(click) && isTRUE(click == 0))
               next

            if (is.character(click) && !is.element(click, keys))
               next

            ##################################################################

            ### general keys

            # q (or ctrl-q) to quit the trainer

            if (identical(click, "q") || identical(click, "ctrl-Q")) {
               session.date.end <- Sys.time()
               session.time.end <- proc.time()[[3]]
               session.playtime <- round(session.time.end - session.time.start)
               if (session.playtime > 120) { # only save session if it was longer than x number of seconds
                  dates <- data.frame(date.start=session.date.start, date.end=session.date.end, playtime=session.playtime, seqsplayed=sum(session.seqsplayed))
                  player.file <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions", paste0(player, ".rds"))
                  if (file.exists(player.file)) {
                     old.dates <- readRDS(player.file)
                     dates <- rbind(old.dates, dates)
                  }
                  saveRDS(dates, file=player.file)
               }
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

            # Escape to update the board

            if (identical(click, "\033") || identical(click, "ctrl-[")) {
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
               harrows <- matrix(nrow=0, ncol=4)
               evalvals <- c()
               next
            }

            # <space> to select the mode (add versus test) (starts a new round)

            if (identical(click, " ")) {
               if (mode %in% c("play","analysis")) {
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

            # m to choose a sequence selection mode

            if (identical(click, "m")) {
               selmodeold <- selmode
               selmode <- .selmode(selmode, lwd)
               if (selmodeold != selmode) {
                  settings$selmode <- selmode
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  write.table(data.frame(selmode), file=file.path(seqdir[seqdirpos], ".selmode"), col.names=FALSE, row.names=FALSE, quote=FALSE)
                  seqno <- 1
                  run.rnd <- FALSE
                  input <- FALSE
               } else {
                  .redrawpos(pos, flip=flip)
                  .drawcircles(circles, lwd=lwd)
                  .drawarrows(arrows, lwd=lwd)
                  .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               }
               next
            }

            # d to choose a difficulty calculation method

            if (identical(click, "d")) {
               difffunold <- difffun
               difflenold <- difflen
               diffminold <- diffmin
               tmp <- .diffset(difffun, difflen, diffmin, lwd)
               difffun <- tmp$difffun
               difflen <- tmp$difflen
               diffmin <- tmp$diffmin
               if (difffunold != difffun || difflenold != difflen || diffminold != diffmin) {
                  .difffun <- eval(parse(text=paste0(".difffun", difffun)))
                  difficulty.all <- lapply(dat.all, function(x) .difffun(x$player[[player]]$score, difflen, diffmin, adjusthint, multiplier))
                  difficulty.all[is.na(difficulty.all) | .is.null(difficulty.all)] <- NA_real_
                  difficulty.all <- unlist(difficulty.all)
                  difficulty.selected <- lapply(dat, function(x) .difffun(x$player[[player]]$score, difflen, diffmin, adjusthint, multiplier))
                  difficulty.selected[is.na(difficulty.selected) | .is.null(difficulty.selected)] <- NA_real_
                  difficulty.selected <- unlist(difficulty.selected)
                  difficulty <- .difffun(sub$player[[player]]$score, difflen, diffmin, adjusthint, multiplier)
                  if (is.null(difficulty) || is.na(difficulty))
                     difficulty <- NA
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                  settings$difffun <- difffun
                  settings$difflen <- difflen
                  settings$diffmin <- diffmin
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
               }
               .redrawpos(pos, flip=flip)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               next
            }

            # \ (or #) to switch into play mode or to analysis mode from play mode

            if (identical(click, "\\") || identical(click, "#")) {
               if (!sfrun) {
                  .texttop(.text("noplaymode"), sleep=1)
                  .texttop(texttop)
                  next
               }
               sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]
               if (mode == "play") {
                  show <- FALSE
                  mode <- "analysis"
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                  .draweval(evalval[1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                  next
               }
               prevmode <- mode
               mode <- "play"
               timed <- FALSE
               show <- FALSE
               fen <- .genfen(pos, flip, sidetoplay, i)
               if ((flip && sidetoplay=="b") || (!flip && sidetoplay=="w")) {
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
               } else {
                  res.sf <- .sf.eval(sfproc, sfrun, depth3, multipv1, sflim, fen, sidetoplay, verbose)
               }
               evalval  <- res.sf$eval
               bestmove <- res.sf$bestmove
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               texttop <- .texttop("")
               .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                  .rmannot(pos, circles, rbind(arrows, harrows), flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
               }
               .draweval(evalval[1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               next
            }

            # n (or N) to start a new sequence / round (from play/analysis mode, jumps back to oldmode)

            if (identical(click, "n") || identical(click, "N")) {
               if (mode %in% c("play","analysis"))
                  mode <- oldmode
               if (mode == "test" && selmode %in% c("sequential","sequential_len","sequential_mov","age_oldest")) {
                  seqno <- seqno + 1
                  if (seqno > k) {
                     seqno <- 1
                     if (k > 1L) {
                        playsound(system.file("sounds", "finished.ogg", package="chesstrainer"), volume=volume)
                        .texttop(.text("finishedround"), sleep=2)
                     }
                  }
               }
               useflip <- FALSE
               run.rnd <- FALSE
               input <- FALSE
               next
            }

            # p to select a player (starts a new round if a new player is selected)

            if (identical(click, "p")) {
               oldplayer <- player
               player <- .selectplayer(player, seqdir[seqdirpos], lwd)
               if (player != oldplayer) {
                  settings$player <- player
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  session.date.end <- Sys.time()
                  session.time.end <- proc.time()[[3]]
                  session.playtime <- round(session.time.end - session.time.start)
                  if (session.playtime > 30) { # only save session if it was longer than 30 seconds
                     dates <- data.frame(date.start=session.date.start, date.end=session.date.end, playtime=session.playtime, seqsplayed=sum(session.seqsplayed))
                     player.file <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions", paste0(oldplayer, ".rds"))
                     if (file.exists(player.file)) {
                        old.dates <- readRDS(player.file)
                        dates <- rbind(old.dates, dates)
                     }
                     saveRDS(dates, file=player.file)
                  }
                  session.seqsplayed <- 0
                  session.mean.scores <- list(NULL)
                  session.length <- 1
                  session.date.start <- Sys.time()
                  session.time.start <- proc.time()[[3]]
                  run.rnd <- FALSE
                  input <- FALSE
               } else {
                  .redrawpos(pos, flip=flip)
                  .drawcircles(circles, lwd=lwd)
                  .drawarrows(arrows, lwd=lwd)
                  .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               }
               next
            }

            # ctrl-r to remove the current player (starts a new round)

            if (identical(click, "ctrl-R")) {
               .texttop(.text("rlydelplayer", player))
               answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
               answer <- .confirm(answer)
               if (answer) {
                  .texttop(.text("delplayer", player), sleep=1)
                  .removeplayer(player, seqdir[seqdirpos])
                  player <- .selectplayer(player, seqdir[seqdirpos], lwd, mustselect=TRUE)
                  run.rnd <- FALSE
                  input <- FALSE
               } else {
                  .texttop(texttop)
               }
               next
            }

            # g to show the progress graph (in test mode) or the evaluation graph (in add/play/analysis mode)

            if (identical(click, "g")) {
               if (mode == "test") {
                  if (is.null(sub$player[[player]]$score))
                     next
                  tmp <- .progressgraph(sub$player[[player]], lwd=lwd, mar=mar, mar2=mar2)
                  if (!identical(mar2, tmp$mar2)) {
                     mar2 <- tmp$mar2
                     settings$mar2 <- mar2
                     saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  }
               }
               if (mode %in% c("add","play","analysis")) {
                  if (any(is.na(sub$moves$eval)) || i == 1)
                     next
                  tmp <- .evalgraph(sub$moves, i=i, flip=flip, lwd=lwd, mar=mar, mar2=mar2)
                  if (!identical(mar2, tmp$mar2)) {
                     mar2 <- tmp$mar2
                     settings$mar2 <- mar2
                     saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  }
               }
               .redrawpos(pos, flip=flip)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               next
            }

            # H to do a deep evaluation in add, play, and analysis mode and show the best move(s)

            if (mode %in% c("add","play","analysis") && identical(click, "H")) {
               if (!sfrun) {
                  .texttop(.text("nomovewoutsf"), sleep=1.5)
                  .texttop(texttop)
                  next
               }
               .texttop(.text("sfdeepeval"))
               fen <- .genfen(pos, flip, sidetoplay, i)
               res.sf <- .sf.eval(sfproc, sfrun, depth2, multipv2, sflim=NA, fen, sidetoplay, verbose, progbar=TRUE)
               evalval  <- res.sf$eval
               bestmove <- res.sf$bestmove
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
               click <- "h"
            }

            # h to get a hint in test mode or show the best move in add/play/analysis mode

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
                  .textbot(mode, zenmode, score=score, onlyscore=TRUE)
               } else {
                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                     .rmannot(pos, circles, rbind(arrows, harrows), flip)
                     harrows <- matrix(nrow=0, ncol=4)
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                  }
                  if (i == 1 && is.na(evalval[1])) {
                     fen <- .genfen(pos, flip, sidetoplay, i)
                     res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                     evalval  <- res.sf$eval
                     bestmove <- res.sf$bestmove
                     matetype <- res.sf$matetype
                     sfproc   <- res.sf$sfproc
                     sfrun    <- res.sf$sfrun
                  }
                  if (bestmove[[1]][1] != "") { # bestmove comes from [d] (unless it was calculate above)
                     nmoves <- length(bestmove)
                     bestmovetxt <- rep(NA_character_, nmoves)
                     evalvals <- rep(NA_real_, nmoves)
                     for (j in 1:nmoves) {
                        if (bestmove[[j]][1] == "")
                           next
                        bestmovetxt[j] <- .parsebestmove(bestmove[[j]], pos=pos, flip=flip, evalval=evalval[j], i=i, sidetoplay=sidetoplay, rename=TRUE, returnline=TRUE, hintdepth=hintdepth)$txt
                        evalvals[j] <- evalval[j]
                        bestx1 <- as.numeric(substr(bestmove[[j]][1], 2, 2))
                        besty1 <- which(letters[1:8] == substr(bestmove[[j]][1], 1, 1))
                        bestx2 <- as.numeric(substr(bestmove[[j]][1], 4, 4))
                        besty2 <- which(letters[1:8] == substr(bestmove[[j]][1], 3, 3))
                        if (flip) {
                           bestx1 <- 9 - bestx1
                           besty1 <- 9 - besty1
                           bestx2 <- 9 - bestx2
                           besty2 <- 9 - besty2
                        }
                        harrows <- rbind(harrows, c(bestx1, besty1, bestx2, besty2))
                     }
                     evalvals <- evalvals[!is.na(evalvals)]
                     bestmovetxt <- bestmovetxt[!is.na(bestmovetxt)]
                     .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
                     texttop <- .texttop(paste0(bestmovetxt, collapse="\n"), left=TRUE)
                     attr(texttop, "left") <- TRUE
                  } else {
                     if (sfrun) {
                        .texttop(.text("nobestmove"), sleep=1.5)
                     } else {
                        .texttop(.text("nomovewoutsf"), sleep=1.5)
                     }
                  }
               }
               next
            }

            # 1 or up arrow to go to the beginning of the first player move / beginning of the game

            if (identical(click, "1") || identical(click, "Up")) {
               if (i == 1)
                  next
               .rmcheck(pos, flip=flip)
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                  .rmannot(pos, circles, rbind(arrows, harrows), flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
                  evalvals <- c()
               }
               oldeval <- sub$moves$eval[i-1]
               posold <- pos
               if (is.null(sub$pos)) {
                  pos <- start.pos
               } else {
                  pos <- sub$pos
                  starteval <- attr(pos, "starteval")
               }
               neweval <- starteval
               sidetoplay <- sidetoplaystart
               comment <- ""
               if (mode %in% c("add","test")) {
                  # find the first move that must be made by the player
                  firstmove <- which(!sub$moves$show[1:(i-1)])
                  if (length(firstmove) > 0L && min(firstmove) > 1) {
                     # if there is such a move, go back one more move
                     for (i in seq_len(min(firstmove)-1)) {
                        pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=0, verbose=verbose, draw=FALSE)
                        sub$moves$move[i] <- attr(pos,"move")
                        sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     }
                     neweval <- sub$moves$eval[i]
                     i <- i + 1
                  } else {
                     i <- 1
                  }
               } else {
                  i <- 1
               }
               playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
               .redrawpos(pos, posold, flip=flip)
               if (mode %in% c("add","test")) {
                  texttop <- .texttop(sub$moves$comment[i])
                  circles <- .parseannot(sub$moves$circles[i], cols=2)
                  arrows  <- .parseannot(sub$moves$arrows[i], cols=4)
                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                  }
               } else {
                  texttop <- .texttop("")
               }
               if (mode == "play")
                  mode <- "analysis"
               .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               .draweval(neweval, oldeval, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
               if (mode %in% c("add","analysis")) {
                  fen <- .genfen(pos, flip, sidetoplay, i)
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                  evalval  <- res.sf$eval
                  bestmove <- res.sf$bestmove
                  matetype <- res.sf$matetype
                  sfproc   <- res.sf$sfproc
                  sfrun    <- res.sf$sfrun
               }
               next
            }

            # t to take back a score increase in test mode or take back a move (two moves in play mode)
            # left arrow to go back one move (in play mode goes into analysis mode)

            if (identical(click, "Left") || identical(click, "t")) {
               if (mode == "test" && identical(click, "t")) {
                  if (scoreadd > 0) {
                     score <- score - scoreadd
                     .texttop(.text("setscoreback", score), sleep=1)
                     .texttop(texttop)
                     .textbot(mode, zenmode, score=score, onlyscore=TRUE)
                     scoreadd <- 0
                  } else {
                     if (score == 100) {
                        .texttop(.text("setscoreback100"), sleep=1)
                        .texttop(texttop)
                     }
                  }
                  mistake <- FALSE
                  next
               }
               if (i == 1)
                  next
               .rmcheck(pos, flip=flip)
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                  .rmannot(pos, circles, rbind(arrows, harrows), flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
                  evalvals <- c()
               }
               oldeval <- sub$moves$eval[i-1]
               posold <- pos
               if (is.null(sub$pos)) {
                  pos <- start.pos
               } else {
                  pos <- sub$pos
                  starteval <- attr(pos, "starteval")
               }
               neweval <- starteval
               sidetoplay <- sidetoplaystart
               comment <- ""
               if (i == 2 || (mode == "play" && identical(click, "t") && i == 3)) {
                  # when a single move has been played, go back to the starting position
                  i <- 1
               } else {
                  if (mode == "play" && identical(click, "t")) {
                     # in play mode, t takes back the computer move and the player move
                     i <- i - 2
                     sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE] # also remove the two moves that were played from sub$moves
                  } else {
                     i <- i - 1
                  }
                  neweval <- sub$moves$eval[i-1]
                  for (i in seq_len(i-1)) {
                     pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=0, verbose=verbose, draw=FALSE)
                     sub$moves$move[i] <- attr(pos,"move")
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                  }
                  i <- i + 1
               }
               playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
               .redrawpos(pos, posold, flip=flip)
               if (mode %in% c("add","test")) {
                  texttop <- .texttop(sub$moves$comment[i])
                  circles <- .parseannot(sub$moves$circles[i], cols=2)
                  arrows  <- .parseannot(sub$moves$arrows[i], cols=4)
                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                  }
               } else {
                  texttop <- .texttop("")
               }
               if (mode == "play" && identical(click, "Left"))
                  mode <- "analysis"
               .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               .draweval(neweval, oldeval, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               if (mode == "test" && timed) {
                  .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
               } else {
                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
               }
               if (mode == "add" && identical(click, "t")) # t in add mode removes all further moves
                  sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]
               if (mode %in% c("add","play","analysis")) {
                  fen <- .genfen(pos, flip, sidetoplay, i)
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                  evalval  <- res.sf$eval
                  bestmove <- res.sf$bestmove
                  matetype <- res.sf$matetype
                  sfproc   <- res.sf$sfproc
                  sfrun    <- res.sf$sfrun
               }
               next
            }

            # right arrow to play the next move (in add/test/analysis mode)

            if (mode %in% c("add","test","analysis") && identical(click, "Right")) {
               if (i > nrow(sub$moves)) {
                  .texttop(.text("waslastmove"), sleep=0.75)
                  if (mode %in% c("add","test"))
                     .texttop(texttop)
                  next
               }
               comment <- ""
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                  .rmannot(pos, circles, rbind(arrows, harrows), flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
                  evalvals <- c()
               }
               pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
               sub$moves$move[i] <- attr(pos,"move")
               .draweval(sub$moves$eval[i], sub$moves$eval[i-1], i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
               if (mode == "test" && timed) {
                  .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
               } else {
                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
               }
               i <- i + 1
               if (i > nrow(sub$moves)) {
                  texttop <- .texttop(sub$commentend)
                  if (!is.null(sub$symbolend)) {
                     circles <- .parseannot(sub$symbolend$circles, cols=2)
                     arrows  <- .parseannot(sub$symbolend$arrows, cols=4)
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                  }
               } else {
                  texttop <- .texttop(sub$moves$comment[i])
                  circles <- .parseannot(sub$moves$circles[i], cols=2)
                  arrows  <- .parseannot(sub$moves$arrows[i], cols=4)
                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                  }
               }
               .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               if (mode %in% c("add","analysis")) {
                  fen <- .genfen(pos, flip, sidetoplay, i)
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                  evalval  <- res.sf$eval
                  bestmove <- res.sf$bestmove
                  matetype <- res.sf$matetype
                  sfproc   <- res.sf$sfproc
                  sfrun    <- res.sf$sfrun
               }
               next
            }

            # 2 or down arrow to go to the end of the sequence/game (3, 4, and 5 to jump to 1/4, 2/4, and 3/4 of the moves)

            if (identical(click, "Down") || identical(click, "2") || identical(click, "3") || identical(click, "4") || identical(click, "5")) {
               if (identical(click, "Down"))
                  click <- "2"
               if (identical(click, "2") && i > nrow(sub$moves)) {
                  .texttop(.text("waslastmove"), sleep=0.75)
                  if (mode %in% c("add","test"))
                     .texttop(texttop)
                  next
               }
               if (identical(click, "2"))
                  target <- nrow(sub$moves)
               if (identical(click, "3"))
                  target <- max(1, round(nrow(sub$moves) * 1/4))
               if (identical(click, "4"))
                  target <- max(1, round(nrow(sub$moves) * 2/4))
               if (identical(click, "5"))
                  target <- max(1, round(nrow(sub$moves) * 3/4))
               if (target == i-1)
                  next
               .rmcheck(pos, flip=flip)
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                  .rmannot(pos, circles, rbind(arrows, harrows), flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
                  evalvals <- c()
               }
               comment <- ""
               oldeval <- sub$moves$eval[i-1]
               posold <- pos
               if (is.null(sub$pos)) {
                  pos <- start.pos
               } else {
                  pos <- sub$pos
                  starteval <- attr(pos, "starteval")
               }
               sidetoplay <- sidetoplaystart
               i <- 1
               while (i <= target) {
                  pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=0, verbose=verbose, draw=FALSE)
                  sub$moves$move[i] <- attr(pos,"move")
                  sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                  i <- i + 1
               }
               playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
               .redrawpos(pos, posold, flip=flip)
               if (mode == "test" && timed) {
                  .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
               } else {
                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
               }
               if (mode %in% c("add","test")) {
                  if (i > nrow(sub$moves)) {
                     texttop <- .texttop(sub$commentend)
                     if (!is.null(sub$symbolend)) {
                        circles <- .parseannot(sub$symbolend$circles, cols=2)
                        arrows  <- .parseannot(sub$symbolend$arrows, cols=4)
                        .drawcircles(circles, lwd=lwd)
                        .drawarrows(arrows, lwd=lwd)
                     }
                  } else {
                     texttop <- .texttop(sub$moves$comment[i])
                     circles <- .parseannot(sub$moves$circles[i], cols=2)
                     arrows  <- .parseannot(sub$moves$arrows[i], cols=4)
                     if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                        .drawcircles(circles, lwd=lwd)
                        .drawarrows(arrows, lwd=lwd)
                     }
                  }
               } else {
                  texttop <- .texttop("")
               }
               if (mode == "play")
                  mode <- "analysis"
               .draweval(sub$moves$eval[i-1], oldeval, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               if (mode %in% c("add","analysis")) {
                  fen <- .genfen(pos, flip, sidetoplay, i)
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                  evalval  <- res.sf$eval
                  bestmove <- res.sf$bestmove
                  matetype <- res.sf$matetype
                  sfproc   <- res.sf$sfproc
                  sfrun    <- res.sf$sfrun
               }
               next
            }

            # 0 to store the current game as the main variation (in play/analysis mode)

            if (mode %in% c("play","analysis") && identical(click, "0")) {
               .texttop(.text("storegame"), sleep=1)
               savgame <- sub$moves
               next
            }

            # 9 to jump to the main variation

            if (mode %in% c("play","analysis") && identical(click, "9") && !is.null(savgame)) {
               .texttop(.text("retstoregame"), sleep=1)
               .rmcheck(pos, flip=flip)
               oldeval <- sub$moves$eval[i-1]
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                  .rmannot(pos, circles, rbind(arrows, harrows), flip)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
                  evalvals <- c()
               }
               posold <- pos
               sub$moves <- savgame
               if (is.null(sub$pos)) {
                  pos <- start.pos
               } else {
                  pos <- sub$pos
                  starteval <- attr(pos, "starteval")
               }
               sidetoplay <- sidetoplaystart
               i <- 1
               while (i <= nrow(sub$moves)) {
                  pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=0, verbose=verbose, draw=FALSE)
                  sub$moves$move[i] <- attr(pos,"move")
                  sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                  i <- i + 1
               }
               playsound(system.file("sounds", "move.ogg", package="chesstrainer"), volume=volume)
               .redrawpos(pos, posold, flip=flip)
               .draweval(sub$moves$eval[i-1], oldeval, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
               fen <- .genfen(pos, flip, sidetoplay, i)
               res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
               evalval  <- res.sf$eval
               bestmove <- res.sf$bestmove
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               next
            }

            # u to update the evaluations

            if (identical(click, "u")) {

               if (is.null(sfproc) || !sfrun) {

                  .texttop(.text("evalupdatenosf"), sleep=1.5)
                  .texttop(texttop)
                  next

               } else {

                  eval(expr=switch1)

                  if (is.null(sub$pos)) {
                     pos <- start.pos
                  } else {
                     pos <- sub$pos
                  }

                  .drawboard(pos, flip=flip, inhibit=inhibit, mar=mar)
                  .drawcheck(pos, flip=flip)
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  sidetoplay <- sidetoplaystart

                  cat(.text("evalupdateold"))
                  print(sub$moves[-c(9:11)])
                  cat(.text("evalupdatestart"))

                  if (!is.null(sub$pos)) {
                     fen <- .genfen(pos, flip, sidetoplay, i=1)
                     res.sf <- .sf.eval(sfproc, sfrun, depth2, multipv2, sflim=NA, fen, sidetoplay, verbose, progbar=TRUE)
                     evalval  <- res.sf$eval
                     bestmove <- res.sf$bestmove
                     matetype <- res.sf$matetype
                     sfproc   <- res.sf$sfproc
                     sfrun    <- res.sf$sfrun
                     starteval <- evalval[1]
                     attr(sub$pos, "starteval") <- starteval
                  }

                  for (i in 1:nrow(sub$moves)) {
                     pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
                     sub$moves$move[i] <- attr(pos,"move")
                     .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
                     fen <- .genfen(pos, flip, sidetoplay, i)
                     res.sf <- .sf.eval(sfproc, sfrun, depth2, multipv2, sflim=NA, fen, sidetoplay, verbose, progbar=TRUE)
                     evalval  <- res.sf$eval
                     bestmove <- res.sf$bestmove
                     matetype <- res.sf$matetype
                     sfproc   <- res.sf$sfproc
                     sfrun    <- res.sf$sfrun
                     sub$moves$eval[i] <- evalval[1]
                     sub$moves$fen[i] <- fen
                     .draweval(sub$moves$eval[i], sub$moves$eval[i-1], i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                  }
                  i <- i + 1
                  .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
                  cat(.text("evalupdatenew"))
                  print(sub$moves[-c(9:11)])
                  if (mode == "test")
                     saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  eval(expr=switch2)
                  next

               }

            }

            # U to toggle upsidedown

            if (identical(click, "U")) {
               upsidedown <- !upsidedown
               assign("upsidedown", upsidedown, envir=.chesstrainer)
               run.rnd <- FALSE
               input <- FALSE
               next
            }

            # j to manually jump to a seqno value when playing sequences sequentially

            if (identical(click, "j")) {
               if (selmode %in% c("sequential","sequential_len","sequential_mov")) {
                  seqnoold <- seqno
                  seqno <- .setseqno(seqno, k, lwd)
                  if (seqnoold != seqno) {
                     run.rnd <- FALSE
                     input <- FALSE
                  } else {
                     .redrawpos(pos, flip=flip)
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                     .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
                  }
               } else {
                  .texttop(.text("jumponlymodes"), sleep=2)
                  .texttop(texttop)
               }
               next
            }

            ################################################################

            ### keys specific to the test mode

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
               seqno <- max(1, seqno - 1)
               run.rnd <- FALSE
               input <- FALSE
               next
            }

            # o to edit the score for the current sequence (only in test mode)

            if (mode == "test" && identical(click, "o")) {
               score <- .setscore(score, lwd)
               #sub$player[[player]]$score[length(sub$player[[player]]$score)] <- score
               #saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
               .redrawpos(pos, flip=flip)
               .textbot(mode, zenmode, score=score, onlyscore=TRUE)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               next
            }

            # z to toggle zenmode (only in test mode) (in add mode, toggles show on/off)

            if (mode == "test" && identical(click, "z")) {
               zenmode <- !zenmode
               .texttop(.text("zenmode", zenmode), sleep=0.75)
               .texttop(texttop)
               .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
               next
            }

            # M to trigger a mistake (only in test mode)

            if (mode == "test" && identical(click, "M")) {
               mistake <- TRUE
               if (score >= 1) {
                  scoreadd <- min(adjustwrong, 100-score)
                  score <- score + scoreadd
                  .textbot(mode, zenmode, score=score, onlyscore=TRUE)
                  playsound(system.file("sounds", "error.ogg", package="chesstrainer"), volume=volume)
               }
               next
            }

            # B to end the sequence (only in test mode)

            if (mode == "test" && identical(click, "B")) {
               if (i <= nrow(sub$moves)) {
                  .texttop(.text("notlastmove"), sleep=0.75)
                  .texttop(texttop)
               } else {
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  if (is.null(sub$commentend)) {
                     if (mistake) {
                        .texttop(.text("keeppracticing"))
                     } else {
                        .texttop(.text("welldone"))
                     }
                  } else {
                     .texttop(sub$commentend)
                  }
                  if (!is.null(sub$symbolend)) {
                     circles <- .parseannot(sub$symbolend$circles, cols=2)
                     arrows  <- .parseannot(sub$symbolend$arrows, cols=4)
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                  }
                  if (!wait && (!is.null(sub$commentend) || !is.null(sub$symbolend)))
                     .waitforclick()
                  lastseq <- seqname
                  if (mistake && repmistake) {
                     replast <- TRUE
                     filename <- seqname
                  }
                  if (!mistake && score > 1)
                     score <- round(score * multiplier)
                  rounds <- rounds + 1
                  tmp <- data.frame(date=as.numeric(Sys.time()), round=rounds, score=score)
                  if (is.null(sub$player[[player]])) {
                     sub$player[[player]] <- tmp
                  } else {
                     sub$player[[player]] <- rbind(sub$player[[player]], tmp)
                  }
                  difficulty <- .difffun(sub$player[[player]]$score, difflen, diffmin, adjusthint, multiplier)
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age=0, difficulty, i+1, totalmoves, selmode, k, seqno)
                  session.seqsplayed[session.length] <- session.seqsplayed[session.length] + 1
                  session.mean.scores[[session.length]] <- c(session.mean.scores[[session.length]], mean(scores.all, na.rm=TRUE))
                  if (showgraph) {
                     tmp <- .progressgraph(sub$player[[player]], lwd=lwd, mar=mar, mar2=mar2)
                     if (!identical(mar2, tmp$mar2)) {
                        mar2 <- tmp$mar2
                        settings$mar2 <- mar2
                        saveRDS(settings, file=file.path(configdir, "settings.rds"))
                     }
                     .redrawpos(pos, flip=flip)
                  }
                  if (!eval[[mode]])
                     .draweval(sub$moves$eval[i], NA, i=i, starteval=starteval, flip=flip, eval=TRUE, evalsteps=evalsteps)
                  if (selmode %in% c("sequential","sequential_len","sequential_mov","age_oldest")) {
                     if (!replast) {
                        seqno <- seqno + 1
                        if (seqno > k) {
                           seqno <- 1
                           if (k > 1L) {
                              playsound(system.file("sounds", "finished.ogg", package="chesstrainer"), volume=volume)
                              .texttop(.text("finishedround"), sleep=2)
                           }
                        }
                     }
                  }
                  saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                  Sys.sleep(2*sleep)
                  run.rnd <- FALSE
                  input <- FALSE
               }
               next
            }

            ################################################################

            ### adding / editing sequences

            # a to copy the current sequence to add new moves as a new sequence
            # A does the same but starts at the position when A is pressed

            if (identical(click, "a") || identical(click, "A")) {

               if (mode == "add") {
                  if (k == 1) {
                     sel <- 1
                     sub <- dat[[sel]]
                     seqname <- files[sel]
                     seqnum <- which(seqname == files.all)
                     score <- 100
                     rounds <- 0
                     totalmoves <- nrow(sub$moves)
                     flip <- sub$flip
                     click <- "a"
                  } else {
                     next
                  }
               }

               if (mode %in% c("play","analysis")) # in play/analysis mode, always use A
                  click <- "A"

               mode <- oldmode <- "add"

               sub$player <- NULL

               texttop <- .texttop("")

               if (identical(click, "A")) {

                  # when 'A' is pressed at the start position sub$pos, then i is 1
                  # and sub$moves has 0 rows, so we need to skip the following step

                  if (nrow(sub$moves) > 0L)
                     sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]

                  if (nrow(circles) >= 1L || nrow(arrows) >= 1L || nrow(harrows) >= 1L) {
                     .rmannot(pos, circles, rbind(arrows, harrows), flip)
                     circles <- matrix(nrow=0, ncol=2)
                     arrows  <- matrix(nrow=0, ncol=4)
                     harrows <- matrix(nrow=0, ncol=4)
                     evalvals <- c()
                  }

                  .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)

               } else {

                  if (is.null(sub$pos)) {
                     pos <- start.pos
                     sidetoplay <- "w"
                  } else {
                     pos <- sub$pos
                     starteval <- attr(pos, "starteval")
                     if (flip) {
                        piece <- pos[9-sub$moves[1,1], 9-sub$moves[1,2]]
                     } else {
                        piece <- pos[sub$moves[1,1], sub$moves[1,2]]
                     }
                     sidetoplay <- ifelse(startsWith(piece, "W"), "w", "b")
                  }

                  .drawboard(pos, flip=flip, inhibit=inhibit, mar=mar)
                  .drawcheck(pos, flip=flip)
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i=1, totalmoves, selmode, k, seqno)
                  circles <- matrix(nrow=0, ncol=2)
                  arrows  <- matrix(nrow=0, ncol=4)
                  harrows <- matrix(nrow=0, ncol=4)
                  evalvals <- c()

                  if (!identical(sub$moves$comment[1], "")) {
                     texttop <- .texttop(sub$moves$comment[1])
                     Sys.sleep(sleep)
                  }

                  sidetoplaystart <- sidetoplay

                  for (i in 1:nrow(sub$moves)) {
                     pos <- .updateboard(pos, move=sub$moves[i,1:6], flip=flip, autoprom=TRUE, volume=volume, verbose=verbose)
                     sub$moves$move[i] <- attr(pos,"move")
                     if (identical(sub$moves$comment[i], "") && !identical(sub$moves$comment[i-1], "")) {
                        texttop <- .texttop(sub$moves$comment[i-1])
                     } else {
                        texttop <- .texttop(sub$moves$comment[i])
                     }
                     .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
                     .draweval(sub$moves$eval[i], sub$moves$eval[i-1], i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                     sidetoplay <- ifelse(sidetoplay == "w", "b", "w")
                     Sys.sleep(sleep)
                  }

                  i <- i + 1
                  show <- FALSE

               }

               .textbot(mode, zenmode, show=show, i=i, totalmoves=totalmoves, onlyshow=TRUE, onlyi=TRUE)
               sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
               fen <- .genfen(pos, flip, sidetoplay, i)
               res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
               evalval  <- res.sf$eval
               bestmove <- res.sf$bestmove
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
               next

            }

            # 0 to make the current position the starting position (only in add mode)

            if (mode == "add" && identical(click, "0")) {
               if (!.is.start.pos(pos)) {
                  .texttop(.text("setposstart"), sleep=1)
                  .texttop(texttop)
                  starteval <- sub$moves$eval[i-1]
                  i <- 1
                  comment <- ""
                  sub$moves <- sub$moves[numeric(0),]
                  sub$pos <- pos
                  .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
               }
               next
            }

            # f to flip the board (only in add mode and only at the start of a sequence)

            if (mode == "add" && identical(click, "f")) {
               if (i > 1) {
                  .texttop(.text("fliponlyatstart", show), sleep=1.25)
                  .texttop(texttop)
                  next
               }
               flip <- !flip
               sub$flip <- flip
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
               harrows <- matrix(nrow=0, ncol=4)
               evalvals <- c()
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(starteval, starteval, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               next
            }

            # z to switch show moves on/off (only in add mode) (in test mode, toggles zenmode on/off)

            if (mode == "add" && identical(click, "z")) {
               show <- !show
               .texttop(.text("showmoves", show), sleep=0.75)
               .texttop(texttop)
               .textbot(mode, zenmode, show=show, onlyshow=TRUE)
               next
            }

            # c to add a comment to the current move (only in add mode)

            if (mode == "add" && identical(click, "c")) {
               eval(expr=switch1)
               comment <- readline(prompt=.text("comment"))
               eval(expr=switch2)
               texttop <- comment
               .texttop(texttop)
               next
            }

            # e to edit the comments using prompts

            if (identical(click, "e")) {
               eval(expr=switch1)
               sub <- .editcomments(sub, seqdir[seqdirpos], seqname, mode) # note: directly saves after edit in test mode
               eval(expr=switch2)
               texttop <- sub$moves$comment[i-1]
               .texttop(texttop)
               next
            }

            # E to edit a sequence using edit()

            if (identical(click, "E")) {
               sub$moves <- edit(sub$moves)
               sub$moves$comment[is.na(sub$moves$comment)] <- ""
               if (!is.null(sub$moves$circles))
                  sub$moves$circles[is.na(sub$moves$circles)] <- ""
               if (!is.null(sub$moves$arrows))
                  sub$moves$arrows[is.na(sub$moves$arrows)] <- ""
               if (mode == "test") # note: save after edit in test mode
                  saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
               next
            }

            # s to save the sequence (only in add mode)

            if (mode == "add" && identical(click, "s")) {
               if ((flip && sidetoplay == "b") || (!flip && sidetoplay == "w")) {
                  .texttop(.text("lastmoveplayer"))
                  next
               }
               if (all(sub$moves$show)) {
                  .texttop(.text("allmovesshown"))
                  next
               }
               if ((i-1) != nrow(sub$moves)) {
                  .texttop(.text("notatend"))
                  next
               }
               eval(expr=switch1)
               .texttop(.text("saveseq"))
               seqident <- sapply(dat.all, function(x) identical(sub$moves[1:5], x$moves[1:5]))
               if (any(seqident))
                  cat(.text("seqexists", files.all[which(seqident)[1]]))
               # if there are circles/arrows at the end of a sequence, save these to the sequence file (as symbolend)
               circlesvar <- ""
               arrowsvar  <- ""
               if (nrow(circles) >= 1L)
                  circlesvar <- paste0(apply(circles, 1, function(x) paste0("(",x[1],",",x[2],")")), collapse=";")
               if (nrow(arrows) >= 1L)
                  arrowsvar <- paste0(apply(arrows, 1, function(x) paste0("(",x[1],",",x[2],",",x[3],",",x[4],")")), collapse=";")
               if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
                  symbolend <- data.frame(circlesvar, arrowsvar)
                  sub$symbolend <- symbolend
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

            # b to start the board editor (in add, play, and analysis mode)

            if (mode %in% c("add","play","analysis") && identical(click, "b")) {
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
               texttop <- ""
               evalval <- NA_real_
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
               harrows <- matrix(nrow=0, ncol=4)
               evalvals <- c()
               sub$moves <- sub$moves[numeric(0),]
               sub$flip <- flip
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               if (!.is.start.pos(pos)) {
                  sub$pos <- pos
                  fen <- .genfen(pos, flip, sidetoplay, i)
                  res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                  evalval  <- res.sf$eval
                  bestmove <- res.sf$bestmove
                  matetype <- res.sf$matetype
                  sfproc   <- res.sf$sfproc
                  sfrun    <- res.sf$sfrun
                  starteval <- evalval[1]
                  attr(sub$pos, "starteval") <- starteval
                  .draweval(starteval, NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               }
               if (verbose)
                  print(pos)
               next
            }

            ################################################################

            ### toggles and keys related to adjusting settings

            # ^ (or 6) to edit the exponent value

            if (identical(click, "^") || identical(click, "6")) {
               expvalold <- expval
               expval <- .setexpval(expval, scores.all, lwd)
               .redrawpos(pos, flip=flip)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               if (expvalold != expval) {
                  settings$expval <- expval
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  run.rnd <- FALSE
                  input <- FALSE
               }
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

            # G to toggle showing the progress graph after each completed sequence

            if (identical(click, "G")) {
               showgraph <- !showgraph
               .texttop(.text("showgraph", showgraph), sleep=1.5)
               .texttop(texttop)
               settings$showgraph <- showgraph
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
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

            # {/} to decrease/increase the margin width

            if (identical(click, "{") || identical(click, "}")) {
               if (identical(click, "{")) {
                  mar <- pmax(1, mar - 0.5)
               } else {
                  mar <- mar + 0.5
               }
               .texttop(.text("maradj", mar), sleep=0.5)
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               settings$mar <- mar
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
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

            # i to toggle the language

            if (identical(click, "i")) {
               if (lang == "de") {
                  lang <- "en"
               } else {
                  lang <- "de"
               }
               assign("lang", lang, envir=.chesstrainer)
               .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
               .texttop(.text("lang"), sleep=0.75)
               .texttop(texttop)
               settings$lang <- lang
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
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
                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
               }
               next
            }

            # v to toggle the evaluation bar on/off

            if (identical(click, "v")) {
               eval[[mode]] <- !eval[[mode]]
               .texttop(.text("eval", eval[[mode]]), sleep=0.75)
               if (eval[[mode]]) {
                  .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               } else {
                  .draweval(clear=TRUE)
               }
               .texttop(texttop)
               settings$eval <- eval
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # ctrl-v to toggle verbose mode on/off

            if (identical(click, "ctrl-V")) {
               verbose <- !verbose
               if (verbose) {
                  eval(expr=switch1)
                  .printverbose(selected, seqno, filename, lastseq, flip, useflip, replast, oldmode, i, seqname, seqnum, score, rounds, totalmoves, show, comment, bestmove, starteval, evalval, texttop, scoreadd, sidetoplay, givehint1, givehint2, mistake, timetotal, movesplayed, movestoplay, drawcircles, drawarrows, showstartcom, pos)
                  eval(expr=switch2)
               }
               .texttop(.text("verbose", verbose), sleep=0.5)
               .texttop(texttop)
               next
            }

            ################################################################

            ### listing, selecting, deleting, and bookmarking sequences

            # l to list all (selected) sequences

            if (identical(click, "l")) {
               eval(expr=switch1)
               if (k > 0L) {
                  if (max(probvals.selected) == min(probvals.selected)) {
                     bars <- rep(5, k)
                  } else {
                     bars <- round(5 * (probvals.selected - min(probvals.selected)) / (max(probvals.selected) - min(probvals.selected)))
                  }
                  bars <- sapply(bars, function(x) paste0(rep("*", x), collapse=""))
                  tab <- data.frame(files, rounds.selected, formatC(age.selected, format="f", digits=1), scores.selected, formatC(difficulty.selected, format="f", digits=1), formatC(probvals.selected, format="f", digits=1), bars)
                  tab$bars <- format(tab$bars, justify="left")
                  names(tab) <- c("Name", .text("rounds"), .text("age"), .text("score"), .text("diff"), .text("prob"), "")
                  tab$Name <- substr(tab$Name, 1, nchar(tab$Name)-4) # remove .rds from name
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  if (!is.null(selected))
                     rownames(tab) <- which(files.all %in% selected)
                  txt <- capture.output(print(tab, print.gap=2))
                  if (length(txt) > 50)
                     txt <- c(txt, txt[1])
                  .print(txt)
               } else {
                  cat(.text("zeroseqsfound"))
               }
               eval(expr=switch2)
               next
            }

            # < to bookmark a sequence (in add mode, last saved sequence; in test mode, current sequence)

            if (mode %in% c("add","test") && identical(click, "<")) {
               bookmark <- ""
               if (mode == "add") {
                  if (filename != "")
                     bookmark <- paste0(filename, ".rds")
               } else {
                  bookmark <- seqname
               }
               if (bookmark != "") {
                  if (file.exists(file.path(seqdir[seqdirpos], ".bookmarks"))) {
                     tmp <- try(read.table(file.path(seqdir[seqdirpos], ".bookmarks"), header=FALSE), silent=TRUE) # if .bookmarks is empty, get an error
                     if (inherits(tmp, "try-error")) {
                        bookmarks <- bookmark
                     } else {
                        bookmarks <- unique(c(bookmark, tmp[[1]])) # add bookmark at front and remove duplicates
                        bookmarks <- bookmarks[is.element(bookmarks, list.files(seqdir[seqdirpos], pattern=".rds$"))] # keep only existing bookmarks
                     }
                  } else {
                     bookmarks <- bookmark
                  }
                  write.table(data.frame(bookmarks), file=file.path(seqdir[seqdirpos], ".bookmarks"), col.names=FALSE, row.names=FALSE, quote=FALSE)
                  .texttop(.text("bookmarked", sub("\\.rds$", "", bookmark)), sleep=1.5)
                  .texttop(texttop)
               }
               next
            }

            # > to select / manage bookmarks

            if (identical(click, ">")) {
               bookmark <- .bookmarks(seqdir, seqdirpos, texttop, lwd) # returns NA if bookmark screen was not draw
               if (isTRUE(bookmark != "")) { # only TRUE if bookmark is neither NA or ""
                  selected <- grepl(bookmark, files.all)
                  selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[selected]
                  run.rnd <- FALSE
                  input <- FALSE
                  mode <- oldmode <- "add"
                  seqno <- 1
               } else {
                  if (bookmark == "") {
                     .redrawpos(pos, flip=flip)
                     .drawcircles(circles, lwd=lwd)
                     .drawarrows(arrows, lwd=lwd)
                     .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
                  }
               }
               next
            }

            # ctrl-f to print and copy the FEN to the clipboard

            if (identical(click, "ctrl-F")) {
               eval(expr=switch1)
               fen <- .genfen(pos, flip, sidetoplay, i)
               cat(fen, "\n")
               eval(expr=switch2)
               clipr::write_clip(fen, object_type="character")
               .texttop(.text("copyfen"), sleep=0.75)
               .texttop(texttop)
               next
            }

            # ctrl-c to print and copy the sequence name to the clipboard

            if (identical(click, "ctrl-C")) {
               if (seqname != "") {
                  eval(expr=switch1)
                  cat(seqnum, " ", seqname, "\n")
                  tmp <- seqname
                  tmp <- sub("\\.rds$", "", tmp) # strip .rds from end of filename
                  clipr::write_clip(tmp, object_type="character")
                  eval(expr=switch2)
               }
               next
            }

            # ctrl-d to delete the current sequence (only in test mode)

            if (mode == "test" && identical(click, "ctrl-D")) {
               .texttop(.text("rlydelseq"))
               answer <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onKeybd=.keyfun)
               answer <- .confirm(answer)
               if (answer) {
                  .texttop(.text("delseq"), sleep=1)
                  file.remove(file.path(seqdir[seqdirpos], seqname))
                  run.rnd <- FALSE
                  input <- FALSE
               } else {
                  .texttop(texttop)
               }
               next
            }

            # / or , to select one or more sequences, . to select last saved sequence, | to search/show sequences based on their filename

            if (identical(click, "/") || identical(click, ",") || identical(click, ".") || identical(click, "|")) {

               doprompt <- TRUE

               if (identical(click, ".")) {
                  if (filename != "") {
                     searchterm <- filename
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

               if (identical(searchterm , "*") && !identical(click, "|")) {
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
                        cat(.text("selmatchesconfirm", sum(seqident)))
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
                        cat(.text("selmatchesconfirm", sum(seqident)))
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
                  tmp <- as.numeric(searchterm)
                  if (tmp < 1 || tmp > k.all) {
                     cat(.text("noseqfound"))
                  } else {
                     cat(.text("selseq", tmp))
                     selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[tmp]
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

               # 'rounds >/</>=/<= value' entered

               tmp <- strcapture(.text("strcaprounds"), searchterm, data.frame(text=character(), sign=character(), cutoff=integer()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqrounds", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("rounds.all", tmp$sign, tmp$cutoff)))
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

               # 'age >/</>=/<= value' entered

               tmp <- strcapture(.text("strcapage"), searchterm, data.frame(text=character(), sign=character(), cutoff=numeric()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqage", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("age.all", tmp$sign, tmp$cutoff)))
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

               # 'difficulty >/</>=/<= value' entered

               tmp <- strcapture(.text("strcapdiff"), searchterm, data.frame(text=character(), sign=character(), cutoff=numeric()))

               if (!is.na(tmp$cutoff)) {
                  cat(.text("selseqdiff", list(tmp$sign, tmp$cutoff)))
                  selected <- eval(parse(text = paste("difficulty.all", tmp$sign, tmp$cutoff)))
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

               andsearch <- FALSE
               orsearch  <- FALSE

               if (startsWith(searchterm, "& ")) {
                  andsearch <- TRUE
                  searchterm <- substr(searchterm, 3, nchar(searchterm))
               }
               if (startsWith(searchterm, "| ")) {
                  orsearch <- TRUE
                  searchterm <- substr(searchterm, 3, nchar(searchterm))
               }

               if (length(grep(searchterm, files.all)) == 0L) {
                  cat(.text("noseqsfound"))
               } else {
                  if (identical(click, "|")) {
                     tmp <- grepl(searchterm, files.all)
                     tab <- data.frame(Name=files.all[tmp])
                     tab$Name <- format(tab$Name, justify="left")
                     names(tab)[1] <- ""
                     rownames(tab) <- which(tmp)
                     print(tab, print.gap=2)
                  } else {
                     if (andsearch || orsearch) {
                        selected2 <- grepl(searchterm, files.all)
                        selected2 <- list.files(seqdir[seqdirpos], pattern=".rds$")[selected2]
                        if (andsearch)
                           selected <- intersect(selected, selected2)
                        if (orsearch)
                           selected <- union(selected, selected2)
                     } else {
                        selected <- grepl(searchterm, files.all)
                        selected <- list.files(seqdir[seqdirpos], pattern=".rds$")[selected]
                     }
                     cat(.text("numseqfound", length(selected)))
                     run.rnd <- FALSE
                     input <- FALSE
                     mode <- oldmode <- "add"
                     seqno <- 1
                  }
               }

               eval(expr=switch2)
               next

            }

            # * (or 8) to select all sequences (starts a new round) (only in add and test mode)

            if (mode %in% c("add","test") && (identical(click, "*") || identical(click, "8"))) {
               if (!is.null(selected)) {
                  .texttop(.text("allseqselected"), sleep=1)
                  selected <- NULL
                  run.rnd <- FALSE
                  input <- FALSE
                  mode <- oldmode <- "add"
                  seqno <- 1
               } else {
                  .texttop(.text("allseqalreadyselected"), sleep=1)
                  .texttop(texttop)
               }
               next
            }

            # ? to find all sequences that start with the same moves

            if (identical(click, "?")) {
               if (i == 1)
                  next
               seqident <- sapply(dat.all, function(x) identical(sub$moves[1:(i-1),1:4], x$moves[1:(i-1),1:4]) && identical(flip, x$flip))
               if (any(seqident)) {
                  eval(expr=switch1)
                  cat(.text("seqsmatchstart"))
                  tab <- data.frame(Name=files.all[seqident])
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  rownames(tab) <- which(seqident)
                  print(tab, print.gap=2)
                  selmatches <- readline(prompt=.text("selmatches"))
                  if (identical(selmatches, "") || .confirm(selmatches)) {
                     cat(.text("selmatchesconfirm", sum(seqident)))
                     selected <- files.all[seqident]
                     run.rnd <- FALSE
                     input <- FALSE
                     seqno <- 1
                  }
                  eval(expr=switch2)
               } else {
                  .texttop(.text("noseqsfound"), sleep=1.5)
                  .texttop(texttop)
               }
               next
            }

            # ' to find all sequences that include the same FEN

            if (identical(click, "'")) {
               if (.is.start.pos(pos))
                  next
               eval(expr=switch1)
               searchterm <- .genfen(pos, flip, sidetoplay, i)
               searchterm <- paste(strsplit(searchterm, " ", fixed=TRUE)[[1]][1:3], collapse=" ")
               print(searchterm)
               seqident <- sapply(dat.all, function(x) any(grepl(searchterm, x$moves$fen, fixed=TRUE)) && identical(flip, x$flip))
               if (any(seqident)) {
                  eval(expr=switch1)
                  cat(.text("seqsmatchfen"))
                  tab <- data.frame(Name=files.all[seqident])
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  rownames(tab) <- which(seqident)
                  print(tab, print.gap=2)
                  selmatches <- readline(prompt=.text("selmatches"))
                  if (identical(selmatches, "") || .confirm(selmatches)) {
                     cat(.text("selmatchesconfirm", sum(seqident)))
                     selected <- files.all[seqident]
                     run.rnd <- FALSE
                     input <- FALSE
                     seqno <- 1
                  }
                  eval(expr=switch2)
               } else {
                  .texttop(.text("noseqsfound"), sleep=1.5)
                  .texttop(texttop)
               }
               next
            }

            # " to find all sequences that end on the same FEN

            if (identical(click, "\"")) {
               if (i == 1)
                  next
               eval(expr=switch1)
               searchterm <- .genfen(pos, flip, sidetoplay, i)
               searchterm <- paste(strsplit(searchterm, " ", fixed=TRUE)[[1]][1:3], collapse=" ")
               seqident <- sapply(dat.all, function(x) grepl(searchterm, tail(x$moves$fen, 1), fixed=TRUE) && identical(flip, x$flip))
               if (any(seqident)) {
                  eval(expr=switch1)
                  cat(.text("seqsmatchfen"))
                  tab <- data.frame(Name=files.all[seqident])
                  tab$Name <- format(tab$Name, justify="left")
                  names(tab)[1] <- ""
                  rownames(tab) <- which(seqident)
                  print(tab, print.gap=2)
                  selmatches <- readline(prompt=.text("selmatches"))
                  if (identical(selmatches, "") || .confirm(selmatches)) {
                     cat(.text("selmatchesconfirm", sum(seqident)))
                     selected <- files.all[seqident]
                     run.rnd <- FALSE
                     input <- FALSE
                     seqno <- 1
                  }
                  eval(expr=switch2)
               } else {
                  .texttop(.text("noseqsfound"), sleep=1.5)
                  .texttop(texttop)
               }
               next
            }

            ################################################################

            ### function keys

            # F1 to show the help overlay

            if (identical(click, "F1")) {
               oldlang <- lang
               .showhelp(lwd=lwd)
               lang <- .get("lang")
               if (oldlang != lang) {
                  settings$lang <- lang # in case lang was switched in help menu
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
               }
               .redrawpos(pos, flip=flip)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               next
            }

            # F2 to show the leaderboard and player statistics

            if (identical(click, "F2")) {
               tmp <- .leaderboard(seqdir[seqdirpos], files, lwd)
               if (tmp == 0) {
                  .texttop(.text("noleader", selmode), sleep=1.5)
                  .texttop(texttop)
               } else {
                  .redrawpos(pos, flip=flip)
                  .drawcircles(circles, lwd=lwd)
                  .drawarrows(arrows, lwd=lwd)
                  .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               }
               next
            }

            # F3 to print the settings

            if (identical(click, "F3")) {
               tab <- list(lang=lang, player=player, mode=mode, seqdir=seqdir[seqdirpos], selmode=selmode, zenmode=zenmode, timed=timed, timepermove=timepermove, expval=expval,
                           multiplier=multiplier, adjustwrong=adjustwrong, adjusthint=adjusthint, eval=eval, evalsteps=evalsteps, wait=wait, sleep=sleep, idletime=idletime, mar=mar, mar2=mar2, lwd=lwd,
                           volume=volume, showgraph=showgraph, repmistake=repmistake, cex.top=cex.top, cex.bot=cex.bot, cex.eval=cex.eval, difffun=difffun, difflen=difflen, diffmin=diffmin,
                           sfpath=sfpath, depth1=depth1, depth2=depth2, depth3=depth3, sflim=sflim, multipv1=multipv1, multipv2=multipv2, threads=threads, hash=hash, hintdepth=hintdepth)
               .showsettings(tab)
               .redrawpos(pos, flip=flip)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               next
            }

            # F4 to adjust the colors

            if (identical(click, "F4")) {
               eval(expr=switch1)
               .colorsettings(cols.all, pos, flip, mode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, lwd, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               eval(expr=switch2)
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               cols <- sapply(cols.all, function(x) .get(x))
               saveRDS(cols, file=file.path(configdir, "colors.rds"))
               next
            }

            # F5 to adjust the cex values

            if (identical(click, "F5")) {
               eval(expr=switch1)
               .cexsettings(pos, flip, mode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, lwd, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               eval(expr=switch2)
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
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
               tmp <- .miscsettings(multiplier, adjustwrong, adjusthint, evalsteps, timepermove, idletime)
               eval(expr=switch2)
               multiplier  <- tmp$multiplier
               adjustwrong <- tmp$adjustwrong
               adjusthint  <- tmp$adjusthint
               evalsteps   <- tmp$evalsteps
               timepermove <- tmp$timepermove
               idletime    <- tmp$idletime
               settings$multiplier  <- multiplier
               settings$adjustwrong <- adjustwrong
               settings$adjusthint  <- adjusthint
               settings$evalsteps   <- evalsteps
               settings$timepermove <- timepermove
               settings$idletime    <- idletime
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F7 to adjust the Stockfish settings

            if (identical(click, "F7")) {
               eval(expr=switch1)
               tmp <- .sfsettings(sfproc, sfrun, sfpath, depth1, depth2, depth3, sflim, multipv1, multipv2, threads, hash, hintdepth)
               eval(expr=switch2)
               sfproc    <- tmp$sfproc
               sfrun     <- tmp$sfrun
               sfpath    <- tmp$sfpath
               depth1    <- tmp$depth1
               depth2    <- tmp$depth2
               depth3    <- tmp$depth3
               sflim     <- tmp$sflim
               multipv1  <- tmp$multipv1
               multipv2  <- tmp$multipv2
               threads   <- tmp$threads
               hash      <- tmp$hash
               hintdepth <- tmp$hintdepth
               settings$sfpath    <- sfpath
               settings$depth1    <- depth1
               settings$depth2    <- depth2
               settings$depth3    <- depth3
               settings$sflim     <- sflim
               settings$multipv1  <- multipv1
               settings$multipv2  <- multipv2
               settings$threads   <- threads
               settings$hash      <- hash
               settings$hintdepth <- hintdepth
               saveRDS(settings, file=file.path(configdir, "settings.rds"))
               next
            }

            # F8 to select / manage sequence directories

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
                  selected <- NULL
                  seqno <- 1
                  filename <- ""
                  lastseq  <- ""
                  bestmove <- list("")
                  useflip  <- TRUE
                  replast  <- FALSE
                  oldmode  <- ifelse(mode %in% c("play","analysis"), "add", mode)
                  session.seqsplayed <- c(session.seqsplayed, 0)
                  session.mean.scores <- c(session.mean.scores, list(NULL))
                  session.length <- session.length + 1
                  selmodeold <- selmode
                  selmode <- .loadselmode(seqdir, seqdirpos, selmode, texttop=TRUE)
                  if (selmodeold != selmode && !file.exists(file.path(seqdir[seqdirpos], ".sequential")))
                     .texttop(.text("selmodeswitch", selmode), sleep=1.5)
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

            # F10 to show the histograms / scatterplot

            if (identical(click, "F10")) {
               if (k <= 1L) {
                  .texttop(.text("toofewscores"), sleep=1.5)
                  .texttop(texttop)
                  next
               }
               if (sum(rounds.selected) == 0) {
                  .texttop(.text("toofewplays"), sleep=1.5)
                  .texttop(texttop)
                  next
               }
               .distributions(scores.selected, rounds.selected, age.selected, difficulty.selected, multiplier, lwd=lwd, mar=mar)
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               next
            }

            # F11 (or ctrl-1) to show the session info

            if (identical(click, "F11") || identical(click, "ctrl-!")) {
               if (sum(session.seqsplayed) <= 1) {
                  .texttop(.text("toofewseqsplayed"), sleep=1.5)
                  .texttop(texttop)
                  next
               }
               session.playtime <- round(proc.time()[[3]] - session.time.start)
               tmp <- .sessiongraph(session.seqsplayed, session.mean.scores, session.playtime, lwd=lwd, mar=mar, mar2=mar2)
               if (!identical(mar2, tmp$mar2)) {
                  mar2 <- tmp$mar2
                  settings$mar2 <- mar2
                  saveRDS(settings, file=file.path(configdir, "settings.rds"))
               }
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               next
            }

            # F12 (or ctrl-2) to show the session history graph

            if (identical(click, "F12") || identical(click, "ctrl-@")) {
               player.file <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions", paste0(player, ".rds"))
               if (file.exists(player.file)) {
                  session.playtime <- round(proc.time()[[3]] - session.time.start)
                  tmp <- .historygraph(player, session.date.start, session.playtime, sum(session.seqsplayed), lwd=lwd, mar=mar, mar2=mar2)
                  if (!identical(mar2, tmp$mar2)) {
                     mar2 <- tmp$mar2
                     settings$mar2 <- mar2
                     saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  }
               } else {
                  .texttop(.text("nosessionhistory"), sleep=1.5)
                  .texttop(texttop)
                  next
               }
               .redrawall(pos, flip, mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, texttop, sidetoplay, selmode, k, seqno, timed, movestoplay, movesplayed, timetotal, timepermove, mar)
               .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               .drawcircles(circles, lwd=lwd)
               .drawarrows(arrows, lwd=lwd)
               .drawarrows(harrows, lwd=lwd, hint=TRUE, evalvals=evalvals, sidetoplay=sidetoplay)
               next
            }

            # ctrl-E to edit the session history file

            if (identical(click, "ctrl-E")) {
               player.file <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions", paste0(player, ".rds"))
               if (file.exists(player.file)) {
                  dat.player <- readRDS(player.file)
                  dosave <- FALSE
                  eval(expr=switch1)
                  while (TRUE) {
                     print(dat.player)
                     cat(.text("edithistory"))
                     rowvals <- readline(prompt="")
                     # enter to exit
                     if (identical(rowvals , ""))
                        break
                     # 'number - number' entered
                     tmp <- strcapture("^([[:digit:]]+)\\s*-\\s*([[:digit:]]+)$", rowvals, data.frame(row.lo=integer(), row.hi=integer()))
                     if (!is.na(tmp$row.lo)) {
                        if (tmp$row.lo < 1L || tmp$row.lo > nrow(dat.player) || tmp$row.hi < tmp$row.lo || tmp$row.hi > nrow(dat.player)) {
                           next
                        } else {
                           dat.player <- dat.player[-(tmp$row.lo:tmp$row.hi),]
                           rownames(dat.player) <- NULL
                           dosave <- TRUE
                           next
                        }
                     }
                     # 'number' entered
                     if (grepl("^[0-9]+$", rowvals)) {
                        rowval <- as.numeric(rowvals)
                        if (rowval < 1 || rowval > nrow(dat.player)) {
                           next
                        } else {
                           dat.player <- dat.player[-rowval,]
                           rownames(dat.player) <- NULL
                           dosave <- TRUE
                           next
                        }
                     }
                  }
                  eval(expr=switch2)
                  if (dosave)
                     saveRDS(dat.player, file=player.file)
                  next
               } else {
                  .texttop(.text("nosessionhistory"), sleep=1.5)
                  .texttop(texttop)
                  next
               }
            }

            ##################################################################

            # if click is an actual click (and drag) on the board

            if (verbose) {
               cat("(click1.x, click1.y): ", click1.x, ", ", click1.y, sep="")
               cat("\n")
               cat("(click2.x, click2.y): ", click2.x, ", ", click2.y, sep="")
               cat("\n")
            }

            # just in case a click is not registered

            if (is.null(click1.x) || is.null(click2.x) || is.null(click1.y) || is.null(click2.y))
               next

            if (is.na(click1.x) || is.na(click2.x) || is.na(click1.y) || is.na(click2.y))
               next

            if (click1.x == click2.x && click1.y == click2.y) {

               # if the start and end positions are the same

               if (identical(button, 2L)) {

                  # if the right button was pressed, then draw a circle on the square or if the square already has a circle, remove it

                  hascircle <- apply(circles, 1, function(x) isTRUE(x[1] == click1.x && x[2] == click1.y))

                  if (any(hascircle)) {
                     .drawsquare(click1.x, click1.y)
                     .drawpiece(click1.x, click1.y, ifelse(flip, pos[9-click1.x,9-click1.y], pos[click1.x,click1.y]))
                     circles <- circles[!hascircle,,drop=FALSE]
                     checkpos <- as.numeric(.get("checkpos"))
                     if (identical(checkpos, c(click1.x, click1.y)))
                        .drawcheck(pos, flip=flip)
                  } else {
                     .drawcircle(click1.x, click1.y, lwd=lwd)
                     circles <- rbind(circles, c(click1.x, click1.y))
                  }

                  next

               }

            }

            input <- FALSE

         }

         if (!run.rnd)
            next # jumps to [b] but since run.rnd is FALSE then jumps to [a]

         # if we are here, then the start and end positions of the mouse move are not the same

         # if the right button was used for the move, draw an arrow and go back to [b]

         if (identical(button, 2L)) {
            .drawarrow(click1.x, click1.y, click2.x, click2.y, lwd=lwd)
            arrows <- rbind(arrows, c(click1.x, click1.y, click2.x, click2.y))
            drawcircles  <- FALSE # to prevent circles from being redrawn
            drawarrows   <- FALSE # to prevent arrows from being redrawn
            showstartcom <- FALSE # to prevent the start comment from being shown again
            next
         }

         # if using <right> all the way to the end of a sequence, then i > nrow(sub$moves) which causes an error when trying to move a piece

         if (mode == "test" && i > nrow(sub$moves))
            next

         drawcircles  <- TRUE
         drawarrows   <- TRUE
         showstartcom <- TRUE

         # if the left button was used for the move and there are arrows/circles, remove the annotations before making the move

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
            evalvals <- c()
         }

         # check that the move is legal

         islegal <- .islegal(click1.x, click1.y, click2.x, click2.y, pos, flip, sidetoplay)

         if (!islegal)
            next

         # check if the king is in check before the move

         if (sidetoplay == "w") {
            check.before <- .isattacked(pos, xy=c(which(pos=="WK", arr.ind=TRUE)), attackcolor="b")
         } else {
            check.before <- .isattacked(pos, xy=c(which(pos=="BK", arr.ind=TRUE)), attackcolor="w")
         }

         # try to make the move (note: using autoprom=TRUE and "=Q" to autopromote to queen, but this is only
         # relevant if a pawn move for promotion would put the king in check, which would not be a legal move)

         tmp <- .updateboard(pos, move=data.frame(click1.x, click1.y, click2.x, click2.y, NA, "=Q"), flip=flip, autoprom=TRUE, volume=0, verbose=verbose, draw=FALSE)

         ischeck.after <- attr(tmp,"ischeck")

         # checks to do if castling

         if (startsWith(attr(tmp,"move"), "0-0")) {

            # check if castling from a checked position (not a legal move)

            if (check.before)
               islegal <- FALSE

            # check that the king does not pass through an attacked square (not a legal move)

            if (sidetoplay == "w" && attr(tmp,"move") == "0-0" && .isattacked(pos, xy=c(1,6), attackcolor="b"))
               islegal <- FALSE
            if (sidetoplay == "w" && attr(tmp,"move") == "0-0-0" && .isattacked(pos, xy=c(1,4), attackcolor="b"))
               islegal <- FALSE
            if (sidetoplay == "b" && attr(tmp,"move") == "0-0" && .isattacked(pos, xy=c(8,6), attackcolor="w"))
               islegal <- FALSE
            if (sidetoplay == "b" && attr(tmp,"move") == "0-0-0" && .isattacked(pos, xy=c(8,4), attackcolor="w"))
               islegal <- FALSE

         }

         # check if the king is in check after the move (not a legal move)

         if (sidetoplay == "w" && ischeck.after[1])
            islegal <- FALSE
         if (sidetoplay == "b" && ischeck.after[2])
            islegal <- FALSE

         if (!islegal)
            next

         if (mode %in% c("add","play","analysis")) {

            # if in add, play, or analysis mode, make the move

            pos <- .updateboard(pos, move=data.frame(click1.x, click1.y, click2.x, click2.y, NA, NA), flip=flip, autoprom=FALSE, volume=volume, verbose=verbose)
            texttop <- .texttop("")

         }

         if (mode == "test") {

            domistake <- TRUE

            # if in test mode, check that the move is correct (and also check that a promotion is made correctly)

            if (all(c(click1.x == sub$moves$x1[i], click1.y == sub$moves$y1[i], click2.x == sub$moves$x2[i], click2.y == sub$moves$y2[i]))) {
               tmp <- .updateboard(pos, move=data.frame(click1.x, click1.y, click2.x, click2.y, sub$moves[i,5:6]), flip=flip, autoprom=FALSE, volume=volume, verbose=verbose)
               if (!identical(tmp, "prommistake")) {
                  movesplayed <- movesplayed + 1
                  timenow <- proc.time()[[3]]
                  timetotal <- timetotal + (timenow - timestart)
                  if (timed) .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
                  domistake <- FALSE
                  pos <- tmp
                  .draweval(sub$moves$eval[i], sub$moves$eval[i-1], i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                  sub$moves$move[i] <- attr(pos,"move")
               }
            }

            # if the move was incorrect, adjust the score, and show that it was the wrong move

            if (domistake) {
               mistake <- TRUE
               if (score >= 1) {
                  scoreadd <- min(adjustwrong, 100-score)
                  score <- score + scoreadd
               }
               .textbot(mode, zenmode, score=score, onlyscore=TRUE)
               .rmrect(click1.x, click1.y, lwd=lwd)
               .addrect(click2.x, click2.y, col=.get("col.wrong"), lwd=lwd)
               playsound(system.file("sounds", "error.ogg", package="chesstrainer"), volume=volume)
               Sys.sleep(sleep/2)
               .rmrect(click2.x, click2.y, lwd=lwd)
               next
            }

            i <- i + 1
            sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

            if (i > nrow(sub$moves)) {

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

               } else {

                  sideindicator <- .drawsideindicator(sidetoplay, flip=flip)

               }

               playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)

               # show end comment (either default or commentend)

               if (is.null(sub$commentend)) {
                  if (mistake) {
                     .texttop(.text("keeppracticing"))
                  } else {
                     .texttop(.text("welldone"))
                  }
               } else {
                  .texttop(sub$commentend)
               }

               # show symbolend if it is not NULL

               if (!is.null(sub$symbolend)) {
                  circles <- .parseannot(sub$symbolend$circles, cols=2)
                  arrows  <- .parseannot(sub$symbolend$arrows, cols=4)
                  .drawcircles(circles, lwd=lwd)
                  .drawarrows(arrows, lwd=lwd)
               }

               if (!wait && (!is.null(sub$commentend) || !is.null(sub$symbolend)))
                  .waitforclick()

               lastseq <- seqname

               if (mistake && repmistake) {
                  replast <- TRUE
                  filename <- seqname
               }

               # adjust the score (but only if no mistake was made)

               if (!mistake && score > 1)
                  score <- round(score * multiplier)

               rounds <- rounds + 1

               tmp <- data.frame(date=as.numeric(Sys.time()), round=rounds, score=score)

               if (is.null(sub$player[[player]])) {
                  sub$player[[player]] <- tmp
               } else {
                  sub$player[[player]] <- rbind(sub$player[[player]], tmp)
               }

               difficulty <- .difffun(sub$player[[player]]$score, difflen, diffmin, adjusthint, multiplier)

               .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age=0, difficulty, i, totalmoves, selmode, k, seqno)

               # increase session.seqsplayed and compute session.mean.scores

               session.seqsplayed[session.length] <- session.seqsplayed[session.length] + 1
               session.mean.scores[[session.length]] <- c(session.mean.scores[[session.length]], mean(scores.all, na.rm=TRUE))

               if (showgraph) {
                  tmp <- .progressgraph(sub$player[[player]], lwd=lwd, mar=mar, mar2=mar2)
                  if (!identical(mar2, tmp$mar2)) {
                     mar2 <- tmp$mar2
                     settings$mar2 <- mar2
                     saveRDS(settings, file=file.path(configdir, "settings.rds"))
                  }
                  .redrawpos(pos, flip=flip)
               }

               if (wait) {

                  skipsave <- FALSE
                  contplay <- FALSE

                  while (TRUE) {

                     if (!eval[[mode]]) # always show evaluation at end even if eval is FALSE
                        .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=TRUE, evalsteps=evalsteps)

                     click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=function(button,x,y) return(c(x,y,button)), onKeybd=.keyfun)

                     if (is.numeric(click) && identical(click[3], 0)) # left button goes to next sequence
                        break

                     if (identical(click, "r") || (is.numeric(click) && identical(click[3], 1))) { # r or middle button repeats the sequence
                        saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                        .texttop(.text("replast"), sleep=0.75)
                        replast <- TRUE
                        filename <- seqname
                        break
                     }

                     if (identical(click, "n")) { # n goes to next sequence without saving
                        skipsave <- TRUE
                        break
                     }

                     if (identical(click, "a") || identical(click, "A") || (is.numeric(click) && identical(click[3], 2))) { # a/A and right mouse button goes to add mode
                        saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                        .rmannot(pos, circles, rbind(arrows, harrows), flip)
                        oldmode <- "test"
                        mode <- "add"
                        sub$player <- NULL
                        show <- FALSE
                        texttop <- .texttop("")
                        .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                        sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
                        fen <- .genfen(pos, flip, sidetoplay, i)
                        res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                        evalval  <- res.sf$eval
                        bestmove <- res.sf$bestmove
                        matetype <- res.sf$matetype
                        sfproc   <- res.sf$sfproc
                        sfrun    <- res.sf$sfrun
                        .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                        contplay <- TRUE
                        break
                     }

                     if (identical(click, "\\") || identical(click, "#")) {
                        saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                        .rmannot(pos, circles, rbind(arrows, harrows), flip)
                        sub$moves <- sub$moves[seq_len(i-1),,drop=FALSE]
                        oldmode <- "test"
                        mode <- "play"
                        show <- FALSE
                        timed <- FALSE
                        texttop <- .texttop("")
                        .draweval(sub$moves$eval[i-1], NA, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                        sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
                        fen <- .genfen(pos, flip, sidetoplay, i)
                        if ((flip && sidetoplay=="b") || (!flip && sidetoplay=="w")) {
                           res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
                        } else {
                           res.sf <- .sf.eval(sfproc, sfrun, depth3, multipv1, sflim, fen, sidetoplay, verbose)
                        }
                        evalval  <- res.sf$eval
                        bestmove <- res.sf$bestmove
                        matetype <- res.sf$matetype
                        sfproc   <- res.sf$sfproc
                        sfrun    <- res.sf$sfrun
                        .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                        contplay <- TRUE
                        break
                     }

                     if (identical(click, "o")) {
                        score <- .setscore(score, lwd)
                        .redrawpos(pos, flip=flip)
                        .textbot(mode, zenmode, score=score, onlyscore=TRUE)
                        .drawcircles(circles, lwd=lwd)
                        .drawarrows(arrows, lwd=lwd)
                        sub$player[[player]]$score[length(sub$player[[player]]$score)] <- score
                        saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))
                     }

                     if (identical(click, "<")) {
                        bookmark <- seqname
                        if (file.exists(file.path(seqdir[seqdirpos], ".bookmarks"))) {
                           tmp <- try(read.table(file.path(seqdir[seqdirpos], ".bookmarks"), header=FALSE), silent=TRUE)
                           if (inherits(tmp, "try-error")) {
                              bookmarks <- bookmark
                           } else {
                              bookmarks <- unique(c(bookmark, tmp[[1]]))
                              bookmarks <- bookmarks[is.element(bookmarks, list.files(seqdir[seqdirpos], pattern=".rds$"))]
                           }
                        } else {
                           bookmarks <- bookmark
                        }
                        write.table(data.frame(bookmarks), file=file.path(seqdir[seqdirpos], ".bookmarks"), col.names=FALSE, row.names=FALSE, quote=FALSE)
                        .texttop(.text("bookmarked", bookmark), sleep=1.5)
                     }

                     if (identical(click, "F9")) {
                        eval(expr=switch1)
                        fen <- .genfen(pos, flip, sidetoplay, i)
                        cat(fen, "\n")
                        eval(expr=switch2)
                        fen <- paste0("https://lichess.org/analysis/standard/", gsub(" ", "_", fen, fixed=TRUE))
                        browseURL(fen)
                     }

                     if (identical(click, "ctrl-F")) {
                        eval(expr=switch1)
                        fen <- .genfen(pos, flip, sidetoplay, i+1)
                        cat(fen, "\n")
                        eval(expr=switch2)
                        clipr::write_clip(fen, object_type="character")
                        .texttop(.text("copyfen"), sleep=0.75)
                     }

                     if (identical(click, "ctrl-C")) {
                        eval(expr=switch1)
                        cat(seqnum, " ", seqname, "\n")
                        clipr::write_clip(seqname, object_type="character")
                        eval(expr=switch2)
                     }

                     if (identical(click, "g")) {
                        tmp <- .progressgraph(sub$player[[player]], lwd=lwd, mar=mar, mar2=mar2)
                        if (!identical(mar2, tmp$mar2)) {
                           mar2 <- tmp$mar2
                           settings$mar2 <- mar2
                           saveRDS(settings, file=file.path(configdir, "settings.rds"))
                        }
                        .redrawpos(pos, flip=flip)
                        .drawcircles(circles, lwd=lwd)
                        .drawarrows(arrows, lwd=lwd)
                     }

                     if (identical(click, "ctrl-V")) {
                        eval(expr=switch1)
                        .printverbose(selected, seqno, filename, lastseq, flip, useflip, replast, oldmode, i, seqname, seqnum, score, rounds, totalmoves, show, comment, bestmove, starteval, evalval, texttop, scoreadd, sidetoplay, givehint1, givehint2, mistake, timetotal, movesplayed, movestoplay, drawcircles, drawarrows, showstartcom, pos)
                        eval(expr=switch2)
                     }

                  }

                  if (!replast) {
                     seqno <- seqno + 1
                     if (seqno > k)
                        seqno <- 1
                  }

                  if (skipsave) # to skip saving when 'n' was pressed
                     break

                  if (contplay)
                     next

               }

               saveRDS(sub, file=file.path(seqdir[seqdirpos], seqname))

               if (selmode %in% c("sequential","sequential_len","sequential_mov","age_oldest") && k > 1L && seqno == 1) {
                  playsound(system.file("sounds", "finished.ogg", package="chesstrainer"), volume=volume)
                  .texttop(.text("finishedround"), sleep=2)
               } else {
                  Sys.sleep(2*sleep)
               }

               run.rnd <- FALSE
               next

            }

         } else {

            i <- i + 1
            sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

         }

         if (mode %in% c("add","play","analysis")) {

            # after the move has been made, get the evaluation and the next bestmove, using depth1 in add/analysis mode and depth3 (with sflim) in play mode

            evalvallast <- evalval[1]
            fen <- .genfen(pos, flip, sidetoplay, i)
            if (mode %in% c("add","analysis")) {
               res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
            } else {
               res.sf <- .sf.eval(sfproc, sfrun, depth3, multipv1, sflim, fen, sidetoplay, verbose)
            }
            evalval  <- res.sf$eval
            bestmove <- res.sf$bestmove # [d]
            matetype <- res.sf$matetype
            sfproc   <- res.sf$sfproc
            sfrun    <- res.sf$sfrun

            # in play mode, have to run .sf.eval() one more time, to base the actual evaluation on depth1 (and without sflim) and not depth3

            if (mode == "play" && (!is.na(sflim) || depth1 > depth3)) {
               res.sf <- .sf.eval(sfproc, sfrun, depth1, multipv1, sflim=NA, fen, sidetoplay, verbose)
               evalval  <- res.sf$eval
               matetype <- res.sf$matetype
               sfproc   <- res.sf$sfproc
               sfrun    <- res.sf$sfrun
            }

            .draweval(evalval[1], evalvallast, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)

            if (is.null(sub$moves$circles))
               sub$moves$circles <- ""
            if (is.null(sub$moves$arrows))
               sub$moves$arrows <- ""
            if (is.null(sub$moves$fen))
               sub$moves$fen <- ""

            # if we are in analysis mode and make a move (i.e., we go into a new variation), then cut away the previous variation

            if (mode == "analysis")
               sub$moves <- sub$moves[seq_len(i-2),,drop=FALSE]

            # determine the correct value for 'show' in sub

            if (mode == "add") {
               if (flip) {
                  showval <- ifelse(sidetoplay == "b", TRUE, show)
               } else {
                  showval <- ifelse(sidetoplay == "w", TRUE, show)
               }
            } else {
               if (flip) {
                  showval <- sidetoplay == "b"
               } else {
                  showval <- sidetoplay == "w"
               }
            }

            # add the current move to sub

            sub$moves <- sub$moves[seq_len(i-2),]
            sub$moves <- rbind(sub$moves, data.frame(x1=click1.x, y1=click1.y, x2=click2.x, y2=click2.y, show=showval, move=attr(pos,"move"), eval=evalval[1], comment=comment, circles=circlesvar, arrows=arrowsvar, fen=fen))
            comment <- ""

            # check for (stale)mate

            if (mode %in% c("play","analysis") && !identical(matetype, "none")) {
               playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
               .texttop(.text(matetype))
               mode <- "analysis"
               .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
               .draweval(evalval[1], evalvallast, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
               next
            }

            # check for threefold repetition

            threefold <- any(table(sapply(strsplit(sub$moves$fen, " "), function(x) paste(x[1:4], collapse = " "))) == 3L)

            if (threefold) {
               .texttop(.text("threefold"))
               if (mode %in% c("play","analysis")) {
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  mode <- "analysis"
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                  .draweval(evalval[1], evalvallast, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                  next
               }
            }

            # check fifty-move rule

            fifty <- identical(strsplit(fen, " ")[[1]][5], "100")

            if (fifty) {
               .texttop(.text("fifty"))
               if (mode %in% c("play","analysis")) {
                  playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), volume=volume)
                  mode <- "analysis"
                  .textbot(mode, zenmode, show, player, seqname, seqnum, score, rounds, age, difficulty, i, totalmoves, selmode, k, seqno)
                  .draweval(evalval[1], evalvallast, i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
                  next
               }
            }

         }

         if (mode == "test") {

            # in test mode, let the trainer play the next move and increase i

            .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
            if (timed) {
               .drawtimer(movestoplay, movesplayed, timetotal, timepermove)
            } else {
               sideindicator <- .drawsideindicator(sidetoplay, flip=flip)
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
            sub$moves$move[i] <- attr(pos,"move")
            .draweval(sub$moves$eval[i], sub$moves$eval[i-1], i=i, starteval=starteval, flip=flip, eval=eval[[mode]], evalsteps=evalsteps)
            texttop <- .texttop(sub$moves$comment[i])
            if (nrow(circles) >= 1L || nrow(arrows) >= 1L) {
               .rmannot(pos, circles, arrows, flip)
               circles <- matrix(nrow=0, ncol=2)
               arrows  <- matrix(nrow=0, ncol=4)
            }
            i <- i + 1
            sidetoplay <- ifelse(sidetoplay == "w", "b", "w")

            if (isTRUE(sub$moves$show[i]))
               Sys.sleep(sleep)

         }

         .textbot(mode, zenmode, i=i, totalmoves=totalmoves, onlyi=TRUE)
         givehint1 <- FALSE
         givehint2 <- FALSE

         # go to [b]

      } # end of while (run.rnd) {}

      # go to [a]

   } # end of while (run.all) {}

   return(invisible())

}
