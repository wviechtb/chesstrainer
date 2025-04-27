.sf.start <- function(sfproc=NULL, sfrun=FALSE, sfpath, threads, hash) {

   tmp <- .sf.stop(sfproc, sfrun)
   sfproc <- tmp$sfproc
   sfrun  <- tmp$sfrun

   if (sfpath != "") {
      cat(.text("sfstart"))
      sfproc <- try(process$new(sfpath, stdin = "|", stdout = "|", stderr = "2>&1"), silent=TRUE)
      if (inherits(sfproc, "try-error")) {
         cat(.text("sfstarterror"))
      } else {
         sfrun <- TRUE
         cat(.text("sfstartsuccess"))
      }
   }

   if (sfrun) {
      sfproc$write_input("uci\n")
      .sf.ready(sfproc)
      .sf.setoptions(sfproc, threads, hash)
   }

   return(list(sfproc=sfproc, sfrun=sfrun))

}

.sf.ready <- function(sfproc) {

   sfproc$write_input("isready\n")

   repeat {
      sfout <- sfproc$read_output_lines()
      if (length(sfout) > 0) {
         if (any(grepl("readyok", sfout, fixed=TRUE)))
            break
      }
   }

   return()

}

.sf.setoptions <- function(sfproc, threads, hash) {

   sfproc$write_input(paste0("setoption name Threads value ", threads, "\n"))
   Sys.sleep(0.2)
   sfproc$write_input(paste0("setoption name Hash value ", hash, "\n"))
   Sys.sleep(0.2)
   return()

}

.sf.stop <- function(sfproc, sfrun) {

   if (!is.null(sfproc) && sfrun) {
      cat(.text("sfstop"))
      tmp <- try(sfproc$kill(), silent=TRUE)
      if (inherits(tmp, "try-error")) {
         cat(.text("sfstoperror"))
      } else {
         sfproc <- NULL
         sfrun <- FALSE
         cat(.text("sfstopsuccess"))
      }
   }

   return(list(sfproc=sfproc, sfrun=sfrun))

}

.sf.newgame <- function(sfproc, sfrun) {

   if (!is.null(sfproc) && sfrun) {
      sfproc$write_input("ucinewgame\n")
      .sf.ready(sfproc)
   }

   return()

}

.sf.eval <- function(sfproc, sfrun, depth, multipv, fen, sidetoplay, verbose, progbar=FALSE) {

   sfout    <- NULL
   eval     <- rep(NA_real_, multipv)
   bestmove <- rep("", multipv)
   alive    <- TRUE

   if (progbar)
      col.top <- .get("col.top")

   olddepth <- 0

   if (progbar) {
      rect(1, 9.3, 9, 9.4, col=NA, border=col.top)
      segments(seq(1,9,length.out=depth+1), 9.3, seq(1,9,length.out=depth+1), 9.4, col=adjustcolor(col.top, alpha.f=0.4))
   }

   if (sfrun) {

      .sf.newgame(sfproc, sfrun)
      sfproc$write_input(paste0("setoption name MultiPV value ", multipv, "\n"))
      Sys.sleep(0.1)
      sfproc$write_input(paste("position fen", fen, "\n"))
      sfproc$write_input(paste0("go depth ", depth, "\n"))

      if (alive) {

         repeat {
            alive <- sfproc$is_alive()
            if (!alive) {
               cat(.text("sfsegfault", sfrun))
               break
            }
            sfoutnew <- sfproc$read_output_lines()
            if (length(sfoutnew) == 0L)
               next
            if (verbose)
               sapply(sfoutnew, function(x) cat(x, "\n"))
            sfout <- c(sfout, sfoutnew)
            if (progbar) {
               curdepth <- grep("info depth ", sfout, fixed=TRUE)
               if (length(curdepth) >= 1L) {
                  curdepth <- max(curdepth)
                  curdepth <- strsplit(sfout[curdepth], " ", fixed=TRUE)[[1]][3] # get <number> from 'info depth <number> ...'
                  curdepth <- as.numeric(curdepth) - 1
                  if (curdepth > olddepth) {
                     rect(1, 9.3, 1+curdepth/depth*8, 9.4, col=col.top, border=NA)
                     olddepth <- curdepth
                  }
               }
            }
            if (any(grepl("bestmove ", sfout, fixed=TRUE))) # when we see 'bestmove <move>', then Stockfish is done
               break
         }

      }

   }

   if (!alive) {
      sfproc <- NULL
      sfrun <- FALSE
   }

   if (is.null(sfout) || !sfrun)
      return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

   # check for mate
   if (any(grepl("info depth 0 score mate 0", sfout, fixed=TRUE)))
      return(list(eval=ifelse(sidetoplay == "b", 99.9, -99.9), bestmove="", matetype="mate", sfproc=sfproc, sfrun=sfrun))

   # check for stalemate
   if (any(grepl("info depth 0 score cp 0", sfout, fixed=TRUE)) && any(grepl("bestmove (none)", sfout, fixed=TRUE)))
      return(list(eval=0, bestmove="", matetype="stalemate", sfproc=sfproc, sfrun=sfrun))

   # find positions in output of 'info depth <depth>' (there should be between 1 and 'multipv' such lines)
   infodepthpos <- grep(paste("info depth", depth), sfout, fixed=TRUE)

   if (length(infodepthpos) == 0L) # just in case
      return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

   # restrict sfout to those elements
   sfout <- sfout[infodepthpos]

   for (i in 1:multipv) {
      # find the position of 'multipv <i> score'
      pos <- grep(paste(" multipv", i, "score "), sfout, fixed=TRUE)
      if (length(pos) == 0L)
         next
      if (length(pos) > 1L) # there really should only be one, but just in case
         pos <- max(pos)
      sfoutpos <- sfout[pos]
      # check if there is a mate in x moves
      mateinx <- grepl(" score mate ", sfoutpos, fixed=TRUE)
      # get the best move for the variation
      tmp <- strsplit(sfoutpos, " pv ", fixed=TRUE)[[1]][2]
      bestmove[i] <- strsplit(tmp, " ", fixed=TRUE)[[1]][1]
      if (mateinx) {
         tmp <- strsplit(sfoutpos, " score mate ", fixed=TRUE)[[1]][2]
         matemoves <- as.numeric(strsplit(tmp, " ", fixed=TRUE)[[1]][1])
         eval[i] <- sign(matemoves) * 99.9
      } else {
         tmp <- strsplit(sfoutpos, " score cp ", fixed=TRUE)[[1]][2]
         cpval <- as.numeric(strsplit(tmp, " ", fixed=TRUE)[[1]][1])
         eval[i] <- cpval / 100
      }
   }

   if (sidetoplay == "b")
      eval <- -eval

   if (verbose) {
      cat("\nFEN:  ", fen, "\n")
      cat("Eval: ", eval, "\n")
      cat("Best: ", bestmove, "\n\n")
   }

   return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

}

.sfsettings <- function(sfproc, sfrun, sfpath, depth1, depth2, multipv1, multipv2, threads, hash) {

   while (TRUE) {

      cat("\n")
      cat(.text("sfrunning", sfrun))
      cat(.text("sfpath",    sfpath))
      cat(.text("depths",    depth1, depth2))
      cat(.text("multipvs",  multipv1, multipv2))
      cat(.text("threads",   threads))
      cat(.text("hash",      hash))

      cat("\n")
      cat(.text("sfoptions"))
      cat("\n\n")

      resp <- readline(prompt=.text("sfoptionwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[1-9]$", resp)) {
         resp <- round(as.numeric(resp))
         if (resp < 1 || resp > 7)
            next
         if (identical(resp, 1)) {
            # (re)start Stockfish
            cat("\n")
            tmp <- .sf.start(sfproc, sfrun, sfpath, threads, hash)
            sfproc <- tmp$sfproc
            sfrun  <- tmp$sfrun
         }
         if (identical(resp, 2)) {
            # stop Stockfish
            cat("\n")
            tmp <- .sf.stop(sfproc, sfrun)
            sfproc <- tmp$sfproc
            sfrun  <- tmp$sfrun
         }
         if (identical(resp, 3)) {
            # change path to Stockfish executable
            oldpath <- sfpath
            cat("\n")
            if (.Platform$OS.type == "windows") {
               sfpath <- choose.files(caption="", multi=FALSE)
               if (length(sfpath) == 0L)
                  sfpath <- oldpath
            } else {
               sfpath <- readline(prompt=.text("sfenterpath"))
               if (!identical(sfpath, "")) {
                  sfpath <- suppressWarnings(normalizePath(sfpath))
                  if (file.exists(sfpath)) {
                     cat(.text("sfpathsuccess"))
                  } else {
                     cat(.text("sfpathfail"))
                  }
               }
            }
            if (oldpath != sfpath) {
               # (re)start stockfish if the path is new
               tmp <- .sf.stop(sfproc, sfrun)
               sfproc <- tmp$sfproc
               sfrun  <- tmp$sfrun
               tmp <- .sf.start(sfproc, sfrun, sfpath, threads, hash)
               sfproc <- tmp$sfproc
               sfrun  <- tmp$sfrun
            }
         }
         if (identical(resp, 4)) {
            # set depth1/depth2 parameter
            cat("\n")
            newdepth <- readline(prompt=.text("depthenter"))
            if (identical(newdepth, "")) {
               next
            } else {
               if (grepl("^[0-9]+,\\s*[0-9]+$", newdepth)) {
                  newdepth <- strsplit(newdepth, ",", fixed=TRUE)[[1]]
                  newdepth1 <- round(as.numeric(newdepth[1]))
                  newdepth2 <- round(as.numeric(newdepth[2]))
                  newdepth1 <- max(1, newdepth1)
                  newdepth2 <- max(1, newdepth2)
                  depth1 <- newdepth1
                  depth2 <- newdepth2
                  cat(.text("depthsetsuccess"))
               } else {
                  cat(.text("depthsetfail"))
                  next
               }
            }
         }
         if (identical(resp, 5)) {
            # set multipv1/multipv2 parameter
            cat("\n")
            newmultipv <- readline(prompt=.text("multipventer"))
            if (identical(newmultipv, "")) {
               next
            } else {
               if (grepl("^[0-9]+,\\s*[0-9]+$", newmultipv)) {
                  newmultipv <- strsplit(newmultipv, ",", fixed=TRUE)[[1]]
                  newmultipv1 <- round(as.numeric(newmultipv[1]))
                  newmultipv2 <- round(as.numeric(newmultipv[2]))
                  newmultipv1 <- max(1, newmultipv1)
                  newmultipv2 <- max(1, newmultipv2)
                  multipv1 <- newmultipv1
                  multipv2 <- newmultipv2
                  cat(.text("multipvsetsuccess"))
                  .sf.setoptions(sfproc, threads, hash)
               } else {
                  cat(.text("multipvsetfail"))
                  next
               }
            }
         }
         if (identical(resp, 6)) {
            # set threads parameter
            cat("\n")
            newthreads <- readline(prompt=.text("threadsenter"))
            if (identical(newthreads, "")) {
               next
            } else {
               if (grepl("^[0-9]+$", newthreads)) {
                  newthreads <- round(as.numeric(newthreads))
                  newthreads <- max(1, newthreads)
                  threads <- newthreads
                  cat(.text("threadssetsuccess"))
                  .sf.setoptions(sfproc, threads, hash)
               } else {
                  cat(.text("threadssetfail"))
                  next
               }
            }
         }
         if (identical(resp, 7)) {
            # set hash parameter
            cat("\n")
            newhash <- readline(prompt=.text("hashenter"))
            if (identical(newhash, "")) {
               next
            } else {
               if (grepl("^[0-9]+$", newhash)) {
                  newhash <- round(as.numeric(newhash))
                  newhash <- max(16, newhash)
                  hash <- newhash
                  cat(.text("hashsetsuccess"))
                  .sf.setoptions(sfproc, threads, hash)
               } else {
                  cat(.text("hashsetfail"))
                  next
               }
            }
         }
      }
   }

   return(list(sfproc=sfproc, sfrun=sfrun, sfpath=sfpath, depth1=depth1, depth2=depth2, multipv1=multipv1, multipv2=multipv2, threads=threads, hash=hash))

}
