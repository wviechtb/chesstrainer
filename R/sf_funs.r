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
   sfproc$write_input(paste0("setoption name Hash value 256\n"))
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

.sf.eval <- function(sfproc, sfrun, depth, fen, sidetoplay, verbose, progbar=FALSE) {

   sfout    <- NULL
   eval     <- NA_real_
   bestmove <- ""
   mate     <- NA_real_
   cp       <- NA_real_
   alive    <- TRUE

   if (progbar)
      col.top <- .get("col.top")

   if (sfrun) {

      .sf.newgame(sfproc, sfrun)
      sfproc$write_input(paste("position fen", fen, "\n"))
      sfproc$write_input(paste0("go depth ", depth, "\n"))

      if (alive) {

         repeat {
            alive <- sfproc$is_alive()
            if (!alive) {
               cat(.text("sfsegfault", sfrun))
               break
            }
            sfout <- c(sfout, sfproc$read_output_lines())
            if (progbar) {
               curdepth <- grep("info depth", sfout, fixed=TRUE)
               if (length(curdepth) >= 1L) {
                  curdepth <- max(curdepth)
                  curdepth <- strsplit(sfout[curdepth], " ", fixed=TRUE)[[1]][3]
                  curdepth <- as.numeric(curdepth)
                  rect(1, 9.3, 9, 9.4, col=NA, border=col.top)
                  rect(1, 9.3, max(1,curdepth/depth*8.9), 9.4, col=col.top, border=NA)
               }
            }
            if (any(grepl("bestmove", sfout, fixed=TRUE))) {
               movepos <- grep("bestmove", sfout, fixed=TRUE)
               bestmove <- strsplit(sfout[movepos], " ", fixed=TRUE)[[1]][2]
               cppos <- grep("score cp", sfout, fixed=TRUE)
               if (length(cppos) >= 1L) {
                  sflast <- sfout[max(cppos)]
                  cp <- strcapture("score cp ([-]*[[:digit:]]+)", sflast, data.frame(x=numeric()))$x
               } else {
                  cp <- NA_real_
               }
               matepos <- grep("score mate", sfout, fixed=TRUE)
               if (length(matepos) >= 1L) {
                  sflast <- sfout[max(matepos)]
                  mate <- strcapture("score mate ([-]*[[:digit:]]+)", sflast, data.frame(x=numeric()))$x
               } else {
                  mate <- NA_real_
               }
               break
            }
         }

         if (!is.na(mate)) {
            if (identical(mate, 0)) {
               eval <- -99.9
            } else {
               matesign <- sign(mate)
               eval <- 99.9 * matesign
            }
         } else {
            eval <- cp / 100
         }

         eval <- ifelse(sidetoplay == "w", eval, -eval)

      }

   }

   if (!alive) {
      sfproc <- NULL
      sfrun <- FALSE
   }

   if (verbose) {
      if (!is.null(sfout))
         print(sfout)
      cat("FEN:  ", fen, "\n", sep="")
      if (!is.na(eval))
         cat("Eval: ", eval, "\n", sep="")
      if (bestmove != "")
         cat("Best: ", bestmove, "\n", sep="")
   }

   return(list(eval=eval, bestmove=bestmove, sfproc=sfproc, sfrun=sfrun))

}

.sfsettings <- function(sfproc, sfrun, sfpath, depth1, depth2, threads, hash) {

   cat(.text("sfrunning", sfrun))
   cat(.text("sfpath",    sfpath))
   cat(.text("depth1",    depth1))
   cat(.text("depth2",    depth2))
   cat(.text("threads",   threads))
   cat(.text("hash",      hash))

   cat("\n")
   cat(.text("sfoptions"))

   while (TRUE) {
      cat("\n")
      resp <- readline(prompt=.text("sfoptionwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[1-9]$", resp)) {
         resp <- round(as.numeric(resp))
         if (resp < 1 || resp > 8)
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
            # change path
            oldpath <- sfpath
            cat("\n")
            sfpath <- readline(prompt=.text("sfenterpath"))
            if (identical(sfpath, "")) {
               sfpath <- oldpath
            } else {
               sfpath <- suppressWarnings(normalizePath(sfpath))
               if (file.exists(sfpath)) {
                  cat(.text("sfpathsuccess"))
               } else {
                  cat(.text("sfpathfail"))
                  sfpath <- oldpath
               }
            }
         }
         if (identical(resp, 4)) {
            # set depth1 parameter
            cat("\n")
            newdepth <- readline(prompt=.text("depthenter"))
            if (identical(newdepth, "")) {
               next
            } else {
               if (grepl("^[1-9]+$", newdepth)) {
                  newdepth <- round(as.numeric(newdepth))
                  newdepth <- max(1, newdepth)
                  depth1 <- newdepth
                  cat(.text("depthsetsuccess"))
               } else {
                  cat(.text("depthsetfail"))
                  next
               }
            }
         }
         if (identical(resp, 5)) {
            # set depth2 parameter
            cat("\n")
            newdepth <- readline(prompt=.text("depthenter"))
            if (identical(newdepth, "")) {
               next
            } else {
               if (grepl("^[1-9]+$", newdepth)) {
                  newdepth <- round(as.numeric(newdepth))
                  newdepth <- max(1, newdepth)
                  depth2 <- newdepth
                  cat(.text("depthsetsuccess"))
               } else {
                  cat(.text("depthsetfail"))
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
               if (grepl("^[1-9]+$", newthreads)) {
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
               if (grepl("^[1-9]+$", newhash)) {
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
         if (identical(resp, 8)) {
            # show settings
            cat("\n")
            cat(.text("sfrunning", sfrun))
            cat(.text("sfpath",    sfpath))
            cat(.text("depth1",    depth1))
            cat(.text("depth2",    depth2))
            cat(.text("threads",   threads))
            cat(.text("hash",      hash))
            cat("\n")
            cat(.text("sfoptions"))
         }
      }
   }

   return(list(sfproc=sfproc, sfrun=sfrun, sfpath=sfpath, depth1=depth1, depth2=depth2, threads=threads))

}
