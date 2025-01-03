.sf.start <- function(sfproc=NULL, sfrun=FALSE, sfpath) {

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
   }

   return(list(sfproc=sfproc, sfrun=sfrun))

}

.sf.ready <- function(sfproc) {

   sfproc$write_input("isready\n")

   repeat {
      sfout <- sfproc$read_output_lines()
      if (length(sfout) > 0) {
         if (any(grepl("readyok", sfout)))
            break
      }
   }

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

.sf.eval <- function(sfproc, sfrun, sfgo, fen, sidetoplay, verbose) {

   if (sfrun) {

      .sf.newgame(sfproc, sfrun)
      sfproc$write_input(paste("position fen", fen, "\n"))
      sfproc$write_input(paste0("go ", sfgo, "\n"))

      sfout <- c()

      repeat {
         sfout <- c(sfout, sfproc$read_output_lines())
         if (any(grepl("bestmove", sfout))) {
            movepos <- grep("bestmove", sfout)
            bestmove <- strsplit(sfout[movepos], " ")[[1]][2]
            cppos <- grep("score cp", sfout)
            if (length(cppos) >= 1L) {
               sflast <- sfout[max(cppos)]
               cp <- strcapture("score cp ([-]*[[:digit:]]+)", sflast, data.frame(x=numeric()))$x
            } else {
               cp <- NA_real_
            }
            matepos <- grep("score mate", sfout)
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

   } else {

      eval <- NA_real_
      bestmove <- ""

   }

   if (verbose) {
      #cat("\nSF:   ", sfout, "\n", sep="")
      print(sfout)
      cat("FEN:  ", fen, "\n", sep="")
      cat("Eval: ", eval, "\n", sep="")
      cat("Best: ", bestmove, "\n", sep="")
   }

   return(list(eval=eval, bestmove=bestmove))

}

.sfsettings <- function(sfproc, sfrun, sfpath, sfgo) {

   cat(.text("sfrunning", sfrun))
   cat(.text("sfpath", sfpath))
   cat(.text("sfgo", sfgo))

   cat("\n")
   cat(.text("sfoptions"))

   while (TRUE) {
      resp <- readline(prompt=.text("sfoptionwhich"))
      if (identical(resp, ""))
         break
      if (grepl("^[1-9]$", resp)) {
         resp <- round(as.numeric(resp))
         if (resp < 1 || resp > 5)
            next
         if (identical(resp, 1)) {
            # (re)start stockfish
            cat("\n")
            tmp <- .sf.start(sfproc, sfrun, sfpath)
            sfproc <- tmp$sfproc
            sfrun  <- tmp$sfrun
         }
         if (identical(resp, 2)) {
            # stop stockfish
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
            # set calculation parameter
            oldsfgo <- sfgo
            cat("\n")
            sfgo <- readline(prompt=.text("sfentergo"))
            if (identical(sfgo, "")) {
               sfgo <- oldsfgo
            } else {
               if (grepl("^depth\\s[[:digit:]]+$", sfgo) || grepl("^movetime\\s[[:digit:]]+$", sfgo)) {
                  cat(.text("sfgosuccess"))
               } else {
                  cat(.text("sfgofail"))
                  sfgo <- oldsfgo
               }
            }
         }
         if (identical(resp, 5)) {
            # show settings
            cat("\n")
            cat(.text("sfrunning", sfrun))
            cat(.text("sfpath", sfpath))
            cat(.text("sfgo", sfgo))
            cat("\n")
            cat(.text("sfoptions"))
         }
      }
   }

   return(list(sfproc=sfproc, sfrun=sfrun, sfpath=sfpath, sfgo=sfgo))

}
