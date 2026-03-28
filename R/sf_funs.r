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

.sf.eval <- function(sfproc, sfrun, depth, multipv, sflim, fen, progbar=FALSE, usesfcache=FALSE) {

   if (usesfcache) {

      cachedir <- .get("cachedir")
      files <- list.files(file.path(cachedir, "stockfish"), pattern=".rds$")
      filessplit <- strsplit(files, "_", fixed=TRUE)
      depths <- as.numeric(sapply(filessplit, function(x) x[1]))
      fileswoutdepth  <- sapply(filessplit, function(x) x[2])
      fenfilename <- gsub(" ", "%20", fen, fixed=TRUE)
      fenfilename <- gsub("/", "%2F", fenfilename, fixed=TRUE)
      fenfilename <- paste0(fenfilename, ".rds")
      if (fenfilename %in% fileswoutdepth) {
         depths <- depths[fenfilename == fileswoutdepth]
         pos <- which(fenfilename == fileswoutdepth)[which.max(depths)]
         file <- file.path(cachedir, "stockfish", files[pos])
         if (depths[which.max(depths)] >= depth) { # read in the cache file if it is of sufficient depth
            out <- readRDS(file)
            lastmod <- file.mtime(file)
            if (difftime(Sys.time(), lastmod, units="days") > 0.5)
               Sys.setFileTime(file, Sys.time()) # touch the file if its modification time was more than 1/2 day ago
            out$sfproc <- sfproc
            out$sfrun <- sfrun
            return(out)
         } else {
            # lower depth cache file(s)
            pos <- which(fenfilename == fileswoutdepth)
            files.to.remove <- file.path(cachedir, "stockfish", files[pos])
         }
      }

   }

   sfout    <- NULL
   eval     <- rep(NA_real_, multipv)
   bestmove <- as.list(rep("", multipv))
   alive    <- TRUE

   if (!sfrun)
      return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

   col.top <- .get("col.top")
   col.bg  <- .get("col.bg")
   verbose <- .get("verbose")

   olddepth <- 0

   if (progbar) {
      rect(1, 9.6, 9, 9.7, col=NA, border=col.top)
      segments(seq(1,9,length.out=depth+1), 9.6, seq(1,9,length.out=depth+1), 9.7, col=adjustcolor(col.top, alpha.f=0.4))
   }

   start.time <- proc.time()

   if (sfrun) {

      .sf.newgame(sfproc, sfrun)
      sfproc$write_input(paste0("setoption name MultiPV value ", multipv, "\n"))
      #Sys.sleep(0.1)
      if (!is.na(sflim)) {
         if (sflim <= 20) {
            sfproc$write_input(paste0("setoption name Skill Level value ", sflim, "\n"))
         } else {
            sfproc$write_input("setoption name UCI_LimitStrength value true\n")
            sfproc$write_input(paste0("setoption name UCI_Elo value ", sflim, "\n"))
         }
      }
      sfproc$write_input(paste0("position fen ", fen, "\n"))
      #Sys.sleep(0.1)
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
                     rect(1, 9.6, 1+curdepth/depth*8, 9.7, col=col.top, border=NA)
                     olddepth <- curdepth
                  }
               }
            }
            if (any(grepl("bestmove ", sfout, fixed=TRUE))) # when we see 'bestmove <move> ponder <move>', then Stockfish is done
               break
         }

      }

   }

   end.time <- proc.time()

   if (progbar)
      .texttop("")

   if (!alive) {
      sfproc <- NULL
      sfrun <- FALSE
   } else {
      if (!is.na(sflim)) {
         sfproc$write_input("setoption name UCI_Elo value 3190\n")
         sfproc$write_input("setoption name UCI_LimitStrength value false\n")
         sfproc$write_input("setoption name Skill Level value 20\n")
      }
   }

   if (is.null(sfout) || !sfrun)
      return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

   sidetoplay <- strsplit(fen, " ", fixed=TRUE)[[1]][2]

   # check for mate
   if (any(grepl("info depth 0 score mate 0", sfout, fixed=TRUE)))
      return(list(eval=ifelse(sidetoplay == "b", 99.9, -99.9), bestmove=bestmove, matetype="mate", sfproc=sfproc, sfrun=sfrun))

   # check for stalemate
   if (any(grepl("info depth 0 score cp 0", sfout, fixed=TRUE)) && any(grepl("bestmove (none)", sfout, fixed=TRUE)))
      return(list(eval=0, bestmove=bestmove, matetype="stalemate", sfproc=sfproc, sfrun=sfrun))

   # find the best move according to the 'bestmove <move> ponder <move>' line at the very end
   # (note: the actual best move is taken from the best 'info depth <number>' line below, except when using 'sflim')
   bestmoveatend <- strsplit(sfout[grep("bestmove ", sfout, fixed=TRUE)], " ", fixed=TRUE)[[1]][2]

   # find positions in output of 'info depth <depth>' (there should be between 1 and 'multipv' such lines)
   infodepthpos <- grep(paste0("info depth ", depth), sfout, fixed=TRUE)

   if (length(infodepthpos) == 0L) # just in case
      return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

   # restrict sfout to those elements
   sfout <- sfout[infodepthpos]

   for (i in 1:multipv) {
      # find the position of 'multipv <i> score'
      pos <- grep(paste0(" multipv ", i, " score "), sfout, fixed=TRUE)
      if (length(pos) == 0L)
         next
      if (length(pos) > 1L) # there really should only be one, but just in case
         pos <- max(pos)
      sfoutpos <- sfout[pos]
      # check if there is a mate in x moves
      mateinx <- grepl(" score mate ", sfoutpos, fixed=TRUE)
      # get the best move for the variation
      tmp <- strsplit(sfoutpos, " pv ", fixed=TRUE)[[1]][2]
      bestmove[[i]] <- strsplit(tmp, " ", fixed=TRUE)[[1]]
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
      cat("Time: ", end.time[[3]] - start.time[[3]], "\n")
      cat("Eval: ", eval, "\n")
      cat("Best: ", sapply(bestmove, head, 1), "\n\n")
   }

   if (!is.na(sflim))
      bestmove <- bestmoveatend

   if (usesfcache) {
      depthfenfilename <- paste0(depth, "_", fenfilename)
      saveRDS(list(eval=eval, bestmove=bestmove, matetype="none"), file=file.path(cachedir, "stockfish", depthfenfilename))
      file.remove(files.to.remove)
   }

   return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

}
