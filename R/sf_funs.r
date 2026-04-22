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

.sf.eval <- function(sfproc, sfrun, depth, multipv=5, sflim=NA, fen, progbar=FALSE, playsound=FALSE, isdeep=FALSE, compmove=FALSE, usecloud=FALSE, retry=4) {

   getcloudeval <- FALSE
   usesfcache <- .get("usesfcache")
   files.to.remove <- NULL

   if (!compmove) {

      # get the names of all cached files without the leading '<depth>_' part and get the corresponding depths
      cachedir <- .get("cachedir")
      cachefiles <- list.files(file.path(cachedir, "stockfish"), pattern=".rds$")
      filessplit <- strsplit(cachefiles, "_", fixed=TRUE)
      fileswoutdepth <- sapply(filessplit, function(x) x[2], USE.NAMES=FALSE)
      depths <- as.numeric(sapply(filessplit, function(x) x[1]))

      # construct the filename for the current FEN
      fenfilename <- .fenpart(fen, parts=1:5) # remove the fullmove number from the FEN
      fenfilename <- gsub(" ", "%20", fenfilename, fixed=TRUE)
      fenfilename <- gsub("/", "%2F", fenfilename, fixed=TRUE)
      fenfilename.rds <- paste0(fenfilename, ".rds")

      if (!usesfcache) {
         if (fenfilename.rds %in% fileswoutdepth) {
            # cached file(s) that should be removed
            pos <- which(fenfilename.rds == fileswoutdepth)
            files.to.remove <- file.path(cachedir, "stockfish", cachefiles[pos])
         }
         fileswoutdepth <- ""
      }

      if (fenfilename.rds %in% fileswoutdepth) { # if there is at least one cached file for the current FEN

         # get the depths of all cached files for the current FEN
         depths <- depths[fenfilename.rds == fileswoutdepth]

         # determine the largest depth that is cached
         maxdepth <- max(depths)

         # read in the cached file with the largest depth
         pos <- which(fenfilename.rds == fileswoutdepth)[which.max(depths)]
         file <- file.path(cachedir, "stockfish", cachefiles[pos])
         out <- readRDS(file)

         # determine if the cached file is a cloud evaluation
         iscloud <- isTRUE(out$cloud)

         if (!usecloud)
            iscloud <- TRUE

         if (iscloud && maxdepth >= depth) { # if maxdepth >= depth, use the cached file

            # touch the file if its modification time was more than 1/2 day ago
            if (difftime(Sys.time(), file.mtime(file), units="days") > 0.5)
               Sys.setFileTime(file, Sys.time())

            # return the cached evaluation
            out$sfproc <- sfproc
            out$sfrun  <- sfrun
            assign("depth", maxdepth, envir=.chesstrainer)
            return(out)

         } else { # if maxdepth is < depth, need to either use Stockfish or get the cloud evaluation

            # cached file(s) that should be removed
            pos <- which(fenfilename.rds == fileswoutdepth)
            files.to.remove <- file.path(cachedir, "stockfish", cachefiles[pos])

            if (usecloud)
               getcloudeval <- TRUE

         }

      } else { # if there is no cached file for the current FEN, then either use Stockfish or get the cloud evaluation

         if (usecloud)
            getcloudeval <- TRUE

      }

   }

   if (getcloudeval) {

      # https://lichess.org/api#tag/analysis/GET/api/cloud-eval

      url <- paste0("https://lichess.org/api/cloud-eval?multiPv=5&fen=", fenfilename)

      retry <- c(0, retry)

      for (j in 1:length(retry)) {

         lastapirequest <- .get("lastapirequest")

         # ensure that there are at least <retry> seconds between each API request
         while (proc.time()[[3]] - lastapirequest < retry[j])
            Sys.sleep(0.1)

         out <- try(VERB("GET", url, content_type("application/octet-stream"), user_agent("R chesstrainer (wvb@wvbauer.com)"), timeout(2)), silent=TRUE)

         assign("lastapirequest", proc.time()[[3]], envir=.chesstrainer)

         if (!inherits(out, "try-error"))
            break

      }

      if (inherits(out, "try-error")) {
         .texttop(.text("noconnect"), sleep=1.5)
         getcloudeval <- FALSE
      }

      if (getcloudeval && out$status == 429) {
         .texttop(.text("ratelimit"))
         getcloudeval <- FALSE
      }

      if (getcloudeval && !is.null(content(out)$error)) {
         .texttop(.text("posnotfound"))
         getcloudeval <- FALSE
      }

   }

   if (getcloudeval) {

      # get the depth of the cloud evaluation
      maxdepth <- content(out)$depth

      if (!sfrun || maxdepth >= depth) { # if Stockfish is not running or maxdepth is >= request depth, use the cloud evaluation

         eval     <- NULL
         bestmove <- list()

         sidetoplay <- strsplit(fen, " ", fixed=TRUE)[[1]][2]
         multipv <- length(content(out)$pvs)

         for (i in 1:multipv) {
            bestmove[[i]] <- strsplit(content(out)$pvs[[i]]$moves, " ", fixed=TRUE)[[1]]
            mateinx <- content(out)$pvs[[i]]$mate
            if (is.null(mateinx)) {
               eval <- c(eval, content(out)$pvs[[i]]$cp / 100)
            } else {
               tmp <- sign(mateinx) * 99.9
               if (sidetoplay == "b")
                  tmp <- -tmp
               eval <- c(eval, tmp)
            }
         }

         if (!is.null(files.to.remove))
            file.remove(files.to.remove)

         # no cloud evals for mates and stalemates as far as I can tell, so this is handled by !is.null(content(out)$error) above

         out <- list(eval=eval, bestmove=bestmove, matetype="none")
         out$cloud  <- TRUE

         depthfenfilename <- paste0(maxdepth, "_", fenfilename.rds)
         saveRDS(out, file=file.path(cachedir, "stockfish", depthfenfilename))

         out$sfproc <- sfproc
         out$sfrun  <- sfrun
         assign("depth", maxdepth, envir=.chesstrainer)

         return(out)

      }

   }

   sfout    <- NULL
   eval     <- rep(NA_real_, multipv)
   bestmove <- as.list(rep("", multipv))
   alive    <- TRUE

   assign("depth", depth, envir=.chesstrainer)

   if (!sfrun)
      return(list(eval=eval, bestmove=bestmove, matetype="none", sfproc=sfproc, sfrun=sfrun))

   col.top <- .get("col.top")
   col.bg  <- .get("col.bg")
   verbose <- .get("verbose")

   prevdepth <- 0

   if (isdeep)
      .texttop(.text("sfdeepeval"))

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
                  if (curdepth > prevdepth) {
                     rect(1, 9.6, 1+curdepth/depth*8, 9.7, col=col.top, border=NA)
                     prevdepth <- curdepth
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

   if (playsound)
      playsound(system.file("sounds", "complete.ogg", package="chesstrainer"))

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

   out <- list(eval=eval, bestmove=bestmove, matetype="none")

   if (!is.null(files.to.remove))
      file.remove(files.to.remove)

   if (!compmove) {
      depthfenfilename <- paste0(depth, "_", fenfilename.rds)
      saveRDS(out, file=file.path(cachedir, "stockfish", depthfenfilename))
   }

   out$sfproc <- sfproc
   out$sfrun  <- sfrun
   return(out)

}
