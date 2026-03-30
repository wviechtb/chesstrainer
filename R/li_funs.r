.litouci <- function(move, pos) {

   # since Lichess uses 960-compatible castling moves, need to translate these back to standard chess

   if (identical(substr(move,1,4), "e1h1") && pos[1,5] == "WK")
      substr(move,1,4) <- "e1g1"
   if (identical(substr(move,1,4), "e1a1") && pos[1,5] == "WK")
      substr(move,1,4) <- "e1c1"
   if (identical(substr(move,1,4), "e8h8") && pos[8,5] == "BK")
      substr(move,1,4) <- "e8g8"
   if (identical(substr(move,1,4), "e8a8") && pos[8,5] == "BK")
      substr(move,1,4) <- "e8c8"

   return(move)

}

.liquery <- function(pos, flip, sidetoplay, sidetoplaystart, i, isonline, lichessdb, token, speeds, ratings, barlen, invertbar, texttop, showout=TRUE, tokencheck=FALSE) {

   res <- list(out=NULL, selmove="")

   if (is.null(token) || token == "") {
      .texttop(.text("needtoken"), sleep=2)
      .texttop(texttop)
      return(res)
   }

   if (!isonline) {
      .texttop(.text("nointqueryli"), sleep=2)
      .texttop(texttop)
      return(res)
   }

   contliquery <- .get("contliquery")

   fen <- .genfen(pos, flip, sidetoplay, sidetoplaystart, i)
   fen <- gsub(" ", "%20", fen, fixed=TRUE)
   filename <- paste0(gsub("/", "_", fen, fixed=TRUE), ".rds")

   cachedir <- .get("cachedir")
   uselicache <- .get("uselicache")

   if (uselicache) {
      files <- list.files(file.path(cachedir, lichessdb), pattern=".rds$")
   } else {
      files <- ""
   }

   if (filename %in% files) {

      file <- file.path(cachedir, lichessdb, filename)
      out <- readRDS(file)
      lastmod <- file.mtime(file)
      if (difftime(Sys.time(), lastmod, units="days") > 0.5)
         Sys.setFileTime(file, Sys.time()) # touch the file if its modification time was more than 1/2 day ago

   } else {

      if (lichessdb == "lichess") {
         url <- paste0("https://explorer.lichess.org/lichess?topGames=0&recentGames=0&speeds=", speeds, "&ratings=", ratings, "&")
      } else {
         url <- paste0("https://explorer.lichess.org/masters?topGames=0&")
      }

      url <- paste0(url, "fen=", fen)
      header <- paste("Bearer", token)

      lastget <- .get("lastget")

      # ensure that there are at least 2.5 seconds between each API request

      while (proc.time()[[3]] - lastget < 2.5)
         Sys.sleep(0.1)

      out <- try(VERB("GET", url, add_headers('Authorization'=header), content_type("application/octet-stream"), user_agent("R chesstrainer (wvb@wvbauer.com)"), timeout(2)), silent=TRUE)

      if (inherits(out, "try-error")) {
         Sys.sleep(4)
         out <- try(VERB("GET", url, add_headers('Authorization'=header), content_type("application/octet-stream"), user_agent("R chesstrainer (wvb@wvbauer.com)"), timeout(2)), silent=TRUE)
      }

      assign("lastget", proc.time()[[3]], envir=.chesstrainer)

      if (inherits(out, "try-error")) {

         .texttop(.text("noconnect"), sleep=1.5)
         .texttop(texttop)
         return(res)

      } else {

         if (out$status == 429) {
            .texttop(.text("ratelimit"))
            return(res)
         }

         out <- do.call(rbind, lapply(content(out)$moves, function(x) data.frame(move=x$uci, white=x$white, draw=x$draws, black=x$black)))

         if (tokencheck) {
            if (is.null(out)) {
               return(res)
            } else {
               return(list(out=TRUE))
            }
         }

         if (is.null(out)) {
            if (!contliquery) {
               .texttop(.text("posnotfound"), sleep=1.5)
               .texttop(texttop)
            }
         }

         saveRDS(out, file=file.path(cachedir, lichessdb, filename))

      }

   }

   if (is.null(out)) {

      if (.get("mode") != "play" && contliquery)
         cat("\n", .text("posnotfound"), "\n\n", sep="")

   } else {

      res$out <- out
      out$total <- rowSums(out[2:4])
      totals    <- colSums(out[2:5])

      if (contliquery)
         .drawlibar(totals, flip=flip)

      out$move  <- sapply(out$move, .litouci, pos=pos)
      res$selmove <- sample(out$move, size=1, prob=out$total)
      #if (!showout) print(paste0("Selected move: ", res$selmove))

      if (!contliquery)
         eval(expr=.get("switch1"))

      if (showout) {
         .flush()
         percs     <- .percent(totals[1:3])
         out$perc  <- .percent(out$total)
         bars      <- apply(out[2:4], 1, .percbar, len=barlen, invert=invertbar)
         out[2:4]  <- t(apply(out[2:4], 1, .percent))
         out[1]    <- sapply(out[[1]], function(x) .parsemove(x, pos=pos, flip=flip, evalval="", i=NULL, sidetoplay=sidetoplay, rename=TRUE, space="", returnline=0, hintdepth=1)$txt)
         out       <- out[c(1,6,5,2:4)]
         out       <- rbind(out, data.frame(move="total", perc=100, total=totals[[4]], white=percs[[1]], draw=percs[[2]], black=percs[[3]]))
         bars      <- c(bars, .percbar(totals[1:3], len=barlen, invert=invertbar))
         out$total <- .numshort(out$total)
         colnames(out)[c(2,4:6)] <- c("%", "white%", "draw%", "black%")
         ncols <- num_ansi_colors()
         if (ncols >= 256)
            out <- out[-c(4:6)]
         txt <- capture.output(print(out, print.gap=2))
         #sink("~/downloads/output.txt")
         for (i in 1:length(txt)) {
            cat(txt[i], "  ")
            if (i > 1)
               cat(bars[i-1])
            cat("\n\n")
         }
         #sink()
      }

      if (!contliquery)
         eval(expr=.get("switch2"))

   }

   return(res)

}

.percent <- function(x, total=100) {

   x <- x / sum(x) * total
   xfloor <- floor(x)
   remainder <- total - sum(xfloor)
   if (remainder > 0) {
      frac.part <- x - xfloor
      add.to <- order(frac.part, decreasing=TRUE)[1:remainder]
      xfloor[add.to] <- xfloor[add.to] + 1
   }
   return(xfloor)

}

.percbar <- function(x, len=50, invert=FALSE) {
   percent <- .percent(x)
   times <- .percent(x, total=len)
   ncols <- num_ansi_colors()
   if (ncols >= 256) {
      if (invert) {
         w <- function(x) make_ansi_style("gray80", bg=TRUE)(make_ansi_style("gray15")(x))
         b <- function(x) make_ansi_style("gray15", bg=TRUE)(make_ansi_style("gray80")(x))
      } else {
         w <- function(x) make_ansi_style("gray15", bg=TRUE)(make_ansi_style("gray80")(x))
         b <- function(x) make_ansi_style("gray80", bg=TRUE)(make_ansi_style("gray15")(x))
      }
      d <- function(x) make_ansi_style("gray40", bg=TRUE)(make_ansi_style("gray80")(x))
      white <- .centertext(times[1], ifelse(nchar(percent[1])+3 < times[1], paste0(percent[1], "%", collapse=""), ""))
      draw  <- .centertext(times[2], ifelse(nchar(percent[2])+3 < times[2], paste0(percent[2], "%", collapse=""), ""))
      black <- .centertext(times[3], ifelse(nchar(percent[3])+3 < times[3], paste0(percent[3], "%", collapse=""), ""))
      bar <- paste0(w(white), d(draw), b(black), collapse="")
   } else {
      if (invert) {
         w <- "\U00002593"
         b <- "\U00002591"
      } else {
         w <- "\U00002591"
         b <- "\U00002593"
      }
      d <- "\U00002592"
      white <- paste0(rep(w, times[1]), collapse="")
      draw  <- paste0(rep(d, times[2]), collapse="")
      black <- paste0(rep(b, times[3]), collapse="")
      bar <- paste0(white, draw, black, collapse="")
   }
   return(bar)
}

.centertext <- function(width, text)
   sprintf("%*s%s%*s", floor((width - nchar(text)) / 2), "", text, ceiling((width - nchar(text)) / 2), "")
