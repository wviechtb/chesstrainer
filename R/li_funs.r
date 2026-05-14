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

.liquery <- function(pos, flip, sidetoplay, sidetoplaystart, i, isonline, lichessdb, token, speeds, ratings, liout, lisort, barlen, invertbar, minfreq, minperc, showout=TRUE, showlibar=TRUE, tokencheck=FALSE, retry=4) {

   res <- list(out=NULL, selmove="")

   if (is.null(token) || token == "") {
      if (liout == 1)
         .texttop(.text("needtoken"), sleep=2)
      if (liout == 2)
         .textliwin(.text("needtoken"), sleep=2)
      return(res)
   }

   if (!isonline) {
      if (liout == 1)
         .texttop(.text("nointqueryli"), sleep=2)
      if (liout == 2)
         .textliwin(.text("nointqueryli"), sleep=2)
      return(res)
   }

   contliquery <- .get("contliquery")

   fen <- .genfen(pos, flip, sidetoplay, sidetoplaystart, i)
   fen <- .fenpart(fen, parts=1:5) # remove the fullmove number
   fen <- gsub(" ", "%20", fen, fixed=TRUE)

   if (lichessdb == "lichess") {
      speeds.opts  <- c("ultraBullet", "bullet", "blitz", "rapid", "classical", "correspondence")
      speeds.bits  <- paste0(ifelse(speeds.opts %in% strsplit(speeds, ",")[[1]], 1, 0), collapse="")
      ratings.opts <- c(0, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2500)
      ratings.bits <- paste0(ifelse(ratings.opts %in% strsplit(ratings, ",")[[1]], 1, 0), collapse="")
      filename <- paste0(gsub("/", "_", fen, fixed=TRUE), "_", speeds.bits, "_", ratings.bits, ".rds")
   } else {
      filename <- paste0(gsub("/", "_", fen, fixed=TRUE), ".rds")
   }

   cachedir <- .get("cachedir")
   uselicache <- .get("uselicache")

   if (uselicache) {
      files <- list.files(file.path(cachedir, lichessdb), pattern=".rds$")
   } else {
      files <- ""
   }

   incache <- filename %in% files

   if (incache) {

      file <- file.path(cachedir, lichessdb, filename)
      out <- readRDS(file)
      lastmod <- file.mtime(file)
      if (difftime(Sys.time(), lastmod, units="days") > 0.5)
         Sys.setFileTime(file, Sys.time()) # touch the file if its modification time was more than 1/2 day ago

   } else {

      # https://lichess.org/api#tag/opening-explorer

      if (lichessdb == "lichess") {
         url <- paste0("https://explorer.lichess.org/lichess?topGames=0&recentGames=0&speeds=", speeds, "&ratings=", ratings, "&")
      } else {
         url <- paste0("https://explorer.lichess.org/masters?topGames=0&")
      }

      url <- paste0(url, "fen=", fen)
      header <- paste("Bearer", token)

      retry <- c(0, retry)

      for (j in 1:length(retry)) {

         lastapirequest <- .get("lastapirequest")

         # ensure that there are at least <retry> seconds between each API request
         while (proc.time()[[3]] - lastapirequest < retry[j])
            Sys.sleep(0.1)

         out <- try(VERB("GET", url, add_headers('Authorization'=header), content_type("application/octet-stream"), user_agent("R chesstrainer (wvb@wvbauer.com)"), timeout(2)), silent=TRUE)

         assign("lastapirequest", proc.time()[[3]], envir=.chesstrainer)

         if (!inherits(out, "try-error"))
            break

      }

      if (inherits(out, "try-error")) {
         if (contliquery && showlibar)
            .drawlibar(clear=TRUE)
         if (liout == 1)
            .texttop(.text("noconnect"), sleep=2)
         if (liout == 2)
            .textliwin(.text("noconnect"), sleep=2)
         return(res)
      }

      if (out$status == 429) {
         if (contliquery && showlibar)
            .drawlibar(clear=TRUE)
         if (liout == 1)
            .texttop(.text("ratelimit"), sleep=2)
         if (liout == 2)
            .textliwin(.text("ratelimit"), sleep=2)
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

      saveRDS(out, file=file.path(cachedir, lichessdb, filename))

   }

   if (is.null(out)) {

      #mode <- .get("mode")

      if (liout == 1)
         cat(.text("posnotfound"), "\n", sep="")
      if (liout == 2)
         .textliwin(.text("posnotfound"), sleep=1.5)

      #if (contliquery) {
      #   if (mode %in% c("play","analysis"))
      #      cat("\n", .text("posnotfound"), "\n\n", sep="")
      #   if (mode == "add" && liout == 1)
      #      .flush()
      #}

      if (contliquery && showlibar)
         .drawlibar(clear=TRUE)

      #if (liout == 2)
      #   .clearliwin(dev.after=2L)

   } else {

      res$out <- out
      out$total <- rowSums(out[2:4])

      sel <- out$total >= minfreq & out$total / sum(out$total) >= minperc/100
      out <- out[sel,,drop=FALSE]
      res$out <- res$out[sel,,drop=FALSE]

      if (nrow(out) == 0L) {
         if (liout == 1)
            .texttop(.text("belowthreshold"), sleep=1.5)
         if (liout == 2)
            .textliwin(.text("belowthreshold"), sleep=1.5)
         if (contliquery && showlibar)
            .drawlibar(clear=TRUE)
         return(res)
      }

      totals <- colSums(out[2:5])

      if (contliquery && showlibar)
         .drawlibar(totals, flip=flip)

      out$move <- sapply(out$move, .litouci, pos=pos)
      res$selmove <- sample(out$move, size=1, prob=out$total)

      if (showout && !contliquery && liout == 1)
         eval(expr=.get("switch1"))

      if (showout && liout == 2 && length(dev.list()) == 1L) {
         .drawliwin()
         dev.set(2L)
      }

      if (showout) {
         totalpercs <- .percent(totals[1:3])
         out$perc   <- .percent(out$total)
         bars       <- apply(out[2:4], 1, .percbar, len=barlen, invert=invertbar)
         out[2:4]   <- t(apply(out[2:4], 1, .percent))
         out[1]     <- sapply(out[[1]], function(x) .parsemove(x, pos=pos, flip=flip, evalval="", i=NULL, sidetoplay=sidetoplay, rename=TRUE, space="", returnline=0, hintdepth=1)$txt)
         out        <- out[c(1,6,5,2:4)]
         out        <- rbind(out, data.frame(move="total", perc=100, total=totals[[4]], white=totalpercs[[1]], draw=totalpercs[[2]], black=totalpercs[[3]]))
         bars       <- c(bars, .percbar(totals[1:3], len=barlen, invert=invertbar))
         out$total  <- .numshort(out$total)
         colnames(out)[c(2,4:6)] <- c("%", "white%", "draw%", "black%")
         if (lisort == 2) {
            if (sidetoplay == "w") {
               winprop <- res$out$white / rowSums(res$out[2:4])
            } else {
               winprop <- res$out$black / rowSums(res$out[2:4])
            }
            ord <- order(winprop, decreasing=TRUE)
            ord <- c(ord, max(ord)+1)
            out <- out[ord,,drop=FALSE]
            rownames(out) <- NULL
            bars <- bars[ord]
         }
         if (liout == 1) {
            ncols <- num_ansi_colors()
            if (ncols >= 256)
               out <- out[-c(4:6)]
            .flush()
            txt <- capture.output(print(out, print.gap=2))
            for (i in 1:length(txt)) {
               cat(txt[i], "  ")
               if (i > 1)
                  cat(bars[i-1])
               cat("\n\n")
            }
         } else {
            .updateliwin(out)
         }
      }

      if (showout && !contliquery)
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
         w <- function(x) make_ansi_style("gray80", bg=TRUE)(make_ansi_style("gray14")(x))
         b <- function(x) make_ansi_style("gray20", bg=TRUE)(make_ansi_style("gray87")(x))
      } else {
         w <- function(x) make_ansi_style("gray20", bg=TRUE)(make_ansi_style("gray87")(x))
         b <- function(x) make_ansi_style("gray80", bg=TRUE)(make_ansi_style("gray14")(x))
      }
      d <- function(x) make_ansi_style("gray42", bg=TRUE)(make_ansi_style("gray87")(x))
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

.drawliwin <- function() {

   col.bg <- .get("col.bg")
   dev.new(bg=col.bg, title="Lichess")
   Sys.sleep(0.05)
   par(mar=c(0,0,0,0))
   plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
   return()

}

.clearliwin <- function(dev.after) {

   dev.set(which=3L)
   col.bg  <- .get("col.bg")
   rect(0, 0, 1, 1, col=col.bg, border=NA)
   if (!missing(dev.after))
      dev.set(which=dev.after)
   return()

}

.updateliwin <- function(out) {

   col.fg <- .get("col.fg")
   cex.lichess <- .get("cex.lichess")

   n <- nrow(out)

   .clearliwin()

   ypos <- seq(0.9, 0.1, length.out=12)
   dist <- (ypos[1] - ypos[2]) / 3

   text(0.1, ypos[1], names(out)[1], col=col.fg, cex=cex.lichess)
   text(0.2, ypos[1], names(out)[2], col=col.fg, cex=cex.lichess)
   text(0.3, ypos[1], names(out)[3], col=col.fg, cex=cex.lichess)

   ypos <- ypos[-1]

   text(0.1, ypos[1:n], out[[1]], col=col.fg, cex=cex.lichess)
   text(0.2, ypos[1:n], out[[2]], col=col.fg, cex=cex.lichess)
   text(0.3, ypos[1:n], out[[3]], col=col.fg, cex=cex.lichess)

   for (i in 1:n) {

      bar.s <- 0.4
      bar.e <- 0.4 + out[[4]][i]/100 * (0.9-0.4)
      rect(bar.s, ypos[i] - dist, bar.e, ypos[i] + dist, col="gray80", border=NA)
      if (out[[4]][i] >= 10)
         text((bar.s + bar.e) / 2, ypos[i], paste0(out[[4]][i], "%"), col="gray14", cex=cex.lichess)
      bar.s <- bar.e
      bar.e <- bar.s + out[[5]][i]/100 * (0.9-0.4)
      rect(bar.s, ypos[i] - dist, bar.e, ypos[i] + dist, col="gray42", border=NA)
      if (out[[5]][i] >= 10)
         text((bar.s + bar.e) / 2, ypos[i], paste0(out[[5]][i], "%"), col="gray87", cex=cex.lichess)
      bar.s <- bar.e
      bar.e <- bar.s + out[[6]][i]/100 * (0.9-0.4)
      rect(bar.s, ypos[i] - dist, bar.e, ypos[i] + dist, col="gray20", border=NA)
      if (out[[6]][i] >= 10)
         text((bar.s + bar.e) / 2, ypos[i], paste0(out[[6]][i], "%"), col="gray87", cex=cex.lichess)

   }

   dev.set(which=2L)

   return()

}

.textliwin <- function(txt, sleep=0) {

   col.fg <- .get("col.fg")
   cex.lichess <- .get("cex.lichess")

   dev.set(which=3L)

   .clearliwin()

   text(0.5, 0.5, labels=txt, cex=cex.lichess, col=col.fg)

   if (sleep > 0) {
      Sys.sleep(sleep + .get("sleepadj"))
      .clearliwin()
   }

   dev.set(which=2L)

   return()

}
