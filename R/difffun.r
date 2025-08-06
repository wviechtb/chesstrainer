.diffset <- function(difffun, difflen, lwd) {

   lang <- .get("lang")

   col.bg          <- .get("col.bg")
   col.help        <- .get("col.help")
   col.help.border <- .get("col.help.border")
   font.mono       <- .get("font.mono")

   if (lang == "en") {

      txt <- c(
      "Choose a difficulty calculation method:",
      "",
      "1 - average score increase over the plays of the sequence",
      "2 - percentage of plays where the score did not improve",
      "3 - number of plays where the score did not improve",
      "4 - percentage of plays with a change in the score trend",
      "5 - average of the scores of the sequence",
      "6 - standard deviation of the scores of the sequence",
      "7 - standard deviation of the scores changes of the sequence",
      "8 - root mean square successive difference",
      "9 - decay parameter from an exponential regression model",
      "n - number of the most recent scores used in the calculation")

   }

   if (lang == "de") {

      txt <- c(
      "W\U000000E4hle eine Methode zur Schwierigkeitsberechnung aus:",
      "",
      "1 - durchschnittliche Punktsteigerung \U000000FCber die Spiele der Sequenz",
      "2 - Prozent der Spiele, in denen sich die Punktzahl nicht verbessert hat",
      "3 - Anzahl der Spiele, in denen sich die Punktzahl nicht verbessert hat",
      "4 - Prozent der Spiele mit einer \U000000C4nderung im Punktestandtrend",
      "5 - Durchschnitt der Punkte der Sequenz",
      "6 - Standardabweichung der Punkte der Sequenz",
      "7 - Standardabweichung der Punktever\U000000E4nderungen der Sequenz",
      "8 - quadratischer Mittelwert der aufeinanderfolgenden Differenzen",
      "9 - Verfallsparameter aus einem exponentiellen Regressionsmodell",
      "n - Anzahl der letzten Ergebnisse, die f\U000000FCr die Berechnung verwendet werden")

   }

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   maxsw <- max(strwidth(txt, family=font.mono))
   maxsh <- strheight("A", family=font.mono) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.9)

   opts <- c(1:9, "n")
   difffunold <- difffun
   difflenold <- difflen

   ypos <- seq(7.0, 4.0, length.out=length(txt))

   text(1+0.5, ypos, txt, pos=4, cex=cex,
        family=font.mono, font=ifelse(c("","",opts)==difffun, 2, 1), col=col.help)

   ypos <- ypos[-c(1:2)]
   dist <- (ypos[1] - ypos[2]) / 2

   setlen <- FALSE

   string.cur <- .text("difflencur")
   string.new <- .text("difflennew")

   sw.string.cur <- strwidth(string.cur, family=font.mono, cex=cex)
   sw.string.new <- strwidth(string.new, family=font.mono, cex=cex)

   text(1+0.5,               tail(ypos, 1) - 4*dist, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
   text(1+0.5+sw.string.cur, tail(ypos, 1) - 4*dist, difflen,    pos=4, cex=cex, family=font.mono, col=col.help)

   while (TRUE) {

      if (setlen) {
         val <- ""
         sw.val <- 0
         text(1+0.5, tail(ypos, 1) - 6*dist, string.new, pos=4, cex=cex, family=font.mono, col=col.help)
      }

      while (setlen) {

         resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

         if (identical(resp, "\033") || identical(resp, "ctrl-[")) {
            setlen <- FALSE
            rect(1+0.2, tail(ypos, 1) - 5*dist, 8.5, tail(ypos, 1) - 7*dist, col=col.bg, border=NA)
            break
         }

         if (identical(resp, "\r") || identical(resp, "ctrl-J")) {
            setlen <- FALSE
            val <- as.numeric(paste0(val, collapse=""))
            if (is.na(val))
               break
            val[val < 2] <- 2
            difflen <- val
            rect(1+0.2, tail(ypos, 1) - 3*dist, 8.5, tail(ypos, 1) - 7*dist, col=col.bg, border=NA)
            text(1+0.5,               tail(ypos, 1) - 4*dist, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
            text(1+0.5+sw.string.cur, tail(ypos, 1) - 4*dist, difflen,    pos=4, cex=cex, family=font.mono, col=col.help)
         }

         if (grepl("^[0-9i]+$", resp)) {
            if (identical(resp, "i")) {
               difflen <- Inf
               setlen <- FALSE
               rect(1+0.2, tail(ypos, 1) - 3*dist, 8.5, tail(ypos, 1) - 7*dist, col=col.bg, border=NA)
               text(1+0.5,               tail(ypos, 1) - 4*dist, string.cur, pos=4, cex=cex, family=font.mono, col=col.help)
               text(1+0.5+sw.string.cur, tail(ypos, 1) - 4*dist, difflen,    pos=4, cex=cex, family=font.mono, col=col.help)
               break
            }
            if (nchar(val) > 10)
               next
            num <- resp
            val <- paste0(val, resp, collapse="")
            text(1+0.5+sw.string.cur+sw.val, tail(ypos, 1) - 6*dist, num, pos=4, cex=cex, family=font.mono, col=col.help)
            sw.val <- strwidth(val, family=font.mono)
         }

         if (identical(resp, "\b") || identical(resp, "ctrl-H")) {
            if (nchar(val) > 1L) {
               val <- substr(val, 1, nchar(val)-1)
            } else {
               val <- ""
            }
            sw.val <- strwidth(val, family=font.mono)
            rect(1+0.2+sw.string.cur, tail(ypos, 1) - 5*dist, 8.5, tail(ypos, 1) - 7*dist, col=col.bg, border=NA)
            text(1+0.5, tail(ypos, 1) - 6*dist, string.new, pos=4, cex=cex, family=font.mono, col=col.help)
            text(1+0.5+sw.string.cur, tail(ypos, 1) - 6*dist, val, pos=4, cex=cex, family=font.mono, col=col.help)
         }

      }

      click <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

      if (is.numeric(click)) {

         x <- grconvertX(click[[1]], from="ndc", to="user")
         y <- grconvertX(click[[2]], from="ndc", to="user")

         if (x >= 1.5 && x <= 8) {
            click <- which(y < ypos + dist & y > ypos - dist)
            if (length(click) == 1L) {
               sel <- opts[click]
               if (sel == "n") {
                  setlen <- TRUE
                  next
               } else {
                  difffun <- opts[click]
                  break
               }
            }
         }

      } else {

         if (identical(click, "\r") || identical(click, "ctrl-J") || identical(click, "q") || identical(click, "\033") || identical(click, "ctrl-["))
            break

         if (identical(click, "n"))
            setlen <- TRUE

         if (is.element(click, 1:(length(opts)-1))) {
            click <- as.numeric(click)
            difffun <- opts[click]
            break
         }

      }

   }

   if (opts[difffunold] != difffun) {
      rect(1.5, ypos[difffunold]-dist, 8, ypos[difffunold]+dist, col=col.bg, border=NA)
      text(1+0.5, ypos[difffunold], txt[difffunold+2], pos=4, cex=cex, family=font.mono, col=col.help)
      text(1+0.5, ypos[click], txt[click+2], pos=4, cex=cex, family=font.mono, font=2, col=col.help)
      Sys.sleep(1)
   }

   #.erase(1, 1, 9, 9)

   return(list(difffun=as.numeric(difffun), difflen=difflen))

}

############################################################################

.difffun1 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- 0
   } else {
      xdiff <- diff(x)
      xdiff[xdiff < 0] <- 0
      val <- mean(xdiff, na.rm=TRUE)
   }
   return(val)
}

.difffun2 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- 0
   } else {
      xdiff <- diff(x)
      val <- mean(xdiff >= 0, na.rm=TRUE) * 100
      # the ratio of number of no improvements versus number of improvements
      # gives values that are just a monotonic transformation of this
      #val <- sum(xdiff >= 0, na.rm=TRUE) / sum(xdiff < 0, na.rm=TRUE) * 10
   }
   return(val)
}

.difffun3 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- 0
   } else {
      xdiff <- diff(x)
      val <- sum(xdiff >= 0, na.rm=TRUE)
   }
   return(val)
}

.difffun4 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- 0
   } else {
      xdiff <- diff(x)
      val <- length(rle(xdiff < 0)$lengths) / length(xdiff) * 100
   }
   return(val)
}

.difffun5 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- 0
   } else {
      val <- mean(x, na.rm=TRUE)
   }
   return(val)
}

.difffun6 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- 0
   } else {
      val <- sd(x, na.rm=TRUE)
   }
   return(val)
}

.difffun7 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- 0
   } else {
      xdiff <- diff(x)
      val <- sd(xdiff, na.rm=TRUE)
   }
   return(val)
}

.difffun8 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- NA_real_
   } else {
      val <- sqrt(mean(diff(x)^2, na.rm=TRUE))
   }
   return(val)
}

.difffun9 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 2L) {
      val <- NA_real_
   } else {
      t <- 1:n
      res <- suppressWarnings(lm(log(x) ~ t))
      val <- exp(coef(res)[2]) * 100
   }
   return(val)
}

.difffun10 <- function(x, len, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n <= 1L) {
      val <- NA_real_
   } else {
      xopt <- c(x[1], rep(NA_real_, n-1))
      for (i in 2:n) {
         xopt[i] <- round(xopt[i-1] * multiplier)
      }
      xdiff <- x - xopt
      val <- sqrt(mean(diff(xdiff)^2))
   }
   return(val)
}

############################################################################
