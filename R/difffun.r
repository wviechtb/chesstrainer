.diffset <- function(difffun, difflen, diffmin, lwd) {

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
      "7 - root mean square successive difference",
      "8 - decay parameter from an exponential regression model",
      "n - number of the most recent scores used in the calculation",
      "m - minimum number of scores needed for the calculation")

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
      "7 - Wurzel des Mittelwertes der Quadrate der aufeinanderfolgenden Differenzen",
      "8 - Verfallsparameter aus einem exponentiellen Regressionsmodell",
      "n - Anzahl der letzten Werte, die f\U000000FCr die Berechnung verwendet werden",
      "m - Mindestanzahl der Werte f\U000000FCr die Berechnung")

   }

   rect(1.2, 1.2, 8.8, 8.8, col=col.bg, border=col.help.border, lwd=lwd+3)

   maxsw <- max(strwidth(txt, family=font.mono))
   maxsh <- strheight("A", family=font.mono) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.9)

   opts <- c(1:9, "n", "m")
   difffunold <- difffun
   difflenold <- difflen
   diffminold <- diffmin

   ypos <- seq(7.0, 4.0, length.out=length(txt))

   text(1+0.5, ypos, txt, pos=4, cex=cex,
        family=font.mono, font=ifelse(c("","",opts)==difffun, 2, 1), col=col.help)

   ypos <- ypos[-c(1:2)]
   dist <- (ypos[1] - ypos[2]) / 2

   setlen <- FALSE
   setmin <- FALSE

   string.len.cur <- .text("difflencur")
   string.len.new <- .text("difflennew")
   string.min.cur <- .text("diffmincur")
   string.min.new <- .text("diffminnew")

   sw.string <- max(strwidth(string.len.cur, family=font.mono, cex=cex), strwidth(string.min.cur, family=font.mono, cex=cex))

   text(1+0.5,           tail(ypos, 1) - 4*dist, string.len.cur, pos=4, cex=cex, family=font.mono, col=col.help)
   text(1+0.5+sw.string, tail(ypos, 1) - 4*dist, difflen,    pos=4, cex=cex, family=font.mono, col=col.help)
   text(1+0.5,           tail(ypos, 1) - 6*dist, string.min.cur, pos=4, cex=cex, family=font.mono, col=col.help)
   text(1+0.5+sw.string, tail(ypos, 1) - 6*dist, diffmin,    pos=4, cex=cex, family=font.mono, col=col.help)

   while (TRUE) {

      if (setlen || setmin) {
         val <- ""
         sw.val <- 0
         text(1+0.5, tail(ypos, 1) - 8*dist, ifelse(setlen, string.len.new, string.min.new), pos=4, cex=cex, family=font.mono, col=col.help)
      }

      while (setlen || setmin) {

         resp <- getGraphicsEvent(prompt="Chesstrainer", consolePrompt="", onMouseDown=.mousedownfun, onKeybd=.keyfun)

         if (identical(resp, "\033") || identical(resp, "ctrl-[")) {
            setlen <- setmin <- FALSE
            rect(1+0.2, tail(ypos, 1) - 7*dist, 8.5, tail(ypos, 1) - 9*dist, col=col.bg, border=NA)
            break
         }

         if (identical(resp, "\r") || identical(resp, "ctrl-J")) {
            val <- as.numeric(paste0(val, collapse=""))
            if (is.na(val)) {
               setlen <- setmin <- FALSE
               rect(1+0.2, tail(ypos, 1) - 7*dist, 8.5, tail(ypos, 1) - 9*dist, col=col.bg, border=NA)
               break
            }
            val[val < 2] <- 2
            if (setlen)
               difflen <- val
            if (setmin)
               diffmin <- val
            setlen <- setmin <- FALSE
            rect(1+0.2, tail(ypos, 1) - 3*dist, 8.5, tail(ypos, 1) - 9*dist, col=col.bg, border=NA)
            text(1+0.5,           tail(ypos, 1) - 4*dist, string.len.cur, pos=4, cex=cex, family=font.mono, col=col.help)
            text(1+0.5+sw.string, tail(ypos, 1) - 4*dist, difflen,        pos=4, cex=cex, family=font.mono, col=col.help)
            text(1+0.5,           tail(ypos, 1) - 6*dist, string.min.cur, pos=4, cex=cex, family=font.mono, col=col.help)
            text(1+0.5+sw.string, tail(ypos, 1) - 6*dist, diffmin,        pos=4, cex=cex, family=font.mono, col=col.help)
            break
         }

         if (grepl("^[0-9i]+$", resp)) {
            if (identical(resp, "i")) {
               if (setmin)
                  next
               difflen <- Inf
               setlen <- FALSE
               rect(1+0.2, tail(ypos, 1) - 3*dist, 8.5, tail(ypos, 1) - 9*dist, col=col.bg, border=NA)
               text(1+0.5,           tail(ypos, 1) - 4*dist, string.len.cur, pos=4, cex=cex, family=font.mono, col=col.help)
               text(1+0.5+sw.string, tail(ypos, 1) - 4*dist, difflen,        pos=4, cex=cex, family=font.mono, col=col.help)
               text(1+0.5,           tail(ypos, 1) - 6*dist, string.min.cur, pos=4, cex=cex, family=font.mono, col=col.help)
               text(1+0.5+sw.string, tail(ypos, 1) - 6*dist, diffmin,        pos=4, cex=cex, family=font.mono, col=col.help)
               break
            }
            if (nchar(val) > 10)
               next
            num <- resp
            val <- paste0(val, resp, collapse="")
            text(1+0.5+sw.string+sw.val, tail(ypos, 1) - 8*dist, num, pos=4, cex=cex, family=font.mono, col=col.help)
            sw.val <- strwidth(val, family=font.mono)
         }

         if (identical(resp, "\b") || identical(resp, "ctrl-H")) {
            if (nchar(val) > 1L) {
               val <- substr(val, 1, nchar(val)-1)
            } else {
               val <- ""
            }
            rect(1+0.2+sw.string, tail(ypos, 1) - 7*dist, 8.5, tail(ypos, 1) - 9*dist, col=col.bg, border=NA)
            #if (setlen) {
            #   text(1+0.5, tail(ypos, 1) - 8*dist, string.len.new, pos=4, cex=cex, family=font.mono, col=col.help)
            #} else {
            #   text(1+0.5, tail(ypos, 1) - 8*dist, string.min.new, pos=4, cex=cex, family=font.mono, col=col.help)
            #}
            text(1+0.5+sw.string, tail(ypos, 1) - 8*dist, val, pos=4, cex=cex, family=font.mono, col=col.help)
            sw.val <- strwidth(val, family=font.mono)
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
               } else if (sel == "m") {
                  setmin <- TRUE
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

         if (identical(click, "m"))
            setmin <- TRUE

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

   return(list(difffun=as.numeric(difffun), difflen=difflen, diffmin=diffmin))

}

############################################################################

.difffun1 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      val <- mean(.mistakediff(x, dbl100pen))
   }
   return(val)
}

.difffun2 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      val <- mean(.mistakediff(x, dbl100pen) > 0) * 100
      # the ratio of number of no improvements versus number of improvements
      # gives very similar values, so not used as another method
      #val <- sum(.mistakediff(x, dbl100pen) > 0) / sum(.mistakediff(x, dbl100pen) <= 0) * 10
   }
   return(val)
}

.difffun3 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      val <- sum(.mistakediff(x, dbl100pen) > 0)
   }
   return(val)
}

.difffun4 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      xdiff <- diff(x)
      val <- length(rle(xdiff < 0)$lengths) / length(xdiff) * 100
   }
   return(val)
}

.difffun5 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      val <- mean(x)
   }
   return(val)
}

.difffun6 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      val <- sd(x)
   }
   return(val)
}

.difffun7 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      xdiff <- diff(x)
      val <- sqrt(mean(xdiff^2))
      #val <- sd(xdiff)
   }
   return(val)
}

.difffun8 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
      val <- NA_real_
   } else {
      t <- 1:n
      res <- suppressWarnings(lm(log(x) ~ t))
      b1 <- exp(coef(res)[2])
      val <- max(0, (b1 / multiplier - 1) * 100)
   }
   return(val)
}

.difffun9 <- function(x, len, minn, dbl100pen, multiplier) {
   x <- tail(x, len)
   n <- length(x)
   if (n < minn) {
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
