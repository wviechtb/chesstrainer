.editseq <- function(sub, key) {

   doreadline <- missing(key)

   while (TRUE) {

      .flush()

      if (!is.null(sub$commentstart)) {
         cat(.text("commentstart", sub$commentstart))
         cat("\n")
      }
      if (nrow(sub$moves) > 0L) {
         if (all(sub$moves$nextseq == "")) {
            print(sub$moves[5:9])
         } else {
            print(sub$moves[c(5:9,13)])
         }
      }
      if (!is.null(sub$commentend))
         cat(.text("commentend", sub$commentend))
      cat("\n")

      if (doreadline) {
         resp <- readline(prompt=.text("commentedit"))
      } else {
         resp <- key
         doreadline <- TRUE
      }

      # enter = exit the while loop

      if (identical(resp, ""))
         break

      # number = edit the corresponding comment

      if (grepl("^[1-9][0-9]*$", resp)) {
         comnum <- as.integer(resp)
         if (comnum < 1L || comnum > nrow(sub$moves))
            next
         newcom <- readline(prompt=.text("commentnew"))
         if (identical(newcom, ""))
            next
         if (grepl("^(-|NA|na)$", newcom)) {
            newcom <- ""
         } else {
            newcom <- gsub("\\n", "\n", newcom, fixed=TRUE)
         }
         sub$moves$comment[comnum] <- newcom
         next
      }

      # e or E = to edit the end comment

      if (grepl("^[Ee]$", resp)) {
         endcom <- readline(prompt=.text("commentendnew"))
         if (identical(endcom, ""))
            next
         if (grepl("^(-|NA|na)$", endcom)) {
            sub$commentend <- NULL
         } else {
            sub$commentend <- endcom
         }
         next
      }

      # s or S = to edit the start comment

      if (grepl("^[Ss]$", resp)) {
         startcom <- readline(prompt=.text("commentstartnew"))
         if (identical(startcom, ""))
            next
         if (grepl("^(-|NA|na)$", startcom)) {
            sub$commentstart <- NULL
         } else {
            sub$commentstart <- startcom
         }
         next
      }

      # n or N = to edit nextseq

      if (grepl("^[Nn]$", resp)) {
         nextseqnum <- nrow(sub$moves)
         nextseqnew <- readline(prompt=.text("nextseqnew"))
         if (identical(nextseqnew, ""))
            next
         if (grepl("^(-|NA|na)$", nextseqnew)) {
            sub$moves$nextseq[nextseqnum] <- ""
         } else {
            sub$moves$nextseq[nextseqnum] <- nextseqnew
         }
         next
      }

      # f = to flip show values

      if (grepl("^[Ff]$", resp)) {
         whichflip <- readline(prompt=.text("flipshow"))
         if (grepl("^[EeGg]$", whichflip)) { # EeGg for even rows
            whichflip <- 2L * seq_len(nrow(sub$moves) %/% 2L)
            sub$moves$show[whichflip] <- !sub$moves$show[whichflip]
            next
         }
         if (grepl("^[OoUu]$", whichflip)) { # OoUu for odd rows
            whichflip <- 2L * seq_len((nrow(sub$moves) + 1L) %/% 2L) - 1L
            sub$moves$show[whichflip] <- !sub$moves$show[whichflip]
            next
         }
         whichflip <- .parserows(whichflip, n=nrow(sub$moves)) # or a number
         sub$moves$show[whichflip] <- !sub$moves$show[whichflip]
         next
      }

   }

   return(sub)

}

.editendmoves <- function(endmoves) {

   rownamessav <- rownames(endmoves)
   rownames(endmoves) <- NULL

   while (TRUE) {

      .flush()

      print(endmoves[c(5:9,13)])
      cat("\n")

      resp <- readline(prompt=.text("endmovesedit"))

      # enter = exit the while loop

      if (identical(resp, ""))
         break

      # number = edit the corresponding comment

      if (grepl("^[1-9][0-9]*$", resp)) {
         comnum <- as.integer(resp)
         if (comnum < 1L || comnum > nrow(endmoves))
            next
         newcom <- readline(prompt=.text("commentnew"))
         if (identical(newcom, ""))
            next
         if (grepl("^(-|NA|na)$", newcom)) {
            newcom <- ""
         } else {
            newcom <- gsub("\\n", "\n", newcom, fixed=TRUE)
         }
         endmoves$comment[comnum] <- newcom
         next
      }

      # n or N = to edit nextseq

      if (grepl("^[Nn]$", resp)) {
         nextseqnum <- readline(prompt=.text("nextseqwhich"))
         if (identical(nextseqnum, ""))
            next
         if (grepl("^[1-9][0-9]*$", nextseqnum)) {
            nextseqnum <- as.integer(nextseqnum)
            if (nextseqnum < 1L || nextseqnum > nrow(endmoves))
               next
         } else {
            next
         }
         nextseqnew <- readline(prompt=.text("nextseqnew"))
         if (identical(nextseqnew, ""))
            next
         if (grepl("^(-|NA|na)$", nextseqnew)) {
            endmoves$nextseq[nextseqnum] <- ""
         } else {
            endmoves$nextseq[nextseqnum] <- nextseqnew
         }
         next
      }

   }

   rownames(endmoves) <- rownamessav

   return(endmoves)

}
