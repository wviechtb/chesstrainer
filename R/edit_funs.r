.editseq <- function(sub, key) {

   doreadline <- missing(key)

   while (TRUE) {

      .flush()

      if (!is.null(sub$commentstart)) {
         cat("\n")
         cat(style_bold(.text("commentstart")))
         cat(sub$commentstart, "\n")
         if (nrow(sub$moves) > 0L)
            cat("\n")
      }
      if (nrow(sub$moves) > 0L)
         print(sub$moves[5:9])
      if (nrow(sub$moves) > 0L && (!is.null(sub$commentend) || any(sub$moves$nextseq != "") || !is.null(sub$tags)))
         cat("\n")
      if (!is.null(sub$commentend)) {
         cat(style_bold(.text("commentend")))
         cat(sub$commentend, "\n")
      }
      if (any(sub$moves$nextseq != "")) {
         cat(style_bold(.text("nextseq")))
         cat(.last(sub$moves$nextseq[sub$moves$nextseq != ""]), "\n")
      }
      if (!is.null(sub$tags)) {
         if (!is.null(sub$commentend) || any(sub$moves$nextseq != ""))
            cat("\n")
         print(sub$tags)
      }

      cat("\n")

      if (doreadline) {
         resp <- readline(prompt=style_bold(.text("elementedit")))
      } else {
         resp <- key
         doreadline <- TRUE
      }

      # enter = exit the while loop

      if (identical(resp, ""))
         break

      # number = edit the corresponding comment

      if (grepl("^[1-9][0-9]*$", resp)) {
         if (nrow(sub$moves) == 0L)
            next
         comnum <- as.integer(resp)
         if (comnum < 1L || comnum > nrow(sub$moves))
            next
         newcom <- readline(prompt=style_bold(.text("commentnew")))
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
         endcom <- readline(prompt=style_bold(.text("commentendnew")))
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
         startcom <- readline(prompt=style_bold(.text("commentstartnew")))
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
         if (nrow(sub$moves) == 0L)
            next
         nextseqnum <- nrow(sub$moves)
         nextseqnew <- readline(prompt=style_bold(.text("nextseqnew")))
         if (identical(nextseqnew, ""))
            next
         if (grepl("^(-|NA|na)$", nextseqnew)) {
            sub$moves$nextseq <- ""
         } else {
            sub$moves$nextseq <- ""
            sub$moves$nextseq[nextseqnum] <- nextseqnew
         }
         next
      }

      # P or p = to edit PGN tags

      if (grepl("^[Pp]$", resp)) {
         cat("\n")
         tag.event  <- readline(prompt=style_bold("Event: "))  # Event
         tag.site   <- readline(prompt=style_bold("Site: "))   # Site
         tag.date   <- readline(prompt=style_bold("Date: "))   # Date
         tag.round  <- readline(prompt=style_bold("Round: "))  # Round
         tag.white  <- readline(prompt=style_bold("White: "))  # White
         tag.black  <- readline(prompt=style_bold("Black: "))  # Black
         tag.result <- readline(prompt=style_bold("Result: ")) # Result
         if (!identical(tag.event, "") || !identical(tag.site, "") || !identical(tag.date, "") || !identical(tag.round, "") || !identical(tag.white, "") || !identical(tag.black, "") || !identical(tag.result, "")) {
            sub$tags <- data.frame(tag=c("Event", "Site", "Date", "Round", "White", "Black", "Result"), value=c(tag.event, tag.site, tag.date, tag.round, tag.white, tag.black, tag.result))
            sub$tags <- sub$tags[sub$tags$value != "",]
            rownames(sub$tags) <- NULL
         } else {
            sub$tags <- NULL
         }
         next
      }

      # f = to flip show values

      if (grepl("^[Ff]$", resp)) {
         if (nrow(sub$moves) == 0L)
            next
         whichflip <- readline(prompt=style_bold(.text("flipshow")))
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

      resp <- readline(prompt=style_bold(.text("endmovesedit")))

      # enter = exit the while loop

      if (identical(resp, ""))
         break

      # number = edit the corresponding comment

      if (grepl("^[1-9][0-9]*$", resp)) {
         comnum <- as.integer(resp)
         if (comnum < 1L || comnum > nrow(endmoves))
            next
         newcom <- readline(prompt=style_bold(.text("commentnew")))
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
         nextseqnum <- readline(prompt=style_bold(.text("nextseqwhich")))
         if (identical(nextseqnum, ""))
            next
         if (grepl("^[1-9][0-9]*$", nextseqnum)) {
            nextseqnum <- as.integer(nextseqnum)
            if (nextseqnum < 1L || nextseqnum > nrow(endmoves))
               next
         } else {
            next
         }
         nextseqnew <- readline(prompt=style_bold(.text("nextseqnew")))
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
