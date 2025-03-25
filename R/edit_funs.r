.editcomments <- function(sub, seqdir, seqname, mode) {

   dosave <- FALSE

   if (!is.null(sub$commentstart)) {
      cat(.text("commentstart", sub$ommentstart))
      cat("\n")
   }
   print(sub$moves[1:8])
   if (!is.null(sub$commentend))
      cat(.text("commentend", sub$commentend))
   cat("\n")

   while (TRUE) {

      resp <- readline(prompt=.text("commentedit"))

      # enter = exit the while loop

      if (identical(resp, ""))
         break

      # number = edit the corresponding comment

      if (grepl("^[0-9]+$", resp)) {
         comnum <- round(as.numeric(resp))
         if (comnum < 1 || comnum > nrow(sub$moves))
            next
         newcom <- readline(prompt=.text("commentnew"))
         if (identical(newcom, ""))
            next
         newcom <- gsub("\\n", "\n", newcom, fixed=TRUE)
         sub$moves$comment[comnum] <- newcom
         dosave <- TRUE
         cat("\n")
         if (!is.null(sub$commentstart)) {
            cat(.text("commentstart", sub$commentstart))
            cat("\n")
         }
         print(sub$moves[1:8])
         if (!is.null(sub$commentend))
            cat(.text("commentend", sub$commentend))
         cat("\n")
      }

      # d (or D, L, or l) = to delete a comment

      if (grepl("^[DdLl]$", resp)) {
         comdel <- readline(prompt=.text("commentdelete"))
         if (identical(comdel, ""))
            next
         if (grepl("^[0-9]+$", comdel)) {
            comdel <- round(as.numeric(comdel))
            if (comdel < 1 || comdel > nrow(sub$moves))
               next
            sub$moves$comment[comdel] <- ""
            dosave <- TRUE
            cat("\n")
            if (!is.null(sub$commentstart)) {
               cat(.text("commentstart", sub$commentstart))
               cat("\n")
            }
            print(sub$moves[1:8])
            if (!is.null(sub$commentend))
               cat(.text("commentend", sub$commentend))
            cat("\n")
         }
         if (grepl("^[Ee]$", comdel)) {
            sub$commentend <- NULL
            dosave <- TRUE
            cat(.text("commentenddeleted"))
            cat("\n")
            if (!is.null(sub$commentstart)) {
               cat(.text("commentstart", sub$commentstart))
               cat("\n")
            }
            print(sub$moves[1:8])
            cat("\n")
         }
         if (grepl("^[Ss]$", comdel)) {
            sub$commentstart <- NULL
            dosave <- TRUE
            cat(.text("commentstartdeleted"))
            cat("\n")
            print(sub$moves[1:8])
            if (!is.null(sub$commentend))
               cat(.text("commentend", sub$commentend))
            cat("\n")
         }
      }

      # e or E = to edit the end comment

      if (grepl("^[Ee]$", resp)) {
         if (!is.null(sub$commentend))
            cat(.text("commentendnow", sub$commentend))
         endcom <- readline(prompt=.text("commentendnew"))
         if (identical(endcom, ""))
            next
         sub$commentend <- endcom
         dosave <- TRUE
         cat("\n")
         if (!is.null(sub$commentstart)) {
            cat(.text("commentstart", sub$commentstart))
            cat("\n")
         }
         print(sub$moves[1:8])
         if (!is.null(sub$commentend))
            cat(.text("commentend", sub$commentend))
         cat("\n")
      }

      # s or S = to edit the start comment

      if (grepl("^[Ss]$", resp)) {
         if (!is.null(sub$commentstart))
            cat(.text("commentstartnow", sub$commentstart))
         startcom <- readline(prompt=.text("commentstartnew"))
         if (identical(startcom, ""))
            next
         sub$commentstart <- startcom
         dosave <- TRUE
         cat("\n")
         if (!is.null(sub$commentstart)) {
            cat(.text("commentstart", sub$commentstart))
            cat("\n")
         }
         print(sub$moves[1:8])
         if (!is.null(sub$commentend))
            cat(.text("commentend", sub$commentend))
         cat("\n")
      }

   }

   if (dosave && mode == "play")
      saveRDS(sub, file=file.path(seqdir, seqname))

   return(sub)

}
