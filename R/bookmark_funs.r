.bookmarks <- function(seqdir, seqdirpos, texttop, switch1, switch2) {

   if (!file.exists(file.path(seqdir[seqdirpos], ".bookmarks"))) {
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return("")
   }

   tmp <- try(read.table(file.path(seqdir[seqdirpos], ".bookmarks"), header=FALSE), silent=TRUE)

   if (inherits(tmp, "try-error")) {
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return("")
   }

   bookmarks <- tmp[[1]]
   bookmarks <- unique(tmp[[1]])
   bookmarks <- bookmarks[is.element(bookmarks, list.files(seqdir[seqdirpos], pattern=".rds$"))]

   if (length(bookmarks) == 0L) {
      .texttop(.text("nobookmarks"), sleep=1)
      .texttop(texttop)
      return("")
   }

   bookmark <- ""

   eval(expr=switch1)

   while (TRUE) {

      cat("\n")
      tab <- data.frame(bookmarks)
      names(tab) <- ""
      out <- capture.output(print(tab, right=FALSE))[-1]
      for (i in 1:length(out)) {
         cat(out[i], "\n")
      }
      cat("\n")

      resp <- readline(prompt=.text("bookmarksoptionwhich"))

      # enter = exit the while loop

      if (identical(resp, ""))
         break

      # number = select a bookmark

      if (grepl("^[0-9]+$", resp)) {
         resp <- round(as.numeric(resp))
         if (resp < 1 || resp > length(bookmarks))
            next
         bookmark <- bookmarks[resp]
         break
      }

      # r or e = remove a bookmark

      if (grepl("^[re]$", resp)) {
         bookmarktoremove <- readline(prompt=.text("bookmarktoremove"))
         if (identical(bookmarktoremove, ""))
            next
         if (grepl("^[0-9]+$", bookmarktoremove)) {
            bookmarktoremove <- round(as.numeric(bookmarktoremove))
            if (bookmarktoremove < 1 || bookmarktoremove > length(bookmarks))
               next
            bookmarks <- bookmarks[-bookmarktoremove]
            if (length(bookmarks) == 0L)
               break
         }
      }

      # a = remove all bookmarks

      if (grepl("^[a]$", resp)) {
         answer <- readline(prompt=.text("rlydelallbookmarks"))
         if (.confirm(answer)) {
            bookmarks <- c()
            break
         }
      }

      # p = to change the position of a bookmark in the list

      if (grepl("^[p]$", resp)) {
         posfrom <- readline(prompt=.text("bookmarktomove"))
         if (identical(posfrom, ""))
            next
         if (grepl("^[0-9]+$", posfrom)) {
            posfrom <- round(as.numeric(posfrom))
            if (posfrom < 1 || posfrom > length(bookmarks))
               next
            posto <- readline(prompt=.text("bookmarknewpos"))
            if (identical(posto, ""))
               next
            posto <- round(as.numeric(posto))
            if (posto < 1 || posto > length(bookmarks))
               next
            bookmarktomove <- bookmarks[posfrom]
            bookmarks <- bookmarks[-posfrom]
            bookmarks <- append(bookmarks, bookmarktomove, after=posto-1)
         }
      }

   }

   if (length(bookmarks) == 0L) {
      try(file.remove(file.path(seqdir[seqdirpos], ".bookmarks")), silent=TRUE)
   } else {
      write.table(data.frame(bookmarks), file=file.path(seqdir[seqdirpos], ".bookmarks"), col.names=FALSE, row.names=FALSE, quote=FALSE)
   }

   eval(expr=switch2)

   return(bookmark)

}
