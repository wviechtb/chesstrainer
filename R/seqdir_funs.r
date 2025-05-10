.seqdirsettings <- function(seqdir, seqdirpos) {

   while (TRUE) {

      cat("\n")
      tab <- data.frame(seqdir=seqdir, selected="( )")
      tab$selected[seqdirpos] <- "(*)"
      names(tab) <- .text("selectedseqdir")
      print(tab, right=FALSE, print.gap=3)
      cat("\n")

      resp <- readline(prompt=.text("seqdiroptionwhich"))

      # enter = exit the while loop

      if (identical(resp, ""))
         break

      # number = set seqdirpos

      if (grepl("^[0-9]+$", resp)) {
         resp <- round(as.numeric(resp))
         if (resp < 1 || resp > length(seqdir))
            next
         seqdirpos <- resp
      }

      # a or h = add a sequence directory

      if (grepl("^[ah]$", resp)) {
         if (.Platform$OS.type == "windows") {
            seqdirnew <- choose.dir(caption="")
            if (is.na(seqdirnew))
               next
         } else {
            seqdirnew <- readline(prompt=.text("addseqdir"))
            if (identical(seqdirnew, ""))
               next
            if (!dir.exists(seqdirnew)) {
               cat(.text("dirdoesnotexist"))
               createdir <- readline(prompt=.text("createdir"))
               if (identical(createdir, "") || .confirm(createdir)) {
                  succces <- dir.create(seqdirnew, showWarnings=FALSE, recursive=TRUE)
                  if (!succces) {
                     cat(.text("dircreateerror"))
                     next
                  }
               } else {
                  next
               }
            }
         }
         seqdirnew <- normalizePath(seqdirnew)
         seqdir <- c(seqdir, seqdirnew)
      }

      # r or e = remove a sequence directory

      if (grepl("^[re]$", resp)) {
         if (length(seqdir) == 1L) {
            cat(.text("cannotremovesingleseqdir"))
            next
         }
         seqdirtoremove <- readline(prompt=.text("seqdirtoremove"))
         if (identical(seqdirtoremove, ""))
            next
         if (grepl("^[0-9]+$", seqdirtoremove)) {
            seqdirtoremove <- round(as.numeric(seqdirtoremove))
            if (seqdirtoremove < 1 || seqdirtoremove > length(seqdir))
               next
            seqdir <- seqdir[-seqdirtoremove]
            if (seqdirtoremove <= seqdirpos)
               seqdirpos <- seqdirpos - 1
         }
      }

      # p = to change the position of a sequence directory in the list

      if (grepl("^[p]$", resp)) {
         posfrom <- readline(prompt=.text("seqdirtomove"))
         if (identical(posfrom, ""))
            next
         if (grepl("^[0-9]+$", posfrom)) {
            posfrom <- round(as.numeric(posfrom))
            if (posfrom < 1 || posfrom > length(seqdir))
               next
            posto <- readline(prompt=.text("seqdirnewpos"))
            if (identical(posto, ""))
               next
            posto <- round(as.numeric(posto))
            if (posto < 1 || posto > length(seqdir))
               next
            seqdirtomove <- seqdir[posfrom]
            seqdir <- seqdir[-posfrom]
            seqdir <- append(seqdir, seqdirtomove, after=posto-1)
         }
      }

   }

   return(list(seqdir=seqdir, seqdirpos=seqdirpos))

}
