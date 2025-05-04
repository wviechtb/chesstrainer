.showsettings <- function(tab, lwd) {

   .clearsideindicator()
   .drawtimer(clear=TRUE)

   seqdir <- tab$seqdir
   sfpath <- tab$sfpath

   tab <- t(tab)
   tab <- cbind(tab, .text("explsettings"))
   colnames(tab) <- c("", "")

   tab <- tab[rownames(tab) != "seqdir",]
   tab <- tab[rownames(tab) != "sfpath",]

   txt <- capture.output(print(tab, quote=FALSE, print.gap=3))
   txt <- txt[-1]

   maxchars <- max(nchar(txt))

   #lines <- paste0(rep("-", maxchars), collapse="")
   #txt <- c(lines, txt[-1], lines)

   if (nchar(seqdir) > maxchars-3)
      seqdir <- paste0("...", substr(seqdir, nchar(seqdir)-maxchars-3+1, nchar(seqdir)))
   if (nchar(sfpath) > maxchars-3)
      sfpath <- paste0("...", substr(sfpath, nchar(sfpath)-maxchars-3+1, nchar(sfpath)))

   txt <- c(txt, "", .text("seqdirsettings"), seqdir, "", .text("sfpathsettings"), sfpath)

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
   maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.95)

   text(1+0.5, seq(8.5, 1.5, length.out=length(txt)), txt, pos=4, cex=cex,
        family=.get("font.mono"), font=ifelse(grepl(":", txt), 2, 1), col=.get("col.help"))

   .waitforclick()

   return()

}
