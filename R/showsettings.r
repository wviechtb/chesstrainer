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

   maxchars <- max(nchar(txt))

   lines <- paste0(rep("-", maxchars), collapse="")
   txt <- c(lines, txt[-1], lines)

   rect(1+0.2, 1+0.2, 9-0.2, 9-0.2, col=.get("col.bg"), border=.get("col.help.border"), lwd=lwd+3)

   maxsw <- max(sapply(txt, strwidth, family=.get("font.mono")))
   maxsh <- max(sapply(txt, strheight, family=.get("font.mono"))) * length(txt)
   cex <- min(1.5, (8.5 - 1.5) / max(maxsw, maxsh) * 0.95)

   nrows <- length(txt)
   ypos <- seq(min(7.5, 5+nrows*0.25), max(2.5, 5-nrows*0.25), length.out=length(txt))
   text(1.5, ypos, txt, pos=4, cex=cex, family=.get("font.mono"), col=.get("col.help"))

   if (nchar(seqdir) > maxchars-3)
      seqdir <- paste0("...", substr(seqdir, nchar(seqdir)-maxchars-3+1, nchar(seqdir)))
   if (nchar(sfpath) > maxchars-3)
      sfpath <- paste0("...", substr(sfpath, nchar(sfpath)-maxchars-3+1, nchar(sfpath)))

   text(1.5, min(ypos)-1*(ypos[1]-ypos[2]), .text("seqdirsettings", seqdir), pos=4, cex=cex, family=.get("font.mono"), col=.get("col.help"))
   text(1.5, min(ypos)-2*(ypos[1]-ypos[2]), .text("sfpathsettings", sfpath), pos=4, cex=cex, family=.get("font.mono"), col=.get("col.help"))

   .waitforclick()

   return()

}
