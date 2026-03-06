install_stockfish <- function() {

   iswin <- .Platform$OS.type == "windows"

   if (!iswin) {
      cat(.text("sfonlywin"))
      return(invisible())
   }

   sfdir <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "stockfish")

   if (dir.exists(sfdir)) {
      cat(.text("priorsf", sfdir))
      overwrite <- readline(prompt=.text("overwriteinst"))
      if (.confirm(overwrite)) {
         unlink(sfdir, recursive=TRUE)
      } else {
         return(invisible())
      }
   }

   zipdir <- tempdir()
   zipfile <- tempfile(fileext=".zip", tmpdir=zipdir)
   download.file("https://github.com/official-stockfish/Stockfish/releases/latest/download/stockfish-windows-x86-64-avx2.zip", destfile=zipfile)
   unzip(zipfile, exdir=zipdir)
   dir.create(sfdir, recursive=TRUE)
   file.copy(from=list.files(file.path(zipdir, "stockfish"), pattern=".exe$", full.names=TRUE, recursive=TRUE), to=sfdir, recursive=TRUE)
   cat(.text("installedsf", sfdir))

   return(invisible())

}
