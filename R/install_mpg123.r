install_mpg123 <- function() {

   iswin <- .Platform$OS.type == "windows"

   if (!iswin) {
      cat(.text("mpg123onlywin"))
      return(invisible())
   }

   mpg123dir <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "mpg123")

   if (dir.exists(mpg123dir)) {
      cat(.text("priormpg123", mpg123dir))
      overwrite <- readline(prompt=.text("overwriteinst"))
      if (.confirm(overwrite)) {
         unlink(mpg123dir, recursive=TRUE)
      } else {
         return(invisible())
      }
   }

   zipdir <- tempdir()
   zipfile <- tempfile(fileext=".zip", tmpdir=zipdir)
   download.file("https://www.mpg123.de/download/win64/1.32.10/mpg123-1.32.10-static-x86-64.zip", destfile=zipfile)
   unzip(zipfile, exdir=zipdir)
   dir.create(mpg123dir, recursive=TRUE)
   file.copy(from=list.files(file.path(zipdir, "mpg123-1.32.10-static-x86-64"), full.names=TRUE), to=mpg123dir, recursive=TRUE)
   cat(.text("installedmpg123", mpg123dir))

   return(invisible())

}
