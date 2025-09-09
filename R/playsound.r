playsound <- function(sound, volume) {

   if (volume > 0) {

      volume[volume > 100] <- 100
      volume <- volume / 100

      if (Sys.info()[["sysname"]] == "Linux") {
         volume <- round(volume * 65536)
         system(paste0("paplay --volume=", volume, " ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      }

      if (Sys.info()[["sysname"]] == "Darwin") {
         sound <- sub(".ogg", ".mp3", sound, fixed=TRUE)
         system(paste0("afplay -v ", volume, " ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      }

      if (Sys.info()[["sysname"]] == "Windows") {
         sound <- sub(".ogg", ".mp3", sound, fixed=TRUE)
         system(paste0("mpg123 ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      }

   }

   invisible()

}
