playsound <- function(sound, volume) {

   volume[volume < 0] <- 0
   volume[volume > 1] <- 1

   if (Sys.info()[["sysname"]] == "Linux") {
      if (volume > 0) {
         volume <- round(volume * 65536)
         system(paste0("paplay --volume=", volume, " ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      }
   }

   if (Sys.info()[["sysname"]] == "Darwin") {
      if (volume > 0) {
         sound <- sub(".ogg", ".mp3", sound, fixed=TRUE)
         system(paste0("afplay -v ", volume, " ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      }
   }

   if (Sys.info()[["sysname"]] == "Windows") {
      if (volume > 0) {
         sound <- sub(".ogg", ".mp3", sound, fixed=TRUE)
         system(paste0("mpg123 ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      }
   }

   invisible()

}
