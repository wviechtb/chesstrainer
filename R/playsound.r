playsound <- function(sound, volume) {

   volume[volume < 0] <- 0
   volume[volume > 1] <- 1

   if (Sys.info()[["sysname"]] == "Linux") {
      volume <- round(volume * 65536)
      if (volume > 0)
         system(paste0("paplay --volume=", volume, " ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
   }

   if (Sys.info()[["sysname"]] == "Darwin") {
      sound <- sub(".ogg", ".mp3", sound, fixed=TRUE)
      if (volume > 0)
         system(paste0("afplay -v ", volume, " ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
   }

   if (Sys.info()[["sysname"]] == "Windows") {
      sound <- sub(".ogg", ".mp3", sound, fixed=TRUE)
      system(paste0("mpg123 ", sound), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      #powershell -c (New-Object Media.SoundPlayer "C:\Windows\Media\notify.wav").PlaySync();
   }

   invisible()

}
