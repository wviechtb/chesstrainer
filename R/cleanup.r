cleanup <- function(config=FALSE, colors=FALSE, seqdir=FALSE, history=FALSE, cache=FALSE) {

   interactive <- TRUE

   if (config || colors || seqdir || history || cache)
      interactive <- FALSE

   cat("\n")

   # remove the config file with the settings

   file.config <- file.path(tools::R_user_dir(package="chesstrainer", which="config"), "settings.rds")

   if (file.exists(file.config)) {

      cat("Config file found at:", file.config, "\n")

      if (interactive) {
         resp <- readline(prompt="Remove the config file with the settings (y/N)? ")
         dorm <- identical(resp, "y")
      } else {
         dorm <- isTRUE(config)
      }

      if (dorm) {
         res <- file.remove(file.config)
         if (res) {
            cat("File removed successfully.\n")
         } else {
            cat("File could not be removed.\n")
         }
      }

      cat("\n")

   }

   # remove the config file with the colors

   file.colors <- file.path(tools::R_user_dir(package="chesstrainer", which="config"), "colors.rds")

   if (file.exists(file.colors)) {

      cat("Color settings file found at:", file.colors, "\n")

      if (interactive) {
         resp <- readline(prompt="Remove the file with the color settings (y/N)? ")
         dorm <- identical(resp, "y")
      } else {
         dorm <- isTRUE(colors)
      }

      if (dorm) {
         res <- file.remove(file.colors)
         if (res) {
            cat("File removed successfully.\n")
         } else {
            cat("File could not be removed.\n")
         }
      }

      cat("\n")

   }

   # check if the config directory is empty; if it is, remove it

   dir.config <- tools::R_user_dir(package="chesstrainer", which="config")
   files.config <- list.files(dir.config)

   if (length(files.config) == 0L)
      unlink(dir.config, recursive=TRUE)

   # remove the default directory where sequences are stored (and all files therein)

   dir.seqdir <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sequences")

   if (file.exists(dir.seqdir)) {

      cat("Directory for sequences found at:", dir.seqdir, "\n")

      if (interactive) {
         resp <- readline(prompt="Remove the directory with the sequences (y/N)? ")
         dorm <- identical(resp, "y")
      } else {
         dorm <- isTRUE(seqdir)
      }

      if (dorm) {
         res <- unlink(dir.seqdir, recursive=TRUE)
         if (res == 0) {
            cat("Directory removed successfully.\n")
         } else {
            cat("Directory could not be removed.\n")
         }
      }

      cat("\n")

   }

   # remove the directory where the session history of players are stored (and all files therein)

   dir.history <- file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions")

   if (file.exists(dir.history)) {

      cat("Directory for session histories found at:", dir.history, "\n")

      if (interactive) {
         resp <- readline(prompt="Remove the directory with the session histories (y/N)? ")
         dorm <- identical(resp, "y")
      } else {
         dorm <- isTRUE(history)
      }

      if (dorm) {
         res <- unlink(dir.history, recursive=TRUE)
         if (res == 0) {
            cat("Directory removed successfully.\n")
         } else {
            cat("Directory could not be removed.\n")
         }
      }

      cat("\n")

   }

   # remove the cache directory (and all files therein)

   dir.cache <- file.path(tools::R_user_dir(package="chesstrainer", which="cache"))

   if (file.exists(dir.cache)) {

      cat("Directory for the cache found at:", dir.cache, "\n")

      if (interactive) {
         resp <- readline(prompt="Remove the cache directory (y/N)? ")
         dorm <- identical(resp, "y")
      } else {
         dorm <- isTRUE(cache)
      }

      if (dorm) {
         res <- unlink(dir.cache, recursive=TRUE)
         if (res == 0) {
            cat("Directory removed successfully.\n")
         } else {
            cat("Directory could not be removed.\n")
         }
      }

      cat("\n")

   }

   # check if the data directory is empty; if it is, remove it

   dir.data <- tools::R_user_dir(package="chesstrainer", which="data")
   files.data <- list.files(dir.data)

   if (length(files.data) == 0L)
      unlink(dir.data, recursive=TRUE)

}
