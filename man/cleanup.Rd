\name{cleanup}
\alias{cleanup}
\title{Clean up Files and Directories}
\description{
   Function to clean up files and directories created by the \pkg{chesstrainer} package.
}
\usage{
cleanup(config=FALSE, colors=FALSE, seqdir=FALSE, history=FALSE)
}
\arguments{
   \item{config}{logical to determine if the config file should be removed.}
   \item{colors}{logical to determine if the color settings file should be removed.}
   \item{seqdir}{logical to determine if the default directory where sequences are stored (and all files therein) should be removed.}
   \item{history}{logical to determine if the directory where the session histories of players are stored (and all files therein) should be removed.}
}
\details{

   The function can be used to clean up files and directories created by the \pkg{chesstrainer} package.

   If the function is called with all argument to set to \code{FALSE} (the default), then it will enter an interactive mode. Prompts are then issued in the console to determine which files and directories should be removed.

   If at least one argument is set to \code{TRUE}, the the arguments determine which files and directories should be removed.

   Removing the config file or the colors settings file may be useful in case these files were corrupted with nonsensical settings. The files will be recreated (with default values) when restarting the trainer.

   Be careful with removing the other files and directories! Once deleted, they cannot be recovered.

}
\value{
   The function does not return an object.
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\examples{
# run the cleanup function in interactive mode
\dontrun{
cleanup()
}
}
