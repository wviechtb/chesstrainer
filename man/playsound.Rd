\name{playsound}
\alias{playsound}
\title{Play a Sound File}
\description{
   Function to play a sound file.
}
\usage{
playsound(sound, volume)
}
\arguments{
   \item{sound}{the sound file (given as a full path).}
   \item{volume}{the volume (as a proportion between 0 and 1).}
}
\details{

   The function is used by the package to play sounds. This is done by invoking system commands via the \code{\link{system}} function. This may or may not work depending on your operating system and configuration.

   For Linux, the function uses \code{paplay}. For macOS, the function uses \code{afplay}. Note that these commands are assumed to be on the user path.

   Windows does not provide a simple way to play sound files from the command line by default. To make sounds work under Windows, download the lightweight and open-source mpg123 player from \verb{https://www.mpg123.de} (click on \sQuote{download}, go to \sQuote{Win32 and Win64 binaries}, choose the link for the 64 bit binary, download the \sQuote{static} zip file, extract it to some directory on your computer, and then place this directory on your system path; if you do not know how to do this last step, see: \verb{https://stackoverflow.com/q/9546324/2615367}). Note that the volume cannot be adjusted with this player, so the \code{volume} argument is ignored.

   \emph{For advanced users:}

   If the sound is not working, but you know what the appropriate command is that will work on your system, you can write your own \code{playsound} function (see the code for \code{playsound} to use as a template). Then use \code{assignInNamespace("playsound", playsound, ns="chesstrainer")} to replace the \code{playsound()} function with your own version. Then use the example code below to test if sound is working.

}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\examples{
# test if the sound is working
\dontrun{
playsound(system.file("sounds", "complete.ogg", package="chesstrainer"), 0.5)
}
}
