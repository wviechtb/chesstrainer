\name{play}
\alias{play}
\title{Start the Chess Trainer}
\description{
   Function to start the chess trainer.
}
\usage{
play(player="", mode="add", sleep=0.5, volume=0.5,
     lwd=2, cex.top=1.4, cex.bot=0.7,
     expval=2, pause=TRUE, random=TRUE, lang="en", \dots)
}
\arguments{
   \item{player}{character string to specify a player. If blank, the trainer will prompt for a (new) player name.}
   \item{mode}{character string to specify the start mode (either \code{"add"} or \code{"play"}).}
   \item{sleep}{numeric value to specify the time (in seconds) between moves.}
   \item{volume}{numeric value to specify the sound volume (as a proportion between 0 and 1).}
   \item{lwd}{numeric value to specify the line width for drawing rectangles.}
   \item{cex.top}{numeric value to specify the size for text at the top.}
   \item{cex.bot}{numeric value to specify the size for text at the bottom.}
   \item{expval}{numeric value to specify the exponent value for sequence scores to adjust the selection probabilities.}
   \item{pause}{logical to specify whether the trainer should pause after a completed sequence.}
   \item{random}{logical to specify whether sequences should be selected at random or sequentially.}
   \item{lang}{character string to specify the language (either \code{"en"} for English or \code{"de"} for German).}
   \item{\dots}{other arguments}
}
\details{
   This is the main function to start the chess trainer. See the \pkg{\link{chesstrainer-package}} documentation for an introduction to the package and the purpose of these arguments.

   All of the arguments above can also be adjusted while the player is running. Note that these settings are also automatically saved by the chess trainer across sessions.
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com}).
}
\examples{
# start playing
\dontrun{
play()
}
}
