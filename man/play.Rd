\name{play}
\alias{play}
\title{Start the Chess Trainer}
\description{
   Function to start the chess trainer.
}
\usage{
play(player="", mode="add", sleep=0.5, volume=0.5, lwd=2,
     expval=2, pause=TRUE, lang="en", random=TRUE, \dots)
}
\arguments{
   \item{player}{character string to specify a player. If blank, the trainer will prompt for a (new) player name.}
   \item{mode}{character string to specify the start mode (either \code{"add"} or \code{"play"}).}
   \item{sleep}{numeric value to specify the time (in seconds) between moves.}
   \item{volume}{numeric value to specify the sound volume (as a proportion between 0 and 1).}
   \item{lwd}{numeric value to specify the line width for drawing rectangles.}
   \item{expval}{numeric value to specify the exponent value for sequence scores to adjust the selection probabilities.}
   \item{pause}{logical to specify whether the trainer should pause after a completed sequence.}
   \item{lang}{character string to specify the language (either \code{"en"} for English or \code{"de"} for German).}
   \item{random}{logical to specify whether sequences should be selected at random or sequentially.}
   \item{\dots}{other arguments}
}
\details{
   This is the main function to start the chess trainer. See the \pkg{\link{chesstrainer-package}} documentation for an introduction to the package.

   All of the arguments above can be adjusted while the player is running, except for \code{lwd}. Depending on the size of the plotting window and the screen resolution, one can adjust this value with some trial and error such that the rectangles that are drawn around squares are clearly visible, but do not overlap into adjacent squares.

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
