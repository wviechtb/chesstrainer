\name{play}
\alias{play}
\title{Start the Chess Trainer}
\description{
   Function to start the chess trainer.
}
\usage{
play(player="", lang="en", seqdir="", sfpath="", depth1=20, depth2=30, \dots)
}
\arguments{
   \item{player}{character string to specify a player. If blank, the trainer will prompt for a (new) player name.}
   \item{lang}{character string to specify the language (either \code{"en"} for English or \code{"de"} for German).}
   \item{seqdir}{character string giving the path to a directory where sequences will be stored. If blank, the default directory is used.}
   \item{sfpath}{character string giving the path (including the executable) to Stockfish. If blank, Stockfish is not used for position evaluations.}
   \item{depth1}{integer to specify the search depth for a quick evaluation (only relevant if Stockfish is running).}
   \item{depth2}{integer to specify the search depth for a deep evaluation (only relevant if Stockfish is running).}
   \item{\dots}{other arguments}
}
\details{
   This is the main function to start the chess trainer. See the \pkg{\link{chesstrainer-package}} documentation for an introduction to the package.
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
