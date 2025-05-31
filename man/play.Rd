\name{play}
\alias{play}
\title{Start the Chess Trainer}
\description{
   Function to start the chess trainer.
}
\usage{
play(lang="en", sfpath="", \dots)
}
\arguments{
   \item{lang}{character string to specify the language (either \code{"en"} for English or \code{"de"} for German).}
   \item{sfpath}{character string giving the path (including the executable) to Stockfish. If blank, Stockfish is not used for position evaluations.}
   \item{\dots}{other arguments}
}
\details{
   This is the main function to start the chess trainer. See the \pkg{\link{chesstrainer-package}} documentation for an introduction to the package.
}
\value{
   The function does not return an object.
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
