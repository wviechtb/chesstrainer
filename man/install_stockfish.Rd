\name{install_stockfish}
\alias{install_stockfish}
\title{Function to Install Stockfish under Windows}
\description{
   Function to install the Stockfish chess engine under Windows.
}
\usage{
install_stockfish()
}
\details{

   The trainer can make use of the Stockfish chess engine to obtain position evaluations and for the \emph{play mode} to play against the engine. The present function installs Stockfish automatically for you. When running the trainer, it will then automatically detect this installation and make use of it.

   Note that this function only works under Windows. Under macOS and Linux, you can download and install Stockfish from \url{https://stockfishchess.org/download/} or obtain it via your operating system package manager.

}
\value{
   The function does not return an object.
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\examples{
\dontrun{
install_stockfish()
}
}
