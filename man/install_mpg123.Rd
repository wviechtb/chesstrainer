\name{install_mpg123}
\alias{install_mpg123}
\title{Function to Install mpg123 under Windows.}
\description{
   Function to install the mpg123 player under Windows.
}
\usage{
install_mpg123()
}
\details{

   The trainer plays various sounds (e.g., when playing moves, when capturing pieces, when making a wrong move, when completing a sequence). See the \code{\link{playsound}} function for further details.

   Windows does not provide a simple way to play sound files from the command line by default. To make sounds work under Windows, one can use the lightweight and open-source mpg123 player from \verb{https://www.mpg123.de}. The present function installs the player automatically for you.

}
\value{
   The function does not return an object.
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\examples{
\dontrun{
install_mpg123()
}
}
