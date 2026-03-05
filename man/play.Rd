\name{play}
\alias{play}
\title{Start the Chess Trainer}
\description{
   Function to start the chess trainer.
}
\usage{
play(lang="en", \dots)
}
\arguments{
   \item{lang}{character string to specify the language (either \code{"en"} for English or \code{"de"} for German).}
   \item{\dots}{other arguments}
}
\details{
   This is the main function to start the chess trainer. See the \pkg{\link{chesstrainer-package}} documentation for an introduction to the package.
}
\value{
   The function does not return an object.
}
\note{

   Most of the interactions with the chess trainer happen in the plotting window where the board is drawn and the pieces are moved. However, for some actions, one must switch to the R console. Unfortunately, except when using R under Windows with \code{Rgui}, there is no programmatic way in R to activate the console or the plotting window. Therefore, one must manually switch to the console or back to the plotting window, which is a bit tedious. However, your operating system may allow activating windows via the command line through various means. For example, under Linux with an X Window manager (not Wayland unfortunately), this should be possible with the \command{wmctrl} command. To make use of such functionality, one can pass two strings via arguments called \code{switch1} and \code{switch2} to the \code{play()} function that are evaluated whenever a switch to the R console or back to the plotting window should happen. For example, with:

\verb{play(switch1="system('wmctrl -x -a gnome-terminal'), switch2="system('wmctrl -x -a r_x11'))"}

   one may be able to achieve this automatic window switching. Of course, this requires \command{wmctrl} to be installed and the window names must match those used by the operating system. Under macOS, it should be possible to achieve the same thing using \command{osascript}. Under Windows, the trainer uses the \code{bringToTop()} function to achieve the same thing automatically when using \code{Rgui}.

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
