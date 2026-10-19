\name{man-devices}
\alias{man-devices}
\alias{man_devices}
\title{Notes on Graphics Devices}
\description{

   The trainer makes extensive use of \code{\link{getGraphicsEvent}} to capture mouse movements and keyboard inputs. Only some graphics devices support this. If you receive the error message \sQuote{The graphics device does not support event handling} when starting the trainer, then the default graphics device used does not support this kind of interactivity. For example, this will be the case for the \code{RStudioGD} graphics device that is used by RStudio. You can try running \code{windows()} (under Windows) or \code{x11()} (under macOS or Linux) before starting the trainer with \code{play()}.

   Under Windows, you could also run R with RGui, which uses the windows plotting device by default (this is actually the recommended way to run the trainer under Windows). Under macOS or Linux, you can just run R from the command line.

   If the graphics look fuzzy under RGui, then this might be related to high DPI scaling (under high resolutions, it is common to scale apps by 150\% or 200\% so that text and graphical elements are not tiny; see the display settings under Windows). To avoid this issue, go the the properties of RGui, select Compatibility, Change high DPI settings, High DPI scaling override, and select \sQuote{Application}. After restarting RGui, the graphics should look sharper.

   The \verb{<F10>} function key is registered under Windows, but is also the operating system key for activating the menu bar, which is annoying. You can use the \verb{<Ctrl-1>} keyboard shortcut instead. For some reason, the \verb{<F11>} and \verb{<F12>} function keys are not registered at all. You can use \verb{<Ctrl-2>} and \verb{<Ctrl-3>} respectively instead.

   Occasionally, the graphics window flickers under Windows, especially when making rapid updates to the graphics being shown. The exact cause of this is unknown.

}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\keyword{documentation}
