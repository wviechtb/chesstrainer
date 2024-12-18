.is.even <- function(x) x %% 2 == 0

.get <- function(x)
   get(x, envir=.chesstrainer)
