\docType{package}
\name{tictoc}
\alias{tictoc}
\alias{tictoc-package}
\title{Functions for timing, as well as implementations of Stack and List structures.}
\description{
Functions for timing, as well as implementations of Stack
and List structures. \tabular{ll}{ Package: \tab tictoc\cr
Type: \tab Package\cr Version: \tab 3.5\cr Date: \tab
2014-06-07\cr License: \tab Apache License, Version 2.0\cr
Author: \tab Sergei Izrailev\cr Maintainer: \tab Sergei
Izrailev <sizrailev@collective.com>\cr LazyLoad: \tab
yes\cr }
}
\details{
This package provides the timing functions \code{tic} and
\code{toc} that can be nested. One can record all timings
while a complex script is running, and examine the values
later.

In addition, this package provides classes
(\code{\link{Stack}}), implemented as a \code{vector}, and
\code{\link{List}}, implemented as a \code{list}, both of
which support operations \code{push}, \code{pop},
\code{first}, \code{last}, \code{clear} and \code{size}.
}
\author{
Sergei Izrailev
}
\keyword{list}
\keyword{profiling}
\keyword{stack}
\keyword{timing}

