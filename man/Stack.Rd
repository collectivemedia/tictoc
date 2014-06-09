\name{Stack}
\alias{List}
\alias{Stack}
\alias{as.Stack}
\alias{clear}
\alias{first}
\alias{last}
\alias{pop}
\alias{shift}
\alias{size}
\title{Stack and List classes and methods}
\usage{
\method{push}{default}(x, value)

\method{pop}{default}(x)

\method{clear}{default}(x)

\method{shift}{default}(x)

\method{first}{default}(x)

\method{last}{default}(x)

\method{size}{default}(x)

\method{as.Stack}{default}(s)

Stack()

List()
}
\arguments{
  \item{x}{A Stack or List object.}

  \item{value}{Value to append.}

  \item{s}{A structure to be converted to a Stack or List.}
}
\description{
\code{push} - Append an element.

\code{pop} - Remove and return the last element.

\code{clear} - Remove all elements.

\code{shift} - Remove and return the first element.

\code{first} - Return the first element.

\code{last} - Return the last element.

\code{size} - Return the number of elements.

\code{as.Stack} - Creates a new Stack from (typically,
vector) \code{s}.

\code{Stack()} - Creates and keeps a stack of items of the
same type, implemented as an R vector. The type is
determined by the first \code{push} operation.

\code{List()} - Creates and keeps a list of items,
implemented as an R list. The type is determined by the
first \code{push} operation.
}

