\name{tic}
\alias{tic}
\alias{tic.clear}
\alias{tic.clearlog}
\alias{tic.log}
\alias{toc}
\alias{toc.outmsg}
\title{Timing utilities.}
\usage{
tic(msg = NULL, quiet = TRUE, func.tic = NULL, ...)

toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg, ...)

toc.outmsg(tic, toc, msg)

tic.clearlog()

tic.clear()

tic.log(format = TRUE)
}
\arguments{
  \item{msg}{- a text string associated with the timer. It
  gets printed on a call to \code{toc()}}

  \item{func.tic}{Function producing the formatted message
  with a signature \code{f(tic, toc, msg, ...)}.  Here,
  parameters \code{tic} and \code{toc} are the elapsed
  process times in seconds, so the time elapsed between the
  \code{tic()} and \code{toc()} calls is computed by
  \code{toc - tic}. \code{msg} is the string passed to the
  \code{tic()} call.}

  \item{...}{The other parameters that are passed to
  \code{func.tic} and \code{func.toc}.}

  \item{log}{- When \code{TRUE}, pushes the timings and the
  message in a list of recorded timings.}

  \item{quiet}{When \code{TRUE}, doesn't print any
  messages}

  \item{func.toc}{Function producing the formatted message
  with a signature \code{f(tic, toc, msg, ...)}.  Here,
  parameters \code{tic} and \code{toc} are the elapsed
  process times in seconds, so the time elapsed between the
  \code{tic()} and \code{toc()} calls is computed by
  \code{toc - tic}. \code{msg} is the string passed to the
  \code{tic()} call.}

  \item{tic}{Time from the call to tic()
  (\code{proc.time()["elapsed"]})}

  \item{toc}{Time from the call to toc()
  (\code{proc.time()["elapsed"]})}

  \item{format}{When true, \code{tic.log} returns a list of
  formatted \code{toc()} output, otherwise, returns the raw
  results.}
}
\value{
\code{tic} returns the timestamp (invisible).

\code{toc} returns an (invisible) list containing the
timestamps \code{tic}, \code{toc}, and the message
\code{msg}.

\code{toc.outmsg} returns formatted message.

\code{tic.log} returns a list of formatted messages
(\code{format = TRUE}) or a list of lists containing the
timestamps and unformatted messages from prior calls to
tic/toc.
}
\description{
\code{tic} - Starts the timer and stores the start time and
the message on the stack.

\code{toc} - Notes the current timer and computes elapsed
time since the matching call to \code{tic()}. When
\code{quiet} is \code{FALSE}, prints the associated message
and the elapsed time.

\code{toc.outmsg} - Formats a message for pretty printing.
Redefine this for different formatting.

\code{tic.clearlog} - Clears the tic/toc log.

\code{tic.clear} - Clears the tic/toc stack. This could be
useful in cases when because of an error the closing toc()
calls never get executed.

\code{tic.log} - Returns log messages from calls to tic/toc
since the last call to \code{\link{tic.clearlog}}.
}
\seealso{
\code{\link{Stack}}
}

