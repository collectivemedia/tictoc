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
\examples{
\dontrun{

## Basic use case
tic()
print("Do something...")
Sys.sleep(1)
toc()
# 1.034 sec elapsed

## Inline timing example, similar to system.time()
tic(); for(i in 1:1000000) { j = i / 2 }; toc()
# 0.527 sec elapsed

## Timing multiple steps
tic("step 1")
print("Do something...")
Sys.sleep(1)
toc()
# step 1: 1.005 sec elapsed

tic("step 2")
print("Do something...")
Sys.sleep(1)
toc()
# step 2: 1.004 sec elapsed

## Timing nested code
tic("outer")
   Sys.sleep(1)
   tic("middle")
      Sys.sleep(2)
      tic("inner")
         Sys.sleep(3)
      toc()
# inner: 3.004 sec elapsed
   toc()
# middle: 5.008 sec elapsed
toc()
# outer: 6.016 sec elapsed

## Timing in a loop and analyzing the results later using tic.log().
tic.clearlog()
for (x in 1:10)
{
   tic(x)
   Sys.sleep(1)
   toc(log = TRUE, quiet = TRUE)
}
log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
tic.clearlog()

timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
mean(timings)
# [1] 1.001
writeLines(unlist(log.txt))
# 1: 1.002 sec elapsed
# 2: 1 sec elapsed
# 3: 1.002 sec elapsed
# 4: 1.001 sec elapsed
# 5: 1.001 sec elapsed
# 6: 1.001 sec elapsed
# 7: 1.001 sec elapsed
# 8: 1.001 sec elapsed
# 9: 1.001 sec elapsed
# 10: 1 sec elapsed

## Using custom callbacks in tic/toc
my.msg.tic <- function(tic, msg)
{
   if (is.null(msg) || is.na(msg) || length(msg) == 0)
   {
      outmsg <- paste(round(toc - tic, 3), " seconds elapsed", sep="")
   }
   else
   {
      outmsg <- paste("Starting ", msg, "...", sep="")
   }
}

my.msg.toc <- function(tic, toc, msg, info)
{
   if (is.null(msg) || is.na(msg) || length(msg) == 0)
   {
      outmsg <- paste(round(toc - tic, 3), " seconds elapsed", sep="")
   }
   else
   {
      outmsg <- paste(info, ": ", msg, ": ",
                   round(toc - tic, 3), " seconds elapsed", sep="")
   }
}

tic("outer", quiet = FALSE, func.tic = my.msg.tic)
# Starting outer...
   Sys.sleep(1)
   tic("middle", quiet = FALSE, func.tic = my.msg.tic)
# Starting middle...
      Sys.sleep(2)
      tic("inner", quiet = FALSE, func.tic = my.msg.tic)
         Sys.sleep(3)
# Starting inner...
      toc(quiet = FALSE, func.toc = my.msg.toc, info = "INFO")
# INFO: inner: 3.005 seconds elapsed
   toc(quiet = FALSE, func.toc = my.msg.toc, info = "INFO")
# INFO: middle: 5.01 seconds elapsed
toc(quiet = FALSE, func.toc = my.msg.toc, info = "INFO")
# INFO: outer: 6.014 seconds elapsed

}
}
\seealso{
\code{\link{tictoc}}, \code{\link{Stack}}
}

