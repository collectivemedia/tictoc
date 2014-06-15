#-------------------------------------------------------------------------------
#
# Package tictoc
#
# tic() and toc() timing functions.  
#
# Sergei Izrailev, 2011-2012
#-------------------------------------------------------------------------------
# Copyright 2011-2014 Collective, Inc.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Attribution notice: the idea to store the timing in the baseenv() came from the post 
# http://stackoverflow.com/questions/1716012/stopwatch-function-in-r
# by http://stackoverflow.com/users/134830/richie-cotton
# stackoverflow license: http://creativecommons.org/licenses/by-sa/2.5/
#
# tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
# {
#    type <- match.arg(type)
#    assign(".type", type, envir=baseenv())
#    if(gcFirst) gc(FALSE)
#    tic <- proc.time()[type]         
#    assign(".tic", tic, envir=baseenv())
#    invisible(tic)
# }
# 
# toc <- function()
# {
#    type <- get(".type", envir=baseenv())
#    toc <- proc.time()[type]
#    tic <- get(".tic", envir=baseenv())
#    print(toc - tic)
#    invisible(toc)
# }
#-------------------------------------------------------------------------------

#' \code{tic} - Starts the timer and stores the start time and the message on the stack.
#' @name tic
#' @aliases toc.outmsg tic.clearlog tic.clear tic.log tic toc 
#' @param msg - a text string associated with the timer. It gets printed on a call to \code{toc()}
#' @param func.tic Function producing the formatted message with a signature \code{f(tic, toc, msg, ...)}.
#'        Here, parameters \code{tic} and \code{toc} are the elapsed process 
#'        times in seconds, so the time elapsed between the \code{tic()} and 
#'        \code{toc()} calls is computed by \code{toc - tic}. \code{msg} is the string
#'        passed to the \code{tic()} call. 
#' @param ... The other parameters that are passed to \code{func.tic} and \code{func.toc}. 
#' @return \code{tic} returns the timestamp (invisible).
#' @title Timing utilities.
#' @examples
#' \dontrun{
#' 
#' ## Basic use case
#' tic()
#' print("Do something...")
#' Sys.sleep(1)
#' toc()
#' # 1.034 sec elapsed
#' 
#' ## Inline timing example, similar to system.time()
#' tic(); for(i in 1:1000000) { j = i / 2 }; toc()
#' # 0.527 sec elapsed
#' 
#' ## Timing multiple steps 
#' tic("step 1")
#' print("Do something...")
#' Sys.sleep(1)
#' toc()
#' # step 1: 1.005 sec elapsed
#' 
#' tic("step 2")
#' print("Do something...")
#' Sys.sleep(1)
#' toc()
#' # step 2: 1.004 sec elapsed
#' 
#' ## Timing nested code
#' tic("outer")
#'    Sys.sleep(1) 
#'    tic("middle") 
#'       Sys.sleep(2) 
#'       tic("inner")
#'          Sys.sleep(3) 
#'       toc()
#' # inner: 3.004 sec elapsed
#'    toc()
#' # middle: 5.008 sec elapsed
#' toc()
#' # outer: 6.016 sec elapsed
#' 
#' ## Timing in a loop and analyzing the results later using tic.log().
#' tic.clearlog()
#' for (x in 1:10)
#' {
#'    tic(x)
#'    Sys.sleep(1)
#'    toc(log = TRUE, quiet = TRUE)
#' }
#' log.txt <- tic.log(format = TRUE)
#' log.lst <- tic.log(format = FALSE)
#' tic.clearlog()
#' 
#' timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
#' mean(timings)
#' # [1] 1.001
#' writeLines(unlist(log.txt))
#' # 1: 1.002 sec elapsed
#' # 2: 1 sec elapsed
#' # 3: 1.002 sec elapsed
#' # 4: 1.001 sec elapsed
#' # 5: 1.001 sec elapsed
#' # 6: 1.001 sec elapsed
#' # 7: 1.001 sec elapsed
#' # 8: 1.001 sec elapsed
#' # 9: 1.001 sec elapsed
#' # 10: 1 sec elapsed
#' 
#' ## Using custom callbacks in tic/toc
#' my.msg.tic <- function(tic, msg)
#' {
#'    if (is.null(msg) || is.na(msg) || length(msg) == 0) 
#'    {
#'       outmsg <- paste(round(toc - tic, 3), " seconds elapsed", sep="")
#'    } 
#'    else 
#'    {
#'       outmsg <- paste("Starting ", msg, "...", sep="")
#'    }      
#' }
#' 
#' my.msg.toc <- function(tic, toc, msg, info)
#' {
#'    if (is.null(msg) || is.na(msg) || length(msg) == 0) 
#'    {
#'       outmsg <- paste(round(toc - tic, 3), " seconds elapsed", sep="")
#'    }
#'    else 
#'    {
#'       outmsg <- paste(info, ": ", msg, ": ", 
#'                    round(toc - tic, 3), " seconds elapsed", sep="")
#'    }      
#' }
#' 
#' tic("outer", quiet = FALSE, func.tic = my.msg.tic)
#' # Starting outer...
#'    Sys.sleep(1) 
#'    tic("middle", quiet = FALSE, func.tic = my.msg.tic) 
#' # Starting middle...
#'       Sys.sleep(2) 
#'       tic("inner", quiet = FALSE, func.tic = my.msg.tic)
#'          Sys.sleep(3) 
#' # Starting inner...
#'       toc(quiet = FALSE, func.toc = my.msg.toc, info = "INFO")
#' # INFO: inner: 3.005 seconds elapsed
#'    toc(quiet = FALSE, func.toc = my.msg.toc, info = "INFO")
#' # INFO: middle: 5.01 seconds elapsed
#' toc(quiet = FALSE, func.toc = my.msg.toc, info = "INFO")
#' # INFO: outer: 6.014 seconds elapsed
#' 
#' }
#' @export
#' @rdname tic
tic <- function(msg = NULL, quiet = TRUE, func.tic = NULL, ...)
{ 
   stim <- get(".tictoc", envir=baseenv())
   smsg <- get(".ticmsg", envir=baseenv())
   tic <- proc.time()["elapsed"]
   if (!is.null(func.tic))
   {
      outmsg <- func.tic(tic, msg, ...)
      if (!quiet) writeLines(outmsg)      
   }
   push(stim, tic)
   push(smsg, msg)
   invisible(tic)
}

#-------------------------------------------------------------------------------
# TODO: pass toc.outmsg to tic/toc and tic.log with a default, passing ... 

#' \code{toc} - Notes the current timer and computes elapsed time since the matching call to \code{tic()}.
#' When \code{quiet} is \code{FALSE}, prints the associated message and the elapsed time.
#' @param log - When \code{TRUE}, pushes the timings and the message in a list of recorded timings.
#' @param quiet When \code{TRUE}, doesn't print any messages
#' @param func.toc Function producing the formatted message with a signature \code{f(tic, toc, msg, ...)}.
#'        Here, parameters \code{tic} and \code{toc} are the elapsed process 
#'        times in seconds, so the time elapsed between the \code{tic()} and 
#'        \code{toc()} calls is computed by \code{toc - tic}. \code{msg} is the string
#'        passed to the \code{tic()} call. 
#' @return \code{toc} returns an (invisible) list containing the timestamps \code{tic}, \code{toc}, and the message \code{msg}.
#' @seealso \code{\link{tictoc}}, \code{\link{Stack}} 
#' @export
#' @rdname tic
toc <- function(log = FALSE, quiet = FALSE, func.toc = toc.outmsg, ...)
{
   toc <- proc.time()["elapsed"]
   stim <- get(".tictoc", envir=baseenv())
   smsg <- get(".ticmsg", envir=baseenv())
   if (size(.tictoc) == 0) return(invisible(NULL))
   tic <- pop(stim)
   msg <- pop(smsg)
   if (!is.null(func.toc))
   {
      outmsg <- func.toc(tic, toc, msg, ...)
      if (!quiet) writeLines(outmsg)      
   }
   res <- list(tic=tic, toc=toc, msg=msg)
   if (log) 
   {
      ticlog <- get(".ticlog", envir=baseenv())
      push(ticlog, res)
   }
   invisible(res)
}

#-------------------------------------------------------------------------------

#' \code{toc.outmsg} - Formats a message for pretty printing. Redefine this for different formatting.
#' @param tic Time from the call to tic() (\code{proc.time()["elapsed"]})
#' @param toc Time from the call to toc() (\code{proc.time()["elapsed"]})
#' @return \code{toc.outmsg} returns formatted message.
#' @export
#' @rdname tic
toc.outmsg <- function(tic, toc, msg)
{
   if (is.null(msg) || is.na(msg) || length(msg) == 0) outmsg <- paste(round(toc - tic, 3), " sec elapsed", sep="")
   else outmsg <- paste(msg, ": ", round(toc - tic, 3), " sec elapsed", sep="")   
}

#-------------------------------------------------------------------------------

#' \code{tic.clearlog} - Clears the tic/toc log.
#' @export
#' @rdname tic
tic.clearlog <- function() 
{
   ticlog <- get(".ticlog", envir=baseenv())
   clear(ticlog)
}

#-------------------------------------------------------------------------------

#' \code{tic.clear} - Clears the tic/toc stack. This could be useful in cases when because of an error
#' the closing toc() calls never get executed.
#' @export
#' @rdname tic
tic.clear <- function() 
{
   stim <- get(".tictoc", envir=baseenv())
   smsg <- get(".ticmsg", envir=baseenv())
   clear(stim)
   clear(smsg)
}

#-------------------------------------------------------------------------------

#' \code{tic.log} - Returns log messages from calls to tic/toc since the last call to \code{\link{tic.clearlog}}.
#' @param format When true, \code{tic.log} returns a list of formatted \code{toc()} output, otherwise, returns the raw results.
#' @return \code{tic.log} returns a list of formatted messages (\code{format = TRUE}) or a list
#'          of lists containing the timestamps and unformatted messages from prior calls to tic/toc.
#' @export
#' @rdname tic
tic.log <- function(format = TRUE)
{
   lst <- get(".ticlog", envir=baseenv())$.Data
   if (format) return(lapply(lst, function(x) toc.outmsg(x$tic, x$toc, x$msg)))
   else return(lst)
}

#-------------------------------------------------------------------------------
