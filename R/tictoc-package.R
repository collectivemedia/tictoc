#-------------------------------------------------------------------------------
#
# Package tictoc
#
# General description
#
# Sergei Izrailev, 2011-2014
#-------------------------------------------------------------------------------
# Copyright 2011-2014 Collective, Inc.
# Portions are Copyright (C) 2017-2022 Jabiru Ventures LLC
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
#' Functions for timing, as well as implementations of Stack and List structures.
#'
#' The \code{tictoc} package provides the timing functions \code{tic} and
#' \code{toc} that can be nested. It provides an alternative to
#' \code{system.time()} with a different syntax similar to that in another
#' well-known software package. \code{tic} and \code{toc} are easy to use, and
#' are especially useful when timing several sections in more than a few lines
#' of code.
#'
#' In general, calls to \code{tic} and \code{toc} start the timer when
#' the \code{tic} call is made and stop the timer when the \code{toc} call is
#' made, recording the elapsed time between the calls from \code{proc.time}.
#' The default behavior is to print a simple message with the elapsed time in
#' the \code{toc} call.
#'
#' The features include the following:
#' \itemize{
#' \item nesting of the \code{tic} and \code{toc} calls
#' \item suppressing the default output with \code{quiet = TRUE}
#' \item collecting the timings in user-defined variables
#' \item {collecting the timings in a log structure provided by the package
#'   (see \code{\link{tic.log}})}
#' \item providing a custom message for each \code{tic} call
#' \item {using custom callbacks for the \code{tic} and \code{toc} calls to redefine
#'   the default behavior and/or add other functionality (such as logging to a database)}
#' }
#' In addition, this package provides classes \code{\link{Stack}} (implemented
#' as a \code{vector}) and \code{\link{List}} (implemented as a \code{list}),
#' both of which support operations \code{push}, \code{pop}, \code{first_element},
#' \code{last_element}, \code{clear} and \code{size}.
#'
#' @name tictoc
#' @aliases tictoc
#' @docType package
#' @title Package tictoc.
#' @author Sergei Izrailev
#' @section Copyright: Copyright (C) Collective, Inc.; with portions Copyright (C) Jabiru Ventures LLC
#' @section License: Apache License, Version 2.0,
#'    available at http://www.apache.org/licenses/LICENSE-2.0
#' @section URL: http://github.com/jabiru/tictoc
#' @section Installation from github:
#' \code{devtools::install_github("jabiru/tictoc")}
#' @keywords timing profiling stack list
#' @seealso \code{\link{tic}}, \code{\link{Stack}}
#' @import methods
#'
# The next and last line should be the word 'NULL'.
NULL
