#-------------------------------------------------------------------------------
#
# Package tictoc 
#
# General description 
# 
# Sergei Izrailev, 2011-2014
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
#' Functions for timing, as well as implementations of Stack and List structures.
#'
#' This package provides the timing functions \code{tic} and \code{toc} that can be nested. 
#' One can record all timings while a complex script is running, and examine the values later.
#' 
#' In addition, this package provides classes (\code{\link{Stack}}), implemented 
#' as a \code{vector}, and \code{\link{List}}, implemented as a \code{list}, 
#' both of which support operations \code{push}, \code{pop}, \code{first}, 
#' \code{last}, \code{clear} and \code{size}.
#' 
#' @name tictoc
#' @aliases tictoc
#' @docType package
#' @title Package tictoc. 
#' @author Sergei Izrailev
#' @section Copyright: Copyright (C) Collective, Inc.
#' @section License: Apache License, Version 2.0, 
#'    available at http://www.apache.org/licenses/LICENSE-2.0
#' @section URL: http://github.com/collectivemedia/tictoc
#' @keywords timing profiling stack list
#' @import methods
#' 
# The next and last line should be the word 'NULL'.
NULL
