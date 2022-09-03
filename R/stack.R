#-------------------------------------------------------------------------------
#
# Package tictoc
#
# Stack and List
#
# Sergei Izrailev, 2011, 2014, 2022
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

# to satisfy R CMD check -- the .Data object is actually defined in S3 class.
.Data <- vector()

#-------------------------------------------------------------------------------
# STACK
#-------------------------------------------------------------------------------

#' Stack and List classes and methods
#'
#' \code{push} - Append an element.
#' @param x A Stack or List object.
#' @param value Value to append.
#' @param s A structure to be converted to a Stack or List.
#' @name Stack and List
#' @aliases Stack List push.default pop.default clear.default shift.default first_element.default last_element.default size.default as.Stack.default as.List.default
#' @rdname Stack
#' @export
push <- function(x, value) UseMethod("push")    # append an element

#-------------------------------------------------------------------------------

#' @description
#' \code{pop} - Remove and return the last element.
#' @rdname Stack
#' @export
pop  <- function(x) UseMethod("pop")            # pop the last element

#-------------------------------------------------------------------------------

#' @description
#' \code{clear} - Remove all elements.
#' @rdname Stack
#' @export
clear  <- function(x) UseMethod("clear")

#-------------------------------------------------------------------------------

#' @description
#' \code{shift} - Remove and return the first element.
#' @rdname Stack
#' @export
shift  <- function(x) UseMethod("shift")        # pop the first element

#-------------------------------------------------------------------------------

#' @description
#' \code{first_element} - Return the first element. We can't use \code{first} because
#' it's taken by the \code{dplyr} package and is not an S3 method.
#' @rdname Stack
#' @export
first_element  <- function(x) UseMethod("first_element")        # return the first element

#-------------------------------------------------------------------------------

#' @description
#' \code{last_element} - Return the last element. We can't use \code{last} because
#' it's taken by the \code{dplyr} package and is not an S3 method.
#' @rdname Stack
#' @export
last_element  <- function(x) UseMethod("last_element")        # return the last element

#-------------------------------------------------------------------------------

#' @description
#' \code{size} - Return the number of  elements.
#' @rdname Stack
#' @export
size  <- function(x) UseMethod("size")        # return the number of elements

#-------------------------------------------------------------------------------

#' @description
#' \code{as.Stack} - Creates a new Stack from (typically, vector) \code{s}.
#' @rdname Stack
#' @export
as.Stack <- function(s) UseMethod("as.Stack")

#' @export
as.Stack.default <- function(s)
{
   stack <- Stack()
   push(stack, s)
   stack
}

#' @description
#' \code{as.List} - Creates a new List from (typically, list) \code{s}.
#' @rdname Stack
#' @export
as.List <- function(s) UseMethod("as.List")

#' @export
as.List.default <- function(s)
{
   lst <- List()
   lst$.Data <- as.list(s)
   lst
}

#-------------------------------------------------------------------------------

#' @aliases push pop clear shift first_element last_element size
#' @export
push.default  <- function(x, value) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

#' @export
pop.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

#' @export
clear.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

#' @export
shift.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

#' @export
first_element.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

#' @export
last_element.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

#' @export
size.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

#' @export
push.Stack <- function(x, value) x$push(value)

#' @export
pop.Stack  <- function(x) x$pop()

#' @export
clear.Stack  <- function(x) x$clear()

#' @export
shift.Stack  <- function(x) x$shift()

#' @export
first_element.Stack  <- function(x) x$first()

#' @export
last_element.Stack  <- function(x) x$last()

#' @export
size.Stack  <- function(x) x$size()

#' @export
as.vector.Stack <- function(x, mode = "any") as.vector(x$.Data)

#' @export
print.Stack <- function(x, ...) print(x$.Data)

#' @export
as.list.List <- function(x, ...) as.list(x$.Data)

#' @export
print.List <- function(x, ...) print(x$.Data)

#-------------------------------------------------------------------------------

#' @description
#' \code{Stack()} - Creates and keeps a stack of items of the same type, implemented as an R vector.
#' The type is determined by the first \code{push} operation.
#' @rdname Stack
#' @export
Stack <- function()
{
   stack <- new.env()

   stack$.Data <- vector()

   stack$push <- function(x)
   {
      if (is.list(x)) stop("Can't push a list on a stack")
      .Data <<- c(.Data, x)
   }

   stack$pop  <- function()
   {
      tmp <- .Data[length(.Data)]
      .Data <<- .Data[-length(.Data)]
      return(tmp)
   }

   stack$clear <- function() .Data <<- vector()

   stack$shift  <- function()
   {
      tmp <- .Data[1]
      .Data <<- .Data[-1]
      return(tmp)
   }

   stack$first <- function()
   {
      if (length(.Data) == 0) {
         return(NA)
      }
      .Data[1]
   }

   stack$last <- function()
   {
      if (length(.Data) == 0) {
         return(NA)
      }
      .Data[length(.Data)]
   }

   stack$size <- function() length(.Data)

   environment(stack$push) <- as.environment(stack)
   environment(stack$pop) <- as.environment(stack)
   environment(stack$clear) <- as.environment(stack)
   environment(stack$shift) <- as.environment(stack)
   environment(stack$first) <- as.environment(stack)
   environment(stack$last) <- as.environment(stack)
   environment(stack$size) <- as.environment(stack)

   class(stack) <- "Stack"
   stack
}

#------------------------------------------------------------------------------

# LIST - keeps a list of items with append and clear operations
#' @export
push.List <- function(x, value, ...) x$push(value)

#' @export
pop.List  <- function(x) x$pop()

#' @export
clear.List  <- function(x) x$clear()

#' @export
shift.List  <- function(x) x$shift()

#' @export
first_element.List  <- function(x) x$first()

#' @export
last_element.List  <- function(x) x$last()

#' @export
size.List  <- function(x) x$size()

#' @description
#' \code{List()} - Creates and keeps a list of items of the same type, implemented as an R list.
#' The type is determined by the first \code{push} operation.
#' @rdname Stack
#' @export
List <- function()
{
   lst <- new.env()

   lst$.Data <- list()

   lst$push <- function(x)
   {
      .Data <<- c(.Data, 1)
      .Data[[length(.Data)]] <<- x
   }

   lst$pop  <- function()
   {
      tmp <- .Data[[length(.Data)]]
      .Data <<- .Data[-length(.Data)]
      return(tmp)
   }

   lst$clear <- function() .Data <<- list()

   lst$shift  <- function()
   {
      tmp <- .Data[[1]]
      .Data <<- .Data[-1]
      return(tmp)
   }

   lst$first <- function()
   {
      if (length(.Data) == 0) {
         return(NA)
      }
      .Data[[1]]
   }

   lst$last <- function()
   {
      if (length(.Data) == 0) {
         return(NA)
      }
      .Data[[length(.Data)]]
   }

   lst$size <- function() length(.Data)

   environment(lst$push) <- as.environment(lst)
   environment(lst$pop) <- as.environment(lst)
   environment(lst$clear) <- as.environment(lst)
   environment(lst$shift) <- as.environment(lst)
   environment(lst$first) <- as.environment(lst)
   environment(lst$last) <- as.environment(lst)
   environment(lst$size) <- as.environment(lst)

   class(lst) <- "List"
   lst
}

#------------------------------------------------------------------------------
