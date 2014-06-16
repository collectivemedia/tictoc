#-------------------------------------------------------------------------------
#
# Package tictoc
#
# Stack and List
#
# Sergei Izrailev, 2011, 2014
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

# to satisfy R CMD check -- the .Data object is actually defined in S3 class.
.Data <- vector() 

# STACK

#' \code{push} - Append an element.
#' @param x A Stack or List object.
#' @param value Value to append.
#' @param s A structure to be converted to a Stack or List.
#' @name Stack
#' @title Stack and List classes and methods
#' @aliases Stack List push.default pop.default clear.default shift.default first.default last.default size.default as.Stack.default as.List.default
#' @rdname Stack
#' @export
#' @method push default
push <- function(x, value) UseMethod("push")    # append an element
#' \code{pop} - Remove and return the last element.
#' @rdname Stack
#' @export
#' @method pop default
pop  <- function(x) UseMethod("pop")            # pop the last element
#' \code{clear} - Remove all elements.
#' @rdname Stack
#' @export
#' @method clear default
clear  <- function(x) UseMethod("clear")
#' \code{shift} - Remove and return the first element.
#' @rdname Stack
#' @export
#' @method shift default
shift  <- function(x) UseMethod("shift")        # pop the first element
#' \code{first} - Return the first element.
#' @rdname Stack
#' @export
#' @method first default
first  <- function(x) UseMethod("first")        # return the first element
#' \code{last} - Return the last element.
#' @rdname Stack
#' @export
#' @method last default
last  <- function(x) UseMethod("last")        # return the last element
#' \code{size} - Return the number of  elements.
#' @rdname Stack
#' @export
#' @method size default
size  <- function(x) UseMethod("size")        # return the number of elements

#' \code{as.Stack} - Creates a new Stack from (typically, vector) \code{s}.
#' @rdname Stack
#' @export
#' @method as.Stack default
as.Stack <- function(s) UseMethod("as.Stack")
as.Stack.default <- function(s) 
{
   stack <- Stack()
   push(stack, s)
   stack
}

#' \code{as.List} - Creates a new List from (typically, list) \code{s}.
#' @rdname Stack
#' @export
#' @method as.List default
as.List <- function(s) UseMethod("as.List")
as.List.default <- function(s) 
{
   lst <- List()
   lst$.Data <- as.list(s)
   lst
}

#' @aliases push pop clear shift first last size blah
#' @export
push.default  <- function(x, value) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))
pop.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))
clear.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))
shift.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))
first.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))
last.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))
size.default  <- function(x) stop(gettextf("Unknown class for '%s'.", deparse(substitute(x))))

push.Stack <- function(x, value) x$push(value)
pop.Stack  <- function(x) x$pop()
clear.Stack  <- function(x) x$clear()
shift.Stack  <- function(x) x$shift()
first.Stack  <- function(x) x$first()
last.Stack  <- function(x) x$last()
size.Stack  <- function(x) x$size()

as.vector.Stack <- function(x, mode = "any") as.vector(x$.Data)
print.Stack <- function(x) print(x$.Data)

as.list.List <- function(x) as.list(x$.Data)
print.List <- function(x) print(x$.Data)

#' \code{Stack()} - Creates and keeps a stack of items of the same type, implemented as an R vector. 
#' The type is determined by the first \code{push} operation.
#' @rdname Stack
Stack <- function() 
{ 
   stack <- new.env()
   stack$.Data <- vector()
   stack$push <- function(x) 
   {
      if (is.list(x)) stop("Can't push a list on a stack")
      .Data <<- c(.Data,x)   
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
   stack$first <- function() .Data[1]
   stack$last <- function() .Data[length(.Data)]
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

push.List <- function(x, value, ...) x$push(value)
pop.List  <- function(x) x$pop()
clear.List  <- function(x) x$clear()
shift.List  <- function(x) x$shift()
first.List  <- function(x) x$first()
last.List  <- function(x) x$last()
size.List  <- function(x) x$size()

#' \code{List()} - Creates and keeps a list of items, implemented as an R list. 
#' The type is determined by the first \code{push} operation.
#' @rdname Stack
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
   lst$first <- function() .Data[[1]]
   lst$last <- function() .Data[[length(.Data)]]
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
