#-------------------------------------------------------------------------------
#
# Package tictoc 
#
# tests for List class
# 
# Sergei Izrailev, 2014
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

context("Test List methods")

#-------------------------------------------------------------------------------

test_that("constructor works", 
{
   lst <- List()
   
   expect_equal(class(lst), "List")    
})   

#-------------------------------------------------------------------------------

test_that("push works", 
{
   lst <- List()
   push(lst, 1)
   push(lst, 8)
   push(lst, 10)
   expect_equal(all.equal(c(1, 8, 10), unlist(lst$.Data)), TRUE)
   expect_equal(size(lst), 3)
   expect_equal(typeof(lst$.Data), "list")
   # string is OK  
   push(lst, "asdf")
   expect_equal(all.equal(lst$.Data, list(1, 8, 10, "asdf")), TRUE)
   
   lst2 <- List()
   push(lst2, "s")
   push(lst2, "d")
   push(lst2, "f")
   # different type ok
   push(lst2, 10)
   expect_equal(all.equal(lst2$.Data, list("s", "d", "f", 10)), TRUE)
   
   # list is ok
   push(lst2, list(a = "b"))
   expect_equal(lst2$.Data[[5]][[1]], "b")
   expect_equal(all.equal(lst2$.Data, list("s", "d", "f", 10, list(a = "b"))), TRUE)
})   


#-------------------------------------------------------------------------------

test_that("size works", 
{
   lst <- List()
   push(lst, 1)
   push(lst, 8)
   push(lst, list(a = "b"))
   expect_equal(size(lst), 3)    
})   

#-------------------------------------------------------------------------------

test_that("pop works", 
{
   lst <- List()
   push(lst, 1)
   push(lst, 8)
   push(lst, list(a = "b"))
   x <- pop(lst)
   expect_equal(is.list(x), TRUE)
   expect_equal(x[[1]], "b")
   expect_equal(all.equal(c(1, 8), unlist(lst$.Data)), TRUE)    
})   

#-------------------------------------------------------------------------------

test_that("shift, first, last and clear work", 
{
   lst <- List()
   push(lst, 1)
   push(lst, 8)
   push(lst, list(a = "b"))
   x <- pop(lst)
   push(lst, 12)
   expect_equal(all.equal(c(1, 8, 12), unlist(lst$.Data)), TRUE)
   expect_equal(first(lst), 1)
   expect_equal(last(lst), 12)
   expect_equal(shift(lst), 1)
   expect_equal(all.equal(c(8, 12), unlist(lst$.Data)), TRUE)
   clear(lst)
   expect_equal(size(lst), 0)
})   

#-------------------------------------------------------------------------------

test_that("as.List works",
{
   lst.orig <- list(a = 1, b = 2, c = "abcd", d = list(p = "xyz", q = c(5, 6, 7)))
   lst <- as.List(lst.orig)
   lst.new <- as.list(lst)
   expect_equal(all.equal(lst.orig, lst.new), TRUE)
})

#-------------------------------------------------------------------------------
