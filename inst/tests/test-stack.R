#-------------------------------------------------------------------------------
#
# Package tictoc 
#
# tests for Stack class
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

context("Test Stack methods")

#-------------------------------------------------------------------------------

test_that("constructor works", 
{
   stack <- Stack()
   
   expect_equal(class(stack), "Stack")    
})   

#-------------------------------------------------------------------------------

test_that("push works", 
{
   stack <- Stack()
   push(stack, 1)
   push(stack, 8)
   push(stack, 10)
   expect_equal(all.equal(c(1, 8, 10), stack$.Data), TRUE)
   expect_equal(size(stack), 3)
   expect_equal(typeof(stack$.Data), "double")
   # string is OK and converts the vector to character 
   push(stack, "asdf")
   expect_equal(all.equal(c("1", "8", "10", "asdf"), stack$.Data), TRUE)
   expect_equal(typeof(stack$.Data), "character")
   
   stack2 <- Stack()
   push(stack2, "s")
   push(stack2, "d")
   push(stack2, "f")
   # different type ok, still a string
   push(stack2, 10)
   expect_equal(all.equal(c("s", "d", "f", "10"), stack2$.Data), TRUE)    
   # list is not ok
   expect_error(push(stack2, list(a = "b")))
})   


#-------------------------------------------------------------------------------

test_that("size works", 
{
   stack <- Stack()
   push(stack, 1)
   push(stack, 8)
   push(stack, 10)
   expect_equal(size(stack), 3)    
})   

#-------------------------------------------------------------------------------

test_that("pop works", 
{
   stack <- Stack()
   push(stack, 1)
   push(stack, 8)
   push(stack, 10)
   x <- pop(stack)
   expect_equal(x, 10)
   expect_equal(all.equal(c(1, 8), stack$.Data), TRUE)
   expect_equal(size(stack), 2)
})   

#-------------------------------------------------------------------------------

test_that("shift, first, last and clear work", 
{
   stack <- Stack()
   push(stack, 1)
   push(stack, 8)
   push(stack, 10)
   x <- pop(stack)
   push(stack, 12)
   expect_equal(all.equal(c(1, 8, 12), stack$.Data), TRUE)
   expect_equal(first(stack), 1)
   expect_equal(last(stack), 12)
   expect_equal(shift(stack), 1)
   expect_equal(all.equal(c(8, 12), stack$.Data), TRUE)
   clear(stack)
   expect_equal(size(stack), 0)
})   

#-------------------------------------------------------------------------------

test_that("as.vector works", 
{
   stack <- Stack()
   push(stack, "s")
   push(stack, "d")
   push(stack, "f")
   # different type ok, still a string
   push(stack, 10)
   expect_equal(all.equal(c("s", "d", "f", "10"), as.vector(stack)), TRUE)    

   stack2 <- Stack()
   push(stack2, "1")
   push(stack2, "2")
   push(stack2, "3")
   # different type ok, still a string
   push(stack2, 10)
   # Can't compare with numeric
   res <- all.equal(c(1, 2, 3, 10), as.vector(stack2))
   expect_equal(length(res), 2)
   expect_equal(typeof(res), "character")
   
})

#-------------------------------------------------------------------------------
