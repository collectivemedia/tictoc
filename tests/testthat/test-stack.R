#-------------------------------------------------------------------------------
#
# Package tictoc
#
# Tests for the Stack and Test classes
#
# Sergei Izrailev, 2014, 2022
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

test_that("Stack constructor works",
{
   stack <- Stack()

   expect_equal(class(stack), "Stack")
})

#-------------------------------------------------------------------------------

test_that("Stack (vector) works",
{
    stk <- Stack()
    expect_true(is.na(first_element(stk)))
    expect_true(is.na(last_element(stk)))
    push(stk, 1)
    push(stk, 2)
    push(stk, 3)
    expect_equal(length(stk$.Data), 3)
    expect_equal(first_element(stk), 1)
    expect_equal(last_element(stk), 3)

    value <- pop(stk)
    # last in first out
    expect_equal(value, 3)
    expect_equal(length(stk$.Data), 2)

    # first value
    value <- shift(stk)
    expect_equal(value, 1)
    expect_equal(length(stk$.Data), 1)
    expect_equal(first_element(stk), 2)
    expect_equal(last_element(stk), 2)
})

#-------------------------------------------------------------------------------

test_that("Stack push works",
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

test_that("Stack size works",
{
   stack <- Stack()
   push(stack, 1)
   push(stack, 8)
   push(stack, 10)
   expect_equal(size(stack), 3)
})

#-------------------------------------------------------------------------------

test_that("Stack pop works",
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

test_that("Stack shift, first, last and clear work",
{
    stack <- Stack()
    push(stack, 1)
    push(stack, 8)
    push(stack, 10)
    x <- pop(stack)
    push(stack, 12)
    expect_equal(all.equal(c(1, 8, 12), stack$.Data), TRUE)
    expect_equal(first_element(stack), 1)
    expect_equal(last_element(stack), 12)
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

test_that("List constructor works",
{
    lst <- List()
    expect_equal(class(lst), "List")
})

#-------------------------------------------------------------------------------

test_that("List works", {
    lst <- List()
    expect_true(is.na(first_element(lst)))
    expect_true(is.na(last_element(lst)))
    push(lst, "first")
    push(lst, 2)
    push(lst, 3)
    expect_equal(length(lst$.Data), 3)
    expect_equal(first_element(lst), "first")
    expect_equal(last_element(lst), 3)

    value <- pop(lst)
    # last in first out
    expect_equal(value, 3)
    expect_equal(length(lst$.Data), 2)

    # first value
    value <- shift(lst)
    expect_equal(value, "first")
    expect_equal(length(lst$.Data), 1)
    expect_equal(first_element(lst), 2)
    expect_equal(last_element(lst), 2)
})

#-------------------------------------------------------------------------------

test_that("List push works",
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

test_that("List size works",
{
    lst <- List()
    push(lst, 1)
    push(lst, 8)
    push(lst, list(a = "b"))
    expect_equal(size(lst), 3)
})

#-------------------------------------------------------------------------------

test_that("List pop works",
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

test_that("List shift, first, last and clear work",
{
    lst <- List()
    push(lst, 1)
    push(lst, 8)
    push(lst, list(a = "b"))
    x <- pop(lst)
    push(lst, 12)
    expect_equal(all.equal(c(1, 8, 12), unlist(lst$.Data)), TRUE)
    expect_equal(first_element(lst), 1)
    expect_equal(last_element(lst), 12)
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
