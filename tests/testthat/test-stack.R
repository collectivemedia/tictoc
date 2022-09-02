#-------------------------------------------------------------------------------
#
# Package tictoc
#
# Tests.
#
# Sergei Izrailev, 2022
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
test_that("Stack (vector) works", {
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
