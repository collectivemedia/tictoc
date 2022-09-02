test_that("Stack (vector) works", {
  stk <- Stack()
  push(stk, 1)
  push(stk, 2)
  push(stk, 3)
  expect_equal(length(stk$.Data), 3)
  expect_equal(first(stk), 1)
  expect_equal(last(stk), 3)

  value <- pop(stk)
  # last in first out
  expect_equal(value, 3)
  expect_equal(length(stk$.Data), 2)

  # first value
  value <- shift(stk)
  expect_equal(value, 1)
  expect_equal(length(stk$.Data), 1)
  expect_equal(first(stk), 2)
  expect_equal(last(stk), 2)
})

test_that("List works", {
  lst <- List()
  push(lst, "first")
  push(lst, 2)
  push(lst, 3)
  expect_equal(length(lst$.Data), 3)
  expect_equal(first(lst), "first")
  expect_equal(last(lst), 3)

  value <- pop(lst)
  # last in first out
  expect_equal(value, 3)
  expect_equal(length(lst$.Data), 2)

  # first value
  value <- shift(lst)
  expect_equal(value, "first")
  expect_equal(length(lst$.Data), 1)
  expect_equal(first(lst), 2)
  expect_equal(last(lst), 2)
})
