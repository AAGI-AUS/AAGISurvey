test_that("%||% returns left side if not NULL", {
  expect_identical("left" %||% "right", "left")
  expect_identical(42 %||% 99, 42)
  expect_identical(c(1, 2, 3) %||% c(4, 5), c(1, 2, 3))
})

test_that("%||% returns right side if left is NULL", {
  expect_identical(NULL %||% "right", "right")
  expect_identical(NULL %||% 99, 99)
  expect_identical(NULL %||% c(4, 5), c(4, 5))
})

test_that("%||% handles FALSE and 0 correctly", {
  expect_false(FALSE %||% TRUE, FALSE)
  expect_identical(0 %||% 1, 0)
})
