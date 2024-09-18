test_that("run lintr", {
  expect_silent(lintr::lint_dir())
})
