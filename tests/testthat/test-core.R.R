context("core.R")

test_that(
  "test core functions",
  {
    expect_equal(map(abs, c(1,2,-3)), c(1,2,3))
  }
)
