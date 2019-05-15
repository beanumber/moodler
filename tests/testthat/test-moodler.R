test_that("make works", {
  # make
  install_makefile(tempdir())
  expect_length(fs::dir_ls(tempdir(), regexp = "makefile"), 1)
})
