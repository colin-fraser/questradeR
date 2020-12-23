test_that("qt_dir", {
  expect_equal(qt_dir(), fs::path_expand(DEFAULT_DIR))
  options(default.dir = '123')
  expect_equal(qt_dir(), '123')
  options(default.dir = NULL)
})


test_that("qt_default_account_set", {
  expect_equal(qt_default_account_set(), fs::path(fs::path_expand(DEFAULT_DIR), DEFAULT_ACCOUNT_SET, ext = 'yaml'))
  options(default.account.set = '123')
  expect_equal(qt_default_account_set(), fs::path(fs::path_expand(DEFAULT_DIR), '123', ext = 'yaml'))
  options(default.dir = NULL)
})


