context('app')

test_that("app works", {
  skip_on_cran()

  library(shinytest)
  app_dir <- system.file('target-app', package = 'target')
  expect_pass(testApp(app_dir, compareImages = FALSE))
})
