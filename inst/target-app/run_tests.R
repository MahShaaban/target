library(testthat)
library(shinytest)

options(shiny.testmode=TRUE)

test_that("Application works", {
  expect_pass(testApp(".", compareImages = FALSE))
})
