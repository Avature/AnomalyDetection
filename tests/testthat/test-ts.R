library(AnomalyDetection)
context("Evaluation: AnomalyDetectionTs")

test_that("last day, both directions, with plot", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last='day', plot=T)
  expect_equal(length(results$anoms), 2)
  expect_equal(length(results$anoms[[2L]]), 25)
  expect_equal(class(results$plot), c("gg", "ggplot"))
})

test_that("both directions, e_value, with longterm", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', longterm=TRUE, e_value=TRUE)
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 131)
  expect_equal(results$plot, NULL)
})

threshold <- function(vector){
  return(median(vector))
}

test_that("both directions, e_value, threshold set to med_max", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', threshold=threshold, e_value=TRUE)
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 4)
  expect_equal(results$plot, NULL)
})
