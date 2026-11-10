test_that("CSV import validates outcome distributions", {
  path <- system.file("extdata", "project_selection.csv", package = "risk.decision")
  data <- import_risk_data(path)
  expect_equal(nrow(data), 4)
  expect_equal(validate_risk_data(data), data)
  expect_error(import_risk_data(""), "path")
  bad <- data
  bad$probability[1] <- 0.9
  expect_error(validate_risk_data(bad), "sum to 1")
  bad$probability[1] <- NA
  expect_error(validate_risk_data(bad), "probability")
  expect_error(validate_risk_data(data.frame()), "non-empty")
  expect_error(validate_risk_data(transform(data, outcome = "bad")), "outcome")
  expect_error(validate_risk_data(transform(data, alternative = "")), "alternative")
})

test_that("expected values and risk metrics use conditional probabilities", {
  data <- data.frame(alternative = c("A", "A", "B"), outcome = c(-10, 20, 5),
                     probability = c(0.2, 0.8, 1))
  expect_equal(calculate_expected_value(data)$expected_value, c(14, 5))
  metrics <- calculate_risk_metrics(data)
  expect_equal(metrics$variance, c(144, 0))
  expect_equal(metrics$standard_deviation, c(12, 0))
  expect_equal(metrics$downside_probability, c(0.2, 0))
  expect_equal(calculate_risk_metrics(data, 20)$downside_probability, c(0.2, 1))
  expect_equal(decision_tree_summary(data)$branches, c(2L, 1L))
  expect_error(calculate_risk_metrics(data, NA_real_), "threshold")
})
