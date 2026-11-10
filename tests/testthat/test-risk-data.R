test_that("CSV examples import and validate", {
  path <- system.file("extdata", "project-risk-template.csv", package = "risk.decision")
  data <- import_risk_data(path)

  expect_s3_class(data, "data.frame")
  expect_named(data, c("alternative", "uncertain_variable", "outcome", "probability", "value"))
  expect_equal(sum(data$probability[data$alternative == "Proceed"]), 1)
})

test_that("risk data rejects incomplete probability distributions", {
  data <- data.frame(
    alternative = c("A", "A"),
    uncertain_variable = c("Demand", "Demand"),
    outcome = c("High", "Low"),
    probability = c(0.6, 0.3),
    value = c(10, -5)
  )

  expect_error(calculate_risk_metrics(data), "sum to 1")
})

test_that("risk metrics calculate expected value and downside probability", {
  data <- import_risk_data(system.file("extdata", "project-risk-template.csv", package = "risk.decision"))
  metrics <- calculate_risk_metrics(data)
  proceed <- metrics[metrics$alternative == "Proceed", ]

  expect_equal(proceed$expected_value, 0.65 * 120 + 0.25 * 45 - 0.10 * 90)
  expect_equal(proceed$probability_negative, 0.10)
  expect_equal(proceed$variance, sum(c(0.65, 0.25, 0.10) *
    (c(120, 45, -90) - proceed$expected_value)^2))
})

test_that("risk matrix preserves outcomes and exposes risk bands", {
  data <- import_risk_data(system.file("extdata", "strategic-alternatives.csv", package = "risk.decision"))
  matrix <- generate_risk_matrix(data, plot = FALSE)

  expect_equal(nrow(matrix), nrow(data))
  expect_equal(matrix$outcome, data$outcome)
  expect_true(all(matrix$impact == abs(matrix$value)))
  expect_true(all(as.character(matrix$probability_level) %in% c("low", "medium", "high")))
})

test_that("decision tree summary ranks alternatives by expected value", {
  data <- import_risk_data(system.file("extdata", "strategic-alternatives.csv", package = "risk.decision"))
  summary <- decision_tree_summary(data)

  expect_true(all(diff(summary$expected_value) <= 0))
  expect_equal(sum(summary$preferred_by_expected_value), 1)
  expect_true(all(summary$outcome_count > 0))
})
