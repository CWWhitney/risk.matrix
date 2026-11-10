test_that("sensitivity keeps the other branches in proportion", {
  data <- data.frame(alternative = c("A", "A", "A", "B"),
                     outcome = c(-10, 10, 30, 5),
                     probability = c(0.2, 0.2, 0.6, 1))
  result <- sensitivity_analysis(data, "A", 1, c(0, 0.5, 1))
  expect_equal(result$expected_value[result$alternative == "A"], c(25, 7.5, -10))
  expect_equal(result$expected_value[result$alternative == "B"], rep(5, 3))
  expect_error(sensitivity_analysis(data, "A", 4), "branch")
  expect_error(sensitivity_analysis(data, "A", 1, 1.1), "probabilities")
  expect_error(sensitivity_analysis(data, "B", 4, 0.5), "redistribute")
})

test_that("additive values enforce scales and trade-off weights", {
  data <- data.frame(alternative = c("A", "B"), cost = c(0.8, 0.4),
                     impact = c(0.3, 0.9))
  expect_equal(value_function(data, c(cost = 0.6, impact = 0.4))$value,
               c(0.6, 0.6))
  expect_error(value_function(data, c(cost = 0.6)), "weights")
  expect_error(value_function(data, c(cost = 1, missing = 0)), "weights")
  expect_error(value_function(transform(data, cost = 2), c(cost = 1)), "attribute")
  expect_error(value_function(rbind(data, data[1, ]), c(cost = 1)), "alternative")
})

test_that("plots return their underlying data without modifying it", {
  data <- data.frame(alternative = c("A", "A", "B"),
                     outcome = c(-10, 20, 5), probability = c(0.2, 0.8, 1))
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off())
  expect_equal(generate_risk_matrix(data), data)
  expect_equal(plot_risk_visualization(data), calculate_expected_value(data))
  expect_error(generate_risk_matrix(transform(data, probability = -1)), "probability")
  expect_error(plot_risk_visualization(transform(data, probability = -1)), "probability")
})
