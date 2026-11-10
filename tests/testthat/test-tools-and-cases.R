test_that("expected loss matches the lognormal mean formula", {
  r <- data.frame(risk = "A", probability = 0.4, lower = 5e5, upper = 1e7,
                  control_cost = 3e5, control_effectiveness = 0.35)
  out <- expected_loss_table(r)
  mu <- (log(1e7) + log(5e5)) / 2
  s <- (log(1e7) - log(5e5)) / (2 * qnorm(0.95))
  expect_equal(out$expected_inherent_loss, 0.4 * exp(mu + s^2 / 2))
  expect_equal(out$expected_residual_loss, 0.65 * out$expected_inherent_loss)
  expect_equal(out$return_on_control,
               0.35 * out$expected_inherent_loss / 3e5 - 1)
  expect_error(expected_loss_table(transform(r, lower = 0)), "bounds")
  expect_error(expected_loss_table(transform(r, probability = 2)), "between")
})

test_that("simulation is reproducible and close to the analytic mean", {
  r <- data.frame(risk = c("A", "B"), probability = c(0.4, 0.15),
                  lower = c(5e5, 2e6), upper = c(1e7, 5e6),
                  control_effectiveness = c(0.35, 0.25))
  a <- simulate_annual_loss(r, 20000, seed = 1)
  expect_equal(a, simulate_annual_loss(r, 20000, seed = 1))
  expect_true(all(a$residual <= a$inherent + 1e-9) || mean(a$residual) < mean(a$inherent))
  expect_equal(mean(a$inherent), sum(expected_loss_table(r)$expected_inherent_loss),
               tolerance = 0.1)
  expect_error(simulate_annual_loss(r, 0), "n must")
})

test_that("loss exceedance curves count strict exceedances", {
  out <- loss_exceedance_curve(c(0, 0, 1, 4, 20), c(0, 1, 10))
  expect_equal(out$exceedance_probability, c(0.6, 0.4, 0.2))
  expect_error(loss_exceedance_curve(-1), "non-negative")
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off())
  expect_equal(plot_loss_exceedance(out), out)
})

test_that("certain monetary equivalent and tolerance fit behave", {
  expect_equal(certain_monetary_equivalent(c(5, -3), c(0.8, 0.2), 2),
               -2 * log(0.8 * exp(-5 / 2) + 0.2 * exp(3 / 2)))
  expect_lt(certain_monetary_equivalent(c(5, -3), c(0.5, 0.5), 2), 1)
  expect_equal(certain_monetary_equivalent(c(5, -3), c(0.5, 0.5), 1e6), 1,
               tolerance = 1e-4)
  bets <- data.frame(gain = c(5, 2), loss = c(-3, -1), p_gain = c(0.8, 0.6))
  fit <- fit_risk_tolerance(bets)
  expect_gt(fit$risk_tolerance, 0)
  expect_error(fit_risk_tolerance(transform(bets, loss = 1)), "bets need")
})

test_that("perfect information is zero when one alternative dominates", {
  d <- data.frame(alternative = rep(c("A", "B"), each = 2),
                  uncertain_variable = rep(c("s", "f"), 2),
                  outcome = c(40, -5, 0, 0), probability = rep(c(0.6, 0.4), 2))
  expect_equal(expected_value_perfect_information(d)$evpi, 0.4 * 5)
  d$outcome[1:2] <- c(40, 1)
  expect_equal(expected_value_perfect_information(d)$evpi, 0)
  d$probability[3:4] <- c(0.5, 0.5)
  expect_error(expected_value_perfect_information(d), "depend")
})

test_that("dawes score uses sample z-scores with direction", {
  d <- data.frame(a = c(1, 2, 3), b = c(3, 2, 1))
  out <- dawes_score(d, c(a = 1, b = -1))
  expect_equal(out$score, c(-2, 0, 2))
  expect_error(dawes_score(d, c(a = 2)), "direction")
})

test_that("case studies reproduce the published expected values", {
  expect_true(all(c("id", "doi") %in% names(case_study())))
  s <- calculate_expected_value(case_study("schulte_2021_syndesmosis"))
  expect_equal(round(s$expected_value, 2), c(7.46, 4.78))
  f <- case_study("cummings_2022_ferret")
  p <- seq(0, 1, 0.001)
  # published treatment threshold: about 40% probability of a foreign body
  diff <- p * 83.3 + (1 - p) * 60 - (p * 23.3 + (1 - p) * 100)
  expect_equal(p[which.min(abs(diff))], 0.4)
  expect_equal(expected_value_perfect_information(f)$best_alternative, "No surgery")
  expect_error(case_study("nope"), "unknown")
})
