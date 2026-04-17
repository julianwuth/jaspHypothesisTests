########## two-sided alternative, Shapiro-Wilk test, chi-square CI ##########
options <- analysisOptions("singleVariance")
options$.meta <- list(dependent = list(shouldEncode = TRUE), dependent.types = list(
  shouldEncode = TRUE))
options$alternative <- "two.sided"
options$dependent <- "contNormal"
options$normalityTest <- TRUE
options$sdEstimate <- TRUE
options$varEstimate <- TRUE
options$varianceCi <- TRUE
options$dependent.types <- "scale"
set.seed(1)
results <- runAnalysis("singleVariance", "test.csv", options)


test_that("Test of Normality (Shapiro-Wilk) table results match", {
  table <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_normalityTest"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.961235439093636, 0.00493016271041485, "contNormal"))
})

test_that("Single Variance Test table two-sided alternative results match", {
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(110.903697444404, 0.863588070980427, 1.51175115136297, 99, 0.340830736665525,
                                      1.05841360919316, 1.1202393681253, "contNormal"))
})



########## alternative = greater, Bonett CI ##########
test_that("Single Variance Test table greater alternative results match", {
  options <- analysisOptions("singleVariance")
  options$.meta <- list(dependent = list(shouldEncode = TRUE), dependent.types = list(
    shouldEncode = TRUE))
  options$alternative <- "greater"
  options$ciMethod <- "bonett"
  options$dependent <- "contNormal"
  options$varEstimate <- TRUE
  options$varianceCi <- TRUE
  options$dependent.types <- "scale"
  set.seed(1)
  results <- runAnalysis("singleVariance", "test.csv", options)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(110.903697444404, 0.826225423524836, "<unicode>", 99, 0.194565918369151,
                                      1.1202393681253, "contNormal"))
})

########## alternative = less ##########
test_that("Single Variance Test table less alternative results match", {
  options <- analysisOptions("singleVariance")
  options$.meta <- list(dependent = list(shouldEncode = TRUE), dependent.types = list(
    shouldEncode = TRUE))
  options$alternative <- "less"
  options$ciMethod <- "bonett"
  options$dependent <- "contNormal"
  options$varianceCi <- TRUE
  options$dependent.types <- "scale"
  set.seed(1)
  results <- runAnalysis("singleVariance", "test.csv", options)
  table <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(110.903697444404, 0, 1.57010584089427, 99, 0.805434081630849,
                                      "contNormal"))
})


######### error tests #########
test_that("Single Variance throws error", {
  options <- analysisOptions("singleVariance")
  dat1 <- data.frame(a = c(rnorm(10), Inf))
  options$dependent = "a"
  set.seed(1)
  results <- runAnalysis("singleVariance", dat1, options)
  expect_identical(results[["status"]], "validationError", label = "Inf check dependent")

  # zero variance
  dat2 <- data.frame(a = rep(1, 5))
  set.seed(1)
  results <- runAnalysis("singleVariance", dat2, options)
  expect_identical(results[["status"]], "validationError", label = "Zero var check dependent")

  dat3 <- data.frame(a = c(1, NA), b = rnorm(2))
  options$alternative <- "two.sided"
  options$dependent <- c("a", "b")
  results <- runAnalysis("singleVariance", dat3, options)
  expect_identical(sum(grepl("too few observations", results[["results"]][["outputTable"]][["footnotes"]])),
                   1L,
                   label = "Not enough observations after na.omit")
})
