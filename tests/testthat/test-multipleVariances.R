.mvOptions <- function() {
  jaspTools::analysisOptions("multipleVariances")
}

test_that("Levene's test table (5 groups) matches", {
  options <- .mvOptions()
  options$dependent  <- "contNormal"
  options$factor     <- "facFive"
  options$leveneTest <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    4, 95, 0.640943834680485, 0.631903918697074, "Levene's", "contNormal"
  ))
})

test_that("All four tests table (2 groups) matches", {
  options <- .mvOptions()
  options$dependent    <- "contNormal"
  options$factor       <- "facGender"
  options$fTest        <- TRUE
  options$leveneTest   <- TRUE
  options$bartlettTest <- TRUE
  options$bonettTest   <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["outputTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    49, 49, 0.160707967049673, 0.667526834828481, "F", "contNormal",
    1, 98, 0.079966912210959, 3.13016285056192, "Levene's", "contNormal",
    1, "", 0.160704585047825, 1.9675835802309, "Bartlett's", "contNormal",
    1, "", 0.298362318498498, 1.08149972498498, "Bonett's", "contNormal"
  ))
})

test_that("F-test footnote shown for > 2 groups", {
  options <- .mvOptions()
  options$dependent  <- "contNormal"
  options$factor     <- "facFive"
  options$fTest      <- TRUE
  options$leveneTest <- FALSE
  set.seed(1)
  results   <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  footnotes <- results[["results"]][["outputTable"]][["footnotes"]]
  expect_length(footnotes, 1)
  expect_equal(footnotes[[1]]$text, "F-test is only available for 2 groups.")
})

test_that("Descriptives table with chi-square CI matches", {
  options <- .mvOptions()
  options$dependent    <- "contNormal"
  options$factor       <- "facGender"
  options$leveneTest   <- TRUE
  options$descriptives <- TRUE
  options$varianceCi   <- TRUE
  options$confLevel    <- 0.95
  options$ciMethod     <- "chiSquare"
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    "f", 0.601384535211166, -0.42131343004, 50, 0.928359025641992,
    1.33832309757562, "contNormal", 0.861850480490949, "m", 0.900914395996814,
    0.04381625496, 50, 1.13627015420614, 2.00489782245158, "contNormal",
    1.29110986333965
  ))
})

test_that("Descriptives table with Bonett CI matches", {
  options <- .mvOptions()
  options$dependent    <- "contNormal"
  options$factor       <- "facGender"
  options$leveneTest   <- TRUE
  options$descriptives <- TRUE
  options$varianceCi   <- TRUE
  options$confLevel    <- 0.95
  options$ciMethod     <- "bonett"
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    "f", 0.467138899097588, -0.42131343004, 50, 0.928359025641992,
    1.72246808315135, "contNormal", 0.861850480490949, "m", 0.828393459508167,
    0.04381625496, 50, 1.13627015420614, 2.17983236525143, "contNormal",
    1.29110986333965
  ))
})

test_that("Variance ratio table (2 groups) matches", {
  options <- .mvOptions()
  options$dependent       <- "contNormal"
  options$factor          <- "facGender"
  options$leveneTest      <- TRUE
  options$varianceRatioCi <- TRUE
  options$confLevel       <- 0.95
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["varianceRatioTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    0.378805571298527, 0.667526834828481, 1.17630813530187, "contNormal"
  ))
})

test_that("Variance ratio footnote for > 2 groups", {
  options <- .mvOptions()
  options$dependent       <- "contNormal"
  options$factor          <- "facFive"
  options$leveneTest      <- TRUE
  options$varianceRatioCi <- TRUE
  set.seed(1)
  results   <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  footnotes <- results[["results"]][["varianceRatioTable"]][["footnotes"]]
  expect_length(footnotes, 1)
  expect_equal(footnotes[[1]]$text, "Variance ratio confidence interval is only available for 2 groups.")
})

test_that("Normality test (Shapiro-Wilk) table matches", {
  options <- .mvOptions()
  options$dependent     <- "contNormal"
  options$factor        <- "facFive"
  options$leveneTest    <- TRUE
  options$normalityTest <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  table   <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_normalityTest"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    0.966386984270573, 0.0117758430505889, "contNormal"
  ))
})

test_that("Q-Q plot is created", {
  options <- .mvOptions()
  options$dependent  <- "contNormal"
  options$factor     <- "facFive"
  options$leveneTest <- TRUE
  options$qqPlot     <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("multipleVariances", "debug.csv", options)
  skip_if(results$status == "fatalError", "jaspGraphs/ggplot2 version incompatibility")
  plotData <- results[["results"]][["assumptionChecks"]][["collection"]][["assumptionChecks_qqPlots"]][["collection"]]
  expect_true(length(plotData) > 0)
})
