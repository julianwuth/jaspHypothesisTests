#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' @import jaspBase
#' @importFrom stats bartlett.test var.test shapiro.test na.omit pchisq pf qchisq residuals aov var
#' @importFrom car leveneTest
#' @export
multipleVariances <- function(jaspResults, dataset, options, ...) {
  # is ready if test is selected and data was provided
  ready <- (length(options$dependent) > 0 && options$factor != "")

  if (ready)
    .hasErrors(dataset, type = c('infinity', 'variance'),
               all.target = options$dependent, variance.equalTo = 0,
               exitAnalysisIfErrors = TRUE)

  .createOutputTableMV(jaspResults, dataset, options, ready)

  if (options$descriptives || options$varianceCi)
    .createDescriptivesTableMV(jaspResults, dataset, options, ready)

  if (options$varianceRatioCi)
    .createVarianceRatioTableMV(jaspResults, dataset, options, ready)

  .assumptionChecksMV(jaspResults, dataset, options, ready)

  return()
}

.createOutputTableMV <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("Test of Equality of Variances"))
  outputTable$dependOn(c("dependent", "factor", "fTest", "leveneTest", "bonettTest", "bartlettTest"))
  outputTable$position <- 1
  jaspResults[["outputTable"]] <- outputTable

  outputTable$addColumnInfo(name = "var",   title = gettext("Variable"),  type = "string")
  outputTable$addColumnInfo(name = "test",  title = gettext("Test"),      type = "string")
  outputTable$addColumnInfo(name = "stat",  title = gettext("Statistic"), type = "number")
  outputTable$addColumnInfo(name = "df1",   title = gettext("df1"),       type = "integer")
  outputTable$addColumnInfo(name = "df2",   title = gettext("df2"),       type = "integer")
  outputTable$addColumnInfo(name = "p",     title = gettext("p"),         type = "pvalue")

  outputTable$showSpecifiedColumnsOnly <- TRUE

  if(!ready)
    return()

  .fillOutputTableMV(outputTable, dataset, options)

  return()
}

.fillOutputTableMV <- function(outputTable, dataset, options) {
  factorName <- options$factor
  factor <- as.factor(dataset[[factorName]])
  levels <- levels(factor)
  nLevels <- length(levels)

  rows <- list()

  for (depName in options$dependent) {
    y <- dataset[[depName]]

    # Data cleaning
    subData <- data.frame(y = y, group = factor)
    subData <- na.omit(subData)

    if (nrow(subData) == 0) next

    y <- subData$y
    group <- subData$group

    # F-Test (only if 2 levels)
    if (options$fTest && nLevels == 2) {
      res <- try(var.test(y ~ group), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      rows[[length(rows) + 1]] <- list(var = depName, test = gettext("F"),
                                       stat = res$statistic, df1 = res$parameter[1], df2 = res$parameter[2], p = res$p.value)
    } else if (options$fTest) {
      outputTable$addFootnote(gettext("F-test is only available for 2 groups."))
    }

    # Levene's Test
    if (options$leveneTest) {
      res <- try(car::leveneTest(y ~ group, center = median), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      rows[[length(rows) + 1]] <- list(var = depName, test = gettext("Levene's"),
                                       stat = res$`F value`[1], df1 = res$Df[1], df2 = res$Df[2], p = res$`Pr(>F)`[1])
    }

    # Bartlett's Test
    if (options$bartlettTest) {
      res <- try(bartlett.test(y ~ group), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      rows[[length(rows) + 1]] <- list(var = depName, test = gettext("Bartlett's"),
                                       stat = res$statistic, df1 = res$parameter[1], df2 = NA, p = res$p.value)
    }

    # Bonett's Test
    if (options$bonettTest) {
      res <- try(.computeBonettTest(y, group, depName), silent = TRUE)
      if (isTryError(res)) {
        outputTable$setError(gettext(as.character(res)))
        return()
      }
      if (!is.null(res$error)) {
        outputTable$addFootnote(res$error)
      } else {
        rows[[length(rows) + 1]] <- list(var = depName, test = gettext("Bonett's"),
                                         stat = res$statistic, df1 = res$df, df2 = NA, p = res$p.value)
      }
    }
  }

  outputTable$addRows(rows)

  return()
}

.computeBonettTest <- function(y, group, varName) {
  # Bonett (2006) test for equality of variances, following Minitab's implementation.
  # Uses Winsorized kurtosis and works for k >= 2 groups.
  # For k = 2 groups: uses the log SD ratio test with equalizer constant.
  # For k > 2 groups: uses a weighted chi-square test on log-variances.
  group <- droplevels(group)
  groups <- split(y, group)
  k <- length(groups)

  ns   <- vapply(groups, length, integer(1))
  sds  <- vapply(groups, sd, numeric(1))
  vars <- sds^2

  # Compute Winsorized kurtosis for each group
  gammas <- vapply(groups, .computeWinsorizedKurtosis, numeric(1))

  # Check for non-finite kurtosis (e.g., zero-variance groups or constant data)
  if (any(!is.finite(gammas)) || any(vars == 0)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  if (k == 2) {
    # --- Two-group Bonett test (Minitab's "2 Variances" method) ---
    return(.computeBonettTestTwoGroups(ns, sds, gammas, varName))
  }

  # --- k > 2 groups: weighted chi-square test on log-variances ---
  logVars  <- log(vars)
  asymVars <- (gammas + 2) / (ns - 1)

  if (any(asymVars <= 0)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  ws        <- 1 / asymVars
  logVarBar <- sum(ws * logVars) / sum(ws)
  statistic <- sum(ws * (logVars - logVarBar)^2)
  df        <- k - 1
  p.value   <- pchisq(statistic, df = df, lower.tail = FALSE)

  if (!is.finite(statistic) || !is.finite(p.value)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  list(statistic = statistic, df = df, p.value = p.value, error = NULL)
}

.computeWinsorizedKurtosis <- function(x) {
  # Winsorized kurtosis estimate following Bonett (2006) / Minitab.
  # Uses a Winsorized mean with trim proportion g/n, where g = floor(0.2 * n).
  # gamma_hat = n * m4 / m2^2 - 3 (excess kurtosis), computed using Winsorized mean.
  n <- length(x)
  if (n < 4) return(NaN)

  g  <- floor(0.2 * n)
  xs <- sort(x)
  # Winsorize: replace lower and upper g observations with boundary values
  xw <- xs
  if (g > 0) {
    xw[seq_len(g)]    <- xs[g + 1]
    xw[(n - g + 1):n] <- xs[n - g]
  }
  m <- mean(xw) # Winsorized mean

  m2 <- sum((x - m)^2)
  m4 <- sum((x - m)^4)

  if (m2 == 0) return(NaN)

  # Excess kurtosis with Winsorized mean
  gamma <- n * m4 / m2^2 - 3

  return(gamma)
}

.computeBonettTestTwoGroups <- function(ns, sds, gammas, varName) {
  # Bonett (2006) two-group test following Minitab.
  # Tests H0: sigma1/sigma2 = 1 (ratio of SDs = 1, i.e., equal variances).
  # Test statistic: Z² = (ln(S1/S2))² / se²(1) ~ chi²(1) for balanced designs.
  # For unbalanced designs, p-value is obtained by CI inversion.
  n1 <- ns[1]; n2 <- ns[2]
  s1 <- sds[1]; s2 <- sds[2]
  g1 <- gammas[1]; g2 <- gammas[2]

  logRatio <- log(s1 / s2)  # ln(S1/S2), testing against ln(rho0) = ln(1) = 0

  # Standard error of the pooled kurtosis under H0: rho0 = 1
  # se²(rho0) = (gamma1 + 2) / (2*(n1-1)) + (gamma2 + 2) / (2*(n2-1))
  se2 <- (g1 + 2) / (2 * (n1 - 1)) + (g2 + 2) / (2 * (n2 - 1))

  if (!is.finite(se2) || se2 <= 0) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  se <- sqrt(se2)

  if (n1 == n2) {
    # Balanced design: Z = ln(S1/S2) / se(1), Z² ~ chi²(1)
    Z         <- logRatio / se
    statistic <- Z^2
    p.value   <- pchisq(statistic, df = 1, lower.tail = FALSE)
  } else {
    # Unbalanced design: p-value by CI inversion using equalizer constant
    p.value <- .bonettUnbalancedPvalue(n1, n2, s1, s2, g1, g2, se2)
    Z       <- logRatio / se
    statistic <- Z^2
  }

  if (!is.finite(statistic) || !is.finite(p.value)) {
    return(list(error = gettextf("Bonett's test could not be computed for variable %s.", varName)))
  }

  list(statistic = statistic, df = 1, p.value = p.value, error = NULL)
}

.bonettEqualizerConstant <- function(za, g1, g2, se2, n1, n2) {
  # Equalizer constant c_alpha from Minitab:
  # c_alpha = sqrt(1 + za² * (g1+2)*(g2+2) / (2*se² * (n1-1)*(n2-1)) * (1/(n1-1) - 1/(n2-1))²)
  term <- za^2 * (g1 + 2) * (g2 + 2) / (2 * se2 * (n1 - 1) * (n2 - 1)) *
    (1 / (n1 - 1) - 1 / (n2 - 1))^2
  sqrt(1 + term)
}

.bonettL <- function(z, n1, n2, s1, s2, g1, g2) {
  # Function L(z, n1, n2, S1, S2) used in CI inversion for unbalanced designs.
  # L = ln(S1/S2) - z * c_z * se
  # where se² and c_z use the kurtosis values.
  se2 <- (g1 + 2) / (2 * (n1 - 1)) + (g2 + 2) / (2 * (n2 - 1))
  if (se2 <= 0) return(NaN)
  se  <- sqrt(se2)
  ca  <- .bonettEqualizerConstant(z, g1, g2, se2, n1, n2)
  log(s1 / s2) - z * ca * se
}

.bonettUnbalancedPvalue <- function(n1, n2, s1, s2, g1, g2, se2) {
  # For unbalanced designs, p = 2 * min(alphaL, alphaU).
  # alphaL: smallest alpha such that lower CI bound >= 0 (i.e., ln(rho) >= 0)
  # alphaU: smallest alpha such that upper CI bound <= 0
  # These are found by root-finding on L(z, ...).

  alphaL <- .bonettFindAlpha(n1, n2, s1, s2, g1, g2)
  alphaU <- .bonettFindAlpha(n2, n1, s2, s1, g2, g1)  # swap groups

  p.value <- 2 * min(alphaL, alphaU)
  return(min(p.value, 1))
}

.bonettFindAlpha <- function(n1, n2, s1, s2, g1, g2) {
  # Find the smallest alpha such that the CI bound crosses zero.
  # This involves finding the root of L(z, n1, n2, s1, s2) = 0.

  if (n1 < n2) {
    # Calculate zm and evaluate L(zm)
    zm <- sqrt(n2 - 1)
    Lzm <- .bonettL(zm, n1, n2, s1, s2, g1, g2)

    if (Lzm > 0) {
      # L has no root, alpha = 0
      return(0)
    }

    # Find root zL in (zm, large_z) — L(zm) < 0 and L increases for z > zm
    # Actually L decreases with z, so root is in (0, zm) if L(0) > 0
    L0 <- .bonettL(0, n1, n2, s1, s2, g1, g2)  # L(0) = ln(S1/S2)

    if (L0 <= 0) {
      # ln(S1/S2) <= 0, root might be at z < 0 or doesn't exist
      # Try searching in a wide interval
      rootRes <- try(uniroot(function(z) .bonettL(z, n1, n2, s1, s2, g1, g2),
                             interval = c(-20, 20), tol = 1e-10), silent = TRUE)
      if (isTryError(rootRes)) return(0)
      zL <- rootRes$root
    } else {
      rootRes <- try(uniroot(function(z) .bonettL(z, n1, n2, s1, s2, g1, g2),
                             interval = c(0, zm), tol = 1e-10), silent = TRUE)
      if (isTryError(rootRes)) return(0)
      zL <- rootRes$root
    }

    return(pnorm(zL, lower.tail = FALSE))

  } else if (n1 > n2) {
    L0 <- .bonettL(0, n1, n2, s1, s2, g1, g2)  # = ln(S1/S2)

    if (L0 >= 0) {
      # Find z0 root in [0, n2)
      rootRes0 <- try(uniroot(function(z) .bonettL(z, n1, n2, s1, s2, g1, g2),
                              interval = c(0, n2 - 1), tol = 1e-10), silent = TRUE)
      if (isTryError(rootRes0)) return(0)
      zL <- rootRes0$root
    } else {
      # L(0) < 0, find root in negative z range
      rootRes <- try(uniroot(function(z) .bonettL(z, n1, n2, s1, s2, g1, g2),
                             interval = c(-20, 0), tol = 1e-10), silent = TRUE)
      if (isTryError(rootRes)) return(0)
      zL <- rootRes$root
    }

    return(pnorm(zL, lower.tail = FALSE))

  } else {
    # Balanced: just use Z directly
    se2 <- (g1 + 2) / (2 * (n1 - 1)) + (g2 + 2) / (2 * (n2 - 1))
    if (se2 <= 0) return(NA)
    Z <- log(s1 / s2) / sqrt(se2)
    return(pnorm(abs(Z), lower.tail = FALSE))
  }
}

.createDescriptivesTableMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  descTable <- createJaspTable(title = gettext("Descriptive Statistics"))
  descTable$dependOn(c("dependent", "factor", "descriptives", "varianceCi", "confLevel", "ciMethod"))
  descTable$position <- 2
  jaspResults[["descriptivesTable"]] <- descTable

  descTable$addColumnInfo(name = "var",    title = gettext("Variable"),  type = "string")
  descTable$addColumnInfo(name = "group",  title = gettext("Group"),     type = "string")
  descTable$addColumnInfo(name = "n",      title = gettext("N"),         type = "integer")
  descTable$addColumnInfo(name = "mean",   title = gettext("Mean"),      type = "number")
  descTable$addColumnInfo(name = "sd",     title = gettext("SD"),        type = "number")
  descTable$addColumnInfo(name = "varEst", title = gettext("Variance"),  type = "number")

  if (options$varianceCi) {
    overtitle <- gettextf("%i%% Confidence Interval<br>Variance", options$confLevel * 100)
    descTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    descTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  }

  descTable$showSpecifiedColumnsOnly <- TRUE

  if(!ready)
    return()

  .fillDescriptivesTableMV(descTable, dataset, options)

  return()
}

.fillDescriptivesTableMV <- function(descTable, dataset, options) {
  factorName <- options$factor
  factor <- as.factor(dataset[[factorName]])
  levels <- levels(factor)

  rows <- list()

  for (depName in options$dependent) {
    y <- dataset[[depName]]

    for (lvl in levels) {
      subY <- y[factor == lvl & !is.na(factor)]
      subY <- na.omit(subY)

      n <- length(subY)

      if (n < 2) {
        meanEst <- NA
        sdEst   <- NA
        varEst  <- NA
        ci      <- c(NA, NA)
      } else {
        meanEst <- mean(subY)
        varEst  <- var(subY)
        sdEst   <- sqrt(varEst)
        ci      <- .computeGroupVarianceCi(subY, varEst, options)
      }

      row <- list(var = depName, group = lvl, n = n, mean = meanEst, sd = sdEst, varEst = varEst)

      if (options$varianceCi) {
        row$lower <- ci[1]
        row$upper <- ci[2]
      }

      rows[[length(rows) + 1]] <- row
    }
  }

  descTable$addRows(rows)

  return()
}

.computeGroupVarianceCi <- function(subY, varEst, options) {
  if (options$ciMethod == "bonett") {
    ciRes <- try(DescTools::VarCI(subY, method = "bonett", conf.level = options$confLevel), silent = TRUE)
    if (isTryError(ciRes))
      return(c(NA, NA))
    return(c(ciRes["lwr.ci"], ciRes["upr.ci"]))
  }

  # Default: chi-square method
  df    <- length(subY) - 1
  alpha <- 1 - options$confLevel
  lower <- df * varEst / qchisq(1 - alpha/2, df)
  upper <- df * varEst / qchisq(alpha/2, df)
  return(c(lower, upper))
}

.createVarianceRatioTableMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["varianceRatioTable"]]))
    return()

  ratioTable <- createJaspTable(title = gettext("Variance Ratio"))
  ratioTable$dependOn(c("dependent", "factor", "varianceRatioCi", "confLevel"))
  ratioTable$position <- 3
  jaspResults[["varianceRatioTable"]] <- ratioTable

  ratioTable$addColumnInfo(name = "var",   title = gettext("Variable"),       type = "string")
  ratioTable$addColumnInfo(name = "ratio", title = gettext("Variance Ratio"), type = "number")

  overtitle <- gettextf("%i%% Confidence Interval", options$confLevel * 100)
  ratioTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  ratioTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  ratioTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .fillVarianceRatioTableMV(ratioTable, dataset, options)

  return()
}

.fillVarianceRatioTableMV <- function(ratioTable, dataset, options) {
  factorName <- options$factor
  factor <- as.factor(dataset[[factorName]])
  levels <- levels(factor)
  nLevels <- length(levels)

  if (nLevels != 2) {
    ratioTable$addFootnote(gettext("Variance ratio confidence interval is only available for 2 groups."))
    return()
  }

  rows <- list()

  for (depName in options$dependent) {
    y <- dataset[[depName]]

    subData <- data.frame(y = y, group = factor)
    subData <- na.omit(subData)

    if (nrow(subData) == 0) next

    res <- try(var.test(subData$y ~ subData$group, conf.level = options$confLevel), silent = TRUE)
    if (isTryError(res)) {
      ratioTable$setError(gettext(as.character(res)))
      return()
    }

    rows[[length(rows) + 1]] <- list(var = depName, ratio = res$estimate,
                                     lower = res$conf.int[1], upper = res$conf.int[2])
  }

  ratioTable$addRows(rows)
  ratioTable$addFootnote(gettextf("Variance ratio: Group %s / Group %s.", levels[1], levels[2]))

  return()
}

.assumptionChecksMV <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["assumptionChecks"]]))
    return()

  assumptionContainer <- createJaspContainer(title = gettext("Assumption Checks"))
  assumptionContainer$dependOn(c("qqPlot", "normalityTest", "dependent", "factor"))
  assumptionContainer$position <- 4

  jaspResults[["assumptionChecks"]] <- assumptionContainer

  if (options$normalityTest)
    .createNormalityTestTableMV(jaspResults, dataset, options, ready)

  if (options$qqPlot)
    .createQQPlotMV(jaspResults, dataset, options, ready)

  return()
}

.createNormalityTestTableMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["assumptionChecks"]][["normalityTest"]]))
    return()

  normalityTable <- createJaspTable(title = gettext("Test of Normality (Shapiro-Wilk)"))
  normalityTable$dependOn(c("normalityTest", "dependent", "factor"))
  jaspResults[["assumptionChecks"]][["normalityTest"]] <- normalityTable

  normalityTable$addColumnInfo(name = "varName", title = gettext("Residuals"), type = "string")
  normalityTable$addColumnInfo(name = "W",       title = gettext("W"),         type = "number")
  normalityTable$addColumnInfo(name = "pValue",  title = gettext("p"),         type = "pvalue")

  normalityTable$addFootnote(gettext("Significant results suggest a deviation from normality."))

  if (ready)
    .fillNormalityTestTableMV(normalityTable, dataset, options)

  return()
}

.fillNormalityTestTableMV <- function(normalityTable, dataset, options) {

  factor <- as.factor(dataset[[options$factor]])

  resList <- lapply(options$dependent, function(depName) {

    y <- dataset[[depName]]
    model <- aov(y ~ factor)
    resids <- residuals(model)

    swTest <- try(shapiro.test(resids), silent = TRUE)

    if (isTryError(swTest)) {
      normalityTable$setError(gettext(as.character(swTest)))
      return(NULL)
    }

    data.frame(varName = depName, W = as.numeric(swTest$statistic), pValue = as.numeric(swTest$p.value), stringsAsFactors = FALSE)
  })

  results <- do.call(rbind, resList)

  if (!is.null(results))
    normalityTable$setData(results)

  return()
}

.createQQPlotMV <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["assumptionChecks"]][["qqPlots"]]))
    return()

  if (!ready) {
    jaspResults[["assumptionChecks"]][["qqPlots"]] <- createJaspPlot(title = gettext("Q-Q Plots"))
    return()
  }

  qqContainer <- createJaspContainer(title = gettext("Q-Q Plots"))
  qqContainer$dependOn(c("qqPlot", "dependent", "factor"))
  jaspResults[["assumptionChecks"]][["qqPlots"]] <- qqContainer

  factor <- as.factor(dataset[[options$factor]])

  for (depName in options$dependent) {
    y <- dataset[[depName]]
    model <- aov(y ~ factor)
    resids <- residuals(model)
    stdResids <- scale(resids)

    tempPlot <- createJaspPlot(title = depName, height = 400, width = 500)
    tempPlot$plotObject <- jaspGraphs::plotQQnorm(as.vector(stdResids),
                                                  ciLevel = 0.95,
                                                  yName = "Standardized Residuals")
    qqContainer[[depName]] <- tempPlot
  }

  return()
}
