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
#' @export
twoSamplePoissonRate <- function(jaspResults, dataset, options) {

  inputType  <- options[["inputType"]]
  hasAnyTest <- options[["exactTest"]] || options[["normalApprox"]]

  if (inputType == "rawData") {
    ready <- options[["count"]] != "" && options[["group"]] != "" && hasAnyTest
    if (ready)
      .hasErrors(dataset, type = c("infinity", "negativeValues", "factorLevels"),
                 infinity.target = options[["count"]],
                 negativeValues.target = c(options[["count"]], options[["time"]]),
                 factorLevels.target = options[["group"]],
                 factorLevels.amount = '!= 2',
                 exitAnalysisIfErrors = TRUE)
  } else {
    ready <- hasAnyTest
  }

  if (options[["descriptives"]])
    .createDescriptivesTableTR(jaspResults, dataset, options, ready)

  .createMainTableTR(jaspResults, dataset, options, ready)

  return()
}

# --- Data extraction ---------------------------------------------------------

.getGroupDataTR <- function(dataset, options) {
  if (options[["inputType"]] == "rawData") {
    countVar <- options[["count"]]
    groupVar <- options[["group"]]

    countCol <- dataset[[countVar]]
    groupCol <- dataset[[groupVar]]
    lvls     <- levels(factor(groupCol))

    groups <- vector("list", 2)
    for (i in 1:2) {
      mask   <- !is.na(groupCol) & groupCol == lvls[i]
      counts <- stats::na.omit(countCol[mask])
      events <- as.integer(round(sum(counts))) # TODO does this have to be rounded?

      if (options[["time"]] != "") {
        timeCol <- dataset[[options[["time"]]]]
        time    <- sum(stats::na.omit(timeCol[mask]))
      } else {
        time <- length(counts)
      }

      groups[[i]] <- list(name = as.character(lvls[i]), events = events, time = time)
    }
  } else {
    n1 <- options[["groupOneName"]]
    n2 <- options[["groupTwoName"]]
    groups <- list(
      list(name   = if (nchar(n1) > 0) n1 else gettext("Group 1"),
           events = options[["groupOneOccurrences"]],
           time   = options[["groupOneSampleSize"]]),
      list(name   = if (nchar(n2) > 0) n2 else gettext("Group 2"),
           events = options[["groupTwoOccurrences"]],
           time   = options[["groupTwoSampleSize"]])
    )
  }
  groups[[1]]$rate <- groups[[1]]$events / groups[[1]]$time
  groups[[2]]$rate <- groups[[2]]$events / groups[[2]]$time
  return(groups)
}

# --- Descriptives table ------------------------------------------------------

.createDescriptivesTableTR <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  descTable <- createJaspTable(title = gettext("Descriptive Statistics"))
  descTable$dependOn(c("inputType", "count", "group", "time",
                       "groupOneName", "groupOneOccurrences", "groupOneSampleSize",
                       "groupTwoName", "groupTwoOccurrences", "groupTwoSampleSize",
                       "descriptives", "descriptiveCi", "descriptiveConfLevel"))
  descTable$position <- 1
  jaspResults[["descriptivesTable"]] <- descTable

  descTable$addColumnInfo(name = "groupName", title = gettext("Group"),  type = "string")
  descTable$addColumnInfo(name = "events",    title = gettext("Events"), type = "integer")
  descTable$addColumnInfo(name = "time",      title = gettext("Time"),   type = "number")
  descTable$addColumnInfo(name = "rate",      title = gettext("Rate"),   type = "number")

  if (options[["descriptiveCi"]]) {
    ciTitle <- gettextf("%i%% Confidence Interval", as.integer(options[["descriptiveConfLevel"]] * 100))
    descTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number",
                            overtitle = ciTitle)
    descTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number",
                            overtitle = ciTitle)
  }

  descTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  groupData <- try(.getGroupDataTR(dataset, options), silent = TRUE)
  if (isTryError(groupData)) {
    descTable$setError(gettext(as.character(groupData)))
    return()
  }

  rows <- lapply(groupData, function(g) {
    row <- data.frame(
      groupName = g$name,
      events    = as.integer(g$events),
      time      = g$time,
      rate      = g$rate,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    if (options[["descriptiveCi"]]) {
      ciOut <- try(
        stats::poisson.test(x = g$events, T = g$time, conf.level = options[["descriptiveConfLevel"]]),
        silent = TRUE
      )
      if (!isTryError(ciOut)) {
        row$ciLower <- ciOut$conf.int[1]
        row$ciUpper <- ciOut$conf.int[2]
      } else {
        row$ciLower <- NA
        row$ciUpper <- NA
      }
    }
    return(row)
  })

  descTable$setData(do.call(rbind, rows))
  descTable$addFootnote(gettext("Confidence interval based on exact Poisson distribution."))

  return()
}

# --- Main test table ---------------------------------------------------------

.createMainTableTR <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("Two-Sample Poisson Rate Test"))
  outputTable$dependOn(c("inputType", "count", "group", "time",
                         "groupOneName", "groupOneOccurrences", "groupOneSampleSize",
                         "groupTwoName", "groupTwoOccurrences", "groupTwoSampleSize",
                         "testTarget", "exactTest", "normalApprox", "pooledSe",
                         "testRatio", "testDifference",
                         "alternative", "confLevel", "ratioCi", "ciMethod"))
  outputTable$position <- 2
  jaspResults[["outputTable"]] <- outputTable

  isDiff       <- options[["testTarget"]] == "difference"
  effectTitle  <- if (isDiff) gettext("Difference") else gettext("Ratio")
  ciEffectName <- if (isDiff) gettext("Difference") else gettext("Ratio")

  outputTable$addColumnInfo(name = "method", title = gettext("Method"),   type = "string")
  outputTable$addColumnInfo(name = "rate1",  title = gettext("Rate\u2081"), type = "number")
  outputTable$addColumnInfo(name = "rate2",  title = gettext("Rate\u2082"), type = "number")
  outputTable$addColumnInfo(name = "effect", title = effectTitle,          type = "number")

  if (options[["normalApprox"]])
    outputTable$addColumnInfo(name = "statistic", title = gettext("z"), type = "number")

  outputTable$addColumnInfo(name = "pValue", title = gettext("p"), type = "pvalue")

  if (options[["ratioCi"]]) {
    ciTitle <- gettextf("%i%% CI for %s", as.integer(options[["confLevel"]] * 100), ciEffectName)
    outputTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number",
                              overtitle = ciTitle)
    outputTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number",
                              overtitle = ciTitle)
  }

  outputTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .fillMainTableTR(outputTable, dataset, options)

  return()
}

.fillMainTableTR <- function(outputTable, dataset, options) {
  groupData <- try(.getGroupDataTR(dataset, options), silent = TRUE)
  if (isTryError(groupData)) {
    outputTable$setError(gettext(as.character(groupData)))
    return()
  }

  g1 <- groupData[[1]]
  g2 <- groupData[[2]]

  isDiff <- options[["testTarget"]] == "difference"
  rows   <- list()

  if (isDiff) {
    if (options[["exactTest"]]) {
      row <- .computeExactDiffTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["exact"]] <- row
    }
    if (options[["normalApprox"]]) {
      row <- .computeNormalApproxDiffTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["normal"]] <- row
    }
  } else {
    if (options[["exactTest"]]) {
      row <- .computeExactTestTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["exact"]] <- row
    }
    if (options[["normalApprox"]]) {
      row <- .computeNormalApproxTR(g1, g2, options, outputTable)
      if (!is.null(row))
        rows[["normal"]] <- row
    }
  }

  if (length(rows) == 0)
    return()

  outputTable$setData(do.call(rbind.data.frame, rows))

  if (isDiff) {
    d0 <- options[["testDifference"]]
    outputTable$addFootnote(
      switch(options[["alternative"]],
        "two.sided" = gettextf("H\u2080: Rate\u2081 \u2212 Rate\u2082 = %.4g.", d0),
        "greater"   = gettextf("H\u2081: Rate\u2081 \u2212 Rate\u2082 > %.4g.", d0),
        "less"      = gettextf("H\u2081: Rate\u2081 \u2212 Rate\u2082 < %.4g.", d0)
      )
    )
    if (options[["ratioCi"]])
      outputTable$addFootnote(gettext("Confidence interval based on the unpooled standard error."))
  } else {
    r0 <- options[["testRatio"]]
    outputTable$addFootnote(
      switch(options[["alternative"]],
        "two.sided" = gettextf("H\u2080: Rate\u2081/Rate\u2082 = %.4g.", r0),
        "greater"   = gettextf("H\u2081: Rate\u2081/Rate\u2082 > %.4g.", r0),
        "less"      = gettextf("H\u2081: Rate\u2081/Rate\u2082 < %.4g.", r0)
      )
    )
  }

  outputTable$addFootnote(
    gettextf("Group 1: %1$s. Group 2: %2$s.", g1$name, g2$name)
  )

  return()
}

.computeExactTestTR <- function(g1, g2, options, outputTable) {
  out <- try(
    stats::poisson.test(x           = c(g1$events, g2$events),
                        T           = c(g1$time,   g2$time),
                        r           = options[["testRatio"]],
                        alternative = options[["alternative"]],
                        conf.level  = options[["confLevel"]]),
    silent = TRUE
  )

  if (isTryError(out)) {
    outputTable$setError(gettext(as.character(out)))
    return(NULL)
  }

  row <- data.frame(
    method    = gettext("Exact"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = g1$rate / g2$rate,
    statistic = NA,
    pValue    = out$p.value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]])
    row <- .addRatioCiTR(row, g1, g2, options, outputTable)

  return(row)
}

.computeNormalApproxTR <- function(g1, g2, options, outputTable) {
  x1 <- g1$events
  x2 <- g2$events
  T1 <- g1$time
  T2 <- g2$time
  r0 <- options[["testRatio"]]

  # Conditional binomial score test: X1 | X1+X2 ~ Bin(n, p0)
  n  <- x1 + x2 # TODO this overflows for huge values
  p0 <- r0 * T1 / (r0 * T1 + T2)

  if (n == 0) {
    outputTable$setError(gettext("Normal approximation requires at least one observed event."))
    return(NULL)
  }

  pHat      <- x1 / n
  statistic <- (pHat - p0) / sqrt(p0 * (1 - p0) / n)

  pValue <- switch(options[["alternative"]],
    "two.sided" = 2 * stats::pnorm(-abs(statistic)),
    "greater"   = stats::pnorm(statistic, lower.tail = FALSE),
    "less"      = stats::pnorm(statistic)
  )

  row <- data.frame(
    method    = gettext("Normal approximation"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = g1$rate / g2$rate,
    statistic = statistic,
    pValue    = pValue,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]])
    row <- .addRatioCiTR(row, g1, g2, options, outputTable)

  return(row)
}

.computeExactDiffTR <- function(g1, g2, options, outputTable) {
  d0 <- options[["testDifference"]]

  if (d0 != 0) {
    outputTable$addFootnote(
      gettext("Exact test for the difference is only available when the hypothesized difference is 0."))
    return(NULL)
  }

  out <- try(
    stats::poisson.test(x           = c(g1$events, g2$events),
                        T           = c(g1$time,   g2$time),
                        r           = 1,
                        alternative = options[["alternative"]]),
    silent = TRUE
  )

  if (isTryError(out)) {
    outputTable$setError(gettext(as.character(out)))
    return(NULL)
  }

  row <- data.frame(
    method    = gettext("Exact"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = g1$rate - g2$rate,
    statistic = NA,
    pValue    = out$p.value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]]) {
    row$ciLower <- NA
    row$ciUpper <- NA
  }

  return(row)
}

.computeNormalApproxDiffTR <- function(g1, g2, options, outputTable) {
  x1 <- g1$events; x2 <- g2$events
  T1 <- g1$time;   T2 <- g2$time
  d0 <- options[["testDifference"]]

  if (T1 <= 0 || T2 <= 0) {
    outputTable$setError(gettext("Normal approximation requires positive sample sizes in both groups."))
    return(NULL)
  }

  diff       <- g1$rate - g2$rate
  pooledRate <- (x1 + x2) / (T1 + T2)
  sePooled   <- sqrt(pooledRate * (1 / T1 + 1 / T2))
  seUnpooled <- sqrt(g1$rate / T1 + g2$rate / T2)
  se         <- if (options[["pooledSe"]]) sePooled else seUnpooled

  if (!is.finite(se) || se == 0) {
    outputTable$setError(gettext("Normal approximation for the difference could not be computed (zero standard error)."))
    return(NULL)
  }

  statistic <- (diff - d0) / se

  pValue <- switch(options[["alternative"]],
    "two.sided" = 2 * stats::pnorm(-abs(statistic)),
    "greater"   = stats::pnorm(statistic, lower.tail = FALSE),
    "less"      = stats::pnorm(statistic)
  )

  row <- data.frame(
    method    = gettext("Normal approximation"),
    rate1     = g1$rate,
    rate2     = g2$rate,
    effect    = diff,
    statistic = statistic,
    pValue    = pValue,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["ratioCi"]])
    row <- .addDiffCiTR(row, g1, g2, seUnpooled, options)

  return(row)
}

.addDiffCiTR <- function(row, g1, g2, seUnpooled, options) {
  if (!is.finite(seUnpooled)) {
    row$ciLower <- NA
    row$ciUpper <- NA
    return(row)
  }

  diff  <- g1$rate - g2$rate
  alpha <- 1 - options[["confLevel"]]
  z     <- stats::qnorm(1 - alpha / ifelse(options[["alternative"]] == "two.sided", 2, 1))

  if (options[["alternative"]] == "two.sided") {
    row$ciLower <- diff - z * seUnpooled
    row$ciUpper <- diff + z * seUnpooled
  } else if (options[["alternative"]] == "greater") {
    row$ciLower <- diff - z * seUnpooled
    row$ciUpper <- Inf
  } else {
    row$ciLower <- -Inf
    row$ciUpper <- diff + z * seUnpooled
  }
  return(row)
}

.addRatioCiTR <- function(row, g1, g2, options, outputTable) {
  if (options[["ciMethod"]] == "exact") {
    ciOut <- try(
      stats::poisson.test(x           = c(g1$events, g2$events),
                          T           = c(g1$time,   g2$time),
                          r           = options[["testRatio"]],
                          alternative = options[["alternative"]],
                          conf.level  = options[["confLevel"]]),
      silent = TRUE
    )
    if (isTryError(ciOut)) {
      outputTable$addFootnote(gettext("Exact CI could not be computed."), symbol = gettext("<b>Warning:</b>"))
      row$ciLower <- NA
      row$ciUpper <- NA
    } else {
      row$ciLower <- ciOut$conf.int[1]
      row$ciUpper <- ciOut$conf.int[2]
    }
  } else { # log-transform normal CI for ratio
    # TODO check if this is correct
    x1 <- g1$events
    x2 <- g2$events
    if (x1 == 0 || x2 == 0) {
      outputTable$addFootnote(
        gettext("Normal approximation CI for ratio requires both event counts > 0."),
        symbol = gettext("<b>Warning:</b>")
      )
      row$ciLower <- NA
      row$ciUpper <- NA
    } else {
      alpha   <- 1 - options[["confLevel"]]
      z       <- stats::qnorm(1 - alpha / ifelse(options[["alternative"]] == "two.sided", 2, 1))
      logRat  <- log(g1$rate / g2$rate)
      seLR    <- sqrt(1 / x1 + 1 / x2)

      if (options[["alternative"]] == "two.sided") {
        row$ciLower <- exp(logRat - z * seLR)
        row$ciUpper <- exp(logRat + z * seLR)
      } else if (options[["alternative"]] == "greater") {
        row$ciLower <- exp(logRat - z * seLR)
        row$ciUpper <- Inf
      } else {
        row$ciLower <- 0
        row$ciUpper <- exp(logRat + z * seLR)
      }
    }
  }
  return(row)
}
