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

#' @importFrom stats poisson.test pnorm qnorm
#' @import jaspBase
#' @export
oneSamplePoissonRate <- function(jaspResults, dataset, options) {

  inputType  <- options[["inputType"]]
  hasAnyTest <- options[["exactTest"]] || options[["normalApprox"]]

  if (inputType == "rawData") {
    ready <- options[["count"]] != "" && hasAnyTest
    if (ready)
      .hasErrors(dataset, type = "infinity",
                 all.target = options[["count"]],
                 exitAnalysisIfErrors = TRUE)
  } else {
    ready <- hasAnyTest
  }

  .createOutputTablePR(jaspResults, dataset, options, ready)

  return()
}

.getEventDataPR <- function(dataset, options) {
  if (options[["inputType"]] == "rawData") {
    countCol <- dataset[[options[["count"]]]]
    countCol <- stats::na.omit(countCol)
    events   <- sum(countCol)
    if (options[["time"]] != "") {
      timeCol <- dataset[[options[["time"]]]]
      time    <- sum(stats::na.omit(timeCol))
    } else {
      time <- length(countCol)
    }
  } else {
    events <- options[["observedEvents"]]
    time   <- options[["observationTime"]]
  }
  return(list(events = events, time = time))
}

.createOutputTablePR <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["outputTable"]]))
    return()

  outputTable <- createJaspTable(title = gettext("One-Sample Poisson Rate Test"))
  outputTable$dependOn(c("inputType", "count", "time", "observedEvents", "observationTime",
                         "exactTest", "normalApprox", "testRate", "alternative",
                         "confLevel", "rateCi", "ciMethod"))
  jaspResults[["outputTable"]] <- outputTable

  outputTable$addColumnInfo(name = "method",  title = gettext("Method"),  type = "string")
  outputTable$addColumnInfo(name = "events",  title = gettext("Events"),  type = "integer")
  outputTable$addColumnInfo(name = "time",    title = gettext("Time"),    type = "number")
  outputTable$addColumnInfo(name = "rate",    title = gettext("Rate"),    type = "number")

  if (options[["normalApprox"]])
    outputTable$addColumnInfo(name = "statistic", title = gettext("Z"), type = "number")

  outputTable$addColumnInfo(name = "pValue", title = gettext("p"), type = "pvalue")

  if (options[["rateCi"]]) {
    ciTitle <- gettextf("%i%% Confidence Interval", as.integer(options[["confLevel"]] * 100))
    outputTable$addColumnInfo(name = "ciLower", title = gettext("Lower"), type = "number",
                              overtitle = ciTitle)
    outputTable$addColumnInfo(name = "ciUpper", title = gettext("Upper"), type = "number",
                              overtitle = ciTitle)
  }

  outputTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()

  .fillOutputTablePR(outputTable, dataset, options)

  return()
}

.fillOutputTablePR <- function(outputTable, dataset, options) {

  eventData <- try(.getEventDataPR(dataset, options), silent = TRUE)
  if (isTryError(eventData)) {
    outputTable$setError(gettext(as.character(eventData)))
    return()
  }

  events <- eventData$events
  time   <- eventData$time
  rate   <- events / time

  rows <- list()

  if (options[["exactTest"]]) {
    row <- .computeExactTestPR(events, time, rate, options, outputTable)
    if (!is.null(row))
      rows[["exact"]] <- row
  }

  if (options[["normalApprox"]]) {
    row <- .computeNormalApproxPR(events, time, rate, options, outputTable)
    if (!is.null(row))
      rows[["normal"]] <- row
  }

  if (length(rows) == 0)
    return()

  result <- do.call(rbind.data.frame, rows)
  outputTable$setData(result)

  rate0 <- options[["testRate"]]
  outputTable$addFootnote(
    switch(options[["alternative"]],
      "two.sided" = gettextf("Hypothesized rate: %.4g.", rate0),
      "greater"   = gettextf("Alternative hypothesis: rate > %.4g.", rate0),
      "less"      = gettextf("Alternative hypothesis: rate < %.4g.", rate0)
    )
  )

  return()
}

.computeExactTestPR <- function(events, time, rate, options, outputTable) {
  out <- try(
    stats::poisson.test(x           = events,
                        T           = time,
                        r           = options[["testRate"]],
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
    events    = as.integer(events),
    time      = time,
    rate      = rate,
    statistic = NA_real_,
    pValue    = out$p.value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["rateCi"]])
    row <- .addCiPR(row, events, time, rate, options, outputTable)

  return(row)
}

.computeNormalApproxPR <- function(events, time, rate, options, outputTable) {
  rate0     <- options[["testRate"]]
  statistic <- (events - rate0 * time) / sqrt(rate0 * time)

  pValue <- switch(options[["alternative"]],
    "two.sided" = 2 * stats::pnorm(-abs(statistic)),
    "greater"   = stats::pnorm(statistic, lower.tail = FALSE),
    "less"      = stats::pnorm(statistic)
  )

  row <- data.frame(
    method    = gettext("Normal approximation"),
    events    = as.integer(events),
    time      = time,
    rate      = rate,
    statistic = statistic,
    pValue    = pValue,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (options[["rateCi"]])
    row <- .addCiPR(row, events, time, rate, options, outputTable)

  return(row)
}

.addCiPR <- function(row, events, time, rate, options, outputTable) {
  if (options[["ciMethod"]] == "exact") {
    ciOut <- try(
      stats::poisson.test(x           = events,
                          T           = time,
                          r           = options[["testRate"]],
                          alternative = options[["alternative"]],
                          conf.level  = options[["confLevel"]]),
      silent = TRUE
    )
    if (isTryError(ciOut)) {
      outputTable$addFootnote(gettext("Exact CI could not be computed."), symbol = gettext("Warning:"))
      row$ciLower <- NA
      row$ciUpper <- NA
    } else {
      row$ciLower <- ciOut$conf.int[1]
      row$ciUpper <- ciOut$conf.int[2]
    }
  } else { # normal approximation CI
    alpha <- 1 - options[["confLevel"]]
    z     <- stats::qnorm(1 - alpha / ifelse(options[["alternative"]] == "two.sided", 2, 1))
    se    <- sqrt(rate / time)

    if (options[["alternative"]] == "two.sided") {
      row$ciLower <- max(0, rate - z * se)
      row$ciUpper <- rate + z * se
    } else if (options[["alternative"]] == "greater") {
      row$ciLower <- max(0, rate - z * se)
      row$ciUpper <- Inf
    } else {
      row$ciLower <- 0
      row$ciUpper <- rate + z * se
    }
  }
  return(row)
}
