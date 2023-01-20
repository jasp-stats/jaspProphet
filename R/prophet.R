#
# Copyright (C) 2018 University of Amsterdam
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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

# Main function ----
Prophet <- function(jaspResults, dataset = NULL, options) {

  options <- .prophetInitOptions(jaspResults, options)

  if (options$growth == "logistic")
    ready <- (options$dependent != "" && options$time != "") && (options$carryingCapacity != "" || options$logisticGrowthCarryingCapacity != options$logisticGrowthSaturatingMin)
  else
    ready <- (options$dependent != "" && options$time != "")

  dataset <- .prophetReadData(options, ready)
  errors  <- .prophetErrorHandling(dataset, options, ready)

  .prophetComputeResults(jaspResults, dataset, options, ready)

  .prophetContainerMain(                 jaspResults, options, ready)
  .prophetCreateModelSummaryTable(       jaspResults, options, ready)
  .prophetCreateChangePointTable(        jaspResults, options, ready)
  .prophetCreateModelEvaluationTable(    jaspResults, options, ready)
  .prophetCreateHistoryPlot(             jaspResults, dataset, options, ready)
  .prophetCreateForecastPlots(           jaspResults, dataset, options, ready)
  .prophetCreateSeasonalityPlotContainer(jaspResults, dataset, options, ready)
  .prophetCreateCovariatePlotContainer(  jaspResults, dataset, options, ready)
  .prophetCreatePerformancePlots(        jaspResults, options, ready)
  .prophetCreateParameterPlots(          jaspResults, options, ready)

  return()
}

# Init functions ----
.prophetInitOptions <- function(jaspResults, options) {
  if (options$growth == "linear") {
    options$carryingCapacity <- ""
    options$minimum  <- ""
  }

  return(options)
}

.prophetReadData <- function(options, ready) {
  if(!ready) return()

  numericVars  <- c(options$dependent, options$carryingCapacity, options$minimum, unlist(options$covariates),
                    options$changepoints, options$historyIndicator)
  numericVars  <- numericVars[numericVars != ""]
  nominalVars  <- options$time
  nominalVars  <- nominalVars[nominalVars != ""]
  dataset      <- .readDataSetToEnd(columns.as.numeric  = numericVars,
                                    columns             = nominalVars)

  return(dataset)
}

.prophetErrorHandling <- function(dataset, options, ready) {
  if(!ready) return()

  checks <- list(
    .dateTimeArgChecks <- function() {
      timeVar <- try(as.POSIXct(dataset[[encodeColNames(options$time)]], tz = "UTC"))

      if (isTryError(timeVar))
        return(gettext("'Time' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))

      if (options$predictionType == "nonperiodicalPrediction"
        && options$nonperiodicalPredictionStart != ""
        && options$nonperiodicalPredictionEnd != "") {
        predStart <- try(as.POSIXct(options$nonperiodicalPredictionStart))
        predEnd <- try(as.POSIXct(options$nonperiodicalPredictionEnd))

        if (isTryError(predStart))
          return(gettext("'Start' for nonperiodical prediction must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))

        if (isTryError(predEnd))
          return(gettext("'End' for nonperiodical prediction must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))
      }

      return()
    },

    .logicalVarChecks <- function() {
      isChangepoint <- dataset[[encodeColNames(options$changepoints)]]

      if (any(is.na(as.logical(isChangepoint))) || !all(unique(as.numeric(isChangepoint)) %in% c(0, 1)))
        return(gettext("'Changepoints' must be a logical variable (e.g., 0/1)"))

      isHistory <- dataset[[encodeColNames(options$historyIndicator)]]

      if (any(is.na(as.logical(isHistory))) || !all(unique(as.numeric(isHistory)) %in% c(0, 1)))
        return(gettext("'Include in Training' must be a logical variable (e.g., 0/1)"))

      return()
    },

    .logisticVarChecks <- function() {
      if (options$carryingCapacity != "" && options$minimum != "") {
        cap   <- dataset[[encodeColNames(options$carryingCapacity)]]
        floor <- dataset[[encodeColNames(options$minimum)]]

        if (!all(cap > floor))
          return(gettext("'Carrying Capacity' must always be larger than 'Saturating Minimum'"))
      }

      if (options$carryingCapacity != "") {
        cap   <- dataset[[encodeColNames(options$carryingCapacity)]]

        if (!all(cap > options$logisticGrowthSaturatingMin))
          return(gettext("'Carrying Capacity' must always be larger than 'Constant saturating minimum'"))
      }

      if (options$growth == "logistic" && options$carryingCapacity == "") {
        if (options$minimum != "") {
          floor <- dataset[[encodeColNames(options$minimum)]]
          if (options$logisticGrowthCarryingCapacity <= floor)
            return(gettext("'Constant carrying capacity' must always be larger than 'Saturating Minimum'"))
        } else {
          if (options$logisticGrowthCarryingCapacity <= options$logisticGrowthSaturatingMin)
            return(gettext("'Constant carrying capacity' must always be larger than 'Constant saturating minimum'"))
        }
      }
    },

    .seasonalityNameChecks <- function() {
      if (!is.null(options$seasonalities)) {
        for(seas in options$seasonalities) {
          if(make.names(seas$name, allow_ = TRUE) != seas$name && seas$name != "")
            return(gettext("Seasonality names must only contain letters, number, dots, or underscores and must start with letters or dots that are not followed by a number"))
        }
      }
      return()
    },

    .predictionChecks <- function() {
      if (options$carryingCapacity == "" && options$minimum == "" && length(options$covariates) == 0)
        return()

      ds    <- as.POSIXct(dataset[[encodeColNames(options$time)]], tz = "UTC")

      if (options$historyIndicator != "")
        idx <- as.logical(dataset[[encodeColNames(options$historyIndicator)]])
      else
        idx <- !logical(length(ds))

      futds <- as.POSIXct(switch(options$predictionType,
                                 periodicalPrediction = seq(max(ds[idx]),
                                                           length.out = options$periodicalPredictionNumber + 1,
                                                           by = options$periodicalPredictionUnit),
                                 nonperiodicalPrediction = seq(as.POSIXct(options$nonperiodicalPredictionStart),
                                                               as.POSIXct(options$nonperiodicalPredictionEnd),
                                                               by = options$nonPeriodicalPredictionUnit)),
                          tz = "UTC")

      if (!all(futds %in% ds) && options$carryingCapacity != "")
        return(gettext("When 'Carrying Capacity' is used in the model, predictions cannot be carried out unless 'Carrying Capacity' is also supplied for the predicted period."))

      if (!all(futds %in% ds) && length(options$covariates) > 0)
        return(gettext("When 'Covariates' are used in the model, predictions cannot be carried out unless the covariates are also observed for the predicted period."))

      return()
    }
  )

  targetVars <- c(options$dependent, unlist(options$covariates))
  if (options$carryingCapacity != "")
    targetVars <- c(targetVars, options$carryingCapacity)
  if (options$minimum != "")
    targetVars <- c(targetVars, options$minimum)

  .hasErrors(dataset, "run", type = c('observations', 'variance', 'infinity', "missingValues"),
             observations.target = targetVars,
             variance.target = c(options$dependent, unlist(options$covariates)),
             infinity.target = targetVars,
             missingValues.target = c(options$date, options$carryingCapacity, options$minimum, unlist(options$covariates)),
             custom = checks,
             observations.amount = '< 2',
             exitAnalysisIfErrors = TRUE)
}

.prophetModelDependencies <- function(options) {
  if (options$changepoints != "") {
    return(c("dependent", "time", "changepoints", "carryingCapacity", "minimum", "covariates",
             "historyIndicator", "growth", "logisticGrowthCarryingCapacity", "logisticGrowthSaturatingMin",
             "assignedCovariates", "seasonalities",
             "estimation", "mcmcSamples", "predictionIntervalLevel",
             "predictionIntervalSamples"))
  } else {
    return(c("dependent", "time", "changepoints", "carryingCapacity", "minimum", "covariates",
             "historyIndicator", "growth", "logisticGrowthCarryingCapacity", "logisticGrowthSaturatingMin",
             "maxChangepoints", "changepointRange", "changepointPriorScale",
             "assignedCovariates", "seasonalities",
             "estimation", "mcmcSamples", "predictionIntervalLevel",
             "predictionIntervalSamples"))
  }
}

.prophetPredictionDependencies <- function() {
  return(c("predictionType",
           "periodicalPredictionNumber",
           "periodicalPredictionUnit",
           "nonperiodicalPredictionStart",
           "nonperiodicalPredictionEnd",
           "nonperiodicalPredictionUnit"))
}

.prophetEvaluationDependencies <- function() {
  return(c("crossValidation", "crossValidationUnit", "crossValidationHorizon",
           "crossValidationPeriod", "crossValidationInitial"))
}

# Results functions ----
.prophetComputeResults <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()

  if (is.null(jaspResults[["prophetResults"]])) {
    prophetResults <- list()

    prophetModelFullResults  <- .prophetModelResultsHelper(dataset, options)
    prophetResults[["prophetModelResults"]] <- .prophetModelResultsReduce(prophetModelFullResults, options)

    if ((options[["predictionType"]] == "periodicalPrediction" && options[["periodicalPredictionNumber"]] > 0) ||
    (options[["predictionType"]] == "nonperiodicalPrediction") && options[["forecastPlotTrendStart"]] != "" &&
    options[["forecastPlotTrendEnd"]] != "") {
    prophetPredictionResults <- .prophetPredictionResultsHelper(dataset, options, prophetModelFullResults)
    prophetResults[["prophetPredictionResults"]] <- prophetPredictionResults
    }

    if (options[["crossValidation"]]) {
      prophetEvaluationResults <- .prophetEvaluationResultsHelper(dataset, options, prophetModelFullResults)
      prophetResults[["prophetEvaluationResults"]] <- prophetEvaluationResults
    }

    jaspResults[["prophetResults"]] <-
      createJaspState(object       = prophetResults,
                      dependencies = unique(c(.prophetModelDependencies(options),
                                       .prophetPredictionDependencies(),
                                       .prophetEvaluationDependencies()
                                       ))
                      )
  }

  .prophetSavePredictions(jaspResults, options)

  return()
}

.prophetModelResultsHelper <- function(dataset, options) {
  y     <- dataset[[encodeColNames(options$dependent)]]
  ds    <- as.POSIXct(dataset[[encodeColNames(options$time)]], tz = "UTC")

  if (options$historyIndicator != "")
    idx <- as.logical(dataset[[encodeColNames(options$historyIndicator)]])
  else
    idx <- !logical(length(ds))

  fitDat <- data.frame(y = y, ds = ds)

  if (options$changepoints != "") {
    isChangepoint <- as.logical(dataset[[encodeColNames(options$changepoints)]])
    cp            <- ds[isChangepoint & idx]
  } else {
    cp            <- NULL
  }

  if (options$growth == "logistic") {
    if (options$carryingCapacity != "")
      fitDat$cap <- as.numeric(dataset[[encodeColNames(options$carryingCapacity)]])
    else
      fitDat$cap <- options$logisticGrowthCarryingCapacity

    if (options$minimum != "")
      fitDat$floor <- as.numeric(dataset[[encodeColNames(options$minimum)]])
    else
      fitDat$floor <- options$logisticGrowthSaturatingMin
  }

  mcmcSamples <- switch(options$estimation,
                        map  = 0,
                        mcmc = options$mcmcSamples)

  predIntSamples <- switch(options$estimation,
                           map  = options$predictionIntervalSamples,
                           mcmc = 1)

  mod <- prophet::prophet(growth = options$growth,
                          changepoints = cp,
                          n.changepoints = options$maxChangepoints,
                          changepoint.range = options$changepointRange,
                          changepoint.prior.scale = options$changepointPriorScale,
                          yearly.seasonality = FALSE,
                          weekly.seasonality = FALSE,
                          daily.seasonality = FALSE,
                          mcmc.samples = mcmcSamples,
                          interval.width = options$predictionIntervalLevel,
                          uncertainty.samples = predIntSamples,
                          fit = FALSE)

  if (length(options$assignedCovariates) > 0) {

    for (cov in options$assignedCovariates) {
      mod     <- prophet::add_regressor(m = mod,
                                        name = cov$variable,
                                        prior.scale = cov$priorSigma,
                                        standardize = cov$standardize,
                                        mode = cov$mode)

      datCov        <- dataset[[encodeColNames(cov$variable)]]
      fitDat[[cov$variable]] <- datCov
    }
  }

  if (!is.null(options$seasonalities)) {
    i <- 1

    for (seas in options$seasonalities) {
      name   <- ifelse(seas$name == "", paste0("seasonality_", i), encodeColNames(seas$name))
      period <- seas$period * switch(seas$unit,
                                     secs  = 1/86400,
                                     mins  = 1/1440,
                                     hours = 1/24,
                                     days  = 1,
                                     weeks = 7,
                                     years = 365)

      mod <- prophet::add_seasonality(m = mod,
                                      name = name,
                                      period = period,
                                      fourier.order = seas$fourierOrder,
                                      prior.scale = seas$priorSigma,
                                      mode = seas$mode)
      i   <- i+1
    }
  }

  fitDat  <- fitDat[idx, ]

  fit     <- try(prophet::fit.prophet(m = mod, df = fitDat))

  if(isTryError(fit)) {
    message <- gettextf("Prophet failed to compute any results. It is possible that the analysis even failed to compute the initial log-likelihood. Check whether the model specification is plausible. Internal error message from 'prophet': %s.", .extractErrorMessage(fit))
    jaspBase::.quitAnalysis(message)
  }

  return(fit)
}

.prophetModelResultsReduce <- function(prophetModelResults, options) {
  if (options$estimation == "mcmc") {
    # posterior summaries
    ci <- .prophetIntervalLevels(options, "credible")
    ciNames <- paste0(100*ci, "%")
    modelSummary <- rstan::monitor(prophetModelResults$stan.fit, probs = .prophetIntervalLevels(options, "credible"))
    modelSummary <- as.data.frame(modelSummary)
    modelSummary <- modelSummary[, c("mean", "sd", ciNames, "n_eff", "Rhat", "Bulk_ESS", "Tail_ESS")]
    colnames(modelSummary) <- c("mean", "sd", "lower", "upper", "n_eff", "Rhat", "Bulk_ESS", "Tail_ESS")
    prophetModelResults[["modelSummary"]] <- modelSummary

    # density estimates of the core parameters
    parameterDensities <- list()
    for (par in c("k", "m", "sigma_obs")) {
      parameterDensities[[par]] <- density(prophetModelResults[["params"]][[par]])
    }
    prophetModelResults[["parameterDensities"]] <- parameterDensities

    # seasonality estimates
    predictedSeasonalities <- list()
    for (seas in options[["seasonalities"]]) {
      name <- seas[["name"]]
      predictedSeasonalities[[name]] <- .prophetPredictSeasonality(name, prophetModelResults, options)
    }
    prophetModelResults[["predictedSeasonalities"]] <- predictedSeasonalities

    # remove stanfit and mcmc samples from the results to reduce size of the results
    prophetModelResults$stan.fit <- NULL
    prophetModelResults$params   <- NULL
  }

  return(prophetModelResults)
}

.prophetPredictionResultsHelper <- function(dataset, options, prophetModelResults) {
  futDat <- NULL

  if (options$predictionType == "periodicalPrediction") {
    futDat <- prophet::make_future_dataframe(m = prophetModelResults,
                                             periods = options$periodicalPredictionNumber,
                                             freq = options$periodicalPredictionUnit,
                                             include_history = TRUE)
  } else {
    futDat <- data.frame(ds = seq(as.POSIXct(options$nonperiodicalPredictionStart),
                                  as.POSIXct(options$nonperiodicalPredictionEnd),
                                  by = options$nonperiodicalPredictionUnit))
  }

  futds <- as.POSIXct(futDat$ds, tz = "UTC")
  ds    <- as.POSIXct(dataset[[encodeColNames(options$time)]], tz = "UTC")

  if (options$growth == "logistic") {
    if (options$carryingCapacity != "") {
      futCap       <- as.numeric(dataset[[encodeColNames(options$carryingCapacity)]])
      futDat$cap   <- futCap[ds %in% futds]
    } else {
      futDat$cap   <- options$logisticGrowthCarryingCapacity
    }

    if (options$minimum != "") {
      futFloor     <- as.numeric(dataset[[encodeColNames(options$minimum)]])
      futDat$floor <- futFloor[ds %in% futds]
    } else {
      futDat$floor   <- options$logisticGrowthSaturatingMin
    }
  }

  if (length(options$covariates) > 0) {
    covs <- unlist(options$covariates)

    for (cov in covs) {
      futCov        <- dataset[[encodeColNames(cov)]]
      futDat[[cov]] <- futCov[ds %in% futds]
    }
  }

  pred <- predict(prophetModelResults, futDat)

  cps <- prophetModelResults$changepoints
  pred$isChangepoint <- pred$ds %in% cps

  return(pred)
}

.prophetEvaluationResultsHelper <- function(dataset, options, prophetModelResults) {

  cvDat <- prophet::cross_validation(model   = prophetModelResults,
                                     horizon = options$crossValidationHorizon,
                                     units   = options$crossValidationUnit,
                                     period  = options$crossValidationPeriod,
                                     initial = options$crossValidationInitial)

  return(cvDat)
}

# Saving functions ----
.prophetSavePredictions <- function(jaspResults, options) {
  if (is.null(jaspResults[["prophetPredictionSavePath"]])) {
    predSavePath <- createJaspState()
    predSavePath$dependOn(c(.prophetPredictionDependencies(), "predictionSavePath"))
    jaspResults[["prophetPredictionSavePath"]] <- predSavePath
  }

  if (options$predictionSavePath != "") {
    write.csv(jaspResults[["prophetResults"]][["object"]][["prophetModelResults"]],
              file = options$predictionSavePath,
              row.names = FALSE)
    predSavePath[["object"]] <- TRUE
  }

  return()
}

# Output functions ----
.prophetContainerMain <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prophetMainContainer"]])) return()

  mainContainer <- createJaspContainer()

  jaspResults[["prophetMainContainer"]] <- mainContainer
  jaspResults[["prophetMainContainer"]]$dependOn(.prophetModelDependencies(options))

  return()
}

.prophetCreateModelSummaryTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prophetMainContainer"]][["prophetModelSummaryTable"]])) return()

  prophetModelResults <- jaspResults[["prophetResults"]][["object"]][["prophetModelResults"]]

  if (options$estimation == "map") {
    prophetTable <- createJaspTable(title = "Parameter Estimates Table")
    prophetTable$dependOn(c("ciLevel"))
    prophetTable$position <- 1

    prophetTable$addColumnInfo(name = "k", title = gettext("Growth rate (k)"), type = "number")
    prophetTable$addColumnInfo(name = "m", title = gettext("Offset (m)"), type = "number")
    prophetTable$addColumnInfo(name = "sigmaObs", title = gettext("Residual variance (\u03C3\u00B2)"), type = "number")

    .prophetModelSummaryTableMapFill(prophetTable, prophetModelResults, ready)

    jaspResults[["prophetMainContainer"]][["prophetTable"]] <- prophetTable

  } else {
    prophetTable <- createJaspTable(title = gettext("Posterior Summary Table"))
    prophetTable$position <- 2

    criLevel <- (1-options[["ciLevel"]])/2

    overtitle <- gettextf("%s%% CI", options[["ciLevel"]]*100)
    prophetTable$addColumnInfo(name = "par", title = gettext("Parameter"), type = "string")
    prophetTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    prophetTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
    prophetTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = overtitle)
    prophetTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = overtitle)
    prophetTable$addColumnInfo(name = "rhat", title = gettext("R-hat"), type = "number")
    prophetTable$addColumnInfo(name = "bulkEss", title = gettext("ESS (bulk)"), type = "integer")
    prophetTable$addColumnInfo(name = "tailEss", title = gettext("ESS (tail)"), type = "integer")

    if (!ready && options$estimation == "mcmc")
      prophetTable$addFootnote(gettext("Prophet might need a long time to compute the results. You can try it first with fewer MCMC samples to see if it works and if you specified the model correctly (e.g., set 'Samples = 10' in the 'Model' section)."))

    prophetTable$addCitation("Taylor, S. J. & Letham, B. (2018). Forecasting at scale. *The American Statistician, 72*(1), 37-45.")

    .prophetModelSummaryTableMcmcFill(prophetTable, prophetModelResults, criLevel, ready)

    jaspResults[["prophetMainContainer"]][["prophetTable"]] <- prophetTable
  }

  return()
}

.prophetModelSummaryTableMapFill <- function(prophetTable, prophetModelResults, ready) {
  if (!ready) return()

  pars <- prophetModelResults$params

  prophetTable$addRows(list(
    k        = pars$k,
    m        = pars$m,
    sigmaObs = pars$sigma_obs
  ))
}

.prophetModelSummaryTableMcmcFill <- function(prophetTable, prophetModelResults, criLevel, ready) {
  if (!ready) return()

  modelSummary         <- prophetModelResults$modelSummary
  modelSummaryRowNames <- rownames(modelSummary)
  parNames             <- c("k", "m", "sigma_obs")
  parSummary           <- modelSummary[modelSummaryRowNames %in% parNames,]
  parLabels            <- c(gettext("Growth rate (k)"), gettext("Offset (m)"), gettext("Residual variance (sigma)"))

  prophetTable$addColumns(list(
    par      = parLabels,
    mean     = parSummary[["mean"]],
    sd       = parSummary[["sd"]],
    lowerCri = parSummary[["lower"]],
    upperCri = parSummary[["upper"]],
    rhat     = parSummary[["Rhat"]],
    bulkEss  = parSummary[["Bulk_ESS"]],
    tailEss  = parSummary[["Tail_ESS"]]
  ))

  return()
}

.prophetCreateChangePointTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prophetMainContainer"]][["prophetChangePointTable"]]) || !options$changePointTable) return()

  prophetModelResults <- jaspResults[["prophetResults"]][["object"]][["prophetModelResults"]]

  title <- switch(options$estimation,
                  map  = gettext("Changepoint Estimates Table"),
                  mcmc = gettext("Changepoint Posterior Summary Table"))
  prophetTable <- createJaspTable(title = title)
  prophetTable$dependOn(c("changePointTable", "ciLevel"))
  prophetTable$position <- 3

  criLevel <- (1-options[["ciLevel"]])/2

  prophetTable$addColumnInfo(name = "ds", title = gettext("Changepoint"), type = "string")

  if (options$estimation == "map") {
    prophetTable$addColumnInfo(name = "delta", title = gettext("Change in growth rate (\u03B4)"), type = "number")
  } else {
    parTitle <- gettext("Change in growth rate (\u03B4)")
    ciTitle  <- gettextf("%s%% CI", options[["ciLevel"]]*100)
    prophetTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number", overtitle = parTitle)
    prophetTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number", overtitle = parTitle)
    prophetTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = ciTitle)
    prophetTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = ciTitle)
  }

  prophetTable$addCitation("Taylor, S. J. & Letham, B. (2018). Forecasting at scale. *The American Statistician, 72*(1), 37-45.")

  .prophetChangePointTableFill(prophetTable, prophetModelResults, options$estimation, criLevel, ready)

  jaspResults[["prophetMainContainer"]][["prophetChangePointTable"]] <- prophetTable

  return()
}

.prophetChangePointTableFill <-  function(prophetTable, prophetModelResults, estimation, criLevel, ready) {
  if (!ready) return()

  cps    <- as.character(prophetModelResults$changepoints)

  if (estimation == "map") {
    prophetTable$addColumns(list(
      ds    = cps,
      delta = as.numeric(prophetModelResults[["params"]][["delta"]])
    ))
  } else {
    delta <- prophetModelResults[["modelSummary"]]
    delta <- delta[startsWith(rownames(delta), "delta"),]
    prophetTable$addColumns(list(
      ds       = cps,
      mean     = delta[["mean"]],
      sd       = delta[["sd"]],
      lowerCri = delta[["lower"]],
      upperCri = delta[["upper"]]
    ))
  }

  return()
}

.prophetCreateModelEvaluationTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prophetMainContainer"]][["prophetModelEvaluationTable"]]) || !(options$performanceMetrics && options$crossValidation)) return()

  prophetEvaluationResults <- jaspResults[["prophetResults"]][["object"]][["prophetEvaluationResults"]]

  prophetTable <- createJaspTable(gettext("Simulated Historical Forecasts Table"))
  prophetTable$dependOn(c(.prophetEvaluationDependencies(), "performanceMetrics",
                         "performanceMetricsMse", "performanceMetricsRmse", "performanceMetricsMape"))
  prophetTable$position <- 4

  prophetTable$addColumnInfo(name = "horizon", title = gettext("Horizon"), type = "string")

  if (options$performanceMetricsMse)  prophetTable$addColumnInfo(name = "mse", title = gettext("MSE"), type = "number")
  if (options$performanceMetricsRmse) prophetTable$addColumnInfo(name = "rmse", title = gettext("RMSE"), type = "number")
  if (options$performanceMetricsMape) prophetTable$addColumnInfo(name = "mape", title = gettext("MAPE"), type = "number", format = "pc")

  if (isTryError(prophetEvaluationResults))
    prophetTable$addFootnote(message = gettext(.extractErrorMessage(prophetEvaluationResults)))
  else
    .prophetModelEvaluationTableFill(prophetTable, options, prophetEvaluationResults, ready)

  prophetTable$addCitation("Taylor, S. J. & Letham, B. (2018). Forecasting at scale. *The American Statistician, 72*(1), 37-45.")

  jaspResults[["prophetMainContainer"]][["prophetModelEvaluationTable"]] <- prophetTable

  return()
}

.prophetModelEvaluationTableFill <- function(prophetTable, options, prophetEvaluationResults, ready) {
  if (!ready) return()

  metrics <- c("mse", "rmse", "mape")
  metrics <- metrics[c(options$performanceMetricsMse, options$performanceMetricsRmse, options$performanceMetricsMape)]

  metDat <- prophet::performance_metrics(df = prophetEvaluationResults, metrics = metrics, rolling_window = 0)

  if (options$crossValidationUnit == "weeks")
    metDat$horizon <- metDat$horizon / 7

  prophetTable$addColumns(as.list(metDat))

  return()
}

.prophetCreateHistoryPlot <- function(jaspResults, dataset, options, ready) {
  if (!ready || !options$historyPlot) return()

  prophetHistoryPlot <- createJaspPlot(title = "History Plot", height = 320, width = 480)
  prophetHistoryPlot$dependOn(c("dependent", "time", "historyIndicator", "historyPlot", "historyPlotType", "historyPlotRange", "historyPlotStart",
                               "historyPlotEnd"))
  prophetHistoryPlot$position <- 1

  p <- try(.prophetHistoryPlotFill(dataset, options))
  if (isTryError(p))
    prophetHistoryPlot$setError(gettext(.extractErrorMessage(p)))
  else
    prophetHistoryPlot$plotObject <- p

  jaspResults[["historyPlot"]] <- prophetHistoryPlot

  return()
}

.prophetHistoryPlotFill <- function(dataset, options) {

  y       <- dataset[[encodeColNames(options$dependent)]]
  ds      <- as.POSIXct(dataset[[encodeColNames(options$time)]], tz = "UTC")

  if (options$historyIndicator != "") {
    idx   <- as.logical(dataset[[encodeColNames(options$historyIndicator)]])
  } else {
    idx   <- !logical(length(ds))
  }

  histDat <- na.omit(data.frame(y = y, x = ds)[idx, ])

  xLimits <- range(histDat$x)

  if (options$historyPlotRange) {
    if (options$historyPlotStart != "") {
      xLimLower <- try(as.POSIXct(options$historyPlotStart))
      if (isTryError(xLimLower))
        stop(gettext("'Start' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))
      else
        xLimits[1] <- xLimLower
    }

    if (options$historyPlotEnd != "") {
      xLimUpper <- try(as.POSIXct(options$historyPlotEnd))
      if (isTryError(xLimUpper))
        stop(gettext("'End' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))
      else
        xLimits[2] <- xLimUpper
    }
  }

  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(histDat$y)

  p <- ggplot2::ggplot(data = histDat[histDat$x >= xLimits[1] & histDat$x <= xLimits[2], ], mapping = ggplot2::aes(x = x, y = y))

  if (options$historyPlotType == "line" || options$historyPlotType == "both")
    p <- p + ggplot2::geom_line(color = "black", size = 1.25)

  if (options$historyPlotType == "points" || options$historyPlotType == "both")
    p <- p + ggplot2::geom_point(size = 3, color = "grey")

  p <- p + ggplot2::scale_x_datetime(name = options$time,
                                     breaks = xBreaks,
                                     labels = xLabels,
                                     limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = options$dependent,
                                limits = range(yBreaks),
                                breaks = yBreaks)

  p <- jaspGraphs::themeJasp(p)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 0, 0))

  return(p)
}

.prophetCreateForecastPlots <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()

  prophetModelResults      <- jaspResults[["prophetResults"]][["object"]][["prophetModelResults"]]
  prophetPredictionResults <- jaspResults[["prophetResults"]][["object"]][["prophetPredictionResults"]]

  prophetForecastPlots <- createJaspContainer(title = gettext("Forecast Plots"))
  prophetForecastPlots$dependOn(.prophetPredictionDependencies())
  prophetForecastPlots$position <- 5

  if((options[["forecastPlotOverall"]] || options [["forecastPlotTrend"]]) && is.null(prophetPredictionResults)) {

    errorTable <- createJaspTable()
    errorTable$setError(gettext("Cannot draw forecast plots; no forecasts computed."))
    prophetForecastPlots[["errorTable"]] <- errorTable
    jaspResults[["prophetMainContainer"]][["prophetForecastPlots"]] <- prophetForecastPlots
    return()
  }

  if (options$forecastPlotOverall) .prophetCreateOverallForecastPlot(prophetForecastPlots, dataset, options, prophetPredictionResults)
  if (options$forecastPlotTrend)   .prophetCreateTrendForecastPlot(prophetForecastPlots, dataset, options, prophetPredictionResults)

  jaspResults[["prophetMainContainer"]][["prophetForecastPlots"]] <- prophetForecastPlots

  return()
}

.prophetCreateOverallForecastPlot <- function(prophetForecastPlots, dataset, options, prophetPredictionResults) {
  if (!is.null(prophetForecastPlots[["prophetOverallForecastPlot"]])) return()

  prophetOverallForecastPlot <- createJaspPlot(title = gettext("Overall"), height = 320, width = 480)
  prophetOverallForecastPlot$dependOn(c("forecastPlotOverall", "forecastPlotOverallDataPoints",
                                        "forecastPlotOverallCarryingCapacity", "forecastPlotOverallSaturatingMinimum",
                                       "forecastPlotOverallChangepoints", "forecastPlotOverallRange",
                                       "forecastPlotOverallStart", "forecastPlotOverallEnd"))

  p <- try(.prophetForecastPlotFill(prophetPredictionResults, dataset, options, type = "yhat"))

  if (isTryError(p))
    prophetOverallForecastPlot$setError(gettext(.extractErrorMessage(p)))
  else
    prophetOverallForecastPlot$plotObject <- p

  prophetForecastPlots[["prophetOverallForecastPlot"]] <- prophetOverallForecastPlot

  return()
}

.prophetCreateTrendForecastPlot <- function(prophetForecastPlots, dataset, options, prophetPredictionResults) {
  if (!is.null(prophetForecastPlots[["prophetTrendForecastPlot"]])) return()

  prophetTrendForecastPlot <- createJaspPlot(title = gettext("Trend"), height = 320, width = 480)
  prophetTrendForecastPlot$dependOn(c("forecastPlotTrend", "forecastPlotTrendChangepoints",
                                       "forecastPlotTrendRange",
                                       "forecastPlotTrendStart", "forecastPlotTrendEnd"))

  p <- try(.prophetForecastPlotFill(prophetPredictionResults, dataset, options, type = "trend"))

  if (isTryError(p))
    prophetTrendForecastPlot$setError(gettext(.extractErrorMessage(p)))
  else
    prophetTrendForecastPlot$plotObject <- p

  prophetForecastPlots[["prophetTrendForecastPlot"]] <- prophetTrendForecastPlot

  return()
}

.prophetCreateSeasonalityPlotContainer <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()

  prophetModelResults <- jaspResults[["prophetResults"]][["object"]][["prophetModelResults"]]

  prophetSeasonalityPlots <- createJaspContainer(title = gettext("Seasonality Plots"))
  prophetSeasonalityPlots$dependOn(c("seasonalityPlots"))
  prophetSeasonalityPlots$position <- 6

  if (length(options$seasonalityPlots) > 0) {
    for (name in options$seasonalityPlots) {
      .prophetCreateSeasonalityPlot(prophetSeasonalityPlots, name, prophetModelResults, options)
    }
  }

  jaspResults[["prophetMainContainer"]][["prophetSeasonalityPlots"]] <- prophetSeasonalityPlots

  return()
}

.prophetCreateSeasonalityPlot <- function(prophetSeasonalityPlots, name, prophetModelResults, options) {
  if (!is.null(prophetSeasonalityPlots[[name]])) return()

  seasData <- prophetModelResults[["predictedSeasonalities"]][[name]]

  seasonalityPlot <- createJaspPlot(title = name, height = 320, width = 480)
  seasonalityPlot$plotObject <- .prophetSeasonalityPlotFill(seasData, encodeColNames(name), options)
  prophetSeasonalityPlots[[name]] <- seasonalityPlot

  return()
}

.prophetPredictSeasonality <- function(name, prophetModelResults, options) {
  period  <- .prophetGetSeasonalityProperties(name, "period", options)
  unit    <- .prophetGetSeasonalityProperties(name, "unit", options)

  if (period > 3 && unit == "years")
    start <- prophetModelResults[["history.dates"]][1]
  else
    start <- as.POSIXct("2018-01-01 00:00:00", tz = "UTC")

  ds      <- seq(from = start, by = unit, length.out = period+1)
  futDat  <- data.frame(ds = ds, cap = 1, floor = 0)

  if (length(options$covariates) > 0) {
    for (cov in options$covariates) {
      futDat[[cov]] <- 0
    }
  }

  predSeas <- predict(prophetModelResults, futDat)

  return(predSeas)
}

.prophetGetPrettyDateLabels <- function(period, unit) {
  unitDays <- switch (unit,
                      secs = 1/86400,
                      mins = 1/1440,
                      hours = 1/24,
                      days =  1,
                      weeks = 7,
                      years = 365)

  daysPeriod <- unitDays*period

  if (daysPeriod <= 3/1440)     dateFormat <- list(format = "%M:%OS", label = gettext("Time (min:sec)"))
  else if (daysPeriod <= 1/8)   dateFormat <- list(format = "%H:%M", label = gettext("Time (hour:min)"))
  else if (daysPeriod <= 3)     dateFormat <- list(format = "%H", label = gettext("Time (hour)"))
  else if (daysPeriod <= 15)    dateFormat <- list(format = "%a", label = gettext("Weekday"))
  else if (daysPeriod <= 29)    dateFormat <- list(format = "%d", label = gettext("Time (day)"))
  else if (daysPeriod <= 183)   dateFormat <- list(format = "%m-%d", label = gettext("Time (month-day)"))
  else if (daysPeriod <= 366)   dateFormat <- list(format = "%m", label = gettext("Time (month)"))
  else if (daysPeriod <= 3*366) dateFormat <- list(format = "%b", label = gettext("Time (month)"))
  else                          dateFormat <- list(format = "%Y", label = gettext("Time (year)"))

  return(dateFormat)
}

.prophetSeasonalityPlotFill <- function(seasData, name, options) {

  y    <- seasData[[name]]
  ymin <- seasData[[paste0(name, "_lower")]]
  ymax <- seasData[[paste0(name, "_upper")]]
  x    <- seasData$ds
  df   <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)

  xBreaks <- pretty(x)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(c(min(ymin), max(ymax)))
  mode    <- .prophetGetSeasonalityProperties(name, "mode", options)
  period  <- .prophetGetSeasonalityProperties(name, "period", options)
  unit    <- .prophetGetSeasonalityProperties(name, "unit", options)
  xFormat <- .prophetGetPrettyDateLabels(period, unit)

  p <- ggplot2::ggplot(df, mapping = ggplot2::aes(x = x, y = y)) +

    ggplot2::geom_line(color = "black", size = 1.25) +

    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.4) +

    ggplot2::scale_x_datetime(name = xFormat$label,
                              breaks = xBreaks,
                              labels = xLabels,
                              date_labels = xFormat$format,
                              limits = range(xBreaks))

  if (mode == "multiplicative") {
    p <- p + ggplot2::scale_y_continuous(name = options$dependent,
                                         labels = scales::percent,
                                         breaks = yBreaks,
                                         limits = range(yBreaks))
  } else {
    p <- p + ggplot2::scale_y_continuous(name = options$dependent,
                                         breaks = yBreaks,
                                         limits = range(yBreaks))
  }

  p <- jaspGraphs::themeJasp(p)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 0, 0))

  return(p)
}

.prophetForecastPlotFill <- function(prophetPredictionResults, dataset, options, type) {

  yHist <- dataset[[encodeColNames(options$dependent)]]
  xHist <- as.POSIXct(dataset[[encodeColNames(options$time)]], tz = "UTC")

  if (options$historyIndicator != "")
    idx <- as.logical(dataset[[encodeColNames(options$historyIndicator)]])
  else
    idx <- !logical(length(xHist))

  histDat <- na.omit(data.frame(y = yHist, x = xHist)[idx, ])

  x <- try(as.POSIXct(prophetPredictionResults[["ds"]], tz = "UTC"))

  if (isTryError(x) && options$predictionType == "nonperiodicalPrediction")
    stop(gettext("Please enter valid 'Start' and 'End' arguments for nonperiodical prediction (e.g., yyyy-mm-dd hh:mm:ss)"))

  y     <- prophetPredictionResults[[type]]
  ymin  <- prophetPredictionResults[[paste0(type, "_lower")]]
  ymax  <- prophetPredictionResults[[paste0(type, "_upper")]]
  df    <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)

  if (options$growth == "logistic") {
    df$cap   <- prophetPredictionResults[["cap"]]
    df$floor <- prophetPredictionResults[["floor"]]
  }

  p <- ggplot2::ggplot()

  if (options$forecastPlotOverallDataPoints && type == "yhat")
    p <- p + ggplot2::geom_point(data = histDat, mapping = ggplot2::aes(x = x, y = y), size = 3, color = "grey")

  p <- p +
    ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y), color = "black", size = 1.25) +

    ggplot2::geom_ribbon(data = df, mapping = ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.4)

  if (options$growth == "logistic" && type == "yhat") {
    if (options$forecastPlotOverallCarryingCapacity)
      p <- p + ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = cap), color = "black", size = 0.75)

    if (options$forecastPlotOverallSaturatingMinimum)
      p <- p + ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = floor), color = "black", size = 0.75)
  }

  xLimits <- range(df$x)

  if (options$forecastPlotOverallRange) {
    if (options$forecastPlotOverallStart != "" && type == "yhat") {
      xLimLower <- try(as.POSIXct(options$forecastPlotOverallStart))
      if (isTryError(xLimLower))
        stop(gettext("'Start' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))
      else
        xLimits[1] <- xLimLower
    }

    if (options$forecastPlotOverallEnd != "" && type == "yhat") {
      xLimUpper <- try(as.POSIXct(options$forecastPlotOverallEnd))
      if (isTryError(xLimUpper))
        stop(gettext("'End' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))
      else
        xLimits[2] <- xLimUpper
    }
  }

  if (options$forecastPlotTrendRange) {
    if (options$forecastPlotTrendStart != "" && type == "trend") {
      xLimLower <- try(as.POSIXct(options$forecastPlotTrendStart))
      if (isTryError(xLimLower))
        stop(gettext("'Start' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))
      else
        xLimits[1] <- xLimLower
    }

    if (options$forecastPlotTrendEnd != "" && type == "trend") {
      xLimUpper <- try(as.POSIXct(options$forecastPlotTrendEnd))
      if (isTryError(xLimUpper))
        stop(gettext("'End' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)"))
      else
        xLimits[2] <- xLimUpper
    }
  }

  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(unlist(list(df[, -which(names(df) == "x")]), histDat[, -which(names(df) == "x")]))

  if ((options$forecastPlotOverallChangepoints && type == "yhat")
    || (options$forecastPlotTrendChangepoints && type == "trend")) {
    isChangepoint <- as.logical(prophetPredictionResults[["isChangepoint"]], tz = "UTC")
    cpDat <- data.frame(x = df$x[isChangepoint], xend = df$x[isChangepoint], y = min(yBreaks), yend = max(yBreaks))
    p <- p + ggplot2::geom_segment(data = cpDat, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend))
  }

  p <- p + ggplot2::geom_segment(mapping = ggplot2::aes(x = histDat$x[length(histDat$x)], y = min(yBreaks),
                                                        xend = histDat$x[length(histDat$x)]), yend = max(yBreaks),
                                 linetype = "dashed") +

    ggplot2::scale_x_datetime(name = options$time, breaks = xBreaks, labels = xLabels, limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = options$dependent, breaks = yBreaks, limits = range(yBreaks))

  p <- jaspGraphs::themeJasp(p)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 0, 0))

  return(p)
}

.prophetCreateCovariatePlotContainer <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()

  prophetModelResults <- jaspResults[["prophetResults"]][["object"]][["prophetModelResults"]]

  prophetCovariatePlots <- createJaspContainer(title = gettext("Covariate Plots"))
  prophetCovariatePlots$dependOn(c("covariatePlots", "covariatePlotsType"))
  prophetCovariatePlots$position <- 7

  if (length(options$covariatePlots) > 0) {
    for (cov in options$covariatePlots) {
      .prophetCreateCovariatePlot(prophetCovariatePlots, dataset, cov, prophetModelResults, options)
    }
  }

  jaspResults[["prophetMainContainer"]][["prophetCovariatePlots"]] <- prophetCovariatePlots

  return()
}

.prophetCreateCovariatePlot <- function(prophetCovariatePlots, dataset, cov, prophetModelResults, options) {
  name <- cov[["variable"]]
  if (!is.null(prophetCovariatePlots[[name]])) return()

  covMode <- prophetModelResults[["extra_regressors"]][[name]][["mode"]]
  covariatePlot <- createJaspPlot(title = name, height = 320, width = 480)
  covariatePlot$plotObject <- .prophetCovariatePlotFill(dataset, cov, options, covMode)
  prophetCovariatePlots[[name]] <- covariatePlot

  return()
}

.prophetCovariatePlotFill <- function(dataset, cov, options, mode) {

  y <- dataset[[encodeColNames(cov[["variable"]])]]
  x <- as.POSIXct(dataset[[encodeColNames(options$time)]], tz = "UTC")

  df <- data.frame(x = x, y = y)

  p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y))

  if (cov[["covariatePlotsType"]] == "line" || cov[["covariatePlotsType"]] == "both")
    p <- p + ggplot2::geom_line(color = "black", size = 1.25)

  if (cov[["covariatePlotsType"]] == "points" || cov[["covariatePlotsType"]] == "both")
    p <- p + ggplot2::geom_point(size = 3, color = "grey")

  xBreaks <- pretty(x)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(y)

  p <- p +
    ggplot2::scale_x_datetime(name = options$time, breaks = xBreaks, labels = xLabels, limits = range(xBreaks))

  if (mode == "multiplicative") {
    p <- p + ggplot2::scale_y_continuous(name = cov[["variable"]],
                                         labels = scales::percent,
                                         breaks = yBreaks,
                                         limits = range(yBreaks))
  } else {
    p <- p + ggplot2::scale_y_continuous(name = cov[["variable"]],
                                         breaks = yBreaks,
                                         limits = range(yBreaks))
  }

  p <- jaspGraphs::themeJasp(p)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 0, 0))

  return(p)
}

.prophetCreatePerformancePlots <- function(jaspResults, options, ready) {
  if (!ready || !options$crossValidation) return()

  prophetEvaluationResults <- jaspResults[["prophetResults"]][["object"]][["prophetEvaluationResults"]]

  prophetPerformancePlots <- createJaspContainer(gettext("Performance Plots"))
  prophetPerformancePlots$dependOn(.prophetEvaluationDependencies())
  prophetPerformancePlots$position <- 8

  if(options$msePlot)  .prophetCreatePerformancePlotMse( prophetPerformancePlots, options, prophetEvaluationResults)
  if(options$rmsePlot) .prophetCreatePerformancePlotRmse(prophetPerformancePlots, options, prophetEvaluationResults)
  if(options$mapePlot) .prophetCreatePerformancePlotMape(prophetPerformancePlots, options, prophetEvaluationResults)

  jaspResults[["prophetMainContainer"]][["prophetEvaluationPlots"]] <- prophetPerformancePlots

  return()
}

.prophetCreatePerformancePlotMse <- function(prophetPerformancePlots, options, prophetEvaluationResults) {
  if (!is.null(prophetPerformancePlots[["prophetPerformancePlotMse"]])) return()

  prophetPerformancePlotMse <- createJaspPlot(title = gettext("MSE"), height = 320, width = 480)
  prophetPerformancePlotMse$dependOn(c("msePlot"))

  prophetPerformancePlotMse$plotObject <- .prophetPerformancePlotFill(prophetEvaluationResults, options, type = "mse")

  prophetPerformancePlots[["prophetPerformancePlotMse"]] <- prophetPerformancePlotMse

  return()
}

.prophetCreatePerformancePlotRmse <- function(prophetPerformancePlots, options, prophetEvaluationResults) {
  if (!is.null(prophetPerformancePlots[["prophetPerformancePlotRmse"]])) return()

  prophetPerformancePlotRmse <- createJaspPlot(title = gettext("RMSE"), height = 320, width = 480)
  prophetPerformancePlotRmse$dependOn(c("rmsePlot"))

  prophetPerformancePlotRmse$plotObject <- .prophetPerformancePlotFill(prophetEvaluationResults, options, type = "rmse")

  prophetPerformancePlots[["prophetPerformancePlotRmse"]] <- prophetPerformancePlotRmse

  return()
}

.prophetCreatePerformancePlotMape <- function(prophetPerformancePlots, options, prophetEvaluationResults) {
  if (!is.null(prophetPerformancePlots[["prophetPerformancePlotMape"]])) return()

  prophetPerformancePlotMape <- createJaspPlot(title = gettext("MAPE"), height = 320, width = 480)
  prophetPerformancePlotMape$dependOn(c("mapePlot"))

  prophetPerformancePlotMape$plotObject <- .prophetPerformancePlotFill(prophetEvaluationResults, options, type = "mape")

  prophetPerformancePlots[["prophetPerformancePlotMape"]] <- prophetPerformancePlotMape

  return()
}

.prophetPerformancePlotFill <- function(prophetEvaluationResults, options, type) {

  sampleDat <- prophet::performance_metrics(prophetEvaluationResults, metrics = type, rolling_window = -1)
  meanDat   <- prophet::performance_metrics(prophetEvaluationResults, metrics = type, rolling_window = 0)

  if (options$crossValidationUnit == "weeks") {
    sampleDat$horizon <- sampleDat$horizon / 7
    meanDat$horizon   <- meanDat$horizon / 7
  }

  if (type == "mape") {
    sampleDat$mape    <- sampleDat$mape*100
    meanDat$mape      <- meanDat$mape*100
  }

  xBreaks <- pretty(sampleDat$horizon)
  yBreaks <- pretty(sampleDat[[type]])

  p <- ggplot2::ggplot(data = sampleDat, mapping = ggplot2::aes_string(x = "horizon", y = type)) +

    ggplot2::geom_point(size = 3, color = "grey") +

    ggplot2::geom_line(data = meanDat, color = "darkred", size = 1.25) +

    ggplot2::scale_x_continuous(name = gettext("Horizon"), breaks = xBreaks, limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = gettext(toupper(type)), breaks = yBreaks, limits = range(yBreaks))

  p <- jaspGraphs::themeJasp(p)

  return(p)
}

.prophetCreateParameterPlots <- function(jaspResults, options, ready) {
  if (!ready) return()

  prophetModelResults <- jaspResults[["prophetResults"]][["object"]][["prophetModelResults"]]

  prophetParameterPlots <- createJaspContainer(gettext("Parameter Plots"))
  prophetParameterPlots$position <- 9

  if(options$changepointPlot)
    .prophetCreateParameterPlotDelta(prophetParameterPlots, options, prophetModelResults)

  if(options$posteriorPlot && options$estimation == "mcmc")
    .prophetCreateParameterPlotMarginal(prophetParameterPlots, options, prophetModelResults)

  jaspResults[["prophetMainContainer"]][["prophetParameterPlots"]] <- prophetParameterPlots
}

.prophetCreateParameterPlotDelta <- function(prophetParameterPlots, options, prophetModelResults) {
  if (!is.null(prophetParameterPlots[["prophetParameterPlotDelta"]])) return()

  prophetParameterPlotDelta <- createJaspPlot(title = gettext("Changepoint Plot"), height = 320, width = 480)
  prophetParameterPlotDelta$dependOn(c("changepointPlot"))

  prophetParameterPlotDelta$plotObject <- .prophetParameterPlotDeltaFill(prophetModelResults, options)

  prophetParameterPlots[["prophetParameterPlotDelta"]] <- prophetParameterPlotDelta

  return()
}

.prophetParameterPlotDeltaFill <- function(prophetModelResults, options) {
  if(options[["estimation"]] == "map") {
    deltas <- as.numeric(prophetModelResults[["params"]][["delta"]])
  } else {
    deltas <- prophetModelResults[["modelSummary"]]
    deltas <- as.numeric(deltas[startsWith(rownames(deltas), "delta"), "mean"])
  }
  cps     <- as.POSIXct(prophetModelResults$changepoints)

  xBreaks <- pretty(cps)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(deltas)

  p <- ggplot2::ggplot(data = data.frame(x = cps, y = deltas), mapping = ggplot2::aes(x = x, y = y)) +

    ggplot2::geom_point(size = 3, color = "grey") +

    ggplot2::scale_x_datetime(name = options$time, breaks = xBreaks, labels = xLabels, limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = gettext("Change in growth rate"), breaks = yBreaks, limits = range(yBreaks))

  p <- jaspGraphs::themeJasp(p)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 0, 0))

  return(p)
}

.prophetCreateParameterPlotMarginal <- function(prophetParameterPlots, options, prophetModelResults) {
  if (!is.null(prophetParameterPlots[["prophetParameterPlotMarginal"]])) return()

  prophetParameterPlotMarginal <- createJaspContainer(title = gettext("Posterior Distributions"))

  parNames <- c("k", "m", "sigma_obs")
  parTitles <- c(gettext("Growth rate"), gettext("Offset"), gettext("Residual variance"))

  marginalPlotList <- list()

  for (i in 1:length(parNames)) {
    marginalPlotList[[i]] <- createJaspPlot(title = parTitles[i], height = 320, width = 480)

    marginalPlotList[[i]]$plotObject <- .prophetParameterPlotMarginalFill(prophetModelResults,
                                                                         options,
                                                                         parNames[i],
                                                                         parTitles[i])

    prophetParameterPlotMarginal[[parNames[i]]] <- marginalPlotList[[i]]
  }

  prophetParameterPlotMarginal$dependOn(c("posteriorPlot"))

  prophetParameterPlots[["prophetParameterPlotMarginal"]] <- prophetParameterPlotMarginal

  return()
}

.prophetParameterPlotMarginalFill <- function(prophetModelResults, options, parName, parTitle) {
  dens    <- prophetModelResults[["parameterDensities"]][[parName]]
  x       <- dens$x
  y       <- dens$y

  ci     <- prophetModelResults[["modelSummary"]][,c("lower", "upper")]
  ci     <- ci[rownames(ci) == parName,]

  xBreaks    <- pretty(range(x))
  yBreaks    <- pretty(c(0, 1.15*max(y)))
  yBarPos    <- 0.9*yBreaks[length(yBreaks)]
  yBarHeight <- 0.05*yBreaks[length(yBreaks)]

  p <- ggplot2::ggplot() +

    ggplot2::geom_line(data = data.frame(x = x, y = y),
                       mapping = ggplot2::aes(x = x, y = y),
                       size = 1.25) +

    ggplot2::geom_errorbarh(data = data.frame(xmin = ci[,1], xmax = ci[,2], y = yBarPos),
                            mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y),
                            height = yBarHeight) +

    ggplot2::scale_x_continuous(name = parTitle, breaks = xBreaks, limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks))

  p <- jaspGraphs::themeJasp(p)

  return(p)
}

.prophetGetSeasonalityProperties <- function(name, prop, options) {
  if(is.null(options$seasonalities))
    return()

  propRes <- options$seasonalities[[which(sapply(options$seasonalities, function(s) encodeColNames(s[["name"]]) == name))]][[prop]]
  return(propRes)
}

# helpers ----
.prophetIntervalLevels <- function(options, what = c("credible", "prediction")) {
  what <- match.arg(what)

  criLevel <- switch(what,
                     credible   = (1-options[["ciLevel"]])/2,
                     prediction = (1-options[["predictionIntervalLevel"]])/2)

  return(c(criLevel, 1-criLevel))
}
