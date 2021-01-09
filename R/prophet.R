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

# Main function ----
Prophet <- function(jaspResults, dataset = NULL, options) {

  options <- .prophetInitOptions(jaspResults, options)
  
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
  .prophetCreatePerformancePlots(        jaspResults, options, ready)
  .prophetCreateParameterPlots(          jaspResults, options, ready)
  
  return()
}

# Init functions ----
.prophetInitOptions <- function(jaspResults, options) {
  if (options$growth == "linear") {
    options$capacity <- ""
    options$minimum  <- ""
  }

  return(options)
}

.prophetReadData <- function(options, ready) {
  if(!ready) return()

  numericVars  <- c(options$dependent, options$capacity, options$minimum, unlist(options$covariates), options$changepoints, options$historyIndicator)
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
        return(gettext("'Time' must be in a date-like format (e.g., yyyy-mm-dd)"))

      if (options$predictionType == "nonperiodicalPrediction"
        && options$nonperiodicalPredictionStart != ""
        && options$nonperiodicalPredictionEnd != "") {
        predStart <- try(as.POSIXct(options$nonperiodicalPredictionStart))
        predEnd <- try(as.POSIXct(options$nonperiodicalPredictionEnd))
        
        if (isTryError(predStart))
          return(gettext("'Start date' for nonperiodical prediction must be in a date-like format (e.g., yyyy-mm-dd)"))
        
        if (isTryError(predEnd))
          return(gettext("'End date' for nonperiodical prediction must be in a date-like format (e.g., yyyy-mm-dd)"))
      }

      return()
    },
    
    .logicalVarChecks <- function() {
      isChangepoint <- dataset[[encodeColNames(options$changepoints)]]

      if (any(is.na(as.logical(isChangepoint))) || length(unique(isChangepoint)) > 2)
        return(gettext("'Changepoints' must be a logical variable (e.g., 0/1)"))

      isHistory <- dataset[[encodeColNames(options$historyIndicator)]]

      if (any(is.na(as.logical(isHistory))) || length(unique(isHistory)) > 2)
        return(gettext("'History Indicator' must be a logical variable (e.g., 0/1)"))
    },

    .logisticVarChecks <- function() {
      if (options$capacity != "" && options$minimum != "") {
        cap   <- dataset[[encodeColNames(options$capacity)]]
        floor <- dataset[[encodeColNames(options$minimum)]]

        if (!all(cap > floor))
          return(gettext("'Carrying Capacity' must always be larger than 'Saturating Minimum'"))
      }

      if (options$capacity != "") {
        cap   <- dataset[[encodeColNames(options$capacity)]]

        if (!all(cap > options$constantMinimum))
          return(gettext("'Carrying Capacity' must always be larger than 'Constant Saturating Minimum'"))
      }

      if (options$growth == "logistic" && options$capacity == "") {
        if (options$minimum != "") {
          floor <- dataset[[encodeColNames(options$minimum)]]
          if (options$constantCapacity <= floor)
            return(gettext("'Constant Carrying Capacity' must always be larger than 'Saturating Minimum'"))
        } else {
          if (options$constantCapacity <= options$constantMinimum)
            return(gettext("'Constant Carrying Capacity' must always be larger than 'Constant Saturating Minimum'"))
        }
      }
    },

    .seasonalityNameChecks <- function() {
      if (!is.null(options$seasonalities)) {
        for(seas in options$seasonalities) {
          if(make.names(seas$name, allow_ = TRUE) != seas$name)
            return(gettext("Seasonality names must only contain letters, number, dots, or underscores and must start with letters or dots that are not followed by a number"))
        }
      }
      return()
    },

    .predictionChecks <- function() {
      if (options$capacity == "" && options$minimum == "" && length(options$covariates) == 0)
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
      
      if (!all(futds %in% ds) && options$capacity != "")
        return(gettext("'Carrying Capacity' must be supplied for predictions"))

      if (!all(futds %in% ds) && length(options$covariates) > 0)
        return(gettext("'Covariates' must be supplied for predictions"))
      
      return()
    }
  )
  
  targetVars <- c(options$dependent, unlist(options$covariates))
  if (options$capacity != "")
    targetVars <- c(targetVars, options$capacity)
  if (options$minimum != "")
    targetVars <- c(targetVars, options$minimum)

  .hasErrors(dataset, "run", type = c('observations', 'variance', 'infinity', "missingValues"),
             observations.target = targetVars,
             variance.target = c(options$dependent, unlist(options$covariates)),
             infinity.target = targetVars,
             missingValues.target = c(options$date, options$capacity, options$minimum, unlist(options$covariates)),
             custom = checks,
             observations.amount = '< 2',
             exitAnalysisIfErrors = TRUE)
}

.prophetModelDependencies <- function(options) {
  if (options$changepoints != "") {
    return(c("dependent", "time", "changepoints", "capacity", "minimum", "covariates", 
             "historyIndicator", "growth", "constantCapacity", "constantMinimum",
             "assignedCovariates", "seasonalities",
             "estimation", "mcmcSamples", "predictionIntervalWidth", 
             "predictionIntervalSamples"))
  } else {
    return(c("dependent", "time", "changepoints", "capacity", "minimum", "covariates", 
             "historyIndicator", "growth", "constantCapacity", "constantMinimum",
             "maxChangepoints", "changepointRange", "changepointPriorScale",
             "assignedCovariates", "seasonalities",
             "estimation", "mcmcSamples", "predictionIntervalWidth", 
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
    prophetResults <- createJaspContainer("prophetResults")
    
    jaspResults[["prophetResults"]] <- prophetResults
    jaspResults[["prophetResults"]]$dependOn(.prophetModelDependencies(options))
  }
  
  if (is.null(jaspResults[["prophetResults"]][["prophetModelResults"]])) {
    prophetModelResultsState <- createJaspState()
    prophetModelResults <- .prophetModelResultsHelper(dataset, options)
    prophetModelResultsState$object <- prophetModelResults
    jaspResults[["prophetResults"]][["prophetModelResults"]] <- prophetModelResultsState
  }
  
  if (is.null(jaspResults[["prophetResults"]][["prophetPredictionResults"]])
    && ((options$predictionType == "nonperiodicalPrediction"
         && options$nonperiodicalPredictionStart != ""
         && options$nonperiodicalPredictionEnd != "")
        || options$predictionType == "periodicalPrediction")) {
    prophetPredictionResultsState <- createJaspState()
    prophetPredictionResultsState$dependOn(.prophetPredictionDependencies())
    prophetModelResults <- jaspResults[["prophetResults"]][["prophetModelResults"]]$object
    prophetPredictionResults <- .prophetPredictionResultsHelper(dataset, options, prophetModelResults)
    prophetPredictionResultsState$object <- prophetPredictionResults
    jaspResults[["prophetResults"]][["prophetPredictionResults"]] <- prophetPredictionResultsState

    if (options$predictionSavePath != "") .prophetSavePredictions(jaspResults, options)
  }
  
  if (is.null(jaspResults[["prophetResults"]][["prophetEvaluationResults"]]) && options$crossValidation) {
    prophetEvaluationResultsState <- createJaspState()
    prophetEvaluationResultsState$dependOn(.prophetEvaluationDependencies())
    prophetModelResults <- jaspResults[["prophetResults"]][["prophetModelResults"]]$object
    prophetEvaluationResults <- .prophetEvaluationResultsHelper(dataset, options, prophetModelResults)
    prophetEvaluationResultsState$object <- prophetEvaluationResults
    jaspResults[["prophetResults"]][["prophetEvaluationResults"]] <- prophetEvaluationResultsState
  }
  
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
    if (options$capacity != "")
      fitDat$cap <- as.numeric(dataset[[encodeColNames(options$capacity)]])
    else
      fitDat$cap <- options$constantCapacity
    
    if (options$minimum != "")
      fitDat$floor <- as.numeric(dataset[[encodeColNames(options$minimum)]])
    else
      fitDat$floor <- options$constantMinimum
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
                          interval.width = options$predictionIntervalWidth,
                          uncertainty.samples = predIntSamples,
                          fit = FALSE)
  
  if (length(options$covariates) > 0) {
    covs <- unlist(options$covariates)
    
    for (cov in covs) {
      covAssigned   <- sapply(options$assignedCovariates, function(c) encodeColNames(c$variable) == cov)

      if (any(covAssigned)) {
        covArgs <- options$assignedCovariates[[which(covAssigned)]]
        mod     <- prophet::add_regressor(m = mod,
                                          name = cov,
                                          prior.scale = covArgs$priorSigma,
                                          standardize = covArgs$standardize,
                                          mode = covArgs$mode)
      } else {
        mod     <- prophet::add_regressor(m = mod,
                                          name = cov,
                                          prior.scale = 10,
                                          standardize = "auto",
                                          mode = "additive")
      }
      datCov        <- dataset[[encodeColNames(cov)]]
      fitDat[[cov]] <- datCov
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

  fit     <- prophet::fit.prophet(m = mod, df = fitDat)
  
  return(fit)
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
    if (options$capacity != "") {
      futCap       <- as.numeric(dataset[[encodeColNames(options$capacity)]])
      futDat$cap   <- futCap[ds %in% futds]
    } else {
      futDat$cap   <- options$constantCapacity
    }

    if (options$minimum != "") {
      futFloor     <- as.numeric(dataset[[encodeColNames(options$minimum)]])
      futDat$floor <- futFloor[ds %in% futds]
    } else {
      futDat$floor   <- options$constantMinimum
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
  if (is.null(jaspResults[["prophetResults"]][["prophetPredictionSavePath"]])) {
    predSavePath <- createJaspState()
    predSavePath$dependOn(c(.prophetPredictionDependencies(),
                            "predictionSavePath"))
    jaspResults[["prophetResults"]][["prophetPredictionSavePath"]] <- predSavePath
  }

  write.csv(jaspResults[["prophetResults"]][["prophetPredictionResults"]]$object,
            file = options$predictionSavePath,
            row.names = FALSE)
  predSavePath$object <- TRUE
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
  
  prophetModelResults <- jaspResults[["prophetResults"]][["prophetModelResults"]]$object
  
  if (options$estimation == "map") {
    prophetTable <- createJaspTable(title = "Parameter Estimates Table")
    prophetTable$position <- 1
    
    prophetTable$addColumnInfo(name = "k", title = gettext("Growth rate (k)"), type = "number")
    prophetTable$addColumnInfo(name = "m", title = gettext("Offset (m)"), type = "number")
    prophetTable$addColumnInfo(name = "sigmaObs", title = gettext("Residual variance (sigma)"), type = "number")
    
    .prophetModelSummaryTableMapFill(prophetTable, prophetModelResults, ready)
    
    jaspResults[["prophetMainContainer"]][["prophetTable"]] <- prophetTable
    
  } else {
    prophetTable <- createJaspTable(title = "Posterior Summary Table")
    prophetTable$position <- 2

    overtitle <- gettext("95% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
    prophetTable$addColumnInfo(name = "par", title = gettext("Parameter"), type = "string")
    prophetTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    prophetTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
    prophetTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = overtitle)
    prophetTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = overtitle)
    prophetTable$addColumnInfo(name = "rhat", title = gettext("R-hat"), type = "number")
    prophetTable$addColumnInfo(name = "bulkEss", title = gettext("ESS (bulk)"), type = "integer")
    prophetTable$addColumnInfo(name = "tailEss", title = gettext("ESS (tail)"), type = "integer")
    
    .prophetModelSummaryTableMcmcFill(prophetTable, prophetModelResults, ready)
    
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

.prophetModelSummaryTableMcmcFill <- function(prophetTable, prophetModelResults, ready) {
  if (!ready) return()
  
  modelSummary         <- rstan::monitor(prophetModelResults$stan.fit, probs = c(0.025, 0.975))
  modelSummaryRowNames <- rownames(modelSummary)
  parNames             <- c("k", "m", "sigma_obs")
  parSummary           <- modelSummary[modelSummaryRowNames %in% parNames, 1:ncol(modelSummary)]
  parLabels            <- c("Growth rate (k)", "Offset (m)", "Residual variance (sigma)")
  
  prophetTable$addColumns(list(
    par      = parLabels,
    mean     = parSummary[["mean"]],
    sd       = parSummary[["sd"]],
    lowerCri = parSummary[["2.5%"]],
    upperCri = parSummary[["97.5%"]],
    rhat     = parSummary[["Rhat"]],
    bulkEss  = parSummary[["Bulk_ESS"]],
    tailEss  = parSummary[["Tail_ESS"]]
  ))

  return()
}

.prophetCreateChangePointTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prophetMainContainer"]][["prophetChangePointTable"]]) || !options$changePointTable) return()

  prophetModelResults <- jaspResults[["prophetResults"]][["prophetModelResults"]]$object
  
  title <- switch(options$estimation,
                  map  = "Changepoint Estimates Table",
                  mcmc = "Changepoint Posterior Summary Table")
  prophetTable <- createJaspTable(title = title)
  prophetTable$dependOn(c("changePointTable"))
  prophetTable$position <- 3

  prophetTable$addColumnInfo(name = "ds", title = gettext("Changepoint"), type = "string")
  
  if (options$estimation == "map") {
    prophetTable$addColumnInfo(name = "delta", title = gettext("Change in growth rate (delta)"), type = "number")
  } else {
    parTitle <- gettext("Change in growth rate (delta)")
    ciTitle <- gettext("95% Credible Interval")
    prophetTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number", overtitle = parTitle)
    prophetTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number", overtitle = parTitle)
    prophetTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = ciTitle)
    prophetTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = ciTitle)
  }
  
  .prophetChangePointTableFill(prophetTable, prophetModelResults, options$estimation, ready)
    
  jaspResults[["prophetMainContainer"]][["prophetChangePointTable"]] <- prophetTable

  return()
}

.prophetChangePointTableFill <-  function(prophetTable, prophetModelResults, estimation, ready) {
  if (!ready) return()

  delta  <- switch(estimation,
                   map  = as.numeric(prophetModelResults$params$delta),
                   mcmc = as.numeric(apply(prophetModelResults$params$delta, 2, mean)))
  cps    <- as.character(prophetModelResults$changepoints)

  if (estimation == "map") {
    prophetTable$addColumns(list(
      ds    = cps,
      delta = delta
    ))
  } else {
    deltaCri <- apply(prophetModelResults$params$delta, 2, quantile, probs = c(0.025, 0.975))
    deltaSd  <- apply(prophetModelResults$params$delta, 2, sd)
    prophetTable$addColumns(list(
      ds       = cps,
      mean     = delta,
      sd       = deltaSd,
      lowerCri = deltaCri[1,],
      upperCri = deltaCri[2,]
    ))
  }

  return()
}

.prophetCreateModelEvaluationTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prophetMainContainer"]][["prophetModelEvaluationTable"]]) || !(options$performanceMetrics && options$crossValidation)) return()
  
  prophetEvaluationResults <- jaspResults[["prophetResults"]][["prophetEvaluationResults"]]$object
  
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
  prophetHistoryPlot$dependOn(c("dependent", "time", "historyIndicator", "historyPlot", "historyPlotAddLine", "historyPlotStart",
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
  
  if (options$historyPlotStart != "") {
    xLimLower <- try(as.POSIXct(options$historyPlotStart))
    if (isTryError(xLimLower))
      stop("'From date' must be in a date-like format (e.g., yyyy-mm-dd)")
    else
      xLimits[1] <- xLimLower
  }

  if (options$historyPlotEnd != "") {
    xLimUpper <- try(as.POSIXct(options$historyPlotEnd))
    if (isTryError(xLimUpper))
      stop("'To date' must be in a date-like format (e.g., yyyy-mm-dd)")
    else
      xLimits[2] <- xLimUpper
  }

  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(histDat$y)
  
  p <- ggplot2::ggplot(data = histDat, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(size = 3, color = "grey")

  if (options$historyPlotAddLine)
    p <- p + ggplot2::geom_line(color = "black", size = 1.25)

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
  
  prophetPredictionResults <- jaspResults[["prophetResults"]][["prophetPredictionResults"]]$object
  prophetModelResults      <- jaspResults[["prophetResults"]][["prophetModelResults"]]$object
  
  prophetForecastPlots <- createJaspContainer(title = gettext("Forecast Plots"))
  prophetForecastPlots$dependOn(.prophetPredictionDependencies())
  prophetForecastPlots$position <- 5
  
  if (options$forecastPlotsOverall) .prophetCreateOverallForecastPlot(prophetForecastPlots, dataset, options, prophetPredictionResults)
  if (options$forecastPlotsTrend)   .prophetCreateTrendForecastPlot(prophetForecastPlots, dataset, options, prophetPredictionResults)
  
  jaspResults[["prophetMainContainer"]][["prophetForecastPlots"]] <- prophetForecastPlots
  
  return()
}

.prophetCreateOverallForecastPlot <- function(prophetForecastPlots, dataset, options, prophetPredictionResults) {
  if (!is.null(prophetForecastPlots[["prophetOverallForecastPlot"]])) return()
  
  prophetOverallForecastPlot <- createJaspPlot(title = gettext("Overall Forecast Plot"), height = 320, width = 480)
  prophetOverallForecastPlot$dependOn(c("forecastPlotsOverall", "forecastPlotsOverallAddData",
                                        "forecastPlotsOverallAddCapacity", "forecastPlotsOverallAddMinimum",
                                       "forecastPlotsOverallAddCovariates",
                                       "forecastPlotsOverallStart", "forecastPlotsOverallEnd"))
  
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
  
  prophetTrendForecastPlot <- createJaspPlot(title = gettext("Trend Forecast Plot"), height = 320, width = 480)
  prophetTrendForecastPlot$dependOn(c("forecastPlotsTrend"))
  
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
  
  prophetModelResults <- jaspResults[["prophetResults"]][["prophetModelResults"]]$object
  
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

  seasData <- .prophetPredictSeasonality(encodeColNames(name), prophetModelResults, options)

  title <- gettext(paste0(name, " Seasonality Plot"))
  seasonalityPlot <- createJaspPlot(title = title, height = 320, width = 480)
  seasonalityPlot$plotObject <- .prophetSeasonalityPlotFill(seasData, encodeColNames(name), options)
  prophetSeasonalityPlots[[name]] <- seasonalityPlot

  return()
}

.prophetPredictSeasonality <- function(name, prophetModelResults, options) {
  start   <- as.POSIXct("2018-01-01 00:00:00", tz = "UTC")
  period  <- .prophetGetSeasonalityProperties(name, "period", options)
  unit    <- .prophetGetSeasonalityProperties(name, "unit", options)

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

  if (daysPeriod <= 3/1440)                          dateFormat <- list(format = "%M:%OS")
  else if (daysPeriod <= 1/8)                        dateFormat <- list(format = "%H:%M")
  else if (daysPeriod <= 3)                          dateFormat <- list(format = "%H")
  else if (daysPeriod > 3 && daysPeriod <= 14)       dateFormat <- list(format = "%a")
  else if (daysPeriod > 14 && daysPeriod <= 28)      dateFormat <- list(format = "%d")
  else if (daysPeriod > 28 && daysPeriod < 365)      dateFormat <- list(format = "%m")
  else if (daysPeriod >= 365 && daysPeriod <= 3*365) dateFormat <- list(format = "%b")
  else                                               dateFormat <- list(format = "%Y")

  return(dateFormat)
}

.prophetSeasonalityPlotFill <- function(seasData, name, options) {

  y    <- seasData[[name]]
  ymin <- seasData[[paste0(name, "_lower")]]
  ymax <- seasData[[paste0(name, "_upper")]]
  x    <- seasData$ds
  df   <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)

  xBreaks <- pretty(x)
  yBreaks <- pretty(c(min(ymin), max(ymax)))
  mode    <- .prophetGetSeasonalityProperties(name, "mode", options)
  period  <- .prophetGetSeasonalityProperties(name, "period", options)
  unit    <- .prophetGetSeasonalityProperties(name, "unit", options)
  xFormat <- .prophetGetPrettyDateLabels(period, unit)

  p <- ggplot2::ggplot(df, mapping = ggplot2::aes(x = x, y = y)) +
    
    ggplot2::geom_line(color = "black", size = 1.25) +
  
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.4) +

    ggplot2::scale_x_datetime(name = options$time,
                              breaks = xBreaks,
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
    stop("Please enter valid 'Start date' and 'End date' arguments for nonperiodical prediction (e.g., yyyy-mm-dd)")

  y     <- prophetPredictionResults[[type]]
  ymin  <- prophetPredictionResults[[paste0(type, "_lower")]]
  ymax  <- prophetPredictionResults[[paste0(type, "_upper")]]
  df    <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)
  
  if (options$growth == "logistic") {
    df$cap   <- prophetPredictionResults[["cap"]]
    df$floor <- prophetPredictionResults[["floor"]]
  }

  if (length(options$covariates) > 0) {
    covs <- unlist(options$covariates)
    for (cov in covs) {
      covHist <- dataset[[encodeColNames(cov)]]
      df[[cov]] <- covHist[xHist %in% x]
    }
  }
  
  p <- ggplot2::ggplot()
  
  if (options$forecastPlotsOverallAddData && type == "yhat")
    p <- p + ggplot2::geom_point(data = histDat, mapping = ggplot2::aes(x = x, y = y), size = 3, color = "grey")

  p <- p + 
    ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = y), color = "black", size = 1.25) +
  
    ggplot2::geom_ribbon(data = df, mapping = ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.4)
  
  if (options$growth == "logistic" && type == "yhat") {
    if (options$forecastPlotsOverallAddCapacity)
      p <- p + ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = cap), color = "black", size = 0.75)

    if (options$forecastPlotsOverallAddMinimum)
      p <- p + ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = x, y = floor), color = "black", size = 0.75)
  }

  if (length(options$covariates) > 0 && options$forecastPlotsOverallAddCovariates && type == "yhat") {
    for (i in 1:length(covs)) {
      p <- p + 
        ggplot2::geom_line(data = df, 
                           mapping = ggplot2::aes_string(x = "x", y = covs[i], color = as.factor(covs[i])),
                           size = 1.25, alpha = 0.75)
    }
  }
  
  xLimits <- range(df$x)

  if (options$forecastPlotsOverallStart != "" && type == "yhat") {
    xLimLower <- try(as.POSIXct(options$forecastPlotsOverallStart))
    if (isTryError(xLimLower))
      stop("'From date' must be in a date-like format (e.g., yyyy-mm-dd)")
    else
      xLimits[1] <- xLimLower
  }

  if (options$forecastPlotsOverallEnd != "" && type == "yhat") {
    xLimUpper <- try(as.POSIXct(options$forecastPlotsOverallEnd))
    if (isTryError(xLimUpper))
      stop("'To date' must be in a date-like format (e.g., yyyy-mm-dd)")
    else
      xLimits[2] <- xLimUpper
  }

  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(unlist(list(df[, -which(names(df) == "x")]), histDat[, -which(names(df) == "x")]))

  p <- p + ggplot2::geom_segment(mapping = ggplot2::aes(x = histDat$x[length(histDat$x)], y = range(yBreaks)[1], 
                                                        xend = histDat$x[length(histDat$x)]), yend = range(yBreaks)[2], 
                                 linetype = "dashed") +

    ggplot2::scale_x_datetime(name = options$time, breaks = xBreaks, labels = xLabels, limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = gettext(options$dependent), breaks = yBreaks, limits = range(yBreaks))
  
  p <- jaspGraphs::themeJasp(p)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 15, 0, 0))

  return(p)
}

.prophetCreatePerformancePlots <- function(jaspResults, options, ready) {
  if (!ready || !options$crossValidation) return()

  prophetEvaluationResults <- jaspResults[["prophetResults"]][["prophetEvaluationResults"]]$object
  
  prophetPerformancePlots <- createJaspContainer("Performance Plots")
  prophetPerformancePlots$dependOn(.prophetEvaluationDependencies())
  prophetPerformancePlots$position <- 7
  
  if(options$performancePlotsMse)  .prophetCreatePerformancePlotMse( prophetPerformancePlots, options, prophetEvaluationResults)
  if(options$performancePlotsRmse) .prophetCreatePerformancePlotRmse(prophetPerformancePlots, options, prophetEvaluationResults)
  if(options$performancePlotsMape) .prophetCreatePerformancePlotMape(prophetPerformancePlots, options, prophetEvaluationResults)
  
  jaspResults[["prophetMainContainer"]][["prophetEvaluationPlots"]] <- prophetPerformancePlots

  return()
}

.prophetCreatePerformancePlotMse <- function(prophetPerformancePlots, options, prophetEvaluationResults) {
  if (!is.null(prophetPerformancePlots[["prophetPerformancePlotMse"]])) return()
  
  prophetPerformancePlotMse <- createJaspPlot(title = "Performance Plot MSE", height = 320, width = 480)
  prophetPerformancePlotMse$dependOn(c("performancePlotsMse"))
  
  prophetPerformancePlotMse$plotObject <- .prophetPerformancePlotFill(prophetEvaluationResults, options, type = "mse")
  
  prophetPerformancePlots[["prophetPerformancePlotMse"]] <- prophetPerformancePlotMse
  
  return()
}

.prophetCreatePerformancePlotRmse <- function(prophetPerformancePlots, options, prophetEvaluationResults) {
  if (!is.null(prophetPerformancePlots[["prophetPerformancePlotRmse"]])) return()
  
  prophetPerformancePlotRmse <- createJaspPlot(title = "Performance Plot RMSE", height = 320, width = 480)
  prophetPerformancePlotRmse$dependOn(c("performancePlotsRmse"))
  
  prophetPerformancePlotRmse$plotObject <- .prophetPerformancePlotFill(prophetEvaluationResults, options, type = "rmse")
  
  prophetPerformancePlots[["prophetPerformancePlotRmse"]] <- prophetPerformancePlotRmse
  
  return()
}

.prophetCreatePerformancePlotMape <- function(prophetPerformancePlots, options, prophetEvaluationResults) {
  if (!is.null(prophetPerformancePlots[["prophetPerformancePlotMape"]])) return()
  
  prophetPerformancePlotMape <- createJaspPlot(title = "Performance Plot Mape", height = 320, width = 480)
  prophetPerformancePlotMape$dependOn(c("performancePlotsMape"))
  
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

    ggplot2::scale_y_continuous(name = gettext(stringr::str_to_upper(type)), breaks = yBreaks, limits = range(yBreaks))
  
  p <- jaspGraphs::themeJasp(p)
  
  return(p)
}

.prophetCreateParameterPlots <- function(jaspResults, options, ready) {
  
  prophetModelResults <- jaspResults[["prophetResults"]][["prophetModelResults"]]$object
  
  prophetParameterPlots <- createJaspContainer("Parameter Plots")
  prophetParameterPlots$position <- 8
  
  if(options$parameterPlotsDelta) 
    .prophetCreateParameterPlotDelta(prophetParameterPlots, options, prophetModelResults)

  if(options$parameterPlotsMarginalDistributions) 
    .prophetCreateParameterPlotMarginal(prophetParameterPlots, options, prophetModelResults)
  
  jaspResults[["prophetMainContainer"]][["prophetParameterPlots"]] <- prophetParameterPlots
}

.prophetCreateParameterPlotDelta <- function(prophetParameterPlots, options, prophetModelResults) {
  if (!is.null(prophetParameterPlots[["prophetParameterPlotDelta"]])) return()
  
  prophetParameterPlotDelta <- createJaspPlot(title = gettext("Changepoint Plot"), height = 320, width = 480)
  prophetParameterPlotDelta$dependOn(c("parameterPlotsDelta"))
  
  prophetParameterPlotDelta$plotObject <- .prophetParameterPlotDeltaFill(prophetModelResults, options)
  
  prophetParameterPlots[["prophetParameterPlotDelta"]] <- prophetParameterPlotDelta
  
  return()
}

.prophetParameterPlotDeltaFill <- function(prophetModelResults, options) {
  deltas  <- switch(options$estimation,
                    map  = as.numeric(prophetModelResults$params$delta),
                    mcmc = as.numeric(apply(prophetModelResults$params$delta, 2, mean)))
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
  parTitles <- c("Growth rate", "Offset", "Residual variance")
  
  marginalPlotList <- list()
  
  for (i in 1:length(parNames)) {
    marginalPlotList[[i]] <- createJaspPlot(title = gettext(parTitles[i]), height = 320, width = 480)
    
    marginalPlotList[[i]]$plotObject <- .prophetParameterPlotMarginalFill(prophetModelResults, 
                                                                         options, 
                                                                         parNames[i], 
                                                                         parTitles[i])
    
    prophetParameterPlotMarginal[[parNames[i]]] <- marginalPlotList[[i]]
  }
  
  prophetParameterPlotMarginal$dependOn(c("parameterPlotsMarginalDistributions"))
  
  prophetParameterPlots[["prophetParameterPlotMarginal"]] <- prophetParameterPlotMarginal
  
  return()
}

.prophetParameterPlotMarginalFill <- function(prophetModelResults, options, parName, parTitle, criLevel = 0.95) {
  samples <- prophetModelResults$params[[parName]]
  dens    <- density(samples)
  x       <- dens$x
  y       <- dens$y
  
  lvl     <- (1-criLevel)/2
  cri     <- stats::quantile(samples, prob = c(lvl, 1-lvl))
  
  xBreaks    <- pretty(range(x))
  yBreaks    <- pretty(c(0, 1.15*max(y)))
  yBarPos    <- 0.9*yBreaks[length(yBreaks)]
  yBarHeight <- 0.05*yBreaks[length(yBreaks)]
  
  p <- ggplot2::ggplot() +
  
    ggplot2::geom_line(data = data.frame(x = x, y = y),
                       mapping = ggplot2::aes(x = x, y = y),
                       size = 1.25) +
  
    ggplot2::geom_errorbarh(data = data.frame(xmin = cri[1], xmax = cri[2], y = yBarPos),
                            mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y),
                            height = yBarHeight) +
  
    ggplot2::scale_x_continuous(name = gettext(parTitle), breaks = xBreaks, limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks))
  
  p <- jaspGraphs::themeJasp(p)
  
  return(p)
}

.prophetGetSeasonalityProperties <- function(name, prop, options) {
  if(is.null(options$seasonalities))
    return()

  propRes <- options$seasonalities[[which(sapply(options$seasonalities, function(s) encodeColNames(s$name) == name))]][[prop]]
  return(propRes)
}
