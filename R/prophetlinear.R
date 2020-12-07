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
ProphetLinear <- function(jaspResults, dataset = NULL, options) {

  options <- .prolinInitOptions(jaspResults, options)
  
  ready <- (options$dependent != "" && options$time != "")
  
  dataset <- .prolinReadData(options, ready)
  errors  <- .prolinErrorHandling(dataset, options, ready)
  
  .prolinComputeResults(jaspResults, dataset, options, ready)

  .prolinContainerMain(                 jaspResults, options, ready)
  .prolinCreateModelSummaryTable(       jaspResults, options, ready)
  .prolinCreateChangePointTable(        jaspResults, options, ready)
  .prolinCreateModelEvaluationTable(    jaspResults, options, ready)
  .prolinCreateHistoryPlot(             jaspResults, dataset, options, ready)
  .prolinCreateForecastPlots(           jaspResults, dataset, options, ready)
  .prolinCreateSeasonalityPlotContainer(jaspResults, dataset, options, ready)
  .prolinCreatePerformancePlots(        jaspResults, options, ready)
  .prolinCreateParameterPlots(          jaspResults, options, ready)
  
  return()
}

# Init functions ----
.prolinInitOptions <- function(jaspResults, options) {
  
  return(options)
}

.prolinReadData <- function(options, ready) {
  if(!ready) return()
  
  # Read in the dataset using the built-in functions
  numericVars <- options$dependent
  numericVars <- numericVars[numericVars != ""]
  timeVars    <- options$time
  timeVars    <- timeVars[timeVars != ""]
  factorVars  <- options$changepoints
  factorVars  <- factorVars[factorVars != ""]
  covVars     <- unlist(options$covariates)
  covVars     <- covVars[covVars != ""]
  dataset     <- .readDataSetToEnd(columns.as.numeric  = c(numericVars, covVars),
                                   columns.as.factor   = c(factorVars, timeVars),
                                   exclude.na.listwise = c(timeVars, covVars))

  return(dataset)
}

.prolinErrorHandling <- function(dataset, options, ready) {
  if(!ready) return()
  
  checks <- list(
    .timeChecks <- function() {
      timeVar <- try(as.Date(dataset[[encodeColNames(options$time)]]))
      
      if(class(timeVar) == "try-error")
        return(gettext("Error in converting variable 'Time' into dates: 'Time' needs to be in a date-like format (e.g., yyyy-mm-dd)"))
      
      return()
    },
    
    .changepointChecks <- function() {
      if(options$changepoints != "") {
        changepointVar <- try(as.logical(na.omit(dataset[[encodeColNames(options$changepoints)]])))
        
        if(class(changepointVar) == "try-error" || length(unique(na.omit(dataset[[encodeColNames(options$changepoints)]]))) != 2)
          return(gettext("Error in setting changepoints: Variable 'Changepoints' must have two levels"))
      }
      
      return()
    },
    
    .nonperiodicalPredChecks <- function() {
      if (options$predictionType == "nonperiodicalPrediction") {
        predStart <- try(as.POSIXct(options$nonperiodicalPredictionStart))
        predEnd <- try(as.POSIXct(options$nonperiodicalPredictionEnd))
        
        if(class(predStart) == "try-error")
          return(gettext("Error in converting argument 'Start date' into a date: 'Start date' needs to be in a date-like format (e.g., yyyy-mm-dd)"))
        
        if(class(predEnd) == "try-error")
          return(gettext("Error in converting argument 'End date' into a date: 'End date' needs to be in a date-like format (e.g., yyyy-mm-dd)"))
      }
      
      return()
    },
    
    .covariateCheks <- function() {
      if (length(options$covariates) > 0) {
        covs   <- unlist(options$covariates)
        y      <- dataset[[encodeColNames(options$dependent)]]
        yNa    <- is.na(y)
        ds     <- as.Date(dataset[[encodeColNames(options$time)]])
        dsHist <- ds[!yNa]

        if (options$predictionType == "nonperiodicalPrediction") {
          predStart <- as.Date(options$nonperiodicalPredictionStart)
          predEnd   <- as.Date(options$nonperiodicalPredictionEnd)
        } else {
          predUnit  <- switch(options$periodicalPredictionUnit,
                              days = 1,
                              weeks = 7,
                              years = 365)
          predInt   <- options$periodicalPredictionNumber * predUnit
          predStart <- dsHist[1]
          predEnd   <- dsHist[length(dsHist)] + predInt
        }
        
        predSeq <- seq(predStart, predEnd, by = "d")
        if (!all(predSeq %in% ds))
          return(gettext("Error in predicting future values with covariates: Covariates need to be supplied for predicted time intervals"))
      }
      
      return()
    }
  )
  
  .hasErrors(dataset, "run", type = c('observations', 'variance', 'infinity'),
             all.target = c(options$dependent, unlist(options$covariates)),
             custom = checks,
             observations.amount = '< 2',
             exitAnalysisIfErrors = TRUE)
}

# Results functions ----
.prolinComputeResults <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  
  if (is.null(jaspResults[["prolinResults"]])) {
    prolinResults <- createJaspContainer("prolinResults")
    
    jaspResults[["prolinResults"]] <- prolinResults
    
    if (options$changepoints != "") {
      jaspResults[["prolinResults"]]$dependOn(c("dependent", "time", "changepoints", "covariates", 
                                                "assignedCovariates", "seasonalities",
                                                "estimation", "mcmcSamples", "predictionIntervalWidth", 
                                                "predictionIntervalSamples"))
    } else {
      jaspResults[["prolinResults"]]$dependOn(c("dependent", "time", "changepoints", "covariates", 
                                                "maxChangepoints", "changepointRange", "changepointPriorScale", 
                                                "assignedCovariates", "seasonalities",
                                                "estimation", "mcmcSamples", "predictionIntervalWidth", 
                                                "predictionIntervalSamples"))
    }
  }
  
  if (is.null(jaspResults[["prolinResults"]][["prolinModelResults"]])) {
    prolinModelResultsState <- createJaspState()
    prolinModelResults <- .prolinModelResultsHelper(dataset, options)
    prolinModelResultsState$object <- prolinModelResults
    jaspResults[["prolinResults"]][["prolinModelResults"]] <- prolinModelResultsState
  }
  
  if (is.null(jaspResults[["prolinResults"]][["prolinPredictionResults"]])) {
    prolinPredictionResultsState <- createJaspState()
    prolinPredictionResultsState$dependOn(c("predictionType", 
                                            "periodicalPredictionNumber", 
                                            "periodicalPredictionUnit",
                                            "nonperiodicalPredictionStart", 
                                            "nonperiodicalPredictionEnd",
                                            "nonperiodicalPredictionUnit"))
    prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
    prolinPredictionResults <- .prolinPredictionResultsHelper(dataset, options, prolinModelResults)
    prolinPredictionResultsState$object <- prolinPredictionResults
    jaspResults[["prolinResults"]][["prolinPredictionResults"]] <- prolinPredictionResultsState

    if (options$predictionSavePath != "") .prolinSavePredictions(jaspResults, options)
  }
  
  if (is.null(jaspResults[["prolinResults"]][["prolinEvaluationResults"]]) && options$crossValidation) {
    prolinEvaluationResultsState <- createJaspState()
    prolinEvaluationResultsState$dependOn(c("crossValidation", "crossValidationUnit", "crossValidationHorizon", 
                                            "crossValidationPeriod", "crossValidationInitial"))
    prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
    prolinEvaluationResults <- .prolinEvaluationResultsHelper(dataset, options, prolinModelResults)
    prolinEvaluationResultsState$object <- prolinEvaluationResults
    jaspResults[["prolinResults"]][["prolinEvaluationResults"]] <- prolinEvaluationResultsState
  }
  
  return()
}

.prolinModelResultsHelper <- function(dataset, options) {
  y  <- dataset[[encodeColNames(options$dependent)]]
  ds <- as.POSIXct(dataset[[encodeColNames(options$time)]])
  
  fitDat <- na.omit(data.frame(y = y, ds = ds))
  
  if (options$changepoints != "") {
    isChangepoint <- as.logical(na.omit(dataset[[encodeColNames(options$changepoints)]]))
    cp            <- fitDat$ds[isChangepoint]
  } else {
    cp <- NULL
  }
  
  mcmcSamples <- switch(options$estimation,
                        map  = 0,
                        mcmc = options$mcmcSamples)

  predIntSamples <- switch(options$estimation,
                           map  = options$predictionIntervalSamples,
                           mcmc = 1)

  mod <- prophet::prophet(growth = "linear", changepoints = cp, n.changepoints = options$maxChangepoints, changepoint.range = options$changepointRange, changepoint.prior.scale = options$changepointPriorScale,
                            yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE,
                            mcmc.samples = mcmcSamples, interval.width = options$predictionIntervalWidth, uncertainty.samples = predIntSamples, fit = FALSE)
  
  if (length(options$covariates) > 0) {
    covs <- unlist(options$covariates)
    covFit <- !is.na(y)
    
    for (cov in covs) {
      covAssigned <- sapply(options$assignedCovariates, function(c) encodeColNames(c$variable) == cov)
      if (any(covAssigned)) {
        covArgs <- options$assignedCovariates[[which(covAssigned)]]
        mod <- prophet::add_regressor(m = mod, name = cov, prior.scale = covArgs$priorSigma,
                                      standardize = covArgs$standardize,
                                      mode = covArgs$mode)
      } else {
        mod <- prophet::add_regressor(m = mod, name = cov, prior.scale = 10,
                                      standardize = "auto",
                                      mode = "additive")
      }
      datCov <- dataset[[encodeColNames(cov)]]
      fitDat[[cov]] <- datCov[covFit]
    }
  }
  
  if (!is.null(options$seasonalities)) {
    i <- 1
    for (seas in options$seasonalities) {
      name   <- ifelse(seas$name == "", paste0("seasonality_", i), encodeColNames(seas$name))
      period <- seas$period * switch(seas$unit,
                                     hours = 1/24,
                                     days  = 1,
                                     weeks = 7,
                                     years = 365)

      if (period < 7)                       fourierOrder <- 1
      else if (period >= 7 && period < 180) fourierOrder <- 3
      else                                  fourierOrder <- 10

      mod <- prophet::add_seasonality(m = mod, name = name, period = period, fourier.order = fourierOrder, prior.scale = seas$priorSigma, mode = seas$mode)
      i   <- i+1
    }
  }
  
  fit <- prophet::fit.prophet(m = mod, df = fitDat)
  
  return(fit)
}

.prolinPredictionResultsHelper <- function(dataset, options, prolinModelResults) { 
  futDat <- NULL
  
  if (options$predictionType == "periodicalPrediction") {
    futDat <- prophet::make_future_dataframe(m = prolinModelResults, 
                                             periods = options$periodicalPredictionNumber, 
                                             freq = options$periodicalPredictionUnit, 
                                             include_history = TRUE)
  } else {
    futDat <- data.frame(ds = seq(as.POSIXct(options$nonperiodicalPredictionStart), 
                                  as.POSIXct(options$nonperiodicalPredictionEnd), 
                                  by = options$nonperiodicalPredictionUnit))
  }
  
  if (length(options$covariates) > 0) {
    covs      <- unlist(options$covariates)
    futds     <- as.POSIXct(futDat$ds)
    ds        <- as.POSIXct(dataset[[encodeColNames(options$time)]])
    
    for (cov in covs) {
      futCov <- dataset[[encodeColNames(cov)]]
      futSel <- ds %in% futds
      futDat[[cov]] <- futCov[futSel]
    }
  }
  
  pred <- predict(prolinModelResults, futDat)
  
  return(pred)
}

.prolinEvaluationGenerateCutoffs <- function (ds, horizon, initial, period) {
  # Adapted copy of generate_cutoffs

  cutoff <- max(ds) - horizon
  tzone <- attr(cutoff, "tzone")
  result <- c(cutoff)
  while (result[length(result)] >= min(ds) + initial) {
    cutoff <- cutoff - period
    #if (!any((ds > cutoff) & (ds <= cutoff + horizon))) {
    #  if (cutoff > min(ds)) {
    #    closest.date <- max(ds[ds <= cutoff])
    #    cutoff <- closest.date - horizon
    #  }
    #}
    result <- c(result, cutoff)
  }
  result <- utils::head(result, -1)
  if (length(result) == 0) {
    stop(paste("Less data than horizon after initial window.", 
      "Make horizon or initial shorter."))
  }
  attr(result, "tzone") <- tzone
  return(rev(result))
}

.prolinEvaluationResultsHelper <- function(dataset, options, prolinModelResults) {
  
  cvDat <- prophet::cross_validation(model   = prolinModelResults, 
                                     horizon = options$crossValidationHorizon, 
                                     units   = options$crossValidationUnit,
                                     period  = options$crossValidationPeriod,
                                     initial = options$crossValidationInitial)

  # Inspired by original cross_validation function

  #unit      <- options$crossValidationUnit
  #dtHorizon <- as.difftime(options$crossValidationHorizon, units = unit)
  #dtPeriod  <- as.difftime(options$crossValidationPeriod,  units = unit)
  #dtInitial <- as.difftime(options$crossValidationInitial, units = unit)
  #ds        <- as.POSIXct(na.omit(dataset)[[encodeColNames(options$time)]])
  #y         <- na.omit(dataset)[[encodeColNames(options$dependent)]]
  #cutoffs   <- .prolinEvaluationGenerateCutoffs(ds, dtHorizon, dtInitial, dtPeriod)
  #cvDat     <- data.frame()

  #for (i in 1:length(cutoffs)) {
  #  
  #  dfCut  <- data.frame(ds = ds[ds <= cutoffs[i]], y = y[ds <= cutoffs[i]])
  #  newMod <- prophet:::prophet_copy(prolinModelResults, cutoff = cutoffs[i])
  #  modFit <- prophet::fit.prophet(newMod, dfCut)
  #  start  <- ds[which(ds == cutoffs[i])+1]
  #  dfPred <- data.frame(ds = seq.POSIXt(start, cutoffs[i]+dtHorizon, by = unit))
  #  
  #  modPred        <- predict(modFit, dfPred)
  #  modPred$cutoff <- cutoffs[i]
  #  modPred$y      <- y[ds %in% dfPred$ds]
  #  cvDat          <- rbind(cvDat, modPred)
  #}

  return(cvDat)
}

# Saving functions ----
.prolinSavePredictions <- function(jaspResults, options) {
  if (is.null(jaspResults[["prolinResults"]][["prolinPredictionSavePath"]])) {
    predSavePath <- createJaspState()
    predSavePath$dependOn(c("predictionType", 
                            "periodicalPredictionNumber", 
                            "periodicalPredictionUnit",
                            "nonperiodicalPredictionStart", 
                            "nonperiodicalPredictionEnd",
                            "nonperiodicalPredictionUnit",
                            "predictionSavePath"))
    jaspResults[["prolinResults"]][["prolinPredictionSavePath"]] <- predSavePath
  }

  write.csv(jaspResults[["prolinResults"]][["prolinPredictionResults"]]$object,
            file = options$predictionSavePath,
            row.names = FALSE)
  predSavePath$object <- TRUE
}

# Output functions ----
.prolinContainerMain <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]])) return()
  
  mainContainer <- createJaspContainer()
  
  jaspResults[["prolinMainContainer"]] <- mainContainer
  jaspResults[["prolinMainContainer"]]$dependOn(c("dependent", "time", "changepoints", "covariates",
                                                  "maxChangepoints", "changepointRange", "changepointPriorScale", 
                                                  "seasonalities",
                                                  "estimation", "mcmcSamples", "predictionIntervalWidth"))
  
  return()
}

.prolinCreateModelSummaryTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]][["prolinModelSummaryTable"]])) return()
  
  prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  if (options$estimation == "map") {
    prolinTable <- createJaspTable(title = "Parameter Estimates Table")
    prolinTable$position <- 1
    
    prolinTable$addColumnInfo(name = "k", title = gettext("Growth rate (k)"), type = "number")
    prolinTable$addColumnInfo(name = "m", title = gettext("Offset (m)"), type = "number")
    prolinTable$addColumnInfo(name = "sigmaObs", title = gettext("Residual variance (sigma)"), type = "number")
    
    .prolinModelSummaryTableMapFill(prolinTable, prolinModelResults, ready)
    
    jaspResults[["prolinMainContainer"]][["prolinTable"]] <- prolinTable
    
  } else {
    prolinTable <- createJaspTable(title = "Posterior Summary Table")
    prolinTable$position <- 2

    overtitle <- gettext("95% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
    prolinTable$addColumnInfo(name = "par", title = gettext("Parameter"), type = "string")
    prolinTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    prolinTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
    prolinTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = overtitle)
    prolinTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = overtitle)
    prolinTable$addColumnInfo(name = "rhat", title = gettext("R-hat"), type = "number")
    prolinTable$addColumnInfo(name = "bulkEss", title = gettext("ESS (bulk)"), type = "integer")
    prolinTable$addColumnInfo(name = "tailEss", title = gettext("ESS (tail)"), type = "integer")
    
    .prolinModelSummaryTableMcmcFill(prolinTable, prolinModelResults, ready)
    
    jaspResults[["prolinMainContainer"]][["prolinTable"]] <- prolinTable
  }
  
  return()
}

.prolinModelSummaryTableMapFill <- function(prolinTable, prolinModelResults, ready) {
  if (!ready) return()
  
  pars <- prolinModelResults$params
  
  prolinTable$addRows(list(
    k        = pars$k,
    m        = pars$m,
    sigmaObs = pars$sigma_obs
  ))
}

.prolinModelSummaryTableMcmcFill <- function(prolinTable, prolinModelResults, ready) {
  if (!ready) return()
  
  modelSummary         <- rstan::monitor(prolinModelResults$stan.fit, probs = c(0.025, 0.975))
  modelSummaryRowNames <- rownames(modelSummary)
  parNames             <- c("k", "m", "sigma_obs")
  parSummary           <- modelSummary[modelSummaryRowNames %in% parNames, 1:ncol(modelSummary)]
  parLabels            <- c("Growth rate (k)", "Offset (m)", "Residual variance (sigma)")
  
  prolinTable$addColumns(list(
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

.prolinCreateChangePointTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]][["prolinChangePointTable"]]) || !options$changePointTable) return()

  prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  title <- switch(options$estimation,
                  map  = "Changepoint Estimates Table",
                  mcmc = "Changepoint Posterior Summary Table")
  prolinTable <- createJaspTable(title = title)
  prolinTable$dependOn(c("changePointTable"))
  prolinTable$position <- 3

  prolinTable$addColumnInfo(name = "ds", title = gettext("Changepoint"), type = "string")
  
  if (options$estimation == "map") {
    prolinTable$addColumnInfo(name = "delta", title = gettext("Change in growth rate (delta)"), type = "number")
  } else {
    parTitle <- gettext("Change in growth rate (delta)")
    ciTitle <- gettext("95% Credible Interval")
    prolinTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number", overtitle = parTitle)
    prolinTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number", overtitle = parTitle)
    prolinTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = ciTitle)
    prolinTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = ciTitle)
  }
  
  .prolinChangePointTableFill(prolinTable, prolinModelResults, options$estimation, ready)
    
  jaspResults[["prolinMainContainer"]][["prolinChangePointTable"]] <- prolinTable

  return()
}

.prolinChangePointTableFill <-  function(prolinTable, prolinModelResults, estimation, ready) {
  if (!ready) return()

  delta  <- switch(estimation,
                   map  = as.numeric(prolinModelResults$params$delta),
                   mcmc = as.numeric(apply(prolinModelResults$params$delta, 2, mean)))
  cps     <- as.character(prolinModelResults$changepoints)

  if (estimation == "map") {
    prolinTable$addColumns(list(
      ds = cps,
      delta = delta
    ))
  } else {
    deltaCri <- apply(prolinModelResults$params$delta, 2, quantile, probs = c(0.025, 0.975))
    deltaSd  <- apply(prolinModelResults$params$delta, 2, sd)
    prolinTable$addColumns(list(
      ds = cps,
      mean = delta,
      sd = deltaSd,
      lowerCri = deltaCri[1,],
      upperCri = deltaCri[2,]
    ))
  }

  return()
}

.prolinCreateModelEvaluationTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]][["prolinModelEvaluationTable"]]) || !(options$performanceMetrics && options$crossValidation)) return()
  
  prolinEvaluationResults <- jaspResults[["prolinResults"]][["prolinEvaluationResults"]]$object
  
  prolinTable <- createJaspTable("Simulated Historical Forecasts Table")
  prolinTable$dependOn(c("crossValidation", "crossValidationUnit", "crossValidationHorizon", 
                         "crossValidationPeriod", "crossValidationInitial", "performanceMetrics", 
                         "performanceMetricsMse", "performanceMetricsRmse", "performanceMetricsMape"))
  prolinTable$position <- 4
  
  prolinTable$addColumnInfo(name = "horizon", title = gettext("Horizon"), type = "string")
  
  if (options$performanceMetricsMse)  prolinTable$addColumnInfo(name = "mse", title = gettext("MSE"), type = "number")
  if (options$performanceMetricsRmse) prolinTable$addColumnInfo(name = "rmse", title = gettext("RMSE"), type = "number")
  if (options$performanceMetricsMape) prolinTable$addColumnInfo(name = "mape", title = gettext("MAPE"), type = "number", format = "pc")
  
  .prolinModelEvaluationTableFill(prolinTable, options, prolinEvaluationResults, ready)
  
  jaspResults[["prolinMainContainer"]][["prolinModelEvaluationTable"]] <- prolinTable

  return()
}

.prolinModelEvaluationTableFill <- function(prolinTable, options, prolinEvaluationResults, ready) {
  if (!ready) return()
  
  metrics <- c("mse", "rmse", "mape")
  metrics <- metrics[c(options$performanceMetricsMse, options$performanceMetricsRmse, options$performanceMetricsMape)]
  
  metDat <- prophet::performance_metrics(df = prolinEvaluationResults, metrics = metrics, rolling_window = 0)
  metDat$horizon <- paste(metDat$horizon, options$crossValidationUnit, sep = " ")

  if (options$performanceMetricsMape) metDat$mape <- metDat$mape/100
  
  prolinTable$addColumns(as.list(metDat))

  return()
}

.prolinCreateHistoryPlot <- function(jaspResults, dataset, options, ready) {
  if (!ready || !options$historyPlot) return()
  
  prolinHistoryPlot <- createJaspPlot(title = "History Plot", height = 320, width = 480)
  prolinHistoryPlot$dependOn(c("dependent", "time", "changepoints", "covariates", "historyPlot", "historyPlotStart",
                                "historyPlotEnd"))
  prolinHistoryPlot$position <- 1
  
  prolinHistoryPlot$plotObject <- .prolinHistoryPlotFill(dataset, options)
  
  jaspResults[["historyPlot"]] <- prolinHistoryPlot
  
  return()
}

.prolinHistoryPlotFill <- function(dataset, options) {
  
  dataset <- na.omit(dataset)

  yHist  <- dataset[[encodeColNames(options$dependent)]]
  xHist <- as.POSIXct(dataset[[encodeColNames(options$time)]])
  histDat <- data.frame(y = yHist, x = xHist)
  
  xLimits <- c(min(xHist), max(xHist))
  
  if (options$historyPlotStart != "")
    xLimits[1] <- as.POSIXct(options$historyPlotStart)
  if (options$historyPlotEnd != "")
    xLimits[2] <- as.POSIXct(options$historyPlotEnd)
  
  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(yHist)
  
  p <- ggplot2::ggplot(histDat, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(data = histDat, mapping = ggplot2::aes(x = x, y = y), size = 3, color = "grey")
  
  p <- p + 
    ggplot2::scale_x_datetime(name = gettext("Time"), 
                              breaks = xBreaks, 
                              labels = gettext(xLabels),
                              limits = range(xBreaks)) + 
    
    ggplot2::scale_y_continuous(name = gettext(options$dependent),
                                limits = range(yBreaks),
                                breaks = yBreaks)
  
  p <- jaspGraphs::themeJasp(p)
  p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 10, 0, 0))

  return(p)
}

.prolinCreateForecastPlots <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  
  prolinPredictionResults <- jaspResults[["prolinResults"]][["prolinPredictionResults"]]$object
  prolinModelResults      <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  prolinForecastPlots <- createJaspContainer(title = gettext("Forecast Plots"))
  prolinForecastPlots$dependOn(c("predictionIntervalSamples", "predictionType", "periodicalPredictionNumber", 
                                 "periodicalPredictionUnit", "nonperiodicalPredictionStart", 
                                 "nonperiodicalPredictionEnd", "nonperiodicalPredictionUnit"))
  prolinForecastPlots$position <- 5
  
  if (options$forecastPlotsOverall) .prolinCreateOverallForecastPlot(prolinForecastPlots, dataset, options, prolinPredictionResults)
  if (options$forecastPlotsTrend)   .prolinCreateTrendForecastPlot(prolinForecastPlots, dataset, options, prolinPredictionResults)
  
  jaspResults[["prolinMainContainer"]][["prolinForecastPlots"]] <- prolinForecastPlots
  
  return()
}

.prolinCreateOverallForecastPlot <- function(prolinForecastPlots, dataset, options, prolinPredictionResults) {
  if (!is.null(prolinForecastPlots[["prolinOverallForecastPlot"]])) return()
  
  prolinOverallForecastPlot <- createJaspPlot(title = gettext("Overall Forecast Plot"), height = 480, width = 620)
  prolinOverallForecastPlot$dependOn(c("forecastPlotsOverall", "forecastPlotsOverallAddData",
                                     "forecastPlotsOverallAddCovariates",
                                     "forecastPlotsOverallStart", "forecastPlotsOverallEnd"))
  
  prolinOverallForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPredictionResults, dataset, options, type = "yhat")
  
  prolinForecastPlots[["prolinOverallForecastPlot"]] <- prolinOverallForecastPlot
  
  return()
}

.prolinCreateTrendForecastPlot <- function(prolinForecastPlots, dataset, options, prolinPredictionResults) {
  if (!is.null(prolinForecastPlots[["prolinTrendForecastPlot"]])) return()
  
  prolinTrendForecastPlot <- createJaspPlot(title = gettext("Trend Forecast Plot"), height = 480, width = 620)
  prolinTrendForecastPlot$dependOn(c("forecastPlotsTrend"))
  
  prolinTrendForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPredictionResults, dataset, options, type = "trend")
  
  prolinForecastPlots[["prolinTrendForecastPlot"]] <- prolinTrendForecastPlot
  
  return()
}

.prolinCreateSeasonalityPlotContainer <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  
  prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  prolinSeasonalityPlots <- createJaspContainer(title = gettext("Seasonality Plots"))
  prolinSeasonalityPlots$dependOn(c("seasonalityPlots"))
  prolinSeasonalityPlots$position <- 6

  if (length(options$seasonalityPlots) > 0) {
    for (name in options$seasonalityPlots) {
      .prolinCreateSeasonalityPlot(prolinSeasonalityPlots, name, prolinModelResults, options)
    }
  }
  
  jaspResults[["prolinMainContainer"]][["prolinSeasonalityPlots"]] <- prolinSeasonalityPlots
  
  return()
}

.prolinCreateSeasonalityPlot <- function(prolinSeasonalityPlots, name, prolinModelResults, options) {
  if (!is.null(prolinSeasonalityPlots[[name]])) return()

  seasData <- .prolinPredictSeasonality(encodeColNames(name), prolinModelResults, options)

  title <- gettext(paste0(name, " Seasonality Plot"))
  seasonalityPlot <- createJaspPlot(title = title, height = 320, width = 480)
  seasonalityPlot$plotObject <- .prolinSeasonalityPlotFill(seasData, encodeColNames(name), options)
  prolinSeasonalityPlots[[name]] <- seasonalityPlot

  return()
}

.prolinPredictSeasonality <- function(name, prolinModelResults, options) {
  start <- as.POSIXct("2018-01-01 01:00:00")
  period <- .prolinGetSeasonalityProperties(name, "period", options)
  unit <- .prolinGetSeasonalityProperties(name, "unit", options)

  ds <- seq(from = start, by = unit, length.out = period+1)
  futDat <- data.frame(ds = ds)

  if (length(options$covariates) > 0) {
    for (cov in options$covariates) {
      futDat[[cov]] <- mean(prolinModelResults$history[[cov]])
    }
  }
  
  predSeas <- predict(prolinModelResults, futDat)

  return(predSeas)
}

.prolinGetPrettyDateLabels <- function(period, unit) {
  unitDays <- switch (unit,
                      hours = 1/24,
                      days =  1,
                      weeks = 7,
                      years = 365)

  daysPeriod <- unitDays*period

  if (daysPeriod <= 3)                               dateFormat <- list(format = "%H", label = "Hour")
  else if (daysPeriod > 3 && daysPeriod <= 14)       dateFormat <- list(format = "%a", label = "Weekday")
  else if (daysPeriod > 14 && daysPeriod <= 28)      dateFormat <- list(format = "%d", label = "Day")
  else if (daysPeriod > 28 && daysPeriod < 365)      dateFormat <- list(format = "%m", label = "Month")
  else if (daysPeriod >= 365 && daysPeriod <= 3*365) dateFormat <- list(format = "%b", label = "Month")
  else                                               dateFormat <- list(format = "%Y", label = "Year")

  return(dateFormat)
}

.prolinSeasonalityPlotFill <- function(seasData, name, options) {

  y    <- seasData[[name]]
  ymin <- seasData[[paste0(name, "_lower")]]
  ymax <- seasData[[paste0(name, "_upper")]]
  x    <- seasData$ds

  df <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)

  xBreaks <- pretty(x)
  yBreaks <- pretty(c(min(ymin), max(ymax)))
  mode    <- .prolinGetSeasonalityProperties(name, "mode", options)
  period  <- .prolinGetSeasonalityProperties(name, "period", options)
  unit    <- .prolinGetSeasonalityProperties(name, "unit", options)
  xFormat <- .prolinGetPrettyDateLabels(period, unit)

  p <- ggplot2::ggplot(df, mapping = ggplot2::aes(x = x, y = y)) +
    
    ggplot2::geom_line(color = "black", size = 1.25) +
  
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.4) +

    ggplot2::scale_x_datetime(name = xFormat$label, date_labels = xFormat$format, breaks = xBreaks, limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = options$dependent,
                                breaks = yBreaks,
                                labels = ifelse(mode == "multiplicative", scales::percent, ggplot2::waiver),
                                limits = range(yBreaks))

  p <- jaspGraphs::themeJasp(p)

  return(p)
}

.prolinForecastPlotFill <- function(prolinPredictionResults, dataset, options, type) {
  
  yHist  <- dataset[[encodeColNames(options$dependent)]]
  xHist <- as.POSIXct(dataset[[encodeColNames(options$time)]])
  histDat <- na.omit(data.frame(y = yHist, x = xHist))
  
  x <- as.POSIXct(prolinPredictionResults[["ds"]])
  y <- prolinPredictionResults[[type]]
  ymin <- prolinPredictionResults[[paste0(type, "_lower")]]
  ymax <- prolinPredictionResults[[paste0(type, "_upper")]]
  df <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)
  
  if (length(options$covariates) > 0) {
    covs <- unlist(options$covariates)
    for (cov in covs) {
      covHist <- dataset[[encodeColNames(cov)]]
      df[[cov]] <- covHist[xHist %in% x]
    }
  }
  
  xLimits <- c(min(x), max(x))
  
  if (options$forecastPlotsOverallStart != "" && type == "yhat")
    xLimits[1] <- as.POSIXct(options$forecastPlotsOverallStart)
  if (options$forecastPlotsOverallEnd != "" && type == "yhat")
    xLimits[2] <- as.POSIXct(options$forecastPlotsOverallEnd)
  
  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- pretty(c(min(ymin), max(ymax)))
  
  p <- ggplot2::ggplot(df, mapping = ggplot2::aes(x = x, y = y))
  
  if (options$forecastPlotsOverallAddData && type == "yhat") {
    
    p <- p + ggplot2::geom_point(data = histDat, mapping = ggplot2::aes(x = x, y = y), size = 3, color = "grey")
    
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(c(min(ymin), min(histDat$y))), max(c(max(ymax), max(histDat$y)))))
  }

  p <- p + 
    ggplot2::geom_line(color = "black", size = 1.25) + 
  
    ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.4)
  
  if (length(options$covariates) > 0 && options$forecastPlotsOverallAddCovariates && type == "yhat") {
    covMin <- numeric(length(covs))
    covMax <- numeric(length(covs))
    
    for (i in 1:length(covs)) {
      p <- p + 
        ggplot2::geom_line(data = df, 
                           mapping = ggplot2::aes_string(x = "x", y = covs[i], 
                                                         color = as.factor(covs[i])), 
                           size = 1.25)
      
      if (options$forecastPlotsOverallAddCovariateLabels) {
        p <- p + ggrepel::geom_label_repel(data = df[nrow(df),], mapping = ggplot2::aes_string(x = "x", y = covs[i], 
                                                                                      label = as.factor(decodeColNames(covs[i]))),
                                           size = 5)
      }
      
      covMin[i] <- min(df[[covs[i]]])
      covMax[i] <- max(df[[covs[i]]])
    }
    
    if (options$forecastPlotsOverallAddCovariateLabels) {
      p <- p + ggrepel::geom_label_repel(data = df[nrow(df),], mapping = ggplot2::aes(x = x, y = y, 
                                                                                      label = options$dependent),
                                         size = 5)
    }
    
    yBreaks <- pretty(c(min(c(min(ymin), min(histDat$y), covMin)), max(c(max(ymax), max(histDat$y), covMax))))
  }
  
  p <- p + ggplot2::geom_segment(mapping = ggplot2::aes(x = histDat$x[length(histDat$x)], y = range(yBreaks)[1], 
                                                        xend = histDat$x[length(histDat$x)]), yend = range(yBreaks)[2], 
                                linetype = "dashed") +

    ggplot2::scale_x_datetime(name = gettext("Time"), breaks = xBreaks, labels = gettext(xLabels), limits = range(xBreaks)) +

    ggplot2::scale_y_continuous(name = gettext(options$dependent), breaks = yBreaks, limits = range(yBreaks))
  
  p <- jaspGraphs::themeJasp(p)
  
  return(p)
}

.prolinCreatePerformancePlots <- function(jaspResults, options, ready) {
  if (!ready || !options$crossValidation) return()

  prolinEvaluationResults <- jaspResults[["prolinResults"]][["prolinEvaluationResults"]]$object
  
  prolinPerformancePlots <- createJaspContainer("Performance Plots")
  prolinPerformancePlots$dependOn(c("crossValidation", "crossValidationUnit", "crossValidationHorizon", 
                                   "crossValidationPeriod", "crossValidationInitial"))
  prolinPerformancePlots$position <- 7
  
  if(options$performancePlotsMse)  .prolinCreatePerformancePlotMse( prolinPerformancePlots, options, prolinEvaluationResults)
  if(options$performancePlotsRmse) .prolinCreatePerformancePlotRmse(prolinPerformancePlots, options, prolinEvaluationResults)
  if(options$performancePlotsMape) .prolinCreatePerformancePlotMape(prolinPerformancePlots, options, prolinEvaluationResults)
  
  jaspResults[["prolinMainContainer"]][["prolinEvaluationPlots"]] <- prolinPerformancePlots

  return()
}

.prolinCreatePerformancePlotMse <- function(prolinPerformancePlots, options, prolinEvaluationResults) {
  if (!is.null(prolinPerformancePlots[["prolinPerformancePlotMse"]])) return()
  
  prolinPerformancePlotMse <- createJaspPlot(title = "Performance Plot MSE", height = 320, width = 480)
  prolinPerformancePlotMse$dependOn(c("performancePlotsMse"))
  
  prolinPerformancePlotMse$plotObject <- .prolinPerformancePlotFill(prolinEvaluationResults, options, type = "mse")
  
  prolinPerformancePlots[["prolinPerformancePlotMse"]] <- prolinPerformancePlotMse
  
  return()
}

.prolinCreatePerformancePlotRmse <- function(prolinPerformancePlots, options, prolinEvaluationResults) {
  if (!is.null(prolinPerformancePlots[["prolinPerformancePlotRmse"]])) return()
  
  prolinPerformancePlotRmse <- createJaspPlot(title = "Performance Plot RMSE", height = 320, width = 480)
  prolinPerformancePlotRmse$dependOn(c("performancePlotsRmse"))
  
  prolinPerformancePlotRmse$plotObject <- .prolinPerformancePlotFill(prolinEvaluationResults, options, type = "rmse")
  
  prolinPerformancePlots[["prolinPerformancePlotRmse"]] <- prolinPerformancePlotRmse
  
  return()
}

.prolinCreatePerformancePlotMape <- function(prolinPerformancePlots, options, prolinEvaluationResults) {
  if (!is.null(prolinPerformancePlots[["prolinPerformancePlotMape"]])) return()
  
  prolinPerformancePlotMape <- createJaspPlot(title = "Performance Plot Mape", height = 320, width = 480)
  prolinPerformancePlotMape$dependOn(c("performancePlotsMape"))
  
  prolinPerformancePlotMape$plotObject <- .prolinPerformancePlotFill(prolinEvaluationResults, options, type = "mape")
  
  prolinPerformancePlots[["prolinPerformancePlotMape"]] <- prolinPerformancePlotMape
  
  return()
}

.prolinPerformancePlotFill <- function(prolinEvaluationResults, options, type) {
  
  sampleDat <- prophet::performance_metrics(prolinEvaluationResults, metrics = type, rolling_window = -1)
  meanDat   <- prophet::performance_metrics(prolinEvaluationResults, metrics = type, rolling_window = 0)
  
  xBreaks <- pretty(sampleDat$horizon)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(sampleDat[[type]])
  
  p <- ggplot2::ggplot(data = sampleDat, mapping = ggplot2::aes_string(x = "horizon", y = type))
  
  p <- p + ggplot2::geom_point(size = 3, color = "grey")
  
  p <- p + ggplot2::geom_line(data = meanDat, color = "darkred", size = 1.25)
  
  p <- p +
    ggplot2::scale_x_continuous(name = gettext("Horizon (in days)"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext(stringr::str_to_upper(type)), breaks = yBreaks, limits = range(yBreaks))
  
  p <- jaspGraphs::themeJasp(p)
  
  return(p)
}

.prolinCreateParameterPlots <- function(jaspResults, options, ready) {
  
  prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  prolinParameterPlots <- createJaspContainer("Parameter Plots")
  prolinParameterPlots$position <- 8
  
  if(options$parameterPlotsDelta) 
    .prolinCreateParameterPlotDelta(prolinParameterPlots, options, prolinModelResults)
  if(options$parameterPlotsMarginalDistributions) 
    .prolinCreateParameterPlotMarginal(prolinParameterPlots, options, prolinModelResults)
  
  jaspResults[["prolinMainContainer"]][["prolinParameterPlots"]] <- prolinParameterPlots
}

.prolinCreateParameterPlotDelta <- function(prolinParameterPlots, options, prolinModelResults) {
  if (!is.null(prolinParameterPlots[["prolinParameterPlotDelta"]])) return()
  
  prolinParameterPlotDelta <- createJaspPlot(title = "Changepoint Distribution Plot", height = 480, width = 620)
  prolinParameterPlotDelta$dependOn(c("parameterPlotsDelta"))
  
  prolinParameterPlotDelta$plotObject <- .prolinParameterPlotDeltaFill(prolinModelResults, options)
  
  prolinParameterPlots[["prolinParameterPlotDelta"]] <- prolinParameterPlotDelta
  
  return()
}

.prolinParameterPlotDeltaFill <- function(prolinModelResults, options) {
  deltas  <- switch(options$estimation,
                   map  = as.numeric(prolinModelResults$params$delta),
                   mcmc = as.numeric(apply(prolinModelResults$params$delta, 2, mean)))
  cps     <- as.Date(prolinModelResults$changepoints)
  
  xBreaks <- pretty(cps)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(deltas), max(deltas)))
  
  p <- ggplot2::ggplot(data = data.frame(x = cps, y = deltas), mapping = ggplot2::aes(x = x, y = y))
  
  p <- p + ggplot2::geom_point(size = 3, color = "grey")
  
  p <- p + 
    ggplot2::scale_x_date(name = gettext("Time"), breaks = xBreaks, labels = gettext(xLabels), limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Change in growth rate (delta)"), breaks = yBreaks, limits = range(yBreaks))
  
  p <- jaspGraphs::themeJasp(p)
  
  return(p)
}

.prolinCreateParameterPlotMarginal <- function(prolinParameterPlots, options, prolinModelResults) {
  if (!is.null(prolinParameterPlots[["prolinParameterPlotMarginal"]])) return()
  
  prolinParameterPlotMarginal <- createJaspContainer(title = gettext("Marginal Posterior Distributions"))
  
  parNames <- c("k", "m", "sigma_obs")
  parTitles <- c("Growth rate", "Offset", "Residual variance")
  
  marginalPlotList <- list()
  
  for (i in 1:length(parNames)) {
    marginalPlotList[[i]] <- createJaspPlot(title = gettext(parTitles[i]), height = 320, width = 480)
    
    marginalPlotList[[i]]$plotObject <- .prolinParameterPlotMarginalFill(prolinModelResults, 
                                                                         options, 
                                                                         parNames[i], 
                                                                         parTitles[i])
    
    prolinParameterPlotMarginal[[parNames[i]]] <- marginalPlotList[[i]]
  }
  
  prolinParameterPlotMarginal$dependOn(c("parameterPlotsMarginalDistributions"))
  
  prolinParameterPlots[["prolinParameterPlotMarginal"]] <- prolinParameterPlotMarginal
  
  return()
}

.prolinParameterPlotMarginalFill <- function(prolinModelResults, options, parName, parTitle, criLevel = 0.95) {
  samples <- prolinModelResults$params[[parName]]
  dens <- density(samples)
  x <- dens$x
  y <- dens$y
  
  lvl <- (1-criLevel)/2
  cri <- stats::quantile(samples, prob = c(lvl, 1-lvl))
  
  xBreaks    <- jaspGraphs::getPrettyAxisBreaks(c(min(x), max(x)))
  yBreaks    <- jaspGraphs::getPrettyAxisBreaks(c(0, 1.15*max(y)))
  yBarPos    <- 0.9*yBreaks[length(yBreaks)]
  yBarHeight <- 0.05*yBreaks[length(yBreaks)]
  
  p <- ggplot2::ggplot()
  
  p <- p + ggplot2::geom_line(data = data.frame(x = x, y = y), mapping = ggplot2::aes(x = x, y = y),
                              size = 1.25)
  
  p <- p + ggplot2::geom_errorbarh(data = data.frame(xmin = cri[1], xmax = cri[2], y = yBarPos),
                                   mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y),
                                   height = yBarHeight)
  
  p <- p + 
    ggplot2::scale_x_continuous(name = gettext(parTitle), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = range(yBreaks))
  
  p <- jaspGraphs::themeJasp(p)
  
  return(p)
}

.prolinGetSeasonalityProperties <- function(name, prop, options) {
  if(is.null(options$seasonalities))
    return()

  propRes <- options$seasonalities[[which(sapply(options$seasonalities, function(s) encodeColNames(s$name) == name))]][[prop]]
  return(propRes)
}
