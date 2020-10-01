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

  .prolinContainerMain(             jaspResults, options, ready)
  .prolinCreateModelSummaryTable(   jaspResults, options, ready)
  .prolinCreateModelEvaluationTable(jaspResults, options, ready)
  .prolinCreateHistoryPlot(         jaspResults, dataset, options, ready)
  .prolinCreateForecastPlots(       jaspResults, dataset, options, ready)
  .prolinCreatePerformancePlots(    jaspResults, options, ready)
  .prolinCreateParameterPlots(      jaspResults, options, ready)
  
  return()
}

# Init functions ----
.prolinInitOptions <- function(jaspResults, options) {
  
  if(!options$yearlySeasonality)    options$forecastPlotsYearly <- FALSE
  if(!options$weeklySeasonality)    options$forecastPlotsWeekly <- FALSE
  if(!options$dailySeasonality)     options$forecastPlotsDaily  <- FALSE
  if(!options$forecastPlotsOverall) options$forecastPlotsOverallAddCovariates <- FALSE
  
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
        changepointVar <- try(as.logical(dataset[[encodeColNames(options$changepoints)]]))
        
        if(class(changepointVar) == "try-error" || length(unique(dataset[[encodeColNames(options$changepoints)]])) != 2)
          return(gettext("Error in setting changepoints: Variable 'Changepoints' must have two levels"))
      }
      
      return()
    },
    
    .nonperiodicalPredChecks <- function() {
      if (options$predictionType == "nonperiodicalPrediction") {
        predStart <- try(as.Date(options$nonperiodicalPredictionStart))
        predEnd <- try(as.Date(options$nonperiodicalPredictionEnd))
        
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
                                                "yearlySeasonality", "yearlySeasonalityPriorScale", 
                                                "yearlySeasonalityMode", "yearlySeasonalityCustom", "yearlyCustomTerms", 
                                                "weeklySeasonality","weeklySeasonalityPriorScale", 
                                                "weeklySeasonalityMode", "weeklySeasonalityCustom", "weeklyCustomTerms",  
                                                "dailySeasonality", "dailySeasonalityPriorScale", 
                                                "dailySeasonalityMode", "dailySeasonalityCustom", "dailyCustomTerms",
                                                "estimation", "mcmcSamples", "predictionIntervalWidth", 
                                                "predictionIntervalSamples"))
    } else {
      jaspResults[["prolinResults"]]$dependOn(c("dependent", "time", "changepoints", "covariates", 
                                                "maxChangepoints", "changepointRange", "changepointPriorScale", 
                                                "yearlySeasonality", "yearlySeasonalityPriorScale", 
                                                "yearlySeasonalityMode", "yearlySeasonalityCustom", "yearlyCustomTerms", 
                                                "weeklySeasonality","weeklySeasonalityPriorScale", 
                                                "weeklySeasonalityMode", "weeklySeasonalityCustom", "weeklyCustomTerms",  
                                                "dailySeasonality", "dailySeasonalityPriorScale", 
                                                "dailySeasonalityMode", "dailySeasonalityCustom", "dailyCustomTerms",
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
                                            "nonperiodicalPredictionEnd"))
    prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
    prolinPredictionResults <- .prolinPredictionResultsHelper(dataset, options, prolinModelResults)
    prolinPredictionResultsState$object <- prolinPredictionResults
    jaspResults[["prolinResults"]][["prolinPredictionResults"]] <- prolinPredictionResultsState
  }
  
  if (is.null(jaspResults[["prolinResults"]][["prolinEvaluationResults"]])) {
    prolinEvaluationResultsState <- createJaspState()
    prolinEvaluationResultsState$dependOn(c("crossValidationUnit", "crossValidationHorizon", 
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
  ds <- as.Date(dataset[[encodeColNames(options$time)]])
  
  fitDat <- na.omit(data.frame(y = y, ds = ds))
  
  if (options$changepoints != "") {
    isChangepoint <- as.logical(dataset[[encodeColNames(options$changepoints)]])
    cp            <- ds[isChangepoint]
  } else {
    cp <- NULL
  }
  
  if (options$estimation == "mcmc") {
    mod <- prophet::prophet(growth = "linear", changepoints = cp, n.changepoints = options$maxChangepoints, changepoint.range = options$changepointRange, changepoint.prior.scale = options$changepointPriorScale,
                            yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE,
                            mcmc.samples = options$mcmcSamples, interval.width = options$predictionIntervalWidth, uncertainty.samples = options$predictionIntervalSamples, fit = FALSE)
  } else {
    mod <- prophet::prophet(growth = "linear", changepoints = cp, n.changepoints = options$maxChangepoints, changepoint.range = options$changepointRange, changepoint.prior.scale = options$changepointPriorScale,
                            yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE,
                            mcmc.samples = 0, interval.width = options$predictionIntervalWidth, uncertainty.samples = options$predictionIntervalSamples, fit = FALSE)
  }
  
  if (length(options$covariates) > 0) {
    covs <- unlist(options$covariates)
    covFit <- !is.na(y)
    
    for (cov in covs) {
      # TODO(maltelueken) add custom prior.scale, standardized, and mode
      mod <- prophet::add_regressor(m = mod, name = cov, prior.scale = 10, standardize = "auto", mode = "additive")
      
      datCov <- dataset[[encodeColNames(cov)]]
      fitDat[[cov]] <- datCov[covFit]
    }
  }
  
  if (options$yearlySeasonality) {
    mod <- prophet::add_seasonality(m = mod, name = "yearly", period = 365, fourier.order = options$yearlyCustomTerms, 
                                    prior.scale = options$yearlySeasonalityPriorScale, mode = options$yearlySeasonalityMode)
  }
  
  if (options$weeklySeasonality) {
    mod <- prophet::add_seasonality(m = mod, name = "weekly", period = 7, fourier.order = options$weeklyCustomTerms, 
                                    prior.scale = options$weeklySeasonalityPriorScale, mode = options$weeklySeasonalityMode)
  }
  
  if (options$dailySeasonality) {
    mod <- prophet::add_seasonality(m = mod, name = "daily", period = 1, fourier.order = options$dailyCustomTerms, 
                                    prior.scale = options$dailySeasonalityPriorScale, mode = options$dailySeasonalityMode)
  }
  
  fit <- prophet::fit.prophet(m = mod, df = fitDat)
  
  return(fit)
}

.prolinPredictionResultsHelper <- function(dataset, options, prolinModelResults) {
  # TODO(maltelueken) add covariates to future data frames
  
  futDat <- NULL
  
  if (options$predictionType == "periodicalPrediction") {
    futDat <- prophet::make_future_dataframe(m = prolinModelResults, 
                                             periods = options$periodicalPredictionNumber, 
                                             freq = options$periodicalPredictionUnit, 
                                             include_history = TRUE)
  } else {
    # TODO(maltelueken) add unit for nonperiodical prediction
    futDat <- data.frame(ds = seq(as.Date(options$nonperiodicalPredictionStart), 
                                  as.Date(options$nonperiodicalPredictionEnd), 
                                  by = "d"))
  }
  
  if (length(options$covariates) > 0) {
    covs      <- unlist(options$covariates)
    futds     <- as.Date(futDat$ds)
    ds        <- as.Date(dataset[[encodeColNames(options$time)]])
    
    for (cov in covs) {
      futCov <- dataset[[encodeColNames(cov)]]
      futSel <- ds %in% futds
      futDat[[cov]] <- futCov[futSel]
    }
  }
  
  pred <- predict(prolinModelResults, futDat)
  
  return(pred)
}

.prolinEvaluationResultsHelper <- function(dataset, options, prolinModelResults) {
  
  cvDat <- prophet::cross_validation(model   = prolinModelResults, 
                                     horizon = options$crossValidationHorizon, 
                                     units   = options$crossValidationUnit,
                                     period  = options$crossValidationPeriod,
                                     initial = options$crossValidationInitial)
  
  return(cvDat)
}

# Output functions ----
.prolinContainerMain <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]])) return()
  
  mainContainer <- createJaspContainer()
  
  jaspResults[["prolinMainContainer"]] <- mainContainer
  jaspResults[["prolinMainContainer"]]$dependOn(c("dependent", "time", "changepoints", "covariates",
                                                  "maxChangepoints", "changepointRange", "changepointPriorScale", 
                                                  "yearlySeasonality", "yearlySeasonalityPriorScale", 
                                                  "yearlySeasonalityMode", "yearlySeasonalityCustom", "yearlyCustomTerms", 
                                                  "weeklySeasonality","weeklySeasonalityPriorScale", 
                                                  "weeklySeasonalityMode", "weeklySeasonalityCustom", "weeklyCustomTerms",  
                                                  "dailySeasonality", "dailySeasonalityPriorScale", 
                                                  "dailySeasonalityMode", "dailySeasonalityCustom", "dailyCustomTerms",
                                                  "estimation", "mcmcSamples", "predictionIntervalWidth"))
  
  return()
}

.prolinCreateModelSummaryTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]][["prolinModelSummaryTable"]])) return()
  
  prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  if (options$estimation == "map") {
    prolinTable <- createJaspTable(title = "Parameter Estimates Table")
    
    prolinTable$addColumnInfo(name = "k", title = gettext("Growth rate (k)"), type = "number")
    prolinTable$addColumnInfo(name = "m", title = gettext("Offset (m)"), type = "number")
    prolinTable$addColumnInfo(name = "sigmaObs", title = gettext("Residual variance (sigma)"), type = "number")
    
    .prolinModelSummaryTableMapFill(prolinTable, prolinModelResults, ready)
    
    jaspResults[["prolinMainContainer"]][["prolinTable"]] <- prolinTable
    
  } else {
    prolinTable <- createJaspTable(title = "Posterior Summary Table")
    
    overtitle <- gettext("95% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
    prolinTable$addColumnInfo(name = "par", title = gettext("Parameter"), type = "string")
    prolinTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    prolinTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
    prolinTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = overtitle)
    prolinTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = overtitle)
    
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
  
  pars <- prolinModelResults$params[c("k", "m", "sigma_obs")]
  
  names(pars) <- c("Growth rate (k)", "Offset (m)", "Residual variance (sigma)")
  
  parsCri <- purrr::reduce(lapply(pars, function(x, lvl) {
    
    marg <- (1-lvl)/2
    
    cri <- stats::quantile(x, probs = c(marg, 1-marg))
    
    return(cri)
  }, lvl = 0.95), rbind)
  
  prolinTable$addColumns(list(
    par      = names(pars),
    mean     = sapply(pars, mean),
    sd       = sapply(pars, sd),
    lowerCri = parsCri[,1],
    upperCri = parsCri[,2]
  ))
}

.prolinCreateModelEvaluationTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]][["prolinModelEvaluationTable"]])) return()
  
  prolinEvaluationResults <- jaspResults[["prolinResults"]][["prolinEvaluationResults"]]$object
  
  prolinTable <- createJaspTable("Simulated Historical Forecasts Table")
  prolinTable$dependOn(c("crossValidationUnit", "crossValidationHorizon", 
                             "crossValidationPeriod", "crossValidationInitial", "performanceMetricsMse", 
                             "performanceMetricsRmse", "performanceMetricsMape"))
  
  prolinTable$addColumnInfo(name = "horizon", title = gettext("Horizon"), type = "string")
  
  if (options$performanceMetricsMse)  prolinTable$addColumnInfo(name = "mse", title = gettext("MSE"), type = "number")
  if (options$performanceMetricsRmse) prolinTable$addColumnInfo(name = "rmse", title = gettext("RMSE"), type = "number")
  if (options$performanceMetricsMape) prolinTable$addColumnInfo(name = "mape", title = gettext("MAPE"), type = "number", format = "pc")
  
  .prolinModelEvaluationTableFill(prolinTable, options, prolinEvaluationResults, ready)
  
  jaspResults[["prolinMainContainer"]][["prolinModelEvaluationTable"]] <- prolinTable
}

.prolinModelEvaluationTableFill <- function(prolinTable, options, prolinEvaluationResults, ready) {
  if (!ready) return()
  
  metrics <- c("mse", "rmse", "mape")
  metrics <- metrics[c(options$performanceMetricsMse, options$performanceMetricsRmse, options$performanceMetricsMape)]
  
  metDat <- prophet::performance_metrics(df = prolinEvaluationResults, metrics = metrics, rolling_window = 0)
  
  if (options$performanceMetricsMape) metDat$mape <- metDat$mape/100
  
  prolinTable$addColumns(as.list(metDat))
}

.prolinCreateHistoryPlot <- function(jaspResults, dataset, options, ready) {
  if (!ready || !options$historyPlot) return()
  
  prolinHistoryPlot <- createJaspPlot(title = "History Plot", height = 480, width = 620)
  prolinHistoryPlot$dependOn(c("dependent", "time", "changepoints", "covariates", "historyPlot", "historyPlotStart",
                                "historyPlotEnd"))
  
  prolinHistoryPlot$plotObject <- .prolinHistoryPlotFill(dataset, options)
  
  jaspResults[["historyPlot"]] <- prolinHistoryPlot
  
  return()
}

.prolinHistoryPlotFill <- function(dataset, options) {
  
  yHist  <- dataset[[encodeColNames(options$dependent)]]
  xHist <- as.Date(dataset[[encodeColNames(options$time)]])
  histDat <- data.frame(y = yHist, x = xHist)
  
  xLimits <- c(min(xHist), max(xHist))
  
  if (options$historyPlotStart != "")
    xLimits[1] <- as.Date(options$historyPlotStart)
  if (options$historyPlotEnd != "")
    xLimits[2] <- as.Date(options$historyPlotEnd)
  
  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(range(yHist))
  
  p <- ggplot2::ggplot(histDat, mapping = ggplot2::aes(x = x, y = y)) +
    
    ggplot2::geom_point(data = histDat, mapping = ggplot2::aes(x = x, y = y), size = 3)
  
  p <- p + 
    ggplot2::scale_x_date(name = gettext("Time"), 
                          breaks = xBreaks, 
                          labels = gettext(xLabels),
                          limits = range(xBreaks)) + 
    
    ggplot2::scale_y_continuous(name = gettext(options$dependent), 
                                breaks = yBreaks, 
                                limits = range(yBreaks))
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}

.prolinCreateForecastPlots <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  
  prolinPredictionResults <- jaspResults[["prolinResults"]][["prolinPredictionResults"]]$object
  prolinModelResults      <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  prolinForecastPlots <- createJaspContainer(title = gettext("Forecast Plots"))
  prolinForecastPlots$dependOn(c("predictionIntervalSamples", "predictionType", "periodicalPredictionNumber", 
                                 "periodicalPredictionUnit", "nonperiodicalPredictionStart", "nonperiodicalPredictionEnd"))
  
  if (options$forecastPlotsOverall) .prolinCreateOverallForecastPlot(prolinForecastPlots, dataset, options, prolinPredictionResults)
  if (options$forecastPlotsTrend)   .prolinCreateTrendForecastPlot(prolinForecastPlots, dataset, options, prolinPredictionResults)
  if (options$forecastPlotsYearly)  .prolinCreateYearlyForecastPlot(prolinForecastPlots, dataset, options, prolinModelResults)
  if (options$forecastPlotsWeekly)  .prolinCreateWeeklyForecastPlot(prolinForecastPlots, dataset, options, prolinModelResults)
  if (options$forecastPlotsDaily)   .prolinCreateDailyForecastPlot(prolinForecastPlots, dataset, options, prolinModelResults)
  
  jaspResults[["prolinMainContainer"]][["prolinForecastPlots"]] <- prolinForecastPlots
  
  return()
}

.prolinCreateOverallForecastPlot <- function(prolinForecastPlots, dataset, options, prolinPredictionResults) {
  if (!is.null(prolinForecastPlots[["prolinOverallForecastPlot"]])) return()
  
  prolinOverallForecastPlot <- createJaspPlot(title = "Overall Forecast Plot", height = 480, width = 620)
  prolinOverallForecastPlot$dependOn("forecastPlotsOverall")
  
  prolinOverallForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPredictionResults, dataset, options, type = "yhat")
  
  prolinForecastPlots[["prolinOverallForecastPlot"]] <- prolinOverallForecastPlot
  
  return()
}

.prolinCreateTrendForecastPlot <- function(prolinForecastPlots, dataset, options, prolinPredictionResults) {
  if (!is.null(prolinForecastPlots[["prolinTrendForecastPlot"]])) return()
  
  prolinTrendForecastPlot <- createJaspPlot(title = "Trend Forecast Plot", height = 480, width = 620)
  prolinTrendForecastPlot$dependOn("forecastPlotsTrend")
  
  prolinTrendForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPredictionResults, dataset, options, type = "trend")
  
  prolinForecastPlots[["prolinTrendForecastPlot"]] <- prolinTrendForecastPlot
  
  return()
}

.prolinCreateYearlyForecastPlot <- function(prolinForecastPlots, dataset, options, prolinModelResults) {
  if (!is.null(prolinForecastPlots[["prolinYearlyForecastPlot"]])) return()
  
  prolinPredictionResults <- .prolinForecastPlotPredictComponent(prolinModelResults, type = "yearly")
  
  prolinYearlyForecastPlot <- createJaspPlot(title = "Yearly Forecast Plot", height = 480, width = 620)
  prolinYearlyForecastPlot$dependOn(c("forecastPlotsYearly", "yearlySeasonality"))
  
  prolinYearlyForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPredictionResults, dataset, options, type = "yearly")
  
  prolinForecastPlots[["prolinYearlyForecastPlot"]] <- prolinYearlyForecastPlot
  
  return()
}

.prolinCreateWeeklyForecastPlot <- function(prolinForecastPlots, dataset, options, prolinModelResults) {
  if (!is.null(prolinForecastPlots[["prolinWeeklyForecastPlot"]])) return()
  
  prolinPredictionResults <- .prolinForecastPlotPredictComponent(prolinModelResults, type = "weekly")
  
  prolinWeeklyForecastPlot <- createJaspPlot(title = "Weekly Forecast Plot", height = 480, width = 620)
  prolinWeeklyForecastPlot$dependOn(c("forecastPlotsWeekly", "weeklySeasonality"))
  
  prolinWeeklyForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPredictionResults, dataset, options, type = "weekly")
  
  prolinForecastPlots[["prolinWeeklyForecastPlot"]] <- prolinWeeklyForecastPlot
  
  return()
}

.prolinCreateDailyForecastPlot <- function(prolinForecastPlots, options, dataset, prolinModelResults) {
  if (!is.null(prolinForecastPlots[["prolinDailyForecastPlot"]])) return()
  
  prolinPredictionResults <- .prolinForecastPlotPredictComponent(prolinModelResults, type = "daily")
  
  prolinDailyForecastPlot <- createJaspPlot(title = "Daily Forecast Plot", height = 480, width = 620)
  prolinDailyForecastPlot$dependOn(c("forecastPlotsDaily", "dailySeasonality"))
  
  prolinDailyForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPredictionResults, dataset, options, type = "daily")
  
  prolinForecastPlots[["prolinDailyForecastPlot"]] <- prolinDailyForecastPlot
  
  return()
}

.prolinForecastPlotPredictComponent <- function(prolinModelResults, type) {
  ds <- switch (type,
    yearly = seq(as.Date("2020-01-01"), by = "d", length.out = 365),
    weekly = seq(as.Date("2020-09-07"), by = "d", length.out = 7),
    daily  = seq(as.POSIXlt("2020-09-07"), by = "h", length.out = 24)
  )
  
  predComp <- predict(prolinModelResults, data.frame(ds = as.Date(ds)))
  
  return(predComp)
}

.prolinForecastPlotFill <- function(prolinPredictionResults, dataset, options, type) {
  mode <- paste0(type, "SeasonalityMode")
  
  yHist  <- dataset[[encodeColNames(options$dependent)]]
  xHist <- as.Date(dataset[[encodeColNames(options$time)]])
  histDat <- na.omit(data.frame(y = yHist, x = xHist))
  
  x <- as.Date(prolinPredictionResults[["ds"]])
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
    xLimits[1] <- as.Date(options$forecastPlotsOverallStart)
  if (options$forecastPlotsOverallEnd != "" && type == "yhat")
    xLimits[2] <- as.Date(options$forecastPlotsOverallEnd)
  
  xBreaks <- pretty(xLimits)
  xLabels <- attr(xBreaks, "labels")
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(min(ymin), max(ymax)))
  
  xFormat <- switch (type,
    yearly = "%b",
    weekly = "%a",
    daily  = "%H",
    ggplot2::waiver()
  )
  
  p <- ggplot2::ggplot(df, mapping = ggplot2::aes(x = x, y = y))
  
  p <- p + ggplot2::geom_line(color = "black", size = 1.25)
  
  p <- p + ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.4)
  
  if (options$forecastPlotsOverallAddData && type == "yhat") {
    
    p <- p + ggplot2::geom_point(data = histDat, mapping = ggplot2::aes(x = x, y = y), size = 3)
    
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(min(c(min(ymin), min(histDat$y))), max(c(max(ymax), max(histDat$y)))))
  }
  
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
                                                                                      label = as.factor(covs[i])),
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
    
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(min(c(min(ymin), min(histDat$y), covMin)), 
                                                 max(c(max(ymax), max(histDat$y), covMax))))
  }
  
  if (type == "yhat" || type == "trend") 
    p <- p + ggplot2::geom_segment(mapping = ggplot2::aes(x = histDat$x[length(histDat$x)], y = range(yBreaks)[1], 
                                                          xend = histDat$x[length(histDat$x)]), yend = range(yBreaks)[2], 
                                   linetype = "dashed")
  
  p <- p + 
    ggplot2::scale_x_date(name = gettext("Time"), 
                          breaks = xBreaks, labels = gettext(xLabels), 
                          date_labels = xFormat, 
                          limits = range(xBreaks))
  
  # Inspired by plot_seasonality from prophet package
  if (type %in% c("yearly", "weekly", "daily") && options[[mode]] == "multiplicative") {
    p <- p + ggplot2::scale_y_continuous(name = gettext(options$dependent), 
                                         breaks = yBreaks, 
                                         labels = scales::percent, 
                                         limits = range(yBreaks))
  } else {
    p <- p + ggplot2::scale_y_continuous(name = gettext(options$dependent), 
                                         breaks = yBreaks, 
                                         limits = range(yBreaks))
  }
  
  p <- JASPgraphs::themeJasp(p)
  
  # p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(size = ggplot2::rel(0.95)))
  
  return(p)
}

.prolinCreatePerformancePlots <- function(jaspResults, options, ready) {
  
  prolinEvaluationResults <- jaspResults[["prolinResults"]][["prolinEvaluationResults"]]$object
  
  prolinPerformancePlots <- createJaspContainer("Performance Plots")
  
  prolinPerformancePlots$dependOn(c("crossValidationUnit", "crossValidationHorizon", 
                                   "crossValidationPeriod", "crossValidationInitial"))
  
  if(options$performancePlotsMse)  .prolinCreatePerformancePlotMse( prolinPerformancePlots, options, prolinEvaluationResults)
  if(options$performancePlotsRmse) .prolinCreatePerformancePlotRmse(prolinPerformancePlots, options, prolinEvaluationResults)
  if(options$performancePlotsMape) .prolinCreatePerformancePlotMape(prolinPerformancePlots, options, prolinEvaluationResults)
  
  jaspResults[["prolinMainContainer"]][["prolinEvaluationPlots"]] <- prolinPerformancePlots
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
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(sampleDat[[type]])
  
  p <- ggplot2::ggplot(data = sampleDat, mapping = ggplot2::aes_string(x = "horizon", y = type))
  
  p <- p + ggplot2::geom_point(size = 3, color = "grey")
  
  p <- p + ggplot2::geom_line(data = meanDat, color = "darkred", size = 1.25)
  
  p <- p +
    ggplot2::scale_x_continuous(name = gettext("Horizon (in days)"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext(stringr::str_to_upper(type)), breaks = yBreaks, limits = range(yBreaks))
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}

.prolinCreateParameterPlots <- function(jaspResults, options, ready) {
  
  prolinModelResults <- jaspResults[["prolinResults"]][["prolinModelResults"]]$object
  
  prolinParameterPlots <- createJaspContainer("Parameter Plots")
  
  if(options$parameterPlotsDelta) 
    .prolinCreateParameterPlotDelta(prolinParameterPlots, options, prolinModelResults)
  if(options$parameterPlotsMarginalDistributions) 
    .prolinCreateParameterPlotMarginal(prolinParameterPlots, options, prolinModelResults)
  
  jaspResults[["prolinMainContainer"]][["prolinParameterPlots"]] <- prolinParameterPlots
}

.prolinCreateParameterPlotDelta <- function(prolinParameterPlots, options, prolinModelResults) {
  if (!is.null(prolinParameterPlots[["prolinParameterPlotDelta"]])) return()
  
  prolinParameterPlotDelta <- createJaspPlot(title = "Changepoint Distribution Plot", height = 320, width = 480)
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
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(min(deltas), max(deltas)))
  
  p <- ggplot2::ggplot(data = data.frame(x = cps, y = deltas), mapping = ggplot2::aes(x = x, y = y))
  
  p <- p + ggplot2::geom_point(size = 3, color = "grey")
  
  p <- p + 
    ggplot2::scale_x_date(name = gettext("Time"), breaks = xBreaks, labels = gettext(xLabels), limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = gettext("Change in k (delta)"), breaks = yBreaks, limits = range(yBreaks))
  
  p <- JASPgraphs::themeJasp(p)
  
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
  
  xBreaks    <- JASPgraphs::getPrettyAxisBreaks(c(min(x), max(x)))
  yBreaks    <- JASPgraphs::getPrettyAxisBreaks(c(0, 1.15*max(y)))
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
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}