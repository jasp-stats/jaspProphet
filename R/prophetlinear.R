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
  
  prolinResults <- .prolinComputeResults(jaspResults, dataset, options, ready)
  prolinPred    <- .prolinPredict(       jaspResults, dataset, options, prolinResults, ready)

  .prolinContainerMain(          jaspResults, options, prolinResults, ready)
  .prolinCreateModelSummaryTable(jaspResults, options, prolinResults, ready)
  .prolinCreateForecastPlots(    jaspResults, options, prolinResults, prolinPred, ready)
  
  return()
}

# Init functions ----
.prolinInitOptions <- function(jaspResults, options) {
  
  options$forecastPlotsYearly <- options$yearlySeasonality
  options$forecastPlotsweekly <- options$weeklySeasonality
  options$forecastPlotsdaily  <- options$dailySeasonality
  
  return(options)
}

.prolinReadData <- function(options, ready) {
  if(!ready) return()
  
  # Read in the dataset using the built-in functions
  numericVars <- unlist(c(options$dependent, options$covariates))
  numericVars <- numericVars[numericVars != ""]
  factorVars  <- c(options$changepoints, options$time)
  factorVars  <- factorVars[factorVars != ""]
  dataset     <- .readDataSetToEnd(columns.as.numeric  = numericVars,
                                   columns.as.factor   = factorVars,
                                   exclude.na.listwise = c(numericVars, factorVars))

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
    prolinResults <- .prolinResultsHelper(dataset, options)
    
    jaspResults[["prolinResults"]] <- createJaspState(prolinResults)
    
    if (options$changepoints != "") {
      jaspResults[["prolinResults"]]$dependOn(c("variables", "yearlySeasonality", "weeklySeasonality", 
                                                "dailySeasonality", "estimation", "predictionIntervalWidth", 
                                                "predictionIntervalSamples"))
    } else {
      jaspResults[["prolinResults"]]$dependOn(c("variables", 
                                                "maxChangepoints", "changepointRange", "changepointPriorScale", 
                                                "yearlySeasonality", "weeklySeasonality", "dailySeasonality", 
                                                "estimation", "predictionIntervalWidth", "predictionIntervalSamples"))
    }
  } else {
    prolinResults <- jaspResults[["prolinResults"]]$object
  }
  
  return(prolinResults)
}

.prolinResultsHelper <- function(dataset, options) {
  y  <- dataset[[encodeColNames(options$dependent)]]
  ds <- as.Date(dataset[[encodeColNames(options$time)]])
  
  fitDat <- data.frame(y = y, ds = ds)
  
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
    
    for (cov in covs) {
      # TODO(maltelueken) add custom prior.scale, standardized, and mode
      # mod <- prophet::add_regressor(m = mod, name = cov, prior.scale = 10, standardize = "auto", mode = "additive")
      # fitDat[[cov]] <- dataset[[encodeColNames(cov)]]
    }
  }
  
  if (options$yearlySeasonality) {
    mod <- prophet::add_seasonality(m = mod, name = "yearly", period = 365.25, fourier.order = options$yearlyCustomTerms, 
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

.prolinPredict <- function(jaspResults, dataset, options, prolinResults, ready) {
  if (!ready) return()
  
  if (is.null(jaspResults[["prolinPred"]])) {
    prolinPred <- .prolinPredictHelper(dataset, options, prolinResults)
    
    jaspResults[["prolinPred"]] <- createJaspState(prolinPred)
    
    jaspResults[["prolinPred"]]$dependOn(c("yearlySeasonality", "weeklySeasonality", "dailySeasonality",
                                           "predictionType", "periodicalPredictionNumber", "periodicalPredictionUnit",
                                           "nonperiodicalPredictionStart", "nonperiodicalPredictionEnd"))
  } else {
    prolinPred <- jaspResults[["prolinPred"]]$object
  }
  
  return(prolinPred)
}

.prolinPredictHelper <- function(dataset, options, prolinResults) {
  # TODO(maltelueken) add covariates to future data frames
  
  futDat <- NULL
  
  if (options$predictionType == "periodicalPrediction") {
    futDat <- prophet::make_future_dataframe(m = prolinResults, 
                                             periods = options$periodicalPredictionNumber, 
                                             freq = options$periodicalPredictionUnit, 
                                             include_history = TRUE)
  } else {
    # TODO(maltelueken) add unit for nonperiodical prediction
    futDat <- data.frame(ds = seq(as.Date(options$nonperiodicalPredictionStart), 
                                  as.Date(options$nonperiodicalPredictionEnd), 
                                  by = "d"))
  }
  
  pred <- predict(prolinResults, futDat)
  
  return(pred)
}

# Output functions ----
.prolinContainerMain <- function(jaspResults, options, prolinResults, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]])) return()
  
  mainContainer <- createJaspContainer("Model Summary")
  
  jaspResults[["prolinMainContainer"]] <- mainContainer
}

.prolinModelSummary <- function(prolinResults, options) {
  if (options$estimation == "map") {
    return(prolinResults)
  } else {
    parSamples <- prolinResults$params
    
    parSummary <- lapply(parSamples, function(par) {
      if (is.matrix(par)) {
        return(apply(par, 2, quantile))
      } else {
        return(quantile(par))
      }
    })
    
    return(parSummary)
  }
}

.prolinCreateModelSummaryTable <- function(jaspResults, options, prolinResults, ready) {
  if (!is.null(jaspResults[["prolinMainContainer"]][["prolinModelSummaryTable"]])) return()
  
  if (options$estimation == "map") {
    prolinTable <- createJaspTable(title = "Parameter Estimates Table")
    
    jaspResults[["prolinMainContainer"]][["prolinTable"]] <- prolinTable
    
    prolinTable$addColumnInfo(name = "k", title = gettext("Growth rate (k)"), type = "number")
    prolinTable$addColumnInfo(name = "m", title = gettext("Offset (m)"), type = "number")
    prolinTable$addColumnInfo(name = "sigmaObs", title = gettext("Residual variance (sigma)"), type = "number")
    prolinTable$addColumnInfo(name = "logPost", title = gettext("Log posterior"), type = "number")
    
    .prolinModelSummaryTableMapFill(prolinTable, prolinResults, ready)
  } else {
    prolinTable <- createJaspTable(title = "Posterior Summary Table")
    
    jaspResults[["prolinMainContainer"]][["prolinTable"]] <- prolinTable
    
    overtitle <- gettext("95% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
    prolinTable$addColumnInfo(name = "par", title = gettext("Parameter"), type = "string")
    prolinTable$addColumnInfo(name = "mean", title = gettext("Mean"), type = "number")
    prolinTable$addColumnInfo(name = "sd", title = gettext("SD"), type = "number")
    prolinTable$addColumnInfo(name = "lowerCri", title = gettext("Lower"), type = "number", overtitle = overtitle)
    prolinTable$addColumnInfo(name = "upperCri", title = gettext("Upper"), type = "number", overtitle = overtitle)
    
    .prolinModelSummaryTableMcmcFill(prolinTable, prolinResults, ready)
  }
  
  return()
}

.prolinModelSummaryTableMapFill <- function(prolinTable, prolinResults, ready) {
  if (!ready) return()
  
  pars <- prolinResults$params
  
  prolinTable$addRows(list(
    k        = pars$k,
    m        = pars$m,
    sigmaObs = pars$sigma_obs,
    logPost  = prolinResults$stan.fit$value
  ))
}

.prolinModelSummaryTableMcmcFill <- function(prolinTable, prolinResults, ready) {
  if (!ready) return()
  
  pars <- prolinResults$params
  
  pars <- purrr::discard(pars, is.matrix)
  
  names(pars) <- c("Growth rate (k)", "Offset (m)", "Residual variance (sigma)", "Log posterior")
  
  parsCri <- purrr::reduce(lapply(pars, bayestestR::ci, ci = 0.95), rbind)
  
  prolinTable$addColumns(list(
    par      = names(pars),
    mean     = sapply(pars, mean),
    sd       = sapply(pars, sd),
    lowerCri = parsCri$CI_low,
    upperCri = parsCri$CI_high
  ))
}

.prolinCreateForecastPlots <- function(jaspResults, options, prolinResults, prolinPred, ready) {
  if (!is.null(jaspResults[["prolinForecastPlots"]])) return()
  
  prolinForecastPlots <- createJaspContainer(title = gettext("Forecast Plots"))
  prolinForecastPlots$dependOn(c("yearlySeasonality", "weeklySeasonality", "dailySeasonality",
                                 "predictionType", "periodicalPredictionNumber", "periodicalPredictionUnit",
                                 "nonperiodicalPredictionStart", "nonperiodicalPredictionEnd",
                                 "forecastPlotsOverall", "forecastPlotsTrend", "forecastPlotsYearly",
                                 "forecastPlotsWeekly", "forecastPlotsDaily"))
  
  if (options$forecastPlotsOverall) .prolinCreateOverallForecastPlot(prolinForecastPlots, options, prolinPred)
  if (options$forecastPlotsTrend)   .prolinCreateTrendForecastPlot(prolinForecastPlots, options, prolinPred)
  if (options$forecastPlotsYearly)  .prolinCreateYearlyForecastPlot(prolinForecastPlots, options, prolinPred)
  if (options$forecastPlotsWeekly)  .prolinCreateWeeklyForecastPlot(prolinForecastPlots, options, prolinPred)
  if (options$forecastPlotsDaily)   .prolinCreateDailyForecastPlot(prolinForecastPlots, options, prolinPred)
  
  jaspResults[["prolinForecastPlots"]] <- prolinForecastPlots
  
  return()
}

.prolinCreateOverallForecastPlot <- function(prolinForecastPlots, options, prolinPred) {
  if (!is.null(prolinForecastPlots[["prolinOverallForecastPlot"]])) return()
  
  prolinOverallForecastPlot <- createJaspPlot(title = "Overall Forecast Plot", height = 320, width = 480)
  prolinOverallForecastPlot$dependOn("forecastPlotsOverall")
  
  prolinOverallForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPred, options, type = "yhat")
  
  prolinForecastPlots[["prolinOverallForecastPlot"]] <- prolinOverallForecastPlot
  
  return()
}

.prolinCreateTrendForecastPlot <- function(prolinForecastPlots, options, prolinPred) {
  if (!is.null(prolinForecastPlots[["prolinTrendForecastPlot"]])) return()
  
  prolinTrendForecastPlot <- createJaspPlot(title = "Trend Forecast Plot", height = 320, width = 480)
  prolinTrendForecastPlot$dependOn("forecastPlotsTrend")
  
  prolinTrendForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPred, options, type = "trend")
  
  prolinForecastPlots[["prolinTrendForecastPlot"]] <- prolinTrendForecastPlot
  
  return()
}

.prolinCreateYearlyForecastPlot <- function(prolinForecastPlots, options, prolinPred) {
  if (!is.null(prolinForecastPlots[["prolinYearlyForecastPlot"]])) return()
  
  prolinYearlyForecastPlot <- createJaspPlot(title = "Yearly Forecast Plot", height = 320, width = 480)
  prolinYearlyForecastPlot$dependOn(c("forecastPlotsYearly", "yearlySeasonality"))
  
  prolinYearlyForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPred, options, type = "yearly")
  
  prolinForecastPlots[["prolinYearlyForecastPlot"]] <- prolinYearlyForecastPlot
  
  return()
}

.prolinCreateWeeklyForecastPlot <- function(prolinForecastPlots, options, prolinPred) {
  if (!is.null(prolinForecastPlots[["prolinWeeklyForecastPlot"]])) return()
  
  prolinWeeklyForecastPlot <- createJaspPlot(title = "Weekly Forecast Plot", height = 320, width = 480)
  prolinWeeklyForecastPlot$dependOn(c("forecastPlotsWeekly", "weeklySeasonality"))
  
  prolinWeeklyForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPred, options, type = "weekly")
  
  prolinForecastPlots[["prolinWeeklyForecastPlot"]] <- prolinWeeklyForecastPlot
  
  return()
}

.prolinCreateDailyForecastPlot <- function(prolinForecastPlots, options, prolinPred) {
  if (!is.null(prolinForecastPlots[["prolinDailyForecastPlot"]])) return()
  
  prolinDailyForecastPlot <- createJaspPlot(title = "Daily Forecast Plot", height = 320, width = 480)
  prolinDailyForecastPlot$dependOn(c("forecastPlotsDaily", "dailySeasonality"))
  
  prolinDailyForecastPlot$plotObject <- .prolinForecastPlotFill(prolinPred, options, type = "daily")
  
  prolinForecastPlots[["prolinDailyForecastPlot"]] <- prolinDailyForecastPlot
  
  return()
}

.prolinForecastPlotFill <- function(prolinPred, options, type) {
  # TODO(maltelueken) change time axis limits for weekly and daily
  p <- ggplot2::ggplot(prolinPred, mapping = ggplot2::aes_string(x = "ds", y = type))
  
  p <- p + ggplot2::geom_line(color = "black")
  
  p <- p + ggplot2::geom_ribbon(mapping = ggplot2::aes_string(ymin = paste0(type, "_lower"), max = paste0(type, "_upper")),
                         fill = "blue", alpha = 0.4)
  
  p <- p + ggplot2::labs(x = "Time", y = options$dependent)
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}
