#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Prophet
#'
Prophet <- function(
          data = NULL,
          version = "1",
          assignedCovariates = list(optionKey = "variable", types = list(), value = list()),
          carryingCapacity = list(types = list(), value = ""),
          changePointTable = FALSE,
          changepointPlot = FALSE,
          changepointPriorScale = 0.05,
          changepointRange = 0.8,
          changepoints = list(types = list(), value = ""),
          ciLevel = 0.95,
          covariatePlots = list(optionKey = "variable", types = list(), value = list()),
          covariates = list(types = list(), value = list()),
          crossValidation = FALSE,
          crossValidationHorizon = 3,
          crossValidationInitial = 9,
          crossValidationPeriod = 1,
          crossValidationUnit = "days",
          dependent = list(types = list(), value = ""),
          estimation = "mcmc",
          forecastPlotOverall = FALSE,
          forecastPlotOverallCarryingCapacity = FALSE,
          forecastPlotOverallChangepoints = FALSE,
          forecastPlotOverallDataPoints = FALSE,
          forecastPlotOverallEnd = "",
          forecastPlotOverallRange = FALSE,
          forecastPlotOverallSaturatingMinimum = FALSE,
          forecastPlotOverallStart = "",
          forecastPlotTrend = FALSE,
          forecastPlotTrendChangepoints = FALSE,
          forecastPlotTrendEnd = "",
          forecastPlotTrendRange = FALSE,
          forecastPlotTrendStart = "",
          growth = "linear",
          historyIndicator = list(types = list(), value = ""),
          historyPlot = FALSE,
          historyPlotEnd = "",
          historyPlotRange = FALSE,
          historyPlotStart = "",
          historyPlotType = "points",
          logisticGrowthCarryingCapacity = 0,
          logisticGrowthSaturatingMin = 0,
          mapePlot = FALSE,
          maxChangepoints = 0,
          mcmcSamples = 1000,
          minimum = list(types = list(), value = ""),
          msePlot = FALSE,
          nonperiodicalPredictionEnd = "",
          nonperiodicalPredictionStart = "",
          nonperiodicalPredictionUnit = "days",
          performanceMetrics = FALSE,
          performanceMetricsMape = FALSE,
          performanceMetricsMse = TRUE,
          performanceMetricsRmse = FALSE,
          periodicalPredictionNumber = 0,
          periodicalPredictionUnit = "days",
          plotHeight = 320,
          plotWidth = 480,
          posteriorPlot = FALSE,
          predictionIntervalLevel = 0.8,
          predictionIntervalSamples = 1000,
          predictionSavePath = "",
          predictionType = "periodicalPrediction",
          rmsePlot = FALSE,
          seasonalities = list(),
          seasonalityPlots = list(types = list(), value = list()),
          time = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("assignedCovariates", "carryingCapacity", "changepoints", "covariatePlots", "covariates", "crossValidationUnit", "dependent", "historyIndicator", "minimum", "nonperiodicalPredictionUnit", "periodicalPredictionUnit", "seasonalities", "seasonalityPlots", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspProphet", "Prophet", "Prophet.qml", options, version, FALSE))
}