context("Prophet")

# Make sure to run the test for plots with date axes on a system with English language 
# because months and weekdays will be displayed depending on that

dateTimeVarNames <- c("dateYear", "dateWeek", "dateDay", "timeHour", "timeMinute", "timeSecond")
dateTimeUnits <- c("years", "weeks", "days", "hours", "mins", "secs")

test_that("Posterior Summary Table results match (linear)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateYear"
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""
  
  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    set.seed(1)
    results <- jaspTools::runAnalysis(name = "Prophet", dataset = "prophetTest.csv", options = options)
    table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, -0.315422171025854, -0.193384551496492, "Growth rate (k)",
                                        1, 0.0795317404287579, 20, -0.10449161775554, 20, 0.0564557647340031,
                                        0.113226988344312, "Offset (m)", 1, 0.0831938284838563, 20,
                                        0.252718078792628, 20, 0.374623449591915, 0.573723365352864,
                                        "Residual variance (sigma)", 1, 0.2235178416768, 20, 0.942133582100974
                                   )) 
  }
})

test_that("Posterior Summary Table results match (logistic)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$capacity <- "contGamma"
  options$historyIndicator <- "histIdx"
  options$mcmcSamples <- 10
  options$growth <- "logistic"
  options$predictionSavePath <- ""
  
  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(20, -7.29654434885841, -7.1818644571539, "Growth rate (k)", 1,
                                        0.0843752327706313, 20, -7.06528194891996, 20, -0.148227664168065,
                                        -0.00937252121369772, "Offset (m)", 1, 0.0963163677055294, 20,
                                        0.1112940095092, 20, 0.397770975592959, 0.462882463647365, "Residual variance (sigma)",
                                        1, 0.0830358930795562, 20, 0.598905146042098))
  }
})

test_that("Posterior Summary Table results match (covariates-linear)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$covariates <- list("contcor1", "contcor2")
  options$historyIndicator <- "histIdx"
  options$mcmcSamples <- 10
  options$assignedCovariates <- list(list(
    variable = "contcor1",
    priorSigma = 10,
    standardize = TRUE,
    mode = "additive"
    ), list(
    variable = "contcor2",
    priorSigma = 10,
    standardize = TRUE,
    mode = "additive"))
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(20, -0.549276682026696, -0.198259467693576, "Growth rate (k)",
                                      1, 0.249651577895865, 20, 0.0682208680727072, 20, -0.0923559186278328,
                                      0.128636258735697, "Offset (m)", 1, 0.138340410137959, 20, 0.254218965024575,
                                      20, 0.27500849891103, 0.48578179796889, "Residual variance (sigma)",
                                      1, 0.182991724057581, 20, 0.767603622085341))
})

test_that("Posterior Summary Table results match (covariates-logistic)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$capacity <- "contGamma"
  options$covariates <- list("contcor1", "contcor2")
  options$historyIndicator <- "histIdx"
  options$mcmcSamples <- 10
  options$growth <- "logistic"
  options$assignedCovariates <- list(list(
    variable = "contcor1",
    priorSigma = 10,
    standardize = TRUE,
    mode = "additive"
    ), list(
    variable = "contcor2",
    priorSigma = 10,
    standardize = TRUE,
    mode = "additive"))
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(20, -7.54755174599736, -7.22367062677889, "Growth rate (k)", 1,
                                      0.234398048735986, 20, -6.9180260682547, 20, -0.755375375200505,
                                      -0.185995124341449, "Offset (m)", 1, 0.364082088507299, 20,
                                      0.202688536418164, 20, 0.264544515886565, 0.43710032684354,
                                      "Residual variance (sigma)", 1, 0.111127496572815, 20, 0.537888848917362
                                 ))
})

test_that("Posterior Summary Table results match (minimum-logistic)", {
     options <- jaspTools::analysisOptions("Prophet")
     options$dependent <- "contNormal"
     options$time <- "dateDay"
     options$capacity <- "contGamma"
     options$minimum <- "contNarrow"
     options$historyIndicator <- "histIdx"
     options$mcmcSamples <- 10
     options$growth <- "logistic"
     options$predictionSavePath <- ""
     set.seed(1)
     results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
     table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
     jaspTools::expect_equal_tables(table,
          list(20, -7.29654434885841, -7.1818644571539, "Growth rate (k)", 1,
                0.0843752327706313, 20, -7.06528194891996, 20, -0.148227664168065,
                -0.00937252121369772, "Offset (m)", 1, 0.0963163677055294, 20,
                0.1112940095092, 20, 0.397770975592959, 0.462882463647365, "Residual variance (sigma)",
                1, 0.0830358930795562, 20, 0.598905146042098))
})

test_that("Posterior Summary Table results match (constant-logistic)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$constantCapacity <- 10
  options$constantMinimum <- 0.1
  options$mcmcSamples <- 10
  options$growth <- "logistic"
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(20, -3.59423543336109, -1.60259694991599, "Growth rate (k)", 1,
       1.18253175777653, 20, -0.810313940048772, 20, -3.35660841596216,
       -3.11650941860635, "Offset (m)", 1, 0.157198845587502, 20, -2.96206374129486,
       20, 0.354910105713339, 0.480225356561229, "Residual variance (sigma)",
       1, 0.0771605826106158, 20, 0.555518365514962))
})

test_that("Parameter Estimates Table results match (MAP)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$predictionIntervalSamples <- 10
  options$estimation <- "map"
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.165701104700655, 0.0288912230542516, 0.315932056357582))

  # Does currently not work on Mac
  # 
  # options$capacity <- "contGamma"
  # options$minimum <- "contNarrow"
  # options$historyIndicator <- "histIdx"
  # options$growth <- "logistic"
  # set.seed(1)
  # results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  # table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetTable"]][["data"]]
  # jaspTools::expect_equal_tables(table,
  #  list(-2.50859195109382, -2.31656506358638, 0.32805219661169))
})

test_that("Changepoint Posterior Summary Table results match (automatic)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$mcmcSamples <- 10
  options$changePointTable <- TRUE
  options$maxChangepoints <- 3
  options$predictionSavePath <- ""
  
  testList <- list(
    list("2044-01-01", -0.102577025508582, 0.000747031374094744, 0.0796092516712301,
       0.0823103488296078, "2071-01-01", -0.183201673409137, -0.0278855575007965,
       0.0596241518411474, 0.0502215747235371, "2097-01-01", -0.0288787250983584,
       0.0549073182209395, 0.103338578736646, 0.22629787786611),
    list("2018-07-02", -0.102576438405298, 0.000726466482101307, 0.0795858032722369,
       0.0823088362772076, "2019-01-07", -0.183203980495524, -0.0278911157685207,
       0.0596231040299391, 0.0502187402975552, "2019-07-08", -0.0288794834942642,
       0.0549111163785803, 0.10334759494585, 0.226317381592537),
    list("2018-01-27", -0.102576438405298, 0.000726466482101307, 0.0795858032722369,
       0.0823088362772076, "2018-02-23", -0.183203980495524, -0.0278911157685207,
       0.0596231040299391, 0.0502187402975552, "2018-03-21", -0.0288794834942642,
       0.0549111163785803, 0.10334759494585, 0.226317381592537),
    list("2018-01-02 02:00:00", -0.102576438405298, 0.000726466482101307,
       0.0795858032722369, 0.0823088362772076, "2018-01-03 05:00:00",
       -0.183203980495524, -0.0278911157685207, 0.0596231040299391,
       0.0502187402975552, "2018-01-04 07:00:00", -0.0288794834942642,
       0.0549111163785803, 0.10334759494585, 0.226317381592537),
    list("2018-01-01 00:26:00", -0.102576438405298, 0.000726466482101307,
       0.0795858032722369, 0.0823088362772076, "2018-01-01 00:53:00",
       -0.183203980495524, -0.0278911157685207, 0.0596231040299391,
       0.0502187402975552, "2018-01-01 01:19:00", -0.0288794834942642,
       0.0549111163785803, 0.10334759494585, 0.226317381592537),
    list("2018-01-01 00:00:26", -0.102576438405298, 0.000726466482101307,
       0.0795858032722369, 0.0823088362772076, "2018-01-01 00:00:53",
       -0.183203980495524, -0.0278911157685207, 0.0596231040299391,
       0.0502187402975552, "2018-01-01 00:01:19", -0.0288794834942642,
       0.0549111163785803, 0.10334759494585, 0.226317381592537))
  
  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetChangePointTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   testList[[i]])
  }
})

test_that("Changepoint Posterior Summary Table results match (variable)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$changepoints <- "isCpNum"
  options$mcmcSamples <- 10
  options$changePointTable <- TRUE
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetChangePointTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("2018-01-10", 0.0127051093923528, 0.0831327133770695, 0.0491114671753479,
       0.156088809121233, "2018-01-20", -0.0750749206301101, -0.00204329702446798,
       0.0491854981247655, 0.046804280250494, "2018-01-30", -0.048237056341768,
       0.00491587751294654, 0.0329336154074312, 0.0652877892363412,
       "2018-02-09", -0.112020889098908, 0.0216849993978073, 0.0743106404239403,
       0.103181999004019, "2018-02-19", -0.200233407325906, 0.0280734843940406,
       0.156023651929321, 0.238766234287812, "2018-03-01", -0.0674611836206225,
       0.0216592623752553, 0.0709709595673843, 0.132015135706003, "2018-03-11",
       -0.0628389749530595, 0.0129751054272318, 0.0424273723514215,
       0.0773785622746723, "2018-03-21", -0.0380991688482681, -0.0067096567485451,
       0.0207038612683172, 0.0198544744564775))
})

test_that("Changepoint Posterior Summary Table results match (MAP)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$estimation <- "map"
  options$predictionIntervalSamples <- 10
  options$changePointTable <- TRUE
  options$maxChangepoints <- 3
  options$predictionSavePath <- ""
  
  testList <- list(
    list(-4.6571455544865e-09, "2044-01-01", -5.36239276854111e-10, "2071-01-01",
       6.72791321806394e-10, "2097-01-01"),
    list(1.40971557048972e-08, "2018-07-02", 7.14102900553978e-10, "2019-01-07",
       4.11746384728716e-08, "2019-07-08"),
    list(1.40971557048972e-08, "2018-01-27", 7.14102900553978e-10, "2018-02-23",
       4.11746384728716e-08, "2018-03-21"),
    list(1.40971557048972e-08, "2018-01-02 02:00:00", 7.14102900553978e-10,
       "2018-01-03 05:00:00", 4.11746384728716e-08, "2018-01-04 07:00:00"),
    list(1.40971557048972e-08, "2018-01-01 00:26:00", 7.14102900553978e-10,
       "2018-01-01 00:53:00", 4.11746384728716e-08, "2018-01-01 01:19:00"),
    list(1.40971557048972e-08, "2018-01-01 00:00:26", 7.14102900553978e-10,
       "2018-01-01 00:00:53", 4.11746384728716e-08, "2018-01-01 00:01:19"))
  
  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetChangePointTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   testList[[i]])
  }
})

test_that("Simulated Historical Forecasts Table results match", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$mcmcSamples <- 10
  options$crossValidation <- TRUE
  options$crossValidationHorizon <- 7
  options$crossValidationPeriod <- 3
  options$crossValidationInitial <- 21
  options$performanceMetrics <- TRUE
  options$performanceMetricsRmse <- TRUE
  options$performanceMetricsMape <- TRUE
  options$predictionSavePath <- ""
  
  for (i in 2:6) { # except years because it doesn't work for cross validation
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    options$crossValidationUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetModelEvaluationTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
                                   list(1, 1.28406252543228, 2.85316090330143, 1.68913022094255, 2, 1.33018084622212,
                                        1.04096765635464, 1.02027822497329, 3, 1.68850160739862, 0.691904142086159,
                                        0.831807755485701, 4, 1.61230995495969, 2.04094174197637, 1.42861532330308,
                                        5, 2.10051030990559, 1.05898632602033, 1.02907061274741, 6,
                                        1.71614002471044, 0.858985092891789, 0.926814486772725, 7, 1.65452485446443,
                                        1.98305642274155, 1.40821036167951))
  }
})

test_that("Simulated Historical Forecasts Table results match (MAP)", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$mcmcSamples <- 10
  options$predictionIntervalSamples <- 10
  options$crossValidation <- TRUE
  options$crossValidationHorizon <- 7
  options$crossValidationPeriod <- 3
  options$crossValidationInitial <- 21
  options$performanceMetrics <- TRUE
  options$performanceMetricsRmse <- TRUE
  options$performanceMetricsMape <- TRUE
  options$estimation <- "map"
  options$predictionSavePath <- ""

  for (i in 2:6) { # except years because it doesn't work for cross validation
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    options$crossValidationUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    table <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetModelEvaluationTable"]][["data"]]
    jaspTools::expect_equal_tables(table,
      list(1, 1.08006422952046, 2.38069721000901, 1.54295081256954, 2, 0.995049383043513,
        0.85162716379271, 0.922836477276831, 3, 1.09618511598854, 0.695710023653661,
        0.834092335208555, 4, 1.16957533550296, 1.93882823473188, 1.3924181249653,
        5, 1.3407635522367, 0.889222149557317, 0.942985763178489, 6,
        1.10179540451944, 0.733504699435278, 0.856448888980118, 7, 1.1522358718067,
        1.84836466837207, 1.35954575810161))
  }
})

test_that("History Plot matches", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$historyPlot <- TRUE
  options$historyPlotShow <- "both"
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""

  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    plotName <- results[["results"]][["historyPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("history-plot-", dateTimeUnits[i]), dir="Prophet")
  }
})

test_that("Overall Forecast Plot matches", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$mcmcSamples <- 10
  options$forecastPlotsOverall <- TRUE
  options$forecastPlotsOverallAddData <- TRUE
  options$predictionSavePath <- ""

  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetForecastPlots"]][["collection"]][["prophetMainContainer_prophetForecastPlots_prophetOverallForecastPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("overall-forecast-plot-", dateTimeUnits[i]), dir="Prophet")
  }

  options$growth <- "logistic"
  options$capacity <- "contGamma"
  options$minimum <- "contNarrow"
  options$historyIndicator <- "histIdx"
  options$forecastPlotsOverallAddCapacity <- TRUE
  options$forecastPlotsOverallAddMinimum <- TRUE
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetForecastPlots"]][["collection"]][["prophetMainContainer_prophetForecastPlots_prophetOverallForecastPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "overall-forecast-plot-logistic", dir="Prophet")
})

test_that("Trend Forecast Plot matches", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$mcmcSamples <- 10
  options$forecastPlotsTrend <- TRUE
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetForecastPlots"]][["collection"]][["prophetMainContainer_prophetForecastPlots_prophetTrendForecastPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "trend-forecast-plot", dir="Prophet")
})

test_that("Seasonality Plot matches", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$mcmcSamples <- 10
  options$seasonalityPlots <- "custom"
  options$predictionSavePath <- ""

  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    options$seasonalities <- list(list(name = "custom",
                                        period = 4,
                                        unit = dateTimeUnits[i],
                                        priorSigma = 10,
                                        fourierOrder = 3,
                                        mode = "additive"))
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetSeasonalityPlots"]][["collection"]][["prophetMainContainer_prophetSeasonalityPlots_custom"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("custom-seasonality-plot-", dateTimeUnits[i]), dir="Prophet")
  }

  options$time <- "dateDay"
  options$periodicalPredictionUnit <- "days"
  options$seasonalities <- list(list(name = "custom",
                                        period = 4,
                                        unit = "days",
                                        priorSigma = 10,
                                        fourierOrder = 3,
                                        mode = "multiplicative"))
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetSeasonalityPlots"]][["collection"]][["prophetMainContainer_prophetSeasonalityPlots_custom"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "custom-seasonality-plot-multi", dir="Prophet")
})

test_that("Covariate Plot matches", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$historyIndicator <- "histIdx"
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""
  options$covariatePlots <- list(list(variable = "contcor1", covariatePlotsShow = "both"))
  options$assignedCovariates <- list(list(
    variable = "contcor1",
    priorSigma = 10,
    standardize = TRUE,
    mode = "additive"))

  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
    plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetCovariatePlots"]][["collection"]][["prophetMainContainer_prophetCovariatePlots_contcor1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("covariate-plot-", dateTimeUnits[i]), dir="Prophet")
  }

  options$time <- "dateDay"
  options$periodicalPredictionUnit <- "days"
  options$assignedCovariates[[1]][["mode"]] <- "multiplicative"

  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetCovariatePlots"]][["collection"]][["prophetMainContainer_prophetCovariatePlots_contcor1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "covariate-plot-multi", dir="Prophet")
})

test_that("Performance Plots match", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$mcmcSamples <- 10
  options$crossValidation <- TRUE
  options$crossValidationHorizon <- 7
  options$crossValidationPeriod <- 3
  options$crossValidationInitial <- 21
  options$predictionSavePath <- ""
  options$performancePlotsMse <- TRUE
  options$performancePlotsRmse <- TRUE
  options$performancePlotsMape <- TRUE
  
  for (i in 2:6) { # except years because it doesn't work for cross validation
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    options$crossValidationUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)

    plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetEvaluationPlots"]][["collection"]][["prophetMainContainer_prophetEvaluationPlots_prophetPerformancePlotMse"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("mse", dateTimeUnits[i]), dir="Prophet")

    plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetEvaluationPlots"]][["collection"]][["prophetMainContainer_prophetEvaluationPlots_prophetPerformancePlotRmse"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("rmse", dateTimeUnits[i]), dir="Prophet")

    plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetEvaluationPlots"]][["collection"]][["prophetMainContainer_prophetEvaluationPlots_prophetPerformancePlotMape"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("mape", dateTimeUnits[i]), dir="Prophet")
  }
})

test_that("Changepoint Plot matches", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$mcmcSamples <- 10
  options$parameterPlotsDelta <- TRUE
  options$predictionSavePath <- ""

  for (i in 1:6) {
    options$time <- dateTimeVarNames[i]
    options$periodicalPredictionUnit <- dateTimeUnits[i]
    set.seed(1)
    results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)

    plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetParameterPlots"]][["collection"]][["prophetMainContainer_prophetParameterPlots_prophetParameterPlotDelta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, paste0("changepoint-plot", dateTimeUnits[i]), dir="Prophet")
  }
})

test_that("Parameter Plots match", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$mcmcSamples <- 10
  options$parameterPlotsMarginalDistributions <- TRUE
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)

  plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetParameterPlots"]][["collection"]][["prophetMainContainer_prophetParameterPlots_prophetParameterPlotMarginal"]][["collection"]][["prophetMainContainer_prophetParameterPlots_prophetParameterPlotMarginal_k"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "growth-rate", dir="Prophet")

  plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetParameterPlots"]][["collection"]][["prophetMainContainer_prophetParameterPlots_prophetParameterPlotMarginal"]][["collection"]][["prophetMainContainer_prophetParameterPlots_prophetParameterPlotMarginal_m"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "offset", dir="Prophet")

  plotName <- results[["results"]][["prophetMainContainer"]][["collection"]][["prophetMainContainer_prophetParameterPlots"]][["collection"]][["prophetMainContainer_prophetParameterPlots_prophetParameterPlotMarginal"]][["collection"]][["prophetMainContainer_prophetParameterPlots_prophetParameterPlotMarginal_sigma_obs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "residual-variance", dir="Prophet")
})

test_that("Analysis handels errors", {
  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "debString"
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Time' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)")

  options$time <- "dateDay"
  options$predictionType <- "nonperiodicalPrediction"
  options$nonperiodicalPredictionStart <- "xxxx"
  options$nonperiodicalPredictionEnd <- "2018-01-03"
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Start' for nonperiodical prediction must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)")

  options$nonperiodicalPredictionStart <- "2018-01-03"
  options$nonperiodicalPredictionEnd <- "xxxx"
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'End' for nonperiodical prediction must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)")

  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "debString"
  options$changepoints <- "contBinom"
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Changepoints' must be a logical variable (e.g., 0/1)")

  options$changepoints <- ""
  options$historyIndicator <- "contBinom"
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'History Indicator' must be a logical variable (e.g., 0/1)")

  options$historyIndicator <- ""
  options$capacity <- "contcor1"
  options$minimum <- "contNarrow"
  options$historyIndicator <- "histIdx"
  options$growth <- "logistic"
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Carrying Capacity' must always be larger than 'Saturating Minimum'")

  options$minimum <- ""
  options$constantMinimum <- 0
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Carrying Capacity' must always be larger than 'Constant saturating minimum'")

  options$capacity <- ""
  options$constantCapacity <- -1
  options$minimum <- "contNarrow"
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Constant carrying capacity' must always be larger than 'Saturating Minimum'")

  options$minimum <- ""
  options$historyIndicator <- ""
  options$constantMinimum <- 0
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Constant carrying capacity' must always be larger than 'Constant saturating minimum'")

  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""
  options$seasonalities <- list(list(name = "---",
                                     period = 4,
                                     unit = "days",
                                     priorSigma = 10,
                                     fourierOrder = 3,
                                     mode = "additive"))
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "Seasonality names must only contain letters, number, dots, or underscores and must start with letters or dots that are not followed by a number")

  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$capacity <- "contGamma"
  options$growth <- "logistic"
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Carrying Capacity' must be supplied for predictions")

  options <- jaspTools::analysisOptions("Prophet")
  options$dependent <- "contNormal"
  options$time <- "dateDay"
  options$covariates <- list("contcor1")
  options$mcmcSamples <- 10
  options$predictionSavePath <- ""
  set.seed(1)
  results <- jaspTools::runAnalysis("Prophet", "prophetTest.csv", options)
  expect_identical(results[["status"]], "validationError", label = "'Covariates' must be supplied for predictions")
})
