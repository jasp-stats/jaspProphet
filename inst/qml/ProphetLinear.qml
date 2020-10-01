import QtQuick 			2.8
import QtQuick.Layouts 	1.3
import JASP.Controls 	1.0
import JASP.Widgets 	1.0

Form
{
    VariablesForm
    {
        AvailableVariablesList  { name: "allVariablesList" }
        AssignedVariablesList   { name: "dependent";	title: qsTr("Dependent Variable");	suggestedColumns: ["scale"]; singleVariable: true               }
        AssignedVariablesList   { name: "time";         title: qsTr("Time");                suggestedColumns: ["ordinal", "nominal"]; singleVariable: true	}
        AssignedVariablesList   { name: "changepoints"; title: qsTr("Changepoints");        suggestedColumns: ["ordinal", "nominal"]; singleVariable: true  }
        AssignedVariablesList   { name: "covariates";	title: qsTr("Covariates");			suggestedColumns: ["scale"]; id: covs                           }
    }

    CheckBox
    {
        name: "historyPlot"
        label: qsTr("History plot")
        TextField
        {
            name: "historyPlotStart"
            label: qsTr("From date")
            placeholderText: "yyyy-mm-dd"
            fieldWidth: 100
        }
        TextField
        {
            name: "historyPlotEnd"
            label: qsTr("To date")
            placeholderText: "yyyy-mm-dd"
            fieldWidth: 100
        }
    }

    Section
    {
        title: qsTr("Model")

        Group
        {
            title: qsTr("Automatic Changepoints")

            IntegerField
            {
                name: "maxChangepoints"
                label: qsTr("Max. changepoints")
                defaultValue: 25
            }

            DoubleField
            {
                name: "changepointRange"
                label: qsTr("Changepoint range")
                defaultValue: 0.8
                decimals: 2
                max: 1
            }

            DoubleField
            {
                name: "changepointPriorScale"
                label: qsTr("Laplace prior tau")
                defaultValue: 0.05
                decimals: 3
            }
        }

        Group
        {
            title: qsTr("Components")

            CheckBox
            {
                name: "yearlySeasonality"
                id: yearly
                label: qsTr("Yearly seasonality")
                DoubleField
                {
                    name: "yearlySeasonalityPriorScale"
                    label: qsTr("Normal prior sigma")
                    defaultValue: 10
                    decimals: 3
                    visible: yearly.checked
                }
                RadioButtonGroup
                {
                    name: "yearlySeasonalityMode"
                    title: qsTr("Mode")
                    visible: yearly.checked
                    RadioButton { value: "additive";        label: qsTr("Additive"); checked: true  }
                    RadioButton { value: "multiplicative";  label: qsTr("Multiplicative")           }
                }
                CheckBox
                {
                    name: "yearlySeasonalityCustom"
                    id: yearlyCustom
                    label: qsTr("Custom")
                    visible: yearly.checked
                    IntegerField
                    {
                        name: "yearlyCustomTerms"
                        label: qsTr("Number of Fourier terms")
                        defaultValue: 1
                        visible: yearly.checked && yearlyCustom.checked
                    }
                }
            }

            CheckBox
            {
                name: "weeklySeasonality"
                id: weekly
                label: qsTr("Weekly seasonality")
                DoubleField
                {
                    name: "weeklySeasonalityPriorScale"
                    label: qsTr("Normal prior sigma")
                    defaultValue: 10
                    decimals: 3
                    visible: weekly.checked
                }
                RadioButtonGroup
                {
                    name: "weeklySeasonalityMode"
                    title: qsTr("Mode")
                    visible: weekly.checked
                    RadioButton { value: "additive";        label: qsTr("Additive"); checked: true  }
                    RadioButton { value: "multiplicative";  label: qsTr("Multiplicative")           }
                }
                CheckBox
                {
                    name: "weeklySeasonalityCustom"
                    id: weeklyCustom
                    label: qsTr("Custom")
                    visible: weekly.checked
                    IntegerField
                    {
                        name: "weeklyCustomTerms"
                        label: qsTr("Number of Fourier terms")
                        defaultValue: 1
                        visible: weekly.checked && weeklyCustom.checked
                    }
                }
            }

            CheckBox
            {
                name: "dailySeasonality"
                id: daily
                label: qsTr("Daily seasonality")
                DoubleField
                {
                    name: "dailySeasonalityPriorScale"
                    label: qsTr("Normal prior sigma")
                    defaultValue: 10
                    decimals: 3
                    visible: daily.checked
                }
                RadioButtonGroup
                {
                    name: "dailySeasonalityMode"
                    title: qsTr("Mode")
                    visible: daily.checked
                    RadioButton { value: "additive";        label: qsTr("Additive"); checked: true  }
                    RadioButton { value: "multiplicative";  label: qsTr("Multiplicative")           }
                }
                CheckBox
                {
                    name: "dailySeasonalityCustom"
                    id: dailyCustom
                    label: qsTr("Custom")
                    visible: daily.checked
                    IntegerField
                    {
                        name: "dailyCustomTerms"
                        label: qsTr("Number of Fourier terms")
                        defaultValue: 1
                        visible: daily.checked && dailyCustom.checked
                    }
                }
            }
        }

        RadioButtonGroup
        {
            name: "estimation"
            title: qsTr("Estimation")
            RadioButton { value: "map";    label: "Maximum a posteriori"                       }
            RadioButton
            {
                value: "mcmc"
                id: mcmc
                label: qsTr("Markov chain Monte Carlo")
                checked: true
                IntegerField
                {
                    name: "mcmcSamples"
                    label: qsTr("Samples")
                    defaultValue: 1000
                }
            }
        }

        Group
        {
            title: qsTr("Uncertainty")
            CIField
            {
                name: "predictionIntervalWidth"
                label: qsTr("Prediction interval level")
                defaultValue: 80
            }
            IntegerField
            {
                name: "predictionIntervalSamples"
                label: qsTr("Prediction interval samples")
                defaultValue: 1000
            }
        }
    }

    Section
    {
        title: qsTr("Prediction")

        RadioButtonGroup
        {
            name: "predictionType"

            RadioButton
            {
                value: "periodicalPrediction"
                label: qsTr("Periodical")
                checked: true
                childrenOnSameRow: true
                IntegerField
                {
                    name: "periodicalPredictionNumber"
                    label: qsTr("Number of periods")
                    defaultValue: 1
                }
                DropDown
                {
                    name: "periodicalPredictionUnit"
                    indexDefaultValue: 0
                    values:
                    [
                        { label: "Days",    value: "days"},
                        { label: "Weeks",   value: "weeks"},
                        { label: "Years",   value: "years"}
                    ]
                }
            }
            RadioButton
            {
                value: "nonperiodicalPrediction"
                label: qsTr("Nonperiodical")
                TextField
                {
                    name: "nonperiodicalPredictionStart"
                    label: qsTr("Start date")
                    placeholderText: "yyyy-mm-dd"
                    fieldWidth: 100
                }
                TextField
                {
                    name: "nonperiodicalPredictionEnd"
                    label: qsTr("End date")
                    placeholderText: "yyyy-mm-dd"
                    fieldWidth: 100
                }
            }
        }
        Button
        {
            id: 			exportPredictions
            anchors.right: 	parent.right
            anchors.bottom: parent.bottom
            text: 			qsTr("<b>Export predictions</b>")

            onClicked:
            {
                form.exportResults()
            }
        }
    }

    Section
    {
        title: qsTr("Evaluation")

        Group
        {
            title: qsTr("Simulated Historical Forecasts")
            DropDown
            {
                name: "crossValidationUnit"
                label: qsTr("Unit")
                indexDefaultValue: 0
                values:
                [
                    { label: "Days",    value: "days"},
                    { label: "Weeks",   value: "weeks"},
                    { label: "Years",   value: "years"}
                ]
            }
            IntegerField
            {
                name: "crossValidationHorizon"
                id: horizon
                label: qsTr("Horizon")
                defaultValue: 3
            }
            IntegerField
            {
                name: "crossValidationPeriod"
                label: qsTr("Period between cutoffs")
                defaultValue: 0.5*horizon.value
            }
            IntegerField
            {
                name: "crossValidationInitial"
                label: qsTr("Initial training time")
                defaultValue: 3*horizon.value
            }
        }

        Group
        {
            title: qsTr("Performance Metrics")
            enabled: horizon.value > 0
            CheckBox
            {
                name: "performanceMetricsMse"
                label: qsTr("Mean squared error (MSE)")
                checked: true
            }
            CheckBox
            {
                name: "performanceMetricsRmse"
                label: qsTr("Root mean squared error (RMSE)")
            }
            CheckBox
            {
                name: "performanceMetricsMape"
                label: qsTr("Mean absolute percentage error (MAPE)")
            }
        }
    }

    Section
    {
        title: qsTr("Plots")

        Group
        {
            title: qsTr("Forecast Plots")
            CheckBox
            {
                name: "forecastPlotsOverall"
                label: qsTr("Overall")
                CheckBox
                {
                    name: "forecastPlotsOverallAddData"
                    label: qsTr("Show data points")
                }
                CheckBox
                {
                    name: "forecastPlotsOverallAddCovariates"
                    label: qsTr("Show covariates")
                    id: showCovs
                    visible: covs.count > 0
                    CheckBox
                    {
                        name: "forecastPlotsOverallAddCovariateLabels"
                        label: qsTr("Add labels")
                        visible: showCovs.checked
                    }
                }
                TextField
                {
                    name: "forecastPlotsOverallStart"
                    label: qsTr("From date")
                    placeholderText: "yyyy-mm-dd"
                    fieldWidth: 100
                }
                TextField
                {
                    name: "forecastPlotsOverallEnd"
                    label: qsTr("To date")
                    placeholderText: "yyyy-mm-dd"
                    fieldWidth: 100
                }
            }
            CheckBox
            {
                name: "forecastPlotsTrend"
                label: qsTr("Trend component")
            }
            CheckBox
            {
                name: "forecastPlotsYearly"
                label: qsTr("Yearly component")
                enabled: yearly.checked
            }
            CheckBox
            {
                name: "forecastPlotsWeekly"
                label: qsTr("Weekly component")
                enabled: weekly.checked
            }
            CheckBox
            {
                name: "forecastPlotsDaily"
                label: qsTr("Daily component")
                enabled: daily.checked
            }
        }

        Group
        {
            title: qsTr("Performance Plots")
            CheckBox
            {
                name: "performancePlotsMse"
                label: qsTr("Mean squared error (MSE)")
            }
            CheckBox
            {
                name: "performancePlotsRmse"
                label: qsTr("Root mean squared error (RMSE)")
            }
            CheckBox
            {
                name: "performancePlotsMape"
                label: qsTr("Mean absolute percentage error (MAPE)")
            }
        }

        Group
        {
            title: qsTr("Parameter Plots")
            CheckBox
            {
                name: "parameterPlotsDelta"
                label: qsTr("Changepoint plot")
            }
            CheckBox
            {
                name: "parameterPlotsBeta"
                label: qsTr("Fourier terms plot")
            }
            CheckBox
            {
                name: "parameterPlotsMarginalDistributions"
                label: qsTr("Posterior distributions")
                enabled: mcmc.checked
            }
        }
    }
}
