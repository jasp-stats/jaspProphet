import QtQuick 			2.8
import QtQuick.Layouts 	1.3
import JASP.Controls 	1.0
import JASP.Widgets 	1.0
import JASP				1.0

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
		title:      qsTr("Model")
		columns:    1
		
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
		
		RadioButtonGroup
		{
			name: "estimation"
			title: qsTr("Estimation")
			RadioButton
			{
				value: "map"
				id: map
				label: qsTr("Maximum a posteriori") }
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
					visible: mcmc.checked
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
				visible: map.checked
				defaultValue: 1000
			}
		}
		
		Group
		{
			title: qsTr("Covariates")
			
			VariablesForm
			{
				preferredHeight: 0.5 * jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList
				{ 
					name: "availableCovariates"
					source: ["covariates"]
					width: form.width * 0.2
				}
				AssignedVariablesList
				{
					name: "assignedCovariates"
					width: form.width * 0.6
					
					rowComponentTitle: qsTr("Mode")
					
					rowComponent: Row
					{
						spacing: 20 * preferencesModel.uiScale
						DoubleField
						{
							name: "priorSigma"
							defaultValue: 10.0
						}
						CheckBox
						{
							name: "standardize"
							checked: true
						}
						DropDown
						{
							name: "mode"
							indexDefaultValue: 0
							values:
							[
								{ label: qsTr("Additive"), value: "additive"				},
								{ label: qsTr("Multiplicative"), value: "multiplicative"	},
							]
						}
					}
				}
			}
		}
		
		ColumnLayout
		{
			spacing:				0
			Layout.preferredWidth:	parent.width
			
			Label { text: qsTr("Seasonalities"); Layout.preferredHeight: 20 * preferencesModel.uiScale }
			RowLayout
			{
				Label { text: qsTr("Name"); Layout.preferredWidth: 100 * preferencesModel.uiScale }
				Label { text: qsTr("Period"); Layout.preferredWidth: 50 * preferencesModel.uiScale }
				Label { text: qsTr("Unit"); Layout.preferredWidth: 80 * preferencesModel.uiScale }
				Label { text: qsTr("Normal prior sigma"); Layout.preferredWidth: 150 * preferencesModel.uiScale }
				Label { text: qsTr("Mode"); Layout.preferredWidth: 100 * preferencesModel.uiScale }
			}
			ComponentsList
			{
				name: "seasonalities"
				rowComponent: RowLayout
				{
					Row
					{
						Layout.preferredWidth: 100 * preferencesModel.uiScale
						spacing: 4 * preferencesModel.uiScale
						
						TextField
						{
							name: "name"
							fieldWidth: 100 * preferencesModel.uiScale
							placeholderText: "Yearly"
						}
					}
					Row
					{
						Layout.preferredWidth: 50 * preferencesModel.uiScale
						spacing: 4 * preferencesModel.uiScale
						
						DoubleField
						{
							name: "period"
							defaultValue: 1
						}
					}
					Row
					{
						Layout.preferredWidth: 80 * preferencesModel.uiScale
						spacing: 4 * preferencesModel.uiScale
						
						DropDown
						{
							name: "unit"
							indexDefaultValue: 0
							values:
								[
								{ label: qsTr("Hours"), value: "hours" },
								{ label: qsTr("Days"), value: "days" },
								{ label: qsTr("Weeks"), value: "weeks" },
								{ label: qsTr("Years"), value: "years" }
							]
						}
					}
					Row
					{
						Layout.preferredWidth: 150 * preferencesModel.uiScale
						spacing: 4 * preferencesModel.uiScale
						
						DoubleField
						{
							name: "priorSigma"
							defaultValue: 10.0
						}
					}
					Row
					{
						Layout.preferredWidth: 100 * preferencesModel.uiScale
						spacing: 4 * preferencesModel.uiScale
						
						DropDown
						{
							name: "mode"
							indexDefaultValue: 0
							values:
								[
								{ label: qsTr("Additive"), value: "additive" },
								{ label: qsTr("Multiplicative"), value: "multiplicative" },
							]
						}
					}
				}
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
					indexDefaultValue: 1
					values:
						[
						{ label: "Hours",   value: "hours"},
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
				DropDown
				{
					name: "nonperiodicalPredictionUnit"
					indexDefaultValue: 1
					values:
						[
						{ label: "Hours",   value: "hours"},
						{ label: "Days",    value: "days"},
						{ label: "Weeks",   value: "weeks"},
						{ label: "Years",   value: "years"}
					]
				}
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
		FileSelector
		{
			Layout.columnSpan:  2
			label:  qsTr("Save Predictions")
			name:   "predictionSavePath"
			filter: "*.csv"
			save:   true
		}
	}
	
	Section
	{
		title: qsTr("Evaluation")
		
		CheckBox
		{
			name: "crossValidation"
			id: crossVal
			label: qsTr("Simulated Historical Forecasts")
			DropDown
			{
				name: "crossValidationUnit"
				label: qsTr("Unit")
				indexDefaultValue: 0
				values:
					[
					{ label: "Hours",   value: "hours"},
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
		
		CheckBox
		{
			name: "performanceMetrics"
			label: qsTr("Performance Metrics")
			enabled: crossVal.checked
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
		
		CheckBox
		{
			name: "changePointTable"    ; label: qsTr("Changepoint table")
		}
	}
	
	Section
	{
		title: qsTr("Plots")
		columns: 1
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
				label: qsTr("Trend")
			}
		}
		
		Group
		{
			title: qsTr("Seasonality Plots")
			VariablesForm
			{
				preferredHeight: 0.5 * jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList { name: "seasonalityNames"; source: ["seasonalities.name"]	}
				AssignedVariablesList { name: "seasonalityPlots"									}
			}
		}
		
		Group
		{
			title: qsTr("Performance Plots")
			CheckBox
			{
				name: "performancePlotsMse"
				label: qsTr("Mean squared error (MSE)")
				enabled: crossVal.checked
			}
			CheckBox
			{
				name: "performancePlotsRmse"
				label: qsTr("Root mean squared error (RMSE)")
				enabled: crossVal.checked
			}
			CheckBox
			{
				name: "performancePlotsMape"
				label: qsTr("Mean absolute percentage error (MAPE)")
				enabled: crossVal.checked
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
				name: "parameterPlotsMarginalDistributions"
				label: qsTr("Posterior distributions")
				enabled: mcmc.checked
			}
		}
	}
}
