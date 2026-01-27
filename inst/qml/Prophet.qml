import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Form
{
	VariablesForm
	{
		preferredHeight: 400 * preferencesModel.uiScale
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent"; title: qsTr("Dependent Variable");	allowedColumns: ["scale"]; singleVariable: true													}
		AssignedVariablesList	{ name: "time"; title: qsTr("Time"); allowedColumns: ["nominal"]; singleVariable: true																	}
		AssignedVariablesList	{ name: "changepoints"; title: qsTr("Changepoints"); allowedColumns: ["scale"]; singleVariable: true														}
		AssignedVariablesList	{ name: "carryingCapacity"; title: qsTr("Carrying Capacity"); allowedColumns: ["scale"]; singleVariable: true; id: cap; enabled: growth.value === "logistic"		}
		AssignedVariablesList	{ name: "minimum"; title: qsTr("Saturating Minimum"); allowedColumns: ["scale"]; singleVariable: true; id: floor; enabled: growth.value === "logistic"	}
		AssignedVariablesList	{ name: "covariates"; title: qsTr("Covariates"); allowedColumns: ["scale"];																				}
		AssignedVariablesList	{ name: "historyIndicator"; title: qsTr("Include in Training"); allowedColumns: ["scale"]; singleVariable: true											}
	}

	columns: 3

	CheckBox
	{
		name: "historyPlot"
		label: qsTr("History plot")
		id: histplot

		RadioButtonGroup
		{
			name: "historyPlotType"
			visible: histplot.checked
			columns: 3

			RadioButton
			{
				value: "points"
				label: qsTr("Points")
				checked: true
			}
			RadioButton
			{
				value: "line"
				label: qsTr("Line")
			}
			RadioButton
			{
				value: "both"
				label: qsTr("Both")
			}
		}

		CheckBox
		{
			name: "historyPlotRange"
			label: qsTr("Plot time interval")

			Group
			{
				TextField
				{
					name: "historyPlotStart"
					label: qsTr("Start")
					placeholderText: "YYYY-MM-DD HH:MM:SS"
					fieldWidth: 150
				}
				TextField
				{
					name: "historyPlotEnd"
					label: qsTr("End")
					placeholderText: "YYYY-MM-DD HH:MM:SS"
					fieldWidth: 150
				}
			}
		}
	}

	RadioButtonGroup
	{
		name: "growth"
		id: growth
		title: qsTr("Growth")
		RadioButton
		{
			value: "linear"
			label: qsTr("Linear")
			checked: true
		}
		RadioButton
		{
			value: "logistic"
			label: qsTr("Logistic")
			childrenOnSameRow: false

			Group
			{
				DoubleField
				{
					name: "logisticGrowthCarryingCapacity"
					label: qsTr("Carrying capacity")
					enabled: cap.count === 0
					visible: growth.value === "logistic"
					negativeValues: true
					fieldWidth: 100
				}
				DoubleField
				{
					name: "logisticGrowthSaturatingMin"
					label: qsTr("Saturating minimum")
					enabled: floor.count === 0
					visible: growth.value === "logistic"
					negativeValues: true
					fieldWidth: 100
				}
			}
		}
	}

	Section
	{
		title: qsTr("Model")
		columns: 1
		Group
		{
			columns: 3
			Group
			{
				title: qsTr("Automatic Changepoints")

				IntegerField
				{
					name: "maxChangepoints"
					label: qsTr("Max. changepoints")
					defaultValue: Math.min(25, dataSetInfo.rowCount)
					max: dataSetInfo.rowCount
				}
				DoubleField
				{
					name: "changepointRange"
					label: qsTr("Changepoint range")
					defaultValue: 0.8
					decimals: 2
					min: 0
					max: 1
				}
				DoubleField
				{
					name: "changepointPriorScale"
					label: qsTr("Laplace prior τ")
					defaultValue: 0.05
					decimals: 3
                    inclusive: JASP.MaxOnly
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
                    label: qsTr("Maximum a posteriori")
                }
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
						inclusive: JASP.MaxOnly
					}
				}
			}

			Group
			{
				title: qsTr("Uncertainty")
				CIField
				{
					name: "predictionIntervalLevel"
					label: qsTr("Prediction interval level")
					defaultValue: 80
				}
				IntegerField
				{
					name: "predictionIntervalSamples"
					label: qsTr("Prediction interval samples")
					visible: map.checked
					defaultValue: 1000
					inclusive: JASP.MaxOnly
				}
				CIField
				{
					name: "ciLevel"
					label: qsTr("Credible interval level")
					visible: mcmc.checked
					defaultValue: 95
				}
			}
		}

		VariablesList
		{
			name: "assignedCovariates"
			source: "covariates"
			listViewType: JASP.AssignedVariables
			preferredHeight: 100 * preferencesModel.uiScale
			draggable: false

			title: qsTr("Covariates                                                  Normal prior σ²                 Standardize                 Mode")
			rowComponent: Row
			{
				spacing: 100 * preferencesModel.uiScale
				DoubleField
				{
					name: "priorSigma"
					defaultValue: 10.0
                    inclusive: JASP.MaxOnly
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

		Group
		{
			title: qsTr("Seasonalities")

			ColumnLayout
			{
				spacing: 0 * preferencesModel.uiScale
				RowLayout
				{
					Label { text: qsTr("Name"); Layout.preferredWidth: 100 * preferencesModel.uiScale				}
					Label { text: qsTr("Period"); Layout.preferredWidth: 45 * preferencesModel.uiScale				}
					Label { text: qsTr("Unit"); Layout.preferredWidth: 80 * preferencesModel.uiScale				}
					Label { text: qsTr("Normal prior σ²"); Layout.preferredWidth: 100 * preferencesModel.uiScale	}
					Label { text: qsTr("Fourier order"); Layout.preferredWidth: 70 * preferencesModel.uiScale		}
					Label { text: qsTr("Mode"); Layout.preferredWidth: 122 * preferencesModel.uiScale				}
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
							Layout.preferredWidth: 45 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							DoubleField
							{
								name: "period"
								defaultValue: 1
                                inclusive: JASP.MaxOnly
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
									{ label: qsTr("Seconds"), value: "secs"		},
									{ label: qsTr("Minutes"), value: "mins"		},
									{ label: qsTr("Hours"),   value: "hours"	},
									{ label: qsTr("Days"),    value: "days"		},
									{ label: qsTr("Weeks"),   value: "weeks"	},
									{ label: qsTr("Years"),   value: "years"	}
								]
							}
						}
						Row
						{
							Layout.preferredWidth: 100 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							DoubleField
							{
								name: "priorSigma"
								defaultValue: 10.0
								inclusive: JASP.MaxOnly
							}
						}
						Row
						{
							Layout.preferredWidth: 70 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

							IntegerField
							{
								name: "fourierOrder"
								defaultValue: 7
								inclusive: JASP.MaxOnly
							}
						}
						Row
						{
							Layout.preferredWidth: 122 * preferencesModel.uiScale
							spacing: 4 * preferencesModel.uiScale

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
		}
	}

	Section
	{
		title: qsTr("Prediction")
		columns: 2

		RadioButtonGroup
		{
			name: "predictionType"

			RadioButton
			{
				value: "periodicalPrediction"
				label: qsTr("Periodical")
				checked: true
				IntegerField
				{
					name: "periodicalPredictionNumber"
					label: qsTr("Number of periods")
					defaultValue: 0
				}
				DropDown
				{
					name: "periodicalPredictionUnit"
					label: qsTr("Unit")
					indexDefaultValue: 3
					values:
					[
						{ label: qsTr("Seconds"), value: "secs"		},
						{ label: qsTr("Minutes"), value: "mins"		},
						{ label: qsTr("Hours"),   value: "hours"	},
						{ label: qsTr("Days"),    value: "days"		},
						{ label: qsTr("Weeks"),   value: "weeks"	},
						{ label: qsTr("Years"),   value: "years"	}
					]
				}
			}
			RadioButton
			{
				value: "nonperiodicalPrediction"
				label: qsTr("Nonperiodical")
				Group
				{
					TextField
					{
						name: "nonperiodicalPredictionStart"
						label: qsTr("Start")
						placeholderText: "YYYY-MM-DD HH:MM:SS"
						fieldWidth: 150
					}
					TextField
					{
						name: "nonperiodicalPredictionEnd"
						label: qsTr("End")
						placeholderText: "YYYY-MM-DD HH:MM:SS"
						fieldWidth: 150
					}
					DropDown
					{
						name: "nonperiodicalPredictionUnit"
						label: qsTr("Unit")
						indexDefaultValue: 3
						values:
						[
							{ label: qsTr("Seconds"), value: "secs"		},
							{ label: qsTr("Minutes"), value: "mins"		},
							{ label: qsTr("Hours"),   value: "hours"	},
							{ label: qsTr("Days"),    value: "days"		},
							{ label: qsTr("Weeks"),   value: "weeks"	},
							{ label: qsTr("Years"),   value: "years"	}
						]
					}
				}
			}
		}
		FileSelector
		{
			name:	"predictionSavePath"
			label:	qsTr("Save predictions")
			filter:	"*.csv"
			save:	true
		}
	}

	Section
	{
		title: qsTr("Evaluation")

		CheckBox
		{
			name: "crossValidation"
			id: crossVal
			label: qsTr("Cross validation (simulated historical forecasts)")
			Group
			{
				DropDown
				{
					name: "crossValidationUnit"
					label: qsTr("Unit")
					indexDefaultValue: 3
					values:
					[
						{ label: qsTr("Seconds"), value: "secs"		},
						{ label: qsTr("Minutes"), value: "mins"		},
						{ label: qsTr("Hours"),   value: "hours"	},
						{ label: qsTr("Days"),    value: "days"		},
						{ label: qsTr("Weeks"),   value: "weeks"	}
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
					defaultValue: Math.floor(0.5*horizon.value)
				}
				IntegerField
				{
					name: "crossValidationInitial"
					label: qsTr("Initial training time")
					defaultValue: 3*horizon.value
				}
			}
		}

		CheckBox
		{
			name: "performanceMetrics"
			label: qsTr("Performance metrics")
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
				name: "forecastPlotOverall"
				label: qsTr("Overall")
				Group
				{
					columns: 2
					CheckBox
					{
						name: "forecastPlotOverallDataPoints"
						label: qsTr("Show data points")
					}
					CheckBox
					{
						name: "forecastPlotOverallCarryingCapacity"
						label: qsTr("Show carrying capacity")
						visible: growth.value === "logistic"
					}
					CheckBox
					{
						name: "forecastPlotOverallChangepoints"
						label: qsTr("Show changepoints")
					}
					CheckBox
					{
						name: "forecastPlotOverallSaturatingMinimum"
						label: qsTr("Show saturating minimum")
						visible: growth.value === "logistic"
					}
					CheckBox
					{
						name: "forecastPlotOverallRange"
						label: qsTr("Plot time interval")

						TextField
						{
							name: "forecastPlotOverallStart"
							label: qsTr("Start")
							placeholderText: "YYYY-MM-DD HH:MM:SS"
							fieldWidth: 150
						}
						TextField
						{
							name: "forecastPlotOverallEnd"
							label: qsTr("End")
							placeholderText: "YYYY-MM-DD HH:MM:SS"
							fieldWidth: 150
						}
					}
				}
			}
			CheckBox
			{
				name: "forecastPlotTrend"
				label: qsTr("Trend")
				CheckBox
				{
					name: "forecastPlotTrendChangepoints"
					label: qsTr("Show changepoints")
				}
				CheckBox
				{
					name: "forecastPlotTrendRange"
					label: qsTr("Plot time interval")

					Group
					{
						TextField
						{
							name: "forecastPlotTrendStart"
							label: qsTr("Start")
							placeholderText: "YYYY-MM-DD HH:MM:SS"
							fieldWidth: 150
						}
						TextField
						{
							name: "forecastPlotTrendEnd"
							label: qsTr("End")
							placeholderText: "YYYY-MM-DD HH:MM:SS"
							fieldWidth: 150
						}
					}
				}
			}
		}

		VariablesForm
		{
			preferredHeight: 0.5 * jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "seasonalityNames"; title: qsTr("Seasonalities"); source: "seasonalities.name"	}
			AssignedVariablesList { name: "seasonalityPlots"; title: qsTr("Seasonality Plots")								}
		}

		VariablesForm
		{
			preferredHeight: 0.5 * jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "covariateNames"; title: qsTr("Covariates"); source: "covariates"	}
			AssignedVariablesList
			{
				name: "covariatePlots"
				title: qsTr("Covariate Plots")
				rowComponent: Row
				{
					DropDown
					{
						name: "covariatePlotsType"
						indexDefaultValue: 2
						values:
						[
							{ label: qsTr("Points"), value: "points"	},
							{ label: qsTr("Line"), value: "line"		},
							{ label: qsTr("Both"), value: "both"		}
						]
					}
				}
			}
		}

		Group
		{
			columns: 2
			Group
			{
				title: qsTr("Performance Plots")
				CheckBox
				{
					name: "msePlot"
					label: qsTr("Mean squared error (MSE)")
					enabled: crossVal.checked
				}
				CheckBox
				{
					name: "rmsePlot"
					label: qsTr("Root mean squared error (RMSE)")
					enabled: crossVal.checked
				}
				CheckBox
				{
					name: "mapePlot"
					label: qsTr("Mean absolute percentage error (MAPE)")
					enabled: crossVal.checked
				}
			}

			Group
			{
				title: qsTr("Parameter Plots")
				CheckBox
				{
					name: "changepointPlot"
					label: qsTr("Changepoint plot")
				}
				CheckBox
				{
					name: "posteriorPlot"
					label: qsTr("Posterior distributions")
					enabled: mcmc.checked
				}
			}
		}
	}
}
