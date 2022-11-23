import QtQuick          2.12
import JASP.Module      1.0

Upgrades
{
	Upgrade
	{
		functionName: 	"Prophet"
		fromVersion: 	"0.16.4"
		toVersion:		"0.17.0"

		ChangeRename { from: "capacity";    to: "carryingCapacity"}

		ChangeRename { from: "historyPlotShow"; to: "historyPlotType"}

		ChangeRename { from: "constantCapacity";    to: "logisticGrowthCarryingCapacity"    }
		ChangeRename { from: "constantMinimum";     to: "logisticGrowthSaturatingMin"       }

		ChangeRename { from: "predictionIntervalWidth"; to: "predictionIntervalLevel"   }
		ChangeRename { from: "credibleIntervalWidth";   to: "ciLevel"                   }

		ChangeRename { from: "forecastPlotsOverall";                    to: "forecastPlotOverall"  }
		ChangeRename { from: "forecastPlotsOverallAddData";             to: "forecastPlotOverallDataPoints"  }
		ChangeRename { from: "forecastPlotsOverallAddCapacity";         to: "forecastPlotOverallCarryingCapacity"  }
		ChangeRename { from: "forecastPlotsOverallAddChangepoints";     to: "forecastPlotOverallChangepoints"  }
		ChangeRename { from: "forecastPlotsOverallAddMinimum";          to: "forecastPlotOverallSaturatingMinimum"  }
		ChangeRename { from: "forecastPlotsOverallRange";               to: "forecastPlotOverallRange"  }
		ChangeRename { from: "forecastPlotsOverallStart";               to: "forecastPlotOverallStart"  }
		ChangeRename { from: "forecastPlotsOverallEnd";                 to: "forecastPlotOverallEnd"  }

		ChangeRename { from: "forecastPlotsTrend";                    to: "forecastPlotTrend"  }
		ChangeRename { from: "forecastPlotsTrendAddChangepoints";     to: "forecastPlotTrendChangepoints"  }
		ChangeRename { from: "forecastPlotsTrendRange";               to: "forecastPlotTrendRange"  }
		ChangeRename { from: "forecastPlotsTrendStart";               to: "forecastPlotTrendStart"  }
		ChangeRename { from: "forecastPlotsTrendEnd";                 to: "forecastPlotTrendEnd"  }

		ChangeJS
		{
			name: "covariatePlots"
			jsFunction: function(options) 
			{
				let newModels = options["covariatePlots"].map(model => {
					let newModel = {};
					newModel.covariatePlotsType	= model.covariatePlotsShow;
					newModel.variable			= model.variable;

					return newModel ;
				});

				return newModels;
			}
		}

		ChangeRename { from: "performancePlotsMse";                  to: "msePlot"}
		ChangeRename { from: "performancePlotsRmse";                to: "rmsePlot"}
		ChangeRename { from: "performancePlotsMape";                to: "mapePlot"}
		ChangeRename { from: "parameterPlotsDelta";                 to: "changepointPlot"}
		ChangeRename { from: "parameterPlotsMarginalDistributions"; to: "posteriorPlot"}
	}
}