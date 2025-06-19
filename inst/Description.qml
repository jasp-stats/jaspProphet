import QtQuick
import JASP.Module

Description
{
	name		:	"jaspProphet"
	title		:	qsTr("Prophet")
	description	:	qsTr("This module offers a simple model for time series prediction.")
	version			: "0.95.0"
	author		:	"JASP Team"
	maintainer	:	"JASP Team <info@jasp-stats.org>"
	website		:	"jasp-stats.org"
	license		:	"GPL (>= 2)"
	icon		:	"analysis-prophet.svg"

	Analysis
	{
		title	:	qsTr("Prophet")
		func	:	"Prophet"
		qml		:	"Prophet.qml"
	}
}
