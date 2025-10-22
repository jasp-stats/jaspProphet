import QtQuick
import JASP.Module

Description
{
	title		:	qsTr("Prophet")
	description	:	qsTr("This module offers a simple model for time series prediction.")
	icon		:	"analysis-prophet.svg"
	hasWrappers: 	false
	
	Analysis
	{
		title	:	qsTr("Prophet")
		func	:	"Prophet"
		qml		:	"Prophet.qml"
	}
}
