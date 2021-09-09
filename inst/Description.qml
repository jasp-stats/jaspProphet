import QtQuick      2.12
import JASP.Module  1.0

Description
{
    name		: "jaspProphet"
    title		: qsTr("Prophet (Beta)")
    description	: qsTr("This module offers a simple model for time series prediction.")
    version		: "0.15"
    author		: "JASP Team"
    maintainer	: "JASP Team <info@jasp-stats.org>"
    website		: "jasp-stats.org"
    license		: "GPL (>= 2)"
    icon        : "analysis-prophet.svg"

    Analysis
    {
        title   : qsTr("Prophet")
		func    : "Prophet"
		qml     : "Prophet.qml"
    }
}
