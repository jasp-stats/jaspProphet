import QtQuick      2.12
import JASP.Module  1.0

Description
{
    name		: "jaspProphet"
    title		: qsTr("Prophet")
    description	: qsTr("This module offers a simple model for time series prediction.")
    version		: "0.13"
    author		: "JASP Team"
    maintainer	: "JASP Team <info@jasp-stats.org>"
    website		: "jasp-stats.org"
    license		: "GPL (>= 2)"
    icon        : "blank_icon.svg"

    Package
    {
        name    : "bayestestR"
    }

    Package
    {
        name    : "prophet"
    }

    Analysis
    {
        title   : qsTr("Prophet")
        func    : "ProphetLinear"
        qml     : "ProphetLinear.qml"
    }
}
