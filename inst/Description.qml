import QtQuick
import JASP.Module

Description
{
	name		: "jaspHypothesisTests"
	title		: qsTr("Hypothesis Tests")
	description	: qsTr("Module that bundles hypothesis tests.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon        : "exampleIcon.png" // Located in /inst/icons/
	preloadData: true
	requiresData: true

	GroupTitle
	{
		title: qsTr('Mean')
	}

	Analysis
	{
		title: qsTr("One Sample Tests") // Title for window
		menu: qsTr("One Sample Tests")  // Title for ribbon
		func: "oneSampleTests"           // Function to be called
		qml: "oneSampleTests.qml"               // Design input window
		requiresData: true                
	}
	

	Analysis
	{
		title: qsTr("Independent Samples Tests") // Title for window
		menu: qsTr("Independent Samples Tests")  // Title for ribbon
		func: "independentSamplesTests"           // Function to be called
		qml: "independentSamplesTests.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Paired Samples T-Test") // Title for window
		menu: qsTr("Paired Samples T-Test")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: true                
	}

	Separator{}

	Analysis
	{
		title: qsTr("Single Proportion") // Title for window
		menu: qsTr("Single Proportion")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Two Proportions") // Title for window
		menu: qsTr("Two Proportions")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("One Sample Poisson Rate") // Title for window
		menu: qsTr("One Sample Poisson Rate")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Two Sample Poisson Rate") // Title for window
		menu: qsTr("Two Sample Poisson Rate")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: true                
	}

	Separator{}

	Analysis
	{
		title: qsTr("Single Variance") // Title for window
		menu: qsTr("Single Variance")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: true                
	}

	Analysis
	{
		title: qsTr("Two Variances") // Title for window
		menu: qsTr("Two Variances")  // Title for ribbon
		func: "interfaceExample"           // Function to be called
		qml: "Interface.qml"               // Design input window
		requiresData: true                
	}
}
