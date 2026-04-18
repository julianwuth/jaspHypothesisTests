//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP.Widgets
import JASP

Form
{
	RadioButtonGroup
	{
		name:    "inputType"
		title:   qsTr("Input")
		columns: 2

		RadioButton
		{
			value:   "rawData"
			label:   qsTr("Raw data")
			checked: true
			id:      inputRawData
		}

		RadioButton
		{
			value: "summarized"
			label: qsTr("Summarized data")
			id:    inputSummarized
		}
	}

	VariablesForm
	{
		infoLabel:       qsTr("Input")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		enabled:         inputRawData.checked

		AvailableVariablesList { name: "allVariablesList" }

		AssignedVariablesList
		{
			name:           "count"
			title:          qsTr("Count variable")
			singleVariable: true
			allowedColumns: ["scale"]
			info:           qsTr("A variable containing the observed event counts.")
		}

		AssignedVariablesList
		{
			name:           "time"
			title:          qsTr("Time variable (optional)")
			singleVariable: true
			allowedColumns: ["scale"]
			info:           qsTr("A variable containing the observation time or exposure for each row. If omitted, each row is assumed to contribute one unit of time.")
		}
	}

	Group
	{
		title:   qsTr("Summarized data")
		enabled: inputSummarized.checked
		columns: 2

		IntegerField
		{
			name:         "observedOccurrences"
			label:        qsTr("Number of occurrences")
			defaultValue: 1
			min:          0
			info:         qsTr("Total number of observed occurrences.")
		}

		DoubleField
		{
			name:         "sampleSize"
			label:        qsTr("Sample size")
			defaultValue: 1
			min:          0
			decimals:     4
			inclusive:    JASP.MaxOnly
			info:         qsTr("Total number of observations.")
		}
	}

	Group
	{
		title:   qsTr("Tests")
		columns: 2

		CheckBox
		{
			name:    "exactTest"
			label:   qsTr("Exact")
			checked: true
			info:    qsTr("Exact Poisson test based on the Poisson distribution (Garwood intervals for CI).")
		}

		CheckBox
		{
			name:  "normalApprox"
			label: qsTr("Normal approximation")
			info:  qsTr("Large-sample normal approximation using the score test statistic.")
		}
	}

	DoubleField
	{
		name:         "testRate"
		label:        qsTr("Hypothesized rate:")
		defaultValue: 1
		min:          0
		decimals:     4
		inclusive:    JASP.MaxOnly
		info:         qsTr("The null hypothesis rate (events per unit time).")
	}

	RadioButtonGroup
	{
		name:  "alternative"
		title: qsTr("Alternative Hypothesis")

		RadioButton
		{
			value:   "two.sided"
			label:   qsTr("≠ Hypothesized rate")
			checked: true
			info:    qsTr("Two-sided test: rate differs from the hypothesized value.")
		}

		RadioButton
		{
			value: "greater"
			label: qsTr("> Hypothesized rate")
			info:  qsTr("One-sided test: rate is greater than the hypothesized value.")
		}

		RadioButton
		{
			value: "less"
			label: qsTr("< Hypothesized rate")
			info:  qsTr("One-sided test: rate is less than the hypothesized value.")
		}
	}

	Group
	{
		title: qsTr("Additional Statistics")

		CheckBox
		{
			name:              "rateCi"
			label:             qsTr("Confidence interval")
			childrenOnSameRow: true
			info:              qsTr("Confidence interval for the event rate. Uses the method of the corresponding row in the test table.")

			CIField { name: "confLevel" }
		}
	}
}
