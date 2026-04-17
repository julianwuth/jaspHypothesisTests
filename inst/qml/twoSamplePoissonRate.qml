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
			info:           qsTr("A variable containing the observed event counts per row.")
		}

		AssignedVariablesList
		{
			name:           "group"
			title:          qsTr("Group variable")
			singleVariable: true
			allowedColumns: ["nominal", "ordinal"]
			info:           qsTr("A variable with exactly 2 levels identifying the two groups.")
		}

		AssignedVariablesList
		{
			name:           "time"
			title:          qsTr("Time variable (optional)")
			singleVariable: true
			allowedColumns: ["scale"]
			info:           qsTr("Per-row observation time or exposure. If omitted, each row contributes one unit.")
		}
	}

	Group
	{
		title:   qsTr("Summarized data")
		enabled: inputSummarized.checked
		columns: 2

		Group
		{
			title: qsTr("Group 1")

			TextField
			{
				name:          "groupOneName"
				label:         qsTr("Name")
				placeholderText: qsTr("Group 1")
				fieldWidth:    100
				info:          qsTr("Label for group 1 (optional).")
			}

			IntegerField
			{
				name:         "groupOneEvents"
				label:        qsTr("Events")
				defaultValue: 1
				min:          0
				info:         qsTr("Number of observed events in group 1.")
			}

			DoubleField
			{
				name:         "groupOneTime"
				label:        qsTr("Time")
				defaultValue: 1
				min:          0
				decimals:     4
				inclusive:    JASP.MaxOnly
				info:         qsTr("Total observation time or exposure for group 1.")
			}
		}

		Group
		{
			title: qsTr("Group 2")

			TextField
			{
				name:          "groupTwoName"
				label:         qsTr("Name")
				placeholderText: qsTr("Group 2")
				fieldWidth:    100
				info:          qsTr("Label for group 2 (optional).")
			}

			IntegerField
			{
				name:         "groupTwoEvents"
				label:        qsTr("Events")
				defaultValue: 1
				min:          0
				info:         qsTr("Number of observed events in group 2.")
			}

			DoubleField
			{
				name:         "groupTwoTime"
				label:        qsTr("Time")
				defaultValue: 1
				min:          0
				decimals:     4
				inclusive:    JASP.MaxOnly
				info:         qsTr("Total observation time or exposure for group 2.")
			}
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
			info:    qsTr("Exact conditional test of the rate ratio (C-test).")
		}

		CheckBox
		{
			name:  "normalApprox"
			label: qsTr("Normal approximation")
			info:  qsTr("Large-sample score test based on the conditional binomial distribution.")
		}
	}

	DoubleField
	{
		name:         "testRatio"
		label:        qsTr("Hypothesized ratio (rate\u2081 / rate\u2082):")
		defaultValue: 1
		min:          0
		decimals:     4
		inclusive:    JASP.MaxOnly
		info:         qsTr("Null hypothesis value for the rate ratio \u03bb\u2081/\u03bb\u2082.")
	}

	RadioButtonGroup
	{
		name:  "alternative"
		title: qsTr("Alternative Hypothesis")

		RadioButton
		{
			value:   "two.sided"
			label:   qsTr("rate\u2081 \u2260 rate\u2082")
			checked: true
			info:    qsTr("Two-sided: the rate ratio differs from the hypothesized value.")
		}

		RadioButton
		{
			value: "greater"
			label: qsTr("rate\u2081 > rate\u2082")
			info:  qsTr("One-sided: rate 1 is greater than rate 2.")
		}

		RadioButton
		{
			value: "less"
			label: qsTr("rate\u2081 < rate\u2082")
			info:  qsTr("One-sided: rate 1 is less than rate 2.")
		}
	}

	Group
	{
		title: qsTr("Additional Statistics")

		CheckBox
		{
			name:    "descriptives"
			label:   qsTr("Descriptive statistics")
			checked: true
			info:    qsTr("Show per-group event counts, times, and rate estimates.")

			CheckBox
			{
				name: "descriptiveCi"
				label: qsTr("Confidence interval (exact)")
				info:  qsTr("Exact per-group confidence intervals for the individual rates.")
			}
		}

		CheckBox
		{
			name:              "ratioCi"
			label:             qsTr("Confidence interval for ratio")
			id:                ratioCi
			childrenOnSameRow: true
			info:              qsTr("Confidence interval for the rate ratio \u03bb\u2081/\u03bb\u2082.")

			CIField { name: "confLevel" }
		}

		RadioButtonGroup
		{
			name:                "ciMethod"
			title:               qsTr("Method")
			enabled:             ratioCi.checked
			radioButtonsOnSameRow: true

			RadioButton { value: "exact";  label: qsTr("Exact");               checked: true }
			RadioButton { value: "normal"; label: qsTr("Normal approximation") }
		}
	}
}
