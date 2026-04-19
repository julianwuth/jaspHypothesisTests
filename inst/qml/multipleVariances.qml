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
    VariablesForm
    {
        infoLabel: qsTr("Input")
        preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "dependent"; title: qsTr("Dependent Variable"); allowedColumns: ["scale"]; info: qsTr("Scale variable(s) whose variances are compared across groups.") }
        AssignedVariablesList { name: "factor"; title: qsTr("Grouping Variable"); allowedColumns: ["nominal"]; singleVariable: true; info: qsTr("Nominal variable defining the groups.") }
    }

    Group
    {
        title: qsTr("Tests")
        CheckBox { name: "fTest"; label: qsTr("F-test (2 groups)"); info: qsTr("F-test for equality of two variances.") }
        CheckBox { name: "leveneTest"; label: qsTr("Levene's test"); checked: true; info: qsTr("Levene's test for equality of variances based on the median.") }
        CheckBox { name: "bonettTest"; label: qsTr("Bonett's test"); info: qsTr("Bonett's test for equality of variances using Winsorized kurtosis.") }
        CheckBox { name: "bartlettTest"; label: qsTr("Bartlett's test"); info: qsTr("Bartlett's test for equality of variances; assumes normality.") }
    }

    Group
    {
        title: qsTr("Assumption Checks")
        CheckBox { name: "normalityTest"; label: qsTr("Normality"); info: qsTr("Shapiro-Wilk test of normality.") }
        CheckBox { name: "qqPlot"; label: qsTr("Q-Q plot residuals"); info: qsTr("Q-Q plot of the residuals.") }
    }

    Group
    {
        title: qsTr("Additional Statistics")

        CheckBox 
        { 
            name: "descriptives" 
            label: qsTr("Descriptives")
            id: descriptives 
            
            CheckBox
            {
                name: "varianceCi"
                label: qsTr("Confidence interval")
                id: varianceCi
                childrenOnSameRow: true
                CIField { name: "confLevel" }
            }

            RadioButtonGroup
            {
                name: "ciMethod"
                title: qsTr("Method")
                enabled: varianceCi.checked
                radioButtonsOnSameRow: true
                RadioButton { value: "chiSquare"; label: qsTr("χ²"); checked: true }
                RadioButton { value: "bonett"; label: qsTr("Bonett") }
            }
        }

        CheckBox
        {
            name: "varianceRatioCi"
            label: qsTr("Variance ratio (2 groups)")
            id: varianceRatioCi
            info: qsTr("Confidence interval for the variance ratio (2 groups only).")

            RadioButtonGroup
            {
                name: "ratioCiMethod"
                title: qsTr("Method")
                radioButtonsOnSameRow: true
                RadioButton { value: "fTest"; label: qsTr("F-test"); checked: true }
                RadioButton { value: "bonett"; label: qsTr("Bonett") }
            }
        }
    }

    Group
    {
        title: qsTr("Summary Plots")

        CheckBox { name: "boxPlot"; label: qsTr("Box plot"); info: qsTr("Box plot of the dependent variable across groups.") }
        CheckBox { name: "varRatioPlot"; label: qsTr("Variance ratio plot (2 groups)"); info: qsTr("Plot of the variance ratio with confidence interval (F-test based, 2 groups only).") }
        CheckBox { name: "varEstimatePlot"; label: qsTr("Variance estimate plot"); info: qsTr("Plot of the variance estimates with confidence intervals.") }
    }

}