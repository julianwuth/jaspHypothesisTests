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
    // TODO add info here for help file

    VariablesForm
	{
		infoLabel: qsTr("Input")
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent"; title: qsTr("Variables"); info: qsTr("In this box the dependent variable is selected.") ; allowedColumns: ["scale"]; minNumericLevels: 2 }
	} 

    Group 
    {
        title: qsTr("Tests")

        CheckBox 
        {
            label: qsTr("χ² test")
            name: "chiSquareTest"
            checked: true
        }
        
        DoubleField
        {
            label: qsTr("Test value:")
            name: "testVariance"
            defaultValue: 1
            decimals: 3
            inclusive: JASP.MaxOnly
        }
    }

    RadioButtonGroup
	{
		name: "alternative"
		title: qsTr("Alternative Hypothesis")
		RadioButton 
        { 
            value: "twoSided"		
            label: qsTr("≠ Test value"); 
            info: qsTr("Two sided alternative hypothesis that the sample variance is not equal to the test value. Selected by default."); checked: true	
        }
		RadioButton 
        { 
            value: "greater"	
            label: qsTr("> Test value")	; 
            info: qsTr("One sided alternative hypothesis that the sample variance is greater than the test value.")				
        }
		RadioButton 
        { 
            value: "less"		
            label: qsTr("< Test value")	; 
            info: qsTr("One sided alternative hypothesis that the sample variance is less than the test value.")				
        }
	}

    Group
	{
		title: qsTr("Additional Statistics")

        CheckBox
        {
            label: qsTr("Sample variance")
            name: "varEstimate"

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
                RadioButton { value: "chiSquare"; label: qsTr("χ²") ; checked: true }
                RadioButton { value: "bonett"; label: qsTr("Bonett") }
            }
        }
        
    }

    Group
	{
		title: qsTr("Assumption checks")
		CheckBox { name: "normalityTest"; label: qsTr("Normality"); info: qsTr("Shapiro-Wilk test of normality.") }
		CheckBox { name: "qqPlot";		 	label: qsTr("Q-Q plot residuals"); info: qsTr("Q-Q plot of the standardized residuals.") }
	}

}