/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */

const nav = {
    "name": "Six Sigma",
    "tab": "six_sigma",
    "buttons": [
        "./SixSigmaOverview",
        "./createCauseEffectDiagram",
        {
            "name": "Pareto Chart",
            "icon": "icon-sixsigma",
            "children": [
                "./paretoChart"
            ]
        },        
        "./lossFunctionAnalysis",
        {
            "name": "MSA",
            "icon": "icon-sixsigma",
            "children": [
                "./gageRnR",
                "./attributeAgreement",
                "./GageBiasAnalysis"
            ]
        },
        {
            "name": "Process Capability",
            "icon": "icon-sixsigma",
            "children": [
				"./processCapabilityQcc"
            ]
        },
        {
            "name": "Shewhart Charts",
            "icon": "icon-sixsigma",
            "children": [
                "./shewhartCharts1",
                "./shewhartCharts2",
                "./shewhartCharts3",
                "./shewhartCharts4"
				
            ]
        },
        "./cusumChart",	
        "./ewmaChart",
        "./mqccChart",
        "./MultiVariChart"
    ]
}

module.exports.nav = nav
