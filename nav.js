/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */

let t = getT('menutoolbar')
const nav = () => ({
    "name": t('sixsigma_top_level_title'),// {ns: 'menutoolbar'}),
    "tab": "six_sigma",
    "buttons": [
        "./SixSigmaOverview",
        "./createCauseEffectDiagram",
        {
            "name": t('sixsigma_Pareto_Chart'),// {ns: 'menutoolbar'}),
            "icon": "icon-sixsigma",
            "children": [
                "./paretoChart"
            ]
        },        
        "./lossFunctionAnalysis",
        {
            "name": t('sixsigma_MSA'),// {ns: 'menutoolbar'}),
            "icon": "icon-sixsigma",
            "children": [
                "./gageRnR",
                "./attributeAgreement",
                "./GageBiasAnalysis"
            ]
        },
        {
            "name": t('sixsigma_Process_Capability'),// {ns: 'menutoolbar'}),
            "icon": "icon-sixsigma",
            "children": [
				"./processCapabilityQcc"
            ]
        },
        {
            "name": t('sixsigma_Shewhart_Charts'),// {ns: 'menutoolbar'}),
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
})

module.exports = {
    nav: nav(),
    render: () => nav()
}
