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
                "./gageRnR(MSA)",
                "./AttributeAgreement",
                "./GageBiasAnalysis"
            ]
        },
        {
            "name": "Process Capability",
            "icon": "icon-sixsigma",
            "children": [
                "./ProcessCapability",
				"./ProcessCapability(qcc)"
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
        "./multiVariChart"
    ]
}

module.exports.nav = nav
