const nav = {
    "id": "menu-sixsigma",
    "buttons": [
        "./SixSigmaOverview",
        "./createCauseEffectDiagram",
        "./lossFunctionAnalysis",
        {
            "id": "menu-sixsigma-msa",
            "icon": "icon-sixsigma",
            "children": [
                "./gageRnR",
                "./attributeAgreement",
                "./GageBiasAnalysis"
            ]
        },
        {
            "id": "menu-sixsigma-processcapability",
            "icon": "icon-sixsigma",
            "children": [
 				"./processCapabilityQcc"
            ]
        },
        {
            "id": "menu-sixsigma-shewhartcharts",
            "icon": "icon-sixsigma",
            "children": [
                "./shewhartCharts1",
                "./shewhartCharts2",
                "./shewhartCharts3",
                "./shewhartCharts4"

            ]
        },
	    {
			"id": "menu-sixsigma-reliability",
			"icon": "icon-sixsigma",
			"children": [

			]
		},
        "./cusumChart",
        "./ewmaChart",
        "./mqccChart"
    ]
}

module.exports.nav = nav
