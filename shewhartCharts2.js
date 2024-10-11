


class shewhartCharts2 extends baseModal {
    static dialogId = 'shewhartCharts2'
    static t = baseModal.makeT(shewhartCharts2.dialogId)

    constructor() {
        var config = {
            id: shewhartCharts2.dialogId,
            label: shewhartCharts2.t('title'),
            modalType: "two",
            RCode:`
require(qcc)

#Shewhart Charts


phases = list(
			c({{selected.rowsTobeUsed | safe}}),
			c({{selected.rowsTobeUsedPhase2 | safe}}),
			c({{selected.rowsTobeUsedPhase3 | safe}}),
			c({{selected.rowsTobeUsedPhase4 | safe}}),
			c({{selected.rowsTobeUsedPhase5 | safe}}),
			c({{selected.rowsTobeUsedPhase6 | safe}}),
			c({{selected.rowsTobeUsedPhase7 | safe}}),
			c({{selected.rowsTobeUsedPhase8 | safe}}),
			c({{selected.rowsTobeUsedPhase9 | safe}}),
			c({{selected.rowsTobeUsedPhase10 | safe}})
			)

BSkySetSixSigmaTestOptions( test1 = {{selected.test1Chk | safe}}, one.point.k.stdv = {{selected.test1 | safe}}, 
							test2 = {{selected.test2Chk | safe}}, k.run.same.side = {{selected.test2 | safe}}, 
							test3 = {{selected.test3Chk | safe}}, k.run.increase.decrease = {{selected.test3 | safe}}, 
							test4 = {{selected.test4Chk | safe}}, k.run.alternating = {{selected.test4 | safe}},
							test5 = {{selected.test5Chk | safe}}, k.plusone.run.beyond.2dev = {{selected.test5 | safe}}, 
							test6 = {{selected.test6Chk | safe}}, k.plusone.run.beyond.1dev = {{selected.test6 | safe}}, 
							test7 = {{selected.test7Chk | safe}}, k.run.within.1dev = {{selected.test7 | safe}}, 
							test8 = {{selected.test8Chk | safe}}, k.run.beyond.1dev = {{selected.test8 | safe}}, 
							digits = {{selected.digits | safe}})

chartTypes = c( 
               "{{selected.chartTypePChk | safe}}", "{{selected.chartTypeNPChk | safe}}", "{{selected.chartTypeCChk | safe}}", 
			   "{{selected.chartTypeUChk | safe}}")

cat("Charts selected:", paste(chartTypes[chartTypes!=" "], collapse = ","))


if(trimws(c('{{selected.sizes | safe}}')) == '')
{
	sizes = c()	
	row_num_with_NAs = which(is.na({{dataset.name}}\${{selected.variableSelcted2 | safe}}) | {{dataset.name}}\${{selected.variableSelcted2 | safe}} == "")
	if(length(row_num_with_NAs)>0){
		data_NonNA = c({{dataset.name}}\${{selected.variableSelcted2 | safe}}[-c(row_num_with_NAs)])
	}else
	{
		data_NonNA = c({{dataset.name}}\${{selected.variableSelcted2 | safe}})
	}
} else  
{
	row_num_with_NAs = which(is.na({{dataset.name}}\${{selected.variableSelcted2 | safe}}) | {{dataset.name}}\${{selected.variableSelcted2 | safe}} == "" | is.na({{dataset.name}}\$'{{selected.sizes | safe}}') | c({{dataset.name}}\$'{{selected.sizes | safe}}') == "")
	if(length(row_num_with_NAs)>0){
		sizes = with({{dataset.name}},c({{selected.sizes | safe}})[-c(row_num_with_NAs)])
		data_NonNA = c({{dataset.name}}\${{selected.variableSelcted2 | safe}}[-c(row_num_with_NAs)])
	}else
	{
		sizes = with({{dataset.name}},c({{selected.sizes | safe}}))
		data_NonNA = c({{dataset.name}}\${{selected.variableSelcted2 | safe}})
	}
}

i=1 
#chart type p
p.spc.qcc.objects = NULL
	
if(trimws(chartTypes[i]) != "") 
{	
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "for", c('{{selected.variableSelcted2 | safe}}')))
				
				
			p.spc.qcc.objects = plot.qcc.spc.phases(
								type = chartTypes[i],
								#data = c({{dataset.name}}\${{selected.variableSelcted2 | safe}}), 
								data = data_NonNA,
								data.name = c('{{selected.variableSelcted2 | safe}}'), 
								sizes = sizes, 
								newdata=c({{selected.rowsTobeUsedAsNewData | safe}}), 
								newdata.name = c(), 
								newsizes = c(), 
								phases.data.list = phases, 
								phase.names = {{selected.phaseNames | safe}}, 
                                nsigmas = c({{selected.nsigmas | safe}}), 
								digits ={{selected.digits | safe}}, 
								spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
								additional.sigma.lines = c({{selected.sdWarnLimits| safe}}),
								mark.test.number = {{selected.markTestNumberChk | safe}}
								)
}


if(!is.null(p.spc.qcc.objects))
{
			print.qcc.spc.phases(qcc.spc.phases.obects = p.spc.qcc.objects,
									print.stats = {{selected.printStatChk | safe}}, 
									print.test.summary = {{selected.printTestSummaryChk | safe}}, 
									print.test.detail = {{selected.printTestDetailChk | safe}},
									print.qcc.object.summary = {{selected.printObjectSummaryChk | safe}},
									digits = {{selected.digits | safe}}, 
									phase.names = {{selected.phaseNames | safe}}
								)
									
}

if({{selected.ocCurvesChk | safe}} && !is.null(p.spc.qcc.objects)){
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "OC curves for", c('{{selected.variableSelcted2 | safe}}')))
			betaX = qcc::oc.curves(p.spc.qcc.objects$qcc.objects[[1]])
}

i=2 
#chart type np
np.spc.qcc.objects = NULL
	
if(trimws(chartTypes[i]) != "") 
{	
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "for", c('{{selected.variableSelcted2 | safe}}')))
			np.spc.qcc.objects = plot.qcc.spc.phases(
								type = chartTypes[i],
								#data = c({{dataset.name}}\${{selected.variableSelcted2 | safe}}),
								data = data_NonNA,
								data.name = c('{{selected.variableSelcted2 | safe}}'), 
								sizes = sizes, 
								newdata=c({{selected.rowsTobeUsedAsNewData | safe}}), 
								newdata.name = c(), 
								newsizes = c(), 
								phases.data.list = phases, 
								phase.names = {{selected.phaseNames | safe}}, 
                                nsigmas = c({{selected.nsigmas | safe}}), 
								digits ={{selected.digits | safe}}, 
								spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
								additional.sigma.lines = c({{selected.sdWarnLimits| safe}}),
								mark.test.number = {{selected.markTestNumberChk | safe}}
								)
}


if(!is.null(np.spc.qcc.objects))
{
			print.qcc.spc.phases(qcc.spc.phases.obects = np.spc.qcc.objects,
									print.stats = {{selected.printStatChk | safe}}, 
									print.test.summary = {{selected.printTestSummaryChk | safe}}, 
									print.test.detail = {{selected.printTestDetailChk | safe}},
									print.qcc.object.summary = {{selected.printObjectSummaryChk | safe}},
									digits = {{selected.digits | safe}}, 
									phase.names = {{selected.phaseNames | safe}}
								)
									
}

if({{selected.ocCurvesChk | safe}} && !is.null(np.spc.qcc.objects)){
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "OC curves for", c('{{selected.variableSelcted2 | safe}}')))
			betaX = qcc::oc.curves(np.spc.qcc.objects$qcc.objects[[1]])
}


i=3 
#chart type c
c.spc.qcc.objects = NULL
	
if(trimws(chartTypes[i]) != "") 
{	
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "for", c('{{selected.variableSelcted2 | safe}}')))
			c.spc.qcc.objects = plot.qcc.spc.phases(
								type = chartTypes[i],
								#data = c({{dataset.name}}\${{selected.variableSelcted2 | safe}}), 
								data = data_NonNA,
								data.name = c('{{selected.variableSelcted2 | safe}}'), 
								sizes = sizes, 
								newdata=c({{selected.rowsTobeUsedAsNewData | safe}}), 
								newdata.name = c(), 
								newsizes = c(), 
								phases.data.list = phases, 
								phase.names = {{selected.phaseNames | safe}}, 
                                nsigmas = c({{selected.nsigmas | safe}}), 
								digits ={{selected.digits | safe}}, 
								spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
								additional.sigma.lines = c({{selected.sdWarnLimits| safe}}),
								mark.test.number = {{selected.markTestNumberChk | safe}}
								)
}


if(!is.null(c.spc.qcc.objects))
{
			print.qcc.spc.phases(qcc.spc.phases.obects = c.spc.qcc.objects,
									print.stats = {{selected.printStatChk | safe}}, 
									print.test.summary = {{selected.printTestSummaryChk | safe}}, 
									print.test.detail = {{selected.printTestDetailChk | safe}},
									print.qcc.object.summary = {{selected.printObjectSummaryChk | safe}},
									digits = {{selected.digits | safe}}, 
									phase.names = {{selected.phaseNames | safe}}
								)
									
}

if({{selected.ocCurvesChk | safe}} && !is.null(c.spc.qcc.objects)){
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "OC curves for", c('{{selected.variableSelcted2 | safe}}')))
			betaX = qcc::oc.curves(c.spc.qcc.objects$qcc.objects[[1]])
}

i=4 
#chart type u
u.spc.qcc.objects = NULL
	
if(trimws(chartTypes[i]) != "") 
{	
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "for", c('{{selected.variableSelcted2 | safe}}')))
			u.spc.qcc.objects = plot.qcc.spc.phases(
								type = chartTypes[i],
								#data = c({{dataset.name}}\${{selected.variableSelcted2 | safe}}), 
								data = data_NonNA,
								data.name = c('{{selected.variableSelcted2 | safe}}'), 
								sizes = sizes, 
								newdata=c({{selected.rowsTobeUsedAsNewData | safe}}), 
								newdata.name = c(), 
								newsizes = c(), 
								phases.data.list = phases, 
								phase.names = {{selected.phaseNames | safe}}, 
                                nsigmas = c({{selected.nsigmas | safe}}), 
								digits ={{selected.digits | safe}}, 
								spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
								additional.sigma.lines = c({{selected.sdWarnLimits| safe}}),
								mark.test.number = {{selected.markTestNumberChk | safe}}
								)
}


if(!is.null(u.spc.qcc.objects))
{
			print.qcc.spc.phases(qcc.spc.phases.obects = u.spc.qcc.objects,
									print.stats = {{selected.printStatChk | safe}}, 
									print.test.summary = {{selected.printTestSummaryChk | safe}}, 
									print.test.detail = {{selected.printTestDetailChk | safe}},
									print.qcc.object.summary = {{selected.printObjectSummaryChk | safe}},
									digits = {{selected.digits | safe}}, 
									phase.names = {{selected.phaseNames | safe}}
								)
									
}

if({{selected.ocCurvesChk | safe}} && !is.null(u.spc.qcc.objects)){
			BSkyFormat(paste("\nChart Type:", chartTypes[i], "OC curves for", c('{{selected.variableSelcted2 | safe}}')))
			betaX = qcc::oc.curves(u.spc.qcc.objects$qcc.objects[[1]])
}
	
`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) },
			chartTypePChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('chartTypePChk'), 
					no: "chartTypePChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "p",
                    false_value: " ",
					state : "checked",
					newline: true,
                })
            },
			chartTypeNPChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('chartTypeNPChk'), 
					no: "chartTypeNPChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "np",
                    false_value: " ",
					newline: true,
                })
            },
			chartTypeCChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('chartTypeCChk'), 
					no: "chartTypeCChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "c",
                    false_value: " ",
					newline: true,
                })
            },
			chartTypeUChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('chartTypeUChk'), 
					no: "chartTypeUChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "u",
                    false_value: " ",
					newline: true,
                })
            },
			variableSelcted2: {
                el: new dstVariable(config, {
                    label: shewhartCharts2.t('variableSelcted2'),
                    no: "variableSelcted2",
                    required: false,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "Numeric|Scale",
					//style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			sizes: {
                el: new dstVariable(config, {
                    label: shewhartCharts2.t('sizes'),
                    no: "sizes",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					style: "mt-2 mb-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			printStatChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('printStatChk'), 
					no: "printStatChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					state: "checked",
					newline: true,
                })
            },
			printObjectSummaryChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('printObjectSummaryChk'), 
					no: "printObjectSummaryChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					//state: "checked",
					newline: true,
                })
            },
			printTestSummaryChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('printTestSummaryChk'), 
					no: "printTestSummaryChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					//state: "checked",
					newline: true,
                })
            },
			printTestDetailChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('printTestDetailChk'), 
					no: "printTestDetailChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			markTestNumberChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('markTestNumberChk'), 
					no: "markTestNumberChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					state: "checked",
					newline: true,
                })
            },
			ocCurvesChk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('ocCurvesChk'), 
					no: "ocCurvesChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			rowsTobeUsed: {
                el: new input(config, {
                    no: 'rowsTobeUsed',
                    label: shewhartCharts2.t('rowsTobeUsed'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedAsNewData: {
                el: new input(config, {
                    no: 'rowsTobeUsedAsNewData',
                    label: shewhartCharts2.t('rowsTobeUsedAsNewData'),
                    placeholder: "",
                    required: false,
					filter: "character|numeric",
                    //type: "character",
					style: "mb-3",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			additionalPhasesLabel: { 
				el: new labelVar(config, { 
					label: shewhartCharts2.t('additionalPhasesLabel'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			phaseNames: {
                el: new input(config, {
                    no: 'phaseNames',
                    label: shewhartCharts2.t('phaseNames'),
                    placeholder: "",
                    required: false,
                    type: "character",
					style: "mb-3",
                    extraction: "CreateArray",
					allow_spaces:true,
                    value: "Phase 1, Phase 2, Phase 3, Phase 4, Phase 5, Phase 6, Phase 7, Phase 8, Phase 9, Phase 10",
                })
            },
			rowsTobeUsedPhase2: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase2',
                    label: shewhartCharts2.t('rowsTobeUsedPhase2'),
                    placeholder: "",
                    required: false,
					filter: "character|numeric",
                    //type: "character",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase3: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase3',
                    label: shewhartCharts2.t('rowsTobeUsedPhase3'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase4: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase4',
                    label: shewhartCharts2.t('rowsTobeUsedPhase4'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase5: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase5',
                    label: shewhartCharts2.t('rowsTobeUsedPhase5'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase6: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase6',
                    label: shewhartCharts2.t('rowsTobeUsedPhase6'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase7: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase7',
                    label: shewhartCharts2.t('rowsTobeUsedPhase7'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase8: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase8',
                    label: shewhartCharts2.t('rowsTobeUsedPhase8'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase9: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase9',
                    label: shewhartCharts2.t('rowsTobeUsedPhase9'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			rowsTobeUsedPhase10: {
                el: new input(config, {
                    no: 'rowsTobeUsedPhase10',
                    label: shewhartCharts2.t('rowsTobeUsedPhase10'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					style: "mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
                })
            },
			nsigmas: {
                el: new input(config, {
                    no: 'nsigmas',
                    label: shewhartCharts2.t('nsigmas'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "3",
					width: "w-25",
                })
            },
			confidence_level: {
                el: new input(config, {
                    no: 'confidence_level',
                    label: shewhartCharts2.t('confidence_level'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: '%val%',
					width: "w-25",
                })
            },
			sdWarnLimits: {
                el: new input(config, {
                    no: 'sdWarnLimits',
                    label: shewhartCharts2.t('sdWarnLimits'),
                    placeholder: "",
                    required: false,
                    filter: "character|numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
					//style: "mb-2",
					width: "w-25",
                })
            },
			digits: {
                el: new inputSpinner(config, {
                    no: 'digits',
                    label: shewhartCharts2.t('digits'),
                    required: true,
                    min: 0,
                    max: 15,
                    step: 1,
                    value: 2,
					width: "w-25",
					style: "mb-2",
                })
            }, 
			lower: {
                el: new input(config, {
                    no: 'lower',
                    label: shewhartCharts2.t('lower'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
					//style: "ml-3",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
					width: "w-25",
                })
            },
			upper: {
                el: new input(config, {
                    no: 'upper',
                    label: shewhartCharts2.t('upper'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
					style: "mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
					width: "w-25",
                })
            },
			performTestLabel: { 
				el: new labelVar(config, { 
					label: shewhartCharts2.t('performTestLabel'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			test1Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test1Chk'), 
					no: "test1Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					//state: "checked",
					newline: true,
                })
            },
			test2Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test2Chk'), 
					no: "test2Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					//state: "checked",
					newline: true,
                })
            },
			test3Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test3Chk'), 
					no: "test3Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			test4Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test4Chk'), 
					no: "test4Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			test5Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test5Chk'), 
					no: "test5Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			test6Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test6Chk'), 
					no: "test6Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			test7Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test7Chk'), 
					no: "test7Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			test8Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts2.t('test8Chk'), 
					no: "test8Chk",
                    bs_type: "valuebox",
                    //style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			test1: {
                el: new input(config, {
                    no: 'test1',
                    label: shewhartCharts2.t('test1'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "3",
					width: "w-25",
                })
            },
			test2: {
                el: new input(config, {
                    no: 'test2',
                    label: shewhartCharts2.t('test2'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "9",
					width: "w-25",
                })
            },
			test3: {
                el: new input(config, {
                    no: 'test3',
                    label: shewhartCharts2.t('test3'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "6",
					width: "w-25",
                })
            },
			test4: {
                el: new input(config, {
                    no: 'test4',
                    label: shewhartCharts2.t('test4'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "14",
					width: "w-25",
                })
            },
			test5: {
                el: new input(config, {
                    no: 'test5',
                    label: shewhartCharts2.t('test5'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "2",
					width: "w-25",
                })
            },
			test6: {
                el: new input(config, {
                    no: 'test6',
                    label: shewhartCharts2.t('test6'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "4",
					width: "w-25",
                })
            },
			test7: {
                el: new input(config, {
                    no: 'test7',
                    label: shewhartCharts2.t('test7'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "15",
					width: "w-25",
                })
            },
			test8: {
                el: new input(config, {
                    no: 'test8',
                    label: shewhartCharts2.t('test8'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					style: "ml-5 mb-2",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "8",
					width: "w-25",
                })
            },
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [
					objects.chartTypePChk.el.content,
					objects.chartTypeNPChk.el.content,
					objects.chartTypeCChk.el.content,
					objects.chartTypeUChk.el.content,
					
					objects.variableSelcted2.el.content, 
					objects.sizes.el.content,
					
					objects.nsigmas.el.content,
					//objects.confidence_level.el.content,
					
					//objects.processCapabilityChk.el.content,
					objects.upper.el.content,
					objects.lower.el.content,
					//objects.target.el.content 
					objects.sdWarnLimits.el.content, 
					
					objects.digits.el.content,
					
					objects.printStatChk.el.content, 
					objects.printObjectSummaryChk.el.content,
					
					objects.ocCurvesChk.el.content,
					
					objects.rowsTobeUsed.el.content,
					objects.rowsTobeUsedAsNewData.el.content,
					
					objects.additionalPhasesLabel.el.content,
					objects.rowsTobeUsedPhase2.el.content,
					objects.rowsTobeUsedPhase3.el.content,
					objects.rowsTobeUsedPhase4.el.content,
					objects.rowsTobeUsedPhase5.el.content,
					objects.rowsTobeUsedPhase6.el.content,
					objects.rowsTobeUsedPhase7.el.content,
					objects.rowsTobeUsedPhase8.el.content,
					objects.rowsTobeUsedPhase9.el.content,
					objects.rowsTobeUsedPhase10.el.content, 
					objects.phaseNames.el.content,
					
					objects.performTestLabel.el.content,
					
					objects.markTestNumberChk.el.content,
					objects.printTestSummaryChk.el.content,
					objects.printTestDetailChk.el.content,
					
					objects.test1Chk.el.content,
					objects.test1.el.content,
					objects.test2Chk.el.content,
					objects.test2.el.content,
					objects.test3Chk.el.content,
					objects.test3.el.content,
					objects.test4Chk.el.content,
					objects.test4.el.content,
					objects.test5Chk.el.content,
					objects.test5.el.content,
					objects.test6Chk.el.content,
					objects.test6.el.content,
					objects.test7Chk.el.content,
					objects.test7.el.content,
					objects.test8Chk.el.content,
					objects.test8.el.content
					],
            nav: {
                name: shewhartCharts2.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: shewhartCharts2.t('help.title'),
            r_help: "help(data,package='utils')",
            body: shewhartCharts2.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new shewhartCharts2().render()
}
