


class shewhartCharts4 extends baseModal {
    static dialogId = 'shewhartCharts4'
    static t = baseModal.makeT(shewhartCharts4.dialogId)

    constructor() {
        var config = {
            id: shewhartCharts4.dialogId,
            label: shewhartCharts4.t('title'),
            modalType: "two",
            RCode:`
require(qcc)

#Shewhart charts (xbar, R, S)


selectedData = NULL

if(c('{{selected.gpbox1 | safe}}') == "variable"){
	
	if(length(trimws(c({{selected.variableControlLimits | safe}}))) == 0)
	{
		if(trimws('{{selected.groupingVariable | safe}}') != "")
		{
			selectedData = with({{dataset.name}}, qcc::qcc.groups(c({{selected.variableSelcted | safe}}), c({{selected.groupingVariable | safe}})))
		}
		
		if({{selected.displayGroupsChk | safe}} && !is.null(selectedData))
		{
			{{selected.variableSelcted | safe}}Gpd = as.data.frame(selectedData)
			
			#gpMean = rowMeans({{selected.variableSelcted | safe}}Gpd, na.rm = TRUE)
            #{{selected.variableSelcted | safe}}Gpd = cbind({{selected.variableSelcted | safe}}Gpd, gpMean = gpMean)
			
			BSkyLoadRefresh('{{selected.variableSelcted | safe}}Gpd')
		}
	}
	else
	{
		selectedData = with({{dataset.name}}, c({{selected.variableSelcted | safe}}))
	}
	
	data_name = '{{selected.variableSelcted | safe}}'
	
}else if(c('{{selected.gpbox1 | safe}}') != "variable"){
	if(length(c({{selected.variablelistSelcted | safe}})) == 0){
		selectedData = {{dataset.name}}[]
	}else{
		selectedData = {{dataset.name}}[, c({{selected.variablelistSelcted | safe}})]
	}
	
	data_name = '{{dataset.name}}'
}

if(length(trimws(c({{selected.variableControlLimits | safe}}))) != 0 && !is.null(selectedData))
{
	if(c('{{selected.gpbox1 | safe}}') == "variable"){
		selectedData_variable_control_limit = with({{dataset.name}}, qcc.groups(selectedData[-c({{selected.variableControlLimits | safe}})], c({{selected.groupingVariable | safe}})[-c({{selected.variableControlLimits | safe}})]))
		
		if({{selected.displayGroupsChk | safe}})
		{
			{{selected.variableSelcted | safe}}ControlLimit = as.data.frame(selectedData_variable_control_limit)
			
			#gpMean = rowMeans({{selected.variableSelcted | safe}}ControlLimit, na.rm = TRUE)
            #{{selected.variableSelcted | safe}}ControlLimit = cbind({{selected.variableSelcted | safe}}ControlLimit, gpMean = gpMean)
			
			BSkyLoadRefresh('{{selected.variableSelcted | safe}}ControlLimit')
		}
		
	}else{
		# selectedData_variable_control_limit = as.matrix(selectedData[-c({{selected.variableControlLimits | safe}}),])
	}
	
	selectedData = selectedData_variable_control_limit
}

gpMean = rowMeans(selectedData, na.rm = TRUE)
selectedData = cbind(selectedData, gpMean = gpMean)
			

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




chartTypes = c("xbar.one")
cat("Charts selected:", "I-MR Between/Within")

#chart type xbar.one
xbar.one.IMR.spc.qcc.objects = NULL
data.moving.range.R = NULL
xbar.one.XMR.spc.qcc.objects = NULL
S.spc.qcc.objects = NULL


i=1
if(trimws(chartTypes) != "") 
{	
			BSkyFormat(paste("\nChart Type:", "I-MR Between/Within", "for", data_name))
				
			xbar.one.IMR.spc.qcc.objects = plot.qcc.spc.phases(
								type = 'xbar.one',
								data = selectedData[,c("gpMean")], 
								data.name = data_name,
								chart.title.name = 'I-MR Between Subgroups',
								size.title = c(""),
								ylab = "Subgroup mean",
								newdata=c({{selected.rowsTobeUsedAsNewData | safe}}), 
								newdata.name = c(),  
								phases.data.list = phases, 
								phase.names = {{selected.phaseNames | safe}}, 
                                nsigmas = c({{selected.nsigmas | safe}}), 
								confidence.level= c({{selected.confidence_level | safe}}), 
								std.dev = 'MR', 
								digits ={{selected.digits | safe}}, 
								spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
								additional.sigma.lines = c({{selected.sdWarnLimits| safe}}),
								mark.test.number = {{selected.markTestNumberChk | safe}}
								)
			
			
		
		{{if(options.selected.showMRChartChk === 'TRUE')}}
	
			data.moving.range.R <- matrix(cbind(c(selectedData[,c("gpMean")])[1:length(selectedData[,c("gpMean")])-1], 
			                                    c(selectedData[,c("gpMean")])[2:length(selectedData[,c("gpMean")])]), 
												ncol=2)

			xbar.one.XMR.spc.qcc.objects = plot.qcc.spc.phases(
								type = 'MR',
								data = data.moving.range.R, 
								data.name = data_name,
								chart.title.name = 'MR',
								size.title = c(""),
								ylab = "MR of subgroup mean",
								newdata=c({{selected.rowsTobeUsedAsNewData | safe}}), 
								newdata.name = c(),  
								phases.data.list = phases, 
								phase.names = {{selected.phaseNames | safe}}, 
                                nsigmas = c({{selected.nsigmas | safe}}), 
								confidence.level= c({{selected.confidence_level | safe}}), 
								std.dev = 'UWAVE-R', 
								digits ={{selected.digits | safe}}, 
								spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
								additional.sigma.lines = c({{selected.sdWarnLimits| safe}}),
								mark.test.number = {{selected.markTestNumberChk | safe}}
								)
		{{/if}}
		
			S.spc.qcc.objects = plot.qcc.spc.phases(
								type = 'S',
								data = selectedData[, colnames(selectedData)!= c("gpMean")], 
								data.name = data_name,
								chart.title.name = 'I-MR Within Subgroups',
								ylab = "Sample std dev",
								newdata=c({{selected.rowsTobeUsedAsNewData | safe}}), 
								newdata.name = c(),  
								phases.data.list = phases, 
								phase.names = {{selected.phaseNames | safe}}, 
                                nsigmas = c({{selected.nsigmas | safe}}), 
								confidence.level= c({{selected.confidence_level | safe}}), 
								std.dev = '{{selected.SStddev | safe}}', 
								digits ={{selected.digits | safe}}, 
								spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
								additional.sigma.lines = c({{selected.sdWarnLimits| safe}}),
								mark.test.number = {{selected.markTestNumberChk | safe}}
								)
								
}


if(!is.null(xbar.one.IMR.spc.qcc.objects))
{
			print.qcc.spc.phases(qcc.spc.phases.obects = xbar.one.IMR.spc.qcc.objects,
									chart.title.name = 'I-MR Between',
									print.stats = {{selected.printStatChk | safe}}, 
									print.test.summary = {{selected.printTestSummaryChk | safe}}, 
									print.test.detail = {{selected.printTestDetailChk | safe}},
									print.qcc.object.summary = {{selected.printObjectSummaryChk | safe}},
									digits = {{selected.digits | safe}}, 
									phase.names = {{selected.phaseNames | safe}}
								)
		
		{{if(options.selected.showMRChartChk === 'TRUE')}}
		
			if(!is.null(xbar.one.XMR.spc.qcc.objects))
			{
				print.qcc.spc.phases(qcc.spc.phases.obects = xbar.one.XMR.spc.qcc.objects,
										chart.title.name = 'MR',
										print.stats = {{selected.printStatChk | safe}}, 
										print.test.summary = {{selected.printTestSummaryChk | safe}}, 
										print.test.detail = {{selected.printTestDetailChk | safe}},
										print.qcc.object.summary = {{selected.printObjectSummaryChk | safe}},
										digits = {{selected.digits | safe}}, 
										phase.names = {{selected.phaseNames | safe}}
									)
			}	
			
		{{/if}}
		
			if(!is.null(S.spc.qcc.objects))
			{
				print.qcc.spc.phases(qcc.spc.phases.obects = S.spc.qcc.objects,
										chart.title.name = 'I-MR Within',
										print.stats = {{selected.printStatChk | safe}}, 
										print.test.summary = {{selected.printTestSummaryChk | safe}}, 
										print.test.detail = {{selected.printTestDetailChk | safe}},
										print.qcc.object.summary = {{selected.printObjectSummaryChk | safe}},
										digits = {{selected.digits | safe}}, 
										phase.names = {{selected.phaseNames | safe}}
									)
			}	
}
	
`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) }, 
			
			/*
			chartTypeXbarChk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('chartTypeXbarChk'), 
					no: "chartTypeXbarChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "xbar",
                    false_value: " ",
					state: "checked",
					newline: true,
                })
            },
			chartTypeRChk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('chartTypeRChk'), 
					no: "chartTypeRChk",
                    bs_type: "valuebox",
                    style: "mb-1",
                    extraction: "BooleanValue",
                    true_value: "R",
                    false_value: " ",
					newline: true,
                })
            },
			chartTypeSChk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('chartTypeSChk'), 
					no: "chartTypeSChk",
                    bs_type: "valuebox",
                    style: "mb-2",
                    extraction: "BooleanValue",
                    true_value: "S",
                    false_value: " ",
					newline: true,
                })
            },
			*/
			
			label2: { 
				el: new labelVar(config, { 
					label: shewhartCharts4.t('label2'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			selectDatasetRad: {
                el: new radioButton(config, {
                    label: shewhartCharts4.t('selectDatasetRad'),
                    no: "gpbox1",
                    increment: "selectDatasetRad",
                    value: "dataset",
                    state: "",
					//style: "mb-3",
                    extraction: "ValueAsIs",
                })
            },
			variablelistSelcted: {
                el: new dstVariableList(config, {
                    label: shewhartCharts4.t('variablelistSelcted'),
                    no: "variablelistSelcted",
                    required: false,
                    filter: "Numeric|Scale",
					style: "mt-1 mb-3",
                    extraction: "NoPrefix|UseComma|Enclosed",
                })
            },
			selectVariableRad: {
                el: new radioButton(config, {
                    label: shewhartCharts4.t('selectVariableRad'),
                    no: "gpbox1",
                    increment: "selectVariableRad",
                    value: "variable",
                    state: "checked",
                    extraction: "ValueAsIs",
                })
            },
			variableSelcted: {
                el: new dstVariable(config, {
                    label: shewhartCharts4.t('variableSelcted'),
                    no: "variableSelcted",
                    required: false,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "Numeric|Scale",
					style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			
			/*
			groupingNeededChk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('groupingNeededChk'), 
					no: "groupingNeededChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1, ml-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			*/
			
			groupingVariable: {
                el: new dstVariable(config, {
                    label: shewhartCharts4.t('groupingVariable'),
                    no: "groupingVariable",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					style: "ml-5",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			displayGroupsChk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('displayGroupsChk'), 
					no: "displayGroupsChk",
                    bs_type: "valuebox",
                    style: "mt-2 ml-5 mb-2",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			variableControlLimits: {
                el: new input(config, {
                    no: 'variableControlLimits',
                    label: shewhartCharts4.t('variableControlLimits'),
                    placeholder: "",
                    required: false,
                    //type: "character",
					filter: "character|numeric",
					style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			
			showMRChartChk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('showMRChartChk'), 
					no: "showMRChartChk",
                    extraction: "Boolean",
					state: "checked",
					newline: true,
                })
            },
			
			printStatChk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('printStatChk'), 
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
                    label: shewhartCharts4.t('printObjectSummaryChk'), 
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
                    label: shewhartCharts4.t('printTestSummaryChk'), 
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
                    label: shewhartCharts4.t('printTestDetailChk'), 
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
                    label: shewhartCharts4.t('markTestNumberChk'), 
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
			
			rowsTobeUsed: {
                el: new input(config, {
                    no: 'rowsTobeUsed',
                    label: shewhartCharts4.t('rowsTobeUsed'),
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
                    label: shewhartCharts4.t('rowsTobeUsedAsNewData'),
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
					label: shewhartCharts4.t('additionalPhasesLabel'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			phaseNames: {
                el: new input(config, {
                    no: 'phaseNames',
                    label: shewhartCharts4.t('phaseNames'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase2'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase3'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase4'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase5'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase6'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase7'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase8'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase9'),
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
                    label: shewhartCharts4.t('rowsTobeUsedPhase10'),
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
                    label: shewhartCharts4.t('nsigmas'),
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
                    label: shewhartCharts4.t('confidence_level'),
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
                    label: shewhartCharts4.t('sdWarnLimits'),
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
                    label: shewhartCharts4.t('digits'),
                    required: true,
                    min: 0,
                    max: 15,
                    step: 1,
                    value: 2,
					width: "w-25",
					style: "mb-2",
                })
            }, 

			/*
			xbarStddev: {
                el: new selectVar(config, {
                    no: 'xbarStddev',
                    label: shewhartCharts4.t('xbarStddev'),
                    multiple: false,
                    extraction: "NoPrefix|UseComma",
                    options: ["UWAVE-SD", "RMSDF", "UWAVE-R", "MVLUE-R", "MVLUE-SD"],
                    default: "UWAVE-SD",
					//style: "mb-3",
					//width: "w-25",
                })
            },
			RStddev: {
                el: new selectVar(config, {
                    no: 'RStddev',
                    label: shewhartCharts4.t('RStddev'),
                    multiple: false,
                    extraction: "NoPrefix|UseComma",
                    options: ["UWAVE-R", "MVLUE-R"],
                    default: "UWAVE-R",
					//style: "mb-3",
					//width: "w-25",
                })
            },
			*/
			SStddev: {
                el: new selectVar(config, {
                    no: 'SStddev',
                    label: shewhartCharts4.t('SStddev'),
                    multiple: false,
                    extraction: "NoPrefix|UseComma",
                    options: ["MVLUE-SD", "UWAVE-SD", "RMSDF"], 
                    default: "MVLUE-SD",
					style: "mb-3",
					//width: "w-25",
                })
            },
			
			
			
			lower: {
                el: new input(config, {
                    no: 'lower',
                    label: shewhartCharts4.t('lower'),
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
                    label: shewhartCharts4.t('upper'),
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
					label: shewhartCharts4.t('performTestLabel'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			test1Chk: {
                el: new checkbox(config, {
                    label: shewhartCharts4.t('test1Chk'), 
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
                    label: shewhartCharts4.t('test2Chk'), 
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
                    label: shewhartCharts4.t('test3Chk'), 
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
                    label: shewhartCharts4.t('test4Chk'), 
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
                    label: shewhartCharts4.t('test5Chk'), 
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
                    label: shewhartCharts4.t('test6Chk'), 
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
                    label: shewhartCharts4.t('test7Chk'), 
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
                    label: shewhartCharts4.t('test8Chk'), 
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
                    label: shewhartCharts4.t('test1'),
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
                    label: shewhartCharts4.t('test2'),
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
                    label: shewhartCharts4.t('test3'),
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
                    label: shewhartCharts4.t('test4'),
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
                    label: shewhartCharts4.t('test5'),
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
                    label: shewhartCharts4.t('test6'),
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
                    label: shewhartCharts4.t('test7'),
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
                    label: shewhartCharts4.t('test8'),
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
					//objects.chartTypeXbarChk.el.content,
					//objects.chartTypeRChk.el.content,
					//objects.chartTypeSChk.el.content,
					
					objects.label2.el.content,
					objects.selectVariableRad.el.content,
					objects.variableSelcted.el.content, 
					
					objects.groupingVariable.el.content,
					objects.variableControlLimits.el.content,
					objects.displayGroupsChk.el.content,
					
					objects.selectDatasetRad.el.content,
					objects.variablelistSelcted.el.content,
					
					objects.showMRChartChk.el.content,
					
					objects.SStddev.el.content,
					objects.nsigmas.el.content,
					objects.confidence_level.el.content,
					
					objects.upper.el.content,
					objects.lower.el.content,
					
					objects.sdWarnLimits.el.content, 
					
					objects.digits.el.content,
					
					//objects.xbarStddev.el.content,
					//objects.RStddev.el.content,
					
					
					objects.printStatChk.el.content, 
					objects.printObjectSummaryChk.el.content,
					
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
                name: shewhartCharts4.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: shewhartCharts4.t('help.title'),
            r_help: "help(data,package='utils')",
            body: shewhartCharts4.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new shewhartCharts4().render()
}
