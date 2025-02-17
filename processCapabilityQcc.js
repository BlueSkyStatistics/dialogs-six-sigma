


class processCapabilityQcc extends baseModal {
    static dialogId = 'processCapabilityQcc'
    static t = baseModal.makeT(processCapabilityQcc.dialogId)

    constructor() {
        var config = {
            id: processCapabilityQcc.dialogId,
            label: processCapabilityQcc.t('title'),
            modalType: "two",
            RCode:`
require(qcc)

	selectedData = NULL
	qccXOneOverall = NULL
	qccXPotential = NULL 
	qccXOnePotential = NULL
	dataOverall = NULL
	dataPotential = NULL
	xbar.overall.qcc.objects = NULL
	xbar.potential.qcc.objects = NULL
	
	sample_size = 1
	
	
{{if(options.selected.lower === 'NA' && options.selected.upper === 'NA')}}
	BSkyFormat("\nError: LSL or USL or both must be specified\n")
{{#else}}
	{{if(options.selected.gpbox1 === 'variable')}}
		
		data_name = '{{selected.variableSelcted | safe}}{{if(options.selected.rowsTobeUsed !== "")}} [c({{selected.rowsTobeUsed | safe}})] {{/if}}'
		
		{{if(options.selected.variableControlLimits !== '')}}
			{{if(options.selected.groupingVariable !== '')}}
				selectedData_variable_control_limit = with({{dataset.name}}, qcc.groups(c({{selected.variableSelcted | safe}})[-c({{selected.variableControlLimits | safe}})], c({{selected.groupingVariable | safe}})[-c({{selected.variableControlLimits | safe}})]))
				
				{{if(options.selected.displayGroupsChk === 'TRUE')}}
					{{selected.variableSelcted | safe}}ControlLimit = as.data.frame(selectedData_variable_control_limit)
					BSkyLoadRefresh('{{selected.variableSelcted | safe}}ControlLimit')
				{{/if}}
				
				sample_size = dim(selectedData_variable_control_limit)[2]
				
				selectedData_variable_control_limitNonNAs = selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}]
				selectedData_variable_control_limitNonNAs = as.vector(as.matrix(t(selectedData_variable_control_limitNonNAs)))
				selectedData_variable_control_limitNonNAs = selectedData_variable_control_limitNonNAs[!is.na(selectedData_variable_control_limitNonNAs)]
				
				dataOverall = with({{dataset.name}}, selectedData_variable_control_limitNonNAs)
				dataPotential = selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}]
				
				qccXOneOverall = with({{dataset.name}}, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
														type="xbar.one", std.dev = "SD", 
														data = selectedData_variable_control_limitNonNAs, 
														data.name = paste(data_name, "( variable sample size of ",sample_size, ")"),
														nsigmas = c({{selected.nsigmas | safe}})))
				
				qccXPotential = qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
									type="xbar", std.dev = '{{selected.stddev | safe}}', 
									data = selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}], 
									data.name = paste(data_name, "( variable sample size of ",sample_size, ")"),
									nsigmas = c({{selected.nsigmas | safe}}))
			
			{{#else}}
				selectedData_variable_control_limit = with({{dataset.name}}, c({{selected.variableSelcted | safe}})[-c({{selected.variableControlLimits | safe}})])
				
				sample_size = 1
				
				dataOverall   = with({{dataset.name}}, selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}])
				dataPotential = with({{dataset.name}}, selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}])
				
				qccXOneOverall = with({{dataset.name}}, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
																type="xbar.one", std.dev = "SD", 
																data = selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}], 
																data.name = paste(data_name, "( sample size of ",sample_size, ")"),
																nsigmas = c({{selected.nsigmas | safe}})))
				
				qccXOnePotential = with({{dataset.name}}, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
															type="xbar.one", std.dev = "MR", 
															data = selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}], 
															data.name = paste(data_name, "( sample size of ",sample_size, ")"),
															nsigmas = c({{selected.nsigmas | safe}}))) 
			{{/if}}
		{{#else}}
			{{if(options.selected.groupingVariable !== '')}}
				selectedData = with({{dataset.name}}, qcc::qcc.groups(c({{selected.variableSelcted | safe}}), c({{selected.groupingVariable | safe}})))
			
				sample_size = dim(selectedData)[2]
				
				{{if(options.selected.displayGroupsChk === 'TRUE')}}
					if(!is.null(selectedData)){
						{{selected.variableSelcted | safe}}Gpd = as.data.frame(selectedData)
						BSkyLoadRefresh('{{selected.variableSelcted | safe}}Gpd')
					}
				{{/if}}
				
				dataOverall = as.vector(as.matrix(t(selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}])))
				dataPotential = selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}]
				
				qccXOneOverall = qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
									type="xbar.one", std.dev = 'SD', 
									data = as.vector(as.matrix(t(selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}]))), 
									data.name = paste(data_name, "( sample size of ",sample_size, ")"),								
									nsigmas = c({{selected.nsigmas | safe}})) 
				
				qccXPotential = qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
									type="xbar", std.dev = '{{selected.stddev | safe}}', 
									data = selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}], 
									data.name = paste(data_name, "( sample size of ",sample_size, ")"),
									nsigmas = c({{selected.nsigmas | safe}})) 
			{{#else}}
				sample_size = 1
				
				dataOverall =  with({{dataset.name}}, c({{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}])
				dataPotential = with({{dataset.name}}, c({{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}])
				
				qccXOneOverall = with({{dataset.name}}, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
															type="xbar.one", std.dev = "SD", 
															data = c({{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}], 
															data.name = paste(data_name, "( sample size of ",sample_size, ")"),
															nsigmas = c({{selected.nsigmas | safe}})))
				
				qccXOnePotential = with({{dataset.name}}, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
															type="xbar.one", std.dev = "MR", 
															data = c({{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}], 
															data.name = paste(data_name, "( sample size of ",sample_size, ")"),
															nsigmas = c({{selected.nsigmas | safe}})))
			{{/if}}
		{{/if}}
	{{#else}}
		data_name = '{{dataset.name}}{{if(options.selected.rowsTobeUsed !== "")}} [c({{selected.rowsTobeUsed | safe}}),] {{/if}}'
		
		{{if(options.selected.variablelistSelcted === '')}}
			selectedData = {{dataset.name}}[]
		{{#else}}
			selectedData = {{dataset.name}}[, c({{selected.variablelistSelcted | safe}})]
		{{/if}}
		
		sample_size = dim(selectedData)[2]
		
		selectedData_variable_control_limitNonNAs = with({{dataset.name}}, selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}])
		selectedData_variable_control_limitNonNAs = as.vector(as.matrix(t(selectedData_variable_control_limitNonNAs)))
		selectedData_variable_control_limitNonNAs = selectedData_variable_control_limitNonNAs[!is.na(selectedData_variable_control_limitNonNAs)]
				
		dataOverall = selectedData_variable_control_limitNonNAs
		dataPotential = selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}]
		
		qccXOneOverall = with({{dataset.name}}, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
													type="xbar.one", std.dev = "SD", 
													data = selectedData_variable_control_limitNonNAs, 
													data.name = paste(data_name, "( sample size of ",sample_size, ")"),
													nsigmas = c({{selected.nsigmas | safe}})))

		qccXPotential = qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
							type="xbar", std.dev = '{{selected.stddev | safe}}', 
							data = selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}], 
							data.name = paste(data_name, "( sample size of ",sample_size, ")"),															
							nsigmas = c({{selected.nsigmas | safe}}))
	{{/if}}
	
	
	if(!is.null(qccXOneOverall)){
		BSkyFormat(paste("Overall Process Capability Indices for ", data_name, "from dataset {{dataset.name}}"))
		process.capability.enhanced(qccXOneOverall {{selected.confidence_level | safe}}, 
									digits = {{selected.digits | safe}}, 
									nsigmas = c({{selected.nsigmas | safe}}), 
									print = {{selected.printObjectSummaryChk | safe}}, 
									capability.type = "overall", 
									spec.limits=c(c({{selected.lower | safe}}),c({{selected.upper | safe}})) {{selected.target | safe}})
	}
	
	if(!is.null(qccXPotential)){
		BSkyFormat(paste("\nPotential (Within) Process Capability Indices for ", data_name, "from dataset {{dataset.name}}"))
		process.capability.enhanced(qccXPotential {{selected.confidence_level | safe}}, 
									digits = {{selected.digits | safe}}, 
									nsigmas = c({{selected.nsigmas | safe}}), 
									print = {{selected.printObjectSummaryChk | safe}}, 
									capability.type = "potential", spec.limits=c(c({{selected.lower | safe}}),c({{selected.upper | safe}})) {{selected.target | safe}})
	}else{
		BSkyFormat(paste("\nPotential (Within) Process Capability Indices for ", data_name, "from dataset {{dataset.name}}"))
		process.capability.enhanced(qccXOnePotential {{selected.confidence_level | safe}}, 
									digits = {{selected.digits | safe}}, 
									nsigmas = c({{selected.nsigmas | safe}}), 
									print = {{selected.printObjectSummaryChk | safe}}, 
									capability.type = "potential", spec.limits=c(c({{selected.lower | safe}}),c({{selected.upper | safe}})) {{selected.target | safe}})
	}
	
	
	{{if(options.selected.summaryPlotChk === 'TRUE')}}
		BSkySetSixSigmaTestOptions(digits = {{selected.digits | safe}})
		
		if(!is.null(qccXOneOverall)){
			BSkyFormat(paste("\nxbar.one chart used for computing Overall Process Capability Indices for ", data_name))
			
			xbar.overall.qcc.objects = plot.qcc.spc.phases(
										type = 'xbar.one',
										data = dataOverall, 
										data.name = data_name,
										chart.title.name = 'I (xbar.one)',
										ylab = "Individual value",
										xlab = "Observation", 
										std.dev = 'SD',  
										digits = {{selected.digits | safe}}, 
										{{if(options.selected.displaySLlineonPlotChk === 'TRUE')}}
											spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
										{{/if}}
										nsigmas = c({{selected.nsigmas | safe}}) 
									)
		}
		if(!is.null(qccXOneOverall)){
			print.qcc.spc.phases(
								qcc.spc.phases.obects = xbar.overall.qcc.objects,
								chart.title.name = 'I (xbar.one)',
								print.stats = TRUE, 
								print.test.summary = TRUE, 
								digits = {{selected.digits | safe}} 
								)
		}
	{{/if}}
	
	{{if(options.selected.summaryPlotChk === 'TRUE')}}
		if(!is.null(qccXPotential)){
			BSkyFormat(paste("xbar chart used for computing Potential (Within) Process Capability Indices for ", data_name))
			#plot(qccXPotential, digits = {{selected.digits | safe}})
			chart_type = 'xbar'
			chart_title = 'xbar'
			ylab_title = "Group summary statistics"
			xlab_title = "Group"
			stddev = '{{selected.stddev | safe}}'
		}else{
			BSkyFormat(paste("xbar.one Chart Used for Computing Potential Process Capability Indices for ", data_name))
			#plot(qccXOnePotential, digits = {{selected.digits | safe}})
			chart_type = 'xbar.one'
			chart_title = 'xbar.one'
			ylab_title = "Individual value"
			xlab_title = "Observation"
			stddev = 'MR'
		}
		
		xbar.potential.qcc.objects = plot.qcc.spc.phases(
										type = chart_type,
										data = dataPotential, 
										data.name = data_name,
										chart.title.name = chart_title,
										ylab = ylab_title,
										xlab = xlab_title, 
										std.dev = stddev,  
										digits = {{selected.digits | safe}}, 
										{{if(options.selected.displaySLlineonPlotChk === 'TRUE')}}
											spec.limits = list(lsl=c({{selected.lower | safe}}), usl= c({{selected.upper | safe}})),
										{{/if}}
										nsigmas = c({{selected.nsigmas | safe}}) 
									)
				
		print.qcc.spc.phases(
								qcc.spc.phases.obects = xbar.potential.qcc.objects,
								chart.title.name = chart_title,
								print.stats = TRUE, 
								print.test.summary = TRUE, 
								digits = {{selected.digits | safe}} 
							)
	{{/if}}

{{/if}}

`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) },
			label1: { 
				el: new labelVar(config, { 
					label: processCapabilityQcc.t('label1'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			label2: { 
				el: new labelVar(config, { 
					label: processCapabilityQcc.t('label2'), 
					h: 4, 
					style: "mb-2",
				}) 
			},
			selectDatasetRad: {
                el: new radioButton(config, {
                    label: processCapabilityQcc.t('selectDatasetRad'),
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
                    label: processCapabilityQcc.t('variablelistSelcted'),
                    no: "variablelistSelcted",
                    required: false,
                    filter: "Numeric|Scale",
					style: "mt-1 mb-3",
                    extraction: "NoPrefix|UseComma|Enclosed",
                })
            },
			selectVariableRad: {
                el: new radioButton(config, {
                    label: processCapabilityQcc.t('selectVariableRad'),
                    no: "gpbox1",
                    increment: "selectVariableRad",
                    value: "variable",
                    state: "checked",
                    extraction: "ValueAsIs",
                })
            },
			variableSelcted: {
                el: new dstVariable(config, {
                    label: processCapabilityQcc.t('variableSelcted'),
                    no: "variableSelcted",
                    required: false,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "Numeric|Scale",
					style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			/*groupingNeededChk: {
                el: new checkbox(config, {
                    label: processCapabilityQcc.t('groupingNeededChk'), 
					no: "groupingNeededChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1, ml-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },*/
			groupingVariable: {
                el: new dstVariable(config, {
                    label: processCapabilityQcc.t('groupingVariable'),
                    no: "groupingVariable",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					style: "ml-5",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			displayGroupsChk: {
                el: new checkbox(config, {
                    label: processCapabilityQcc.t('displayGroupsChk'), 
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
                    label: processCapabilityQcc.t('variableControlLimits'),
                    placeholder: "",
                    required: false,
                    type: "character",
					style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			rowsTobeUsed: {
                el: new input(config, {
                    no: 'rowsTobeUsed',
                    label: processCapabilityQcc.t('rowsTobeUsed'),
                    placeholder: "",
                    required: false,
                    type: "character",
					style: "mb-4",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
					//wrapped: 'c(%val%) ',
                })
            },
			nsigmas: {
                el: new input(config, {
                    no: 'nsigmas',
                    label: processCapabilityQcc.t('nsigmas'),
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
                    label: processCapabilityQcc.t('confidence_level'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "0.95",
					wrapped: ', confidence.level=c(%val%)',
					width: "w-25",
					//style: "mb-2",
                })
            },
			lower: {
                el: new input(config, {
                    no: 'lower',
                    label: processCapabilityQcc.t('lower'),
                    placeholder: "",
                    required: true,
                    //type: "numeric",
					//style: "ml-3",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "NA",
					width: "w-25",
                })
            },
			upper: {
                el: new input(config, {
                    no: 'upper',
                    label: processCapabilityQcc.t('upper'),
                    placeholder: "",
                    required: true,
                    //type: "numeric",
					//style: "ml-3",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "NA",
					width: "w-25",
                })
            },
			target: {
                el: new input(config, {
                    no: 'target',
                    label: processCapabilityQcc.t('target'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
					//style: "ml-3",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: ', target=c(%val%)',
					width: "w-25",
                })
            },
			stddev: {
                el: new selectVar(config, {
                    no: 'stddev',
                    label: processCapabilityQcc.t('stddev'),
                    multiple: false,
                    extraction: "NoPrefix|UseComma",
                    options: ["RMSDF", "UWAVE-R", "UWAVE-SD", "MVLUE-R", "MVLUE-SD"],
                    default: "RMSDF",
					//width: "w-25",
                })
            },
			digits: {
                el: new inputSpinner(config, {
                    no: 'digits',
                    label: processCapabilityQcc.t('digits'),
                    required: true,
                    min: 0,
                    max: 15,
                    step: 1,
                    value: 6,
					width: "w-25",
					style: "mb-2",
                })
            }, 
			printObjectSummaryChk: {
                el: new checkbox(config, {
                    label: processCapabilityQcc.t('printObjectSummaryChk'), 
					no: "printObjectSummaryChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			summaryPlotChk: { 
                el: new checkbox(config, {
                    label: processCapabilityQcc.t('summaryPlotChk'), 
					no: "summaryPlotChk",
                    bs_type: "valuebox",
                    style: "mt-2",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			displaySLlineonPlotChk: { 
                el: new checkbox(config, {
                    label: processCapabilityQcc.t('displaySLlineonPlotChk'), 
					no: "displaySLlineonPlotChk",
                    bs_type: "valuebox",
                    style: "ml-3 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [
					objects.label1.el.content,
					objects.selectVariableRad.el.content,
					objects.variableSelcted.el.content, 
					
					objects.groupingVariable.el.content,
					objects.variableControlLimits.el.content,
					objects.displayGroupsChk.el.content,
					
					objects.selectDatasetRad.el.content,
					objects.variablelistSelcted.el.content,
					
					objects.lower.el.content,
					objects.upper.el.content,
					objects.target.el.content,
					
					objects.rowsTobeUsed.el.content,
					
					objects.label2.el.content,
					objects.nsigmas.el.content,
					objects.confidence_level.el.content,
					objects.stddev.el.content,
					objects.digits.el.content, 
					objects.summaryPlotChk.el.content,
					objects.displaySLlineonPlotChk.el.content,
					objects.printObjectSummaryChk.el.content
					],
            nav: {
                name: processCapabilityQcc.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: processCapabilityQcc.t('help.title'),
            r_help: processCapabilityQcc.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: processCapabilityQcc.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new processCapabilityQcc().render()
}
