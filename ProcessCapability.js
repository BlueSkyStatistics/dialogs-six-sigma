
var localization = {
    en: {
        title: "Process Capability",
		navigation: "Process Capability",
		
		//chartTypeXbarChk: "xbar chart",
		
		summaryPlotChk: "Plot the underlying xbar and I charts used for computing the process capability",
		displaySLlineonPlotChk: "Display LSL and USL lines on the underlying plots",
		printObjectSummaryChk: "Print QCC object summary for the process capability",
		
		label1: "Two options - either select a variable (and a grouping variable as needed) or select the variables in dataset if already grouped",
		
		label2: "Advanced options",
		
		selectVariableRad: "Option 1: Select a variable from the dataset (optionally that may need to be grouped)",
		variableSelcted: "Select a variable (observed data) to analyze",
		groupingVariable: "Grouping variable (optional - if the data needs to be grouped)",
		variableControlLimits: "Data rows to be discarded, if any, before grouping for variable control limits ( e.g. specify as 1:25 or 1,4,5,7:12)",
		displayGroupsChk: "Display the groupings on the dataset UI grid for reference only",
		
		selectDatasetRad: "Option 2: Select the variables from the Dataset if already grouped",
		variablelistSelcted: "Select one or more grouped variables (observed data) to analyze", 
		
		rowsTobeUsed: "If selected (grouped) rows to be used to analyze for process capability ( e.g.  specify as 1:25 or 1,4,5,7:12)",
		nsigmas: "Sigma - number of sigmas to use for computing control limits",
		confidence_level: "A numeric value between 0 and 1 specifying the level to use for computing confidence intervals",
		stddev: "Standard deviation method for calculating xbar (for group size > 1)", 
		//sdWarnLimits: "Add warning limits (e.g. 2) at the specific std. deviations (for xbar charts only)",
		
		lower: "LSL - numeric value of lower specification limit (type NA to specify one-sided specification limits)",
		upper: "USL - numeric value of upper specification limit (type NA to specify one-sided specification limits)",
		target: "Target specification limits (optional)",
		digits: "Digits - number of digits to display",
		
		help: {
            title: "Process Capability (qcc pkg)",
            r_help: "help(process.capability, package = qcc)",
			body: `
				<b>Description</b></br>
				qcc function to to computes capability indices 
				<br/>
				<br/>
				For the detail help - use R help(process.capability, package = qcc), help(qcc, package = qcc), help(sd.xbar, package = qcc), and help(sd.xbar.one, package = qcc)
				<br/>
				<br/>
				To try this, you may load the dataset called pistonrings from the qcc package with Load Dataset menu by selecting qcc package and then select pistonrings dataset
				<br/>
				Choose option 1, diameter to variable(observed..), sample to Grouping variable, 
				<br/>
				Type 1:25 in Grouped rows to be used
				<br/>
				For Process capability analysis - type 73.95 in LSL, 74.05 in USL, and 74.0 in Target Limit
				<br/>
				<br/>
				Follow the qcc tutorial at https://cran.r-project.org/web/packages/qcc/vignettes/qcc_a_quick_tour.html
				<br/>
			`
		},
	
	}
}

class processCapabilityQcc extends baseModal {
    constructor() {
        var config = {
            id: "processCapability",
            label: localization.en.title,
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
				row_num_with_NAs = which(is.na({{dataset.name}}\${{selected.variableSelcted | safe}}) | {{dataset.name}}\${{selected.variableSelcted | safe}} == "" | is.na({{dataset.name}}\${{selected.groupingVariable | safe}}) | {{dataset.name}}\${{selected.groupingVariable | safe}} == "")
				row_num_with_NAs_variableControlLimits = c(row_num_with_NAs, c({{selected.variableControlLimits | safe}}))
				if(length(row_num_with_NAs_variableControlLimits) > 0)
				{
					dataset_NAs_removed = {{dataset.name}}[-c(row_num_with_NAs_variableControlLimits),]
				}else
				{
					dataset_NAs_removed = {{dataset.name}}
				}
				
				#selectedData_variable_control_limit = with(dataset_NAs_removed, qcc.groups(c({{selected.variableSelcted | safe}})[-c({{selected.variableControlLimits | safe}})], c({{selected.groupingVariable | safe}})[-c({{selected.variableControlLimits | safe}})]))
				selectedData_variable_control_limit = with(dataset_NAs_removed, qcc.groups(c({{selected.variableSelcted | safe}}), c({{selected.groupingVariable | safe}})))
				
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
														#data = dataset_NAs_removed\${{selected.variableSelcted | safe}},
														data.name = paste(data_name, "( variable sample size of ",sample_size, ")"),
														nsigmas = c({{selected.nsigmas | safe}})))
				
				qccXPotential = qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
									type="xbar", std.dev = '{{selected.stddev | safe}}', 
									data = selectedData_variable_control_limit[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}], 
									data.name = paste(data_name, "( variable sample size of ",sample_size, ")"),
									nsigmas = c({{selected.nsigmas | safe}}))
			
			{{#else}}
				row_num_with_NAs = which(is.na({{dataset.name}}\${{selected.variableSelcted | safe}}) | {{dataset.name}}\${{selected.variableSelcted | safe}} == "")
				row_num_with_NAs_variableControlLimits = c(row_num_with_NAs, c({{selected.variableControlLimits | safe}}))
				if(length(row_num_with_NAs_variableControlLimits) > 0)
				{
					dataset_NAs_removed = {{dataset.name}}[-c(row_num_with_NAs_variableControlLimits),]
				}else
				{
					dataset_NAs_removed = {{dataset.name}}
				}
				
				#selectedData_variable_control_limit = with(dataset_NAs_removed, c({{selected.variableSelcted | safe}})[-c({{selected.variableControlLimits | safe}})])
				selectedData_variable_control_limit = with(dataset_NAs_removed, c({{selected.variableSelcted | safe}}))
				
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
				row_num_with_NAs = which(is.na({{dataset.name}}\${{selected.variableSelcted | safe}}) | {{dataset.name}}\${{selected.variableSelcted | safe}} == "" | is.na({{dataset.name}}\${{selected.groupingVariable | safe}}) | {{dataset.name}}\${{selected.groupingVariable | safe}} == "")
				if(length(row_num_with_NAs) > 0)
				{
					dataset_NAs_removed = {{dataset.name}}[-c(row_num_with_NAs),]
				}else
				{
					dataset_NAs_removed = {{dataset.name}}
				}
				
				selectedData = with(dataset_NAs_removed, qcc::qcc.groups(c({{selected.variableSelcted | safe}}), c({{selected.groupingVariable | safe}})))
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
									#data = as.vector(as.matrix(t(selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}]))), 
									data = (dataset_NAs_removed\${{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}], 
									data.name = paste(data_name, "( sample size of ",sample_size, ")"),								
									nsigmas = c({{selected.nsigmas | safe}})) 
				
				qccXPotential = qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
									type="xbar", std.dev = '{{selected.stddev | safe}}', 
									data = selectedData[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}), {{/if}}],
									data.name = paste(data_name, "( sample size of ",sample_size, ")"),
									nsigmas = c({{selected.nsigmas | safe}})) 
			{{#else}}
				row_num_with_NAs = which(is.na({{dataset.name}}\${{selected.variableSelcted | safe}}) | {{dataset.name}}\${{selected.variableSelcted | safe}} == "")
				if(length(row_num_with_NAs) > 0)
				{
					dataset_NAs_removed = {{dataset.name}}[-c(row_num_with_NAs),]
				}else
				{
					dataset_NAs_removed = {{dataset.name}}
				}
				sample_size = 1
				
				dataOverall =  with(dataset_NAs_removed, c({{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}])
				dataPotential = with(dataset_NAs_removed, c({{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}])
				
				qccXOneOverall = with(dataset_NAs_removed, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
															type="xbar.one", std.dev = "SD", 
															data = c({{selected.variableSelcted | safe}})[{{if(options.selected.rowsTobeUsed !== "")}} c({{selected.rowsTobeUsed | safe}}) {{/if}}], 
															data.name = paste(data_name, "( sample size of ",sample_size, ")"),
															nsigmas = c({{selected.nsigmas | safe}})))
				
				qccXOnePotential = with(dataset_NAs_removed, qcc::qcc(plot = FALSE,  rules = c(), digits = {{selected.digits | safe}}, 
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
					label: localization.en.label1, 
					h: 6, 
					style: "mb-2",
				}) 
			},
			label2: { 
				el: new labelVar(config, { 
					label: localization.en.label2, 
					h: 4, 
					style: "mb-2",
				}) 
			},
			selectDatasetRad: {
                el: new radioButton(config, {
                    label: localization.en.selectDatasetRad,
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
                    label: localization.en.variablelistSelcted,
                    no: "variablelistSelcted",
                    required: false,
                    filter: "Numeric|Scale",
					style: "mt-1 mb-3",
                    extraction: "NoPrefix|UseComma|Enclosed",
                })
            },
			selectVariableRad: {
                el: new radioButton(config, {
                    label: localization.en.selectVariableRad,
                    no: "gpbox1",
                    increment: "selectVariableRad",
                    value: "variable",
                    state: "checked",
                    extraction: "ValueAsIs",
                })
            },
			variableSelcted: {
                el: new dstVariable(config, {
                    label: localization.en.variableSelcted,
                    no: "variableSelcted",
                    required: false,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "Numeric|Scale",
					style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			groupingNeededChk: {
                el: new checkbox(config, {
                    label: localization.en.groupingNeededChk, 
					no: "groupingNeededChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1, ml-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			groupingVariable: {
                el: new dstVariable(config, {
                    label: localization.en.groupingVariable,
                    no: "groupingVariable",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					style: "ml-5",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			displayGroupsChk: {
                el: new checkbox(config, {
                    label: localization.en.displayGroupsChk, 
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
                    label: localization.en.variableControlLimits,
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
                    label: localization.en.rowsTobeUsed,
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
                    label: localization.en.nsigmas,
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
                    label: localization.en.confidence_level,
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
                    label: localization.en.lower,
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
                    label: localization.en.upper,
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
                    label: localization.en.target,
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
                    label: localization.en.stddev,
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
                    label: localization.en.digits,
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
                    label: localization.en.printObjectSummaryChk, 
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
                    label: localization.en.summaryPlotChk, 
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
                    label: localization.en.displaySLlineonPlotChk, 
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
                name: localization.en.navigation,
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        this.help = localization.en.help;
    }
}
module.exports.item = new processCapabilityQcc().render()