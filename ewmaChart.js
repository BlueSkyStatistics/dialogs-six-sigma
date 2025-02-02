


class ewmaChart extends baseModal {
    static dialogId = 'ewmaChart'
    static t = baseModal.makeT(ewmaChart.dialogId)

    constructor() {
        var config = {
            id: ewmaChart.dialogId,
            label: ewmaChart.t('title'),
            modalType: "two",
            RCode:`
require(qcc)

#chart type EWMA
qccEwma = NULL 	
selectedData = NULL


{{if(options.selected.gpbox1  === "variable")}}
	{{if(options.selected.variableSelcted  !== "")}}
		{{if(options.selected.groupingVariable  !== "")}}
			selectedData = with({{dataset.name}}, qcc.groups(c({{selected.variableSelcted | safe}}), c({{selected.groupingVariable | safe}})))
		{{#else}}
			selectedData = with({{dataset.name}}, c({{selected.variableSelcted | safe}}))
		{{/if}}
		
		{{if(options.selected.displayGroupsChk  === "TRUE")}}
			if(!is.null(selectedData))
			{
				{{selected.variableSelcted | safe}}Gpd = as.data.frame(selectedData)
				#BSkyFormat({{selected.variableSelcted | safe}}, outputTableRenames = paste("Grouping generated for {{selected.variableSelcted | safe}} by {{selected.groupingVariable | safe}}"))
				BSkyLoadRefresh('{{selected.variableSelcted | safe}}Gpd')
			}
		{{/if}}
		
		data_name = '{{selected.variableSelcted | safe}}'
		
	{{#else}}
		BSkyFormat("A variable must be selected")
	{{/if}}
{{#else}}
	{{if(options.selected.variablelistSelcted  === "")}}
		selectedData = {{dataset.name}}[]
	{{#else}}
		selectedData = {{dataset.name}}[, c({{selected.variablelistSelcted | safe}})]
	{{/if}}
	data_name = '{{dataset.name}}'
{{/if}}

	if(!is.null(selectedData)) {
		{{if(options.selected.groupingVariable  !== ""|| options.selected.gpbox1  !== "variable")}}
			{{if(options.selected.rowsTobeUsedAsNewData  === "" || options.selected.rowsTobeUsed  === "")}}
				BSkyFormat(paste("\nEWMA chart", "for", data_name, "without new data"))
				qccEwma = qcc::ewma(data = selectedData[{{selected.rowsTobeUsed | safe}},], 
									data.name = paste(data_name, '[{{selected.rowsTobeUsed | safe}},]', sep=""),
									nsigmas = c({{selected.nsigmas | safe}}), 
									lambda = c({{selected.lambda | safe}})) 
			{{#else}}
				{{if(options.selected.rowsTobeUsed  !== "")}}
					BSkyFormat(paste("\nEWMA chart", "for", data_name,  "with new data"))
					qccEwma = qcc::ewma(data = selectedData[{{selected.rowsTobeUsed | safe}},], 
										data.name = paste(data_name, '[{{selected.rowsTobeUsed | safe}},]', sep=""),
										newdata=selectedData[c({{selected.rowsTobeUsedAsNewData | safe}}),], 
										nsigmas = c({{selected.nsigmas | safe}}), 
										lambda = c({{selected.lambda | safe}})) 
				{{/if}}
			{{/if}}
		{{#else}}	
			{{if(options.selected.rowsTobeUsedAsNewData  === "" || options.selected.rowsTobeUsed  === "")}}
				BSkyFormat(paste("\nEWMA chart", "for", data_name, "without new data"))
				qccEwma = qcc::ewma(data = selectedData[{{selected.rowsTobeUsed | safe}}], 
									data.name = paste(data_name, '[{{selected.rowsTobeUsed | safe}}]', sep=""),
									nsigmas = c({{selected.nsigmas | safe}}), 
									lambda = c({{selected.lambda | safe}})) 
			{{#else}}
				{{if(options.selected.rowsTobeUsed  !== "")}}
					BSkyFormat(paste("\nEWMA chart", "for", data_name,  "with new data"))
					qccEwma = qcc::ewma(data = selectedData[{{selected.rowsTobeUsed | safe}}], 
										data.name = paste(data_name, '[{{selected.rowsTobeUsed | safe}}]', sep=""),
										newdata=selectedData[c({{selected.rowsTobeUsedAsNewData | safe}})], 
										nsigmas = c({{selected.nsigmas | safe}}), 
										lambda = c({{selected.lambda | safe}})) 
				{{/if}}
			{{/if}}
		{{/if}}
	}
	
	
{{if(options.selected.summaryPrintChk  === "TRUE")}}
	if(!is.null(qccEwma)){
		summary(qccEwma)
	}
{{/if}}

`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) },
			summaryPrintChk: {
                el: new checkbox(config, {
                    label: ewmaChart.t('summaryPrintChk'), 
					no: "summaryPrintChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			label2: { 
				el: new labelVar(config, { 
					label: ewmaChart.t('label2'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			selectDatasetRad: {
                el: new radioButton(config, {
                    label: ewmaChart.t('selectDatasetRad'),
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
                    label: ewmaChart.t('variablelistSelcted'),
                    no: "variablelistSelcted",
                    required: false,
                    filter: "Numeric|Scale",
					style: "mt-1 mb-3",
                    extraction: "NoPrefix|UseComma|Enclosed",
                })
            },
			selectVariableRad: {
                el: new radioButton(config, {
                    label: ewmaChart.t('selectVariableRad'),
                    no: "gpbox1",
                    increment: "selectVariableRad",
                    value: "variable",
                    state: "checked",
                    extraction: "ValueAsIs",
                })
            },
			variableSelcted: {
                el: new dstVariable(config, {
                    label: ewmaChart.t('variableSelcted'),
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
                    label: ewmaChart.t('groupingNeededChk'), 
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
                    label: ewmaChart.t('groupingVariable'),
                    no: "groupingVariable",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					style: "ml-5",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			displayGroupsChk: {
                el: new checkbox(config, {
                    label: ewmaChart.t('displayGroupsChk'), 
					no: "displayGroupsChk",
                    bs_type: "valuebox",
                    style: "mt-2 ml-5 mb-2",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			/*
			variableControlLimits: {
                el: new input(config, {
                    no: 'variableControlLimits',
                    label: ewmaChart.t('variableControlLimits'),
                    placeholder: "",
                    required: false,
                    type: "character",
					style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			*/
			rowsTobeUsed: {
                el: new input(config, {
                    no: 'rowsTobeUsed',
                    label: ewmaChart.t('rowsTobeUsed'),
                    placeholder: "",
                    required: false,
                    type: "character",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: 'c(%val%)',
                })
            },
			rowsTobeUsedAsNewData: {
                el: new input(config, {
                    no: 'rowsTobeUsedAsNewData',
                    label: ewmaChart.t('rowsTobeUsedAsNewData'),
                    placeholder: "",
                    required: false,
                    type: "character",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			nsigmas: {
                el: new input(config, {
                    no: 'nsigmas',
                    label: ewmaChart.t('nsigmas'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "3",
					width: "w-25",
                })
            },
			lambda: {
                el: new input(config, {
                    no: 'lambda',
                    label: ewmaChart.t('lambda'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "0.2",
					width: "w-25",
                })
            },
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [
					objects.summaryPrintChk.el.content,
					
					objects.label2.el.content,
					objects.selectVariableRad.el.content,
					objects.variableSelcted.el.content, 
					//objects.groupingNeededChk.el.content,
					objects.groupingVariable.el.content,
					//objects.variableControlLimits.el.content,
					objects.displayGroupsChk.el.content,
					
					objects.selectDatasetRad.el.content,
					objects.variablelistSelcted.el.content,
					
					objects.nsigmas.el.content,
					objects.lambda.el.content,
					
					objects.rowsTobeUsed.el.content,
					objects.rowsTobeUsedAsNewData.el.content
					],
            nav: {
                name: ewmaChart.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: ewmaChart.t('help.title'),
            r_help: ewmaChart.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: ewmaChart.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new ewmaChart().render()
}
