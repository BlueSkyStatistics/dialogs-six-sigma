


class cusumChart extends baseModal {
    static dialogId = 'cusumChart'
    static t = baseModal.makeT(cusumChart.dialogId)

    constructor() {
        var config = {
            id: cusumChart.dialogId,
            label: cusumChart.t('title'),
            modalType: "two",
            RCode:`
require(qcc)

#Cusum Chart


selectedData = NULL

if(c('{{selected.gpbox1 | safe}}') == "variable"){
	
	if(trimws('{{selected.groupingVariable | safe}}') != "")
	{
		selectedData = with({{dataset.name}}, qcc.groups(c({{selected.variableSelcted | safe}}), c({{selected.groupingVariable | safe}})))
	}
	
	if({{selected.displayGroupsChk | safe}} && !is.null(selectedData))
	{
		{{selected.variableSelcted | safe}}Gpd = as.data.frame(selectedData)
		#BSkyFormat({{selected.variableSelcted | safe}}, outputTableRenames = paste("Grouping generated for {{selected.variableSelcted | safe}} by {{selected.groupingVariable | safe}}"))
		BSkyLoadRefresh('{{selected.variableSelcted | safe}}Gpd')
	}
	
	data_name = '{{selected.variableSelcted | safe}}'
	
}else
{
	if(length(c({{selected.variablelistSelcted | safe}})) == 0){
		selectedData = {{dataset.name}}[]
	}else{
		selectedData = {{dataset.name}}[, c({{selected.variablelistSelcted | safe}})]
	}
	
	data_name = '{{dataset.name}}'
}


cat("Charts selected: Cusum")


#chart type Cusum
	qccCusum = NULL 
	
	
	if(!is.null(selectedData)) {
		if(trimws('{{selected.rowsTobeUsedAsNewData | safe}}') == "" || trimws('{{selected.rowsTobeUsed | safe}}') ==""){
			BSkyFormat(paste("\nCusum chart", "for", data_name, "without new data"))
			qccCusum = qcc::cusum(data = selectedData[{{selected.rowsTobeUsed | safe}}], decision.interval = c({{selected.decisionInterval | safe}}), se.shift = c({{selected.seShift | safe}})) 
		}
		else if(trimws('{{selected.rowsTobeUsed | safe}}') != "" ){
			BSkyFormat(paste("\nCusum chart", "for", data_name,  "with new data"))
			qccCusum = qcc::cusum(data = selectedData[{{selected.rowsTobeUsed | safe}}], newdata=selectedData[c({{selected.rowsTobeUsedAsNewData | safe}}),], decision.interval = c({{selected.decisionInterval | safe}}), se.shift = c({{selected.seShift | safe}})) 
		}
		
		if({{selected.summaryPrintChk | safe}} && !is.null(qccCusum)){
			summary(qccCusum)
		}
	}
	
`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) },
			summaryPrintChk: {
                el: new checkbox(config, {
                    label: cusumChart.t('summaryPrintChk'), 
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
					label: cusumChart.t('label2'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			selectDatasetRad: {
                el: new radioButton(config, {
                    label: cusumChart.t('selectDatasetRad'),
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
                    label: cusumChart.t('variablelistSelcted'),
                    no: "variablelistSelcted",
                    required: false,
                    filter: "Numeric|Scale",
					style: "mt-1 mb-3",
                    extraction: "NoPrefix|UseComma|Enclosed",
                })
            },
			selectVariableRad: {
                el: new radioButton(config, {
                    label: cusumChart.t('selectVariableRad'),
                    no: "gpbox1",
                    increment: "selectVariableRad",
                    value: "variable",
                    state: "checked",
                    extraction: "ValueAsIs",
                })
            },
			variableSelcted: {
                el: new dstVariable(config, {
                    label: cusumChart.t('variableSelcted'),
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
                    label: cusumChart.t('groupingNeededChk'), 
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
                    label: cusumChart.t('groupingVariable'),
                    no: "groupingVariable",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					style: "ml-5",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			displayGroupsChk: {
                el: new checkbox(config, {
                    label: cusumChart.t('displayGroupsChk'), 
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
                    label: cusumChart.t('variableControlLimits'),
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
                    label: cusumChart.t('rowsTobeUsed'),
                    placeholder: "",
                    required: false,
                    type: "character",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "",
					wrapped: 'c(%val%) , ',
                })
            },
			rowsTobeUsedAsNewData: {
                el: new input(config, {
                    no: 'rowsTobeUsedAsNewData',
                    label: cusumChart.t('rowsTobeUsedAsNewData'),
                    placeholder: "",
                    required: false,
                    type: "character",
					//style: "ml-5",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			decisionInterval: {
                el: new input(config, {
                    no: 'decisionInterval',
                    label: cusumChart.t('decisionInterval'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "5",
					width: "w-25",
                })
            },
			seShift: {
                el: new input(config, {
                    no: 'seShift',
                    label: cusumChart.t('seShift'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "1",
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
					
					objects.decisionInterval.el.content,
					objects.seShift.el.content,
					
					objects.rowsTobeUsed.el.content,
					objects.rowsTobeUsedAsNewData.el.content
					],
            nav: {
                name: cusumChart.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: cusumChart.t('help.title'),
            r_help: cusumChart.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: cusumChart.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new cusumChart().render()
}
