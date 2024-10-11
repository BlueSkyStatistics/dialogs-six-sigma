


class mqccChart extends baseModal {
    static dialogId = 'mqccChart'
    static t = baseModal.makeT(mqccChart.dialogId)

    constructor() {
        var config = {
            id: mqccChart.dialogId,
            label: mqccChart.t('title'),
            modalType: "two",
            RCode:`
	
	require(qcc)
	
	{{if(options.selected.genVarianceChk === 'TRUE')}} 
	require(IAcsSPCR)
	{{/if}}

	var_list = NULL
	var_list_gpd = NULL
	var_list_gpd_selected = NULL
	data_labels = NULL
	new_data = NULL
	new_labels = NULL
	mqccX = NULL
	{{dataset.name}}_GVcontrol_df = NULL
	m_GVcontrol = NULL
	n_GVcontrol = NULL 
	GVcontrolX = NULL
					
	var_list = list({{selected.variableListSelcted | safe}})
	
	{{if(options.selected.groupingVariable !== '')}} 
		var_list_gpd <- lapply(var_list, 
							   function(x)
							   {
									var_gpd = qcc.groups(x, {{dataset.name}}\${{selected.groupingVariable | safe}})
									row.names(var_gpd) = NULL
									return(var_gpd)
							   } 
							)
		{{if(options.selected.excludeGroups !== '')}}
			var_list_gpd_selected = lapply(var_list_gpd, function(x){x[-c({{selected.excludeGroups | safe}}),]})
			data_labels = unique({{dataset.name}}\${{selected.groupingVariable | safe}})[!(unique({{dataset.name}}\${{selected.groupingVariable | safe}}) %in% c({{selected.excludeGroups | safe}}))]
		{{#else}}
			var_list_gpd_selected = var_list_gpd
			data_labels = unique({{dataset.name}}\${{selected.groupingVariable | safe}})
		{{/if}}
		
		{{if(options.selected.groupsTobeUsedAsNewData !== '')}}
			new_data = lapply(var_list_gpd, function(x){x[c({{selected.groupsTobeUsedAsNewData | safe}}),]})
			new_labels = sort(c({{selected.groupsTobeUsedAsNewData | safe}}))
			new_data = lapply(new_data, function(x){matrix(x, ncol = dim(var_list_gpd[[1]])[2])})
		{{/if}}
	
	{{#else}}
		var_list_gpd = var_list
		{{if(options.selected.excludeGroups !== '')}}
			var_list_gpd_selected = lapply(var_list_gpd, function(x){x[-c({{selected.excludeGroups | safe}})]})
			data_labels = c(1:length(var_list_gpd[[1]]))
			data_labels = data_labels[!( data_labels %in% c({{selected.excludeGroups | safe}}))]
		{{#else}}
			var_list_gpd_selected = var_list_gpd
			data_labels = c(1:length(var_list_gpd[[1]]))
		{{/if}}
		
		{{if(options.selected.groupsTobeUsedAsNewData !== '')}}
			new_data = lapply(var_list_gpd, function(x){x[c({{selected.groupsTobeUsedAsNewData | safe}})]})
			new_labels = sort(c({{selected.groupsTobeUsedAsNewData | safe}}))
		{{/if}}
		
	{{/if}}
	
	p = length(var_list_gpd_selected)
	{{if(options.selected.groupsTobeUsedAsNewData === '')}}
		mqccX = mqcc(data = var_list_gpd_selected, type = "T2",
				limits = {{selected.limitsChk | safe}}, 
				labels = data_labels,
				data.name = paste(names(var_list_gpd),collapse=','),
				pred.limits = {{selected.pred_limitsChk | safe}} {{selected.confidence_level | safe}} 
			 )
	{{#else}}
		mqccX = mqcc(data = var_list_gpd_selected, type = "T2", 
				limits = {{selected.limitsChk | safe}}, 
				labels = data_labels,
				newdata = new_data,
				newlabels = new_labels,
				data.name = paste(names(var_list_gpd),collapse=','),
				pred.limits = {{selected.pred_limitsChk | safe}} {{selected.confidence_level | safe}} 
			 )
	{{/if}}
	
	
	{{if(options.selected.genVarianceChk === 'TRUE')}} 
		{{if(options.selected.groupingVariable !== '')}}
			if(length(var_list) > 1){
				{{dataset.name}}_GVcontrol_df = cbind(subgroup = {{dataset.name}}[,'{{selected.groupingVariable | safe}}'], as.data.frame(var_list))
				
				{{if(options.selected.excludeGroups !== '')}}
					{{dataset.name}}_GVcontrol_df = {{dataset.name}}_GVcontrol_df[!{{dataset.name}}_GVcontrol_df[,1]%in% c({{selected.excludeGroups | safe}}),]
					m_GVcontrol = length(unique(as.numeric({{dataset.name}}_GVcontrol_df[,1])))
					n_GVcontrol = nrow({{dataset.name}}_GVcontrol_df)/length(unique(as.numeric({{dataset.name}}_GVcontrol_df[,1])))
					{{dataset.name}}_GVcontrol_df[,1] = c(rep(1:m_GVcontrol, each=n_GVcontrol, times=1))
					BSkyFormat(paste("\nAfter excluding the groups {{selected.excludeGroups | safe}}, the remaining subgroups have been re-numbered sequentially as 1:",m_GVcontrol," to generate the Generalized Variance control chart",sep=""))
				{{#else}}
					m_GVcontrol = length(unique(as.numeric({{dataset.name}}_GVcontrol_df[,1])))
					n_GVcontrol = nrow({{dataset.name}}_GVcontrol_df)/length(unique(as.numeric({{dataset.name}}_GVcontrol_df[,1])))
					{{dataset.name}}_GVcontrol_df[,1] = c(rep(1:m_GVcontrol, each=n_GVcontrol, times=1))
				{{/if}}
				
				GVcontrolX = GVcontrol(DF = {{dataset.name}}_GVcontrol_df, 
										m = m_GVcontrol, 
										n = n_GVcontrol, 
										p = dim({{dataset.name}}_GVcontrol_df)[2]-1)
			}else{
				BSkyFormat("\nNumber of variable chosen must be 2 or more to generate the Generalized Variance control chart")
			}
		{{#else}}
			BSkyFormat("\nSelect a grouping variable with subgroup numbers to generate the Generalized Variance control chart")
		{{/if}}
	{{/if}}
	
	if(!is.null(mqccX))
		BSkyFormat(mqccX$cov, outputTableRenames = c(paste("{{dataset.name}}:","Covariance matrix")))
	
	if({{selected.summaryPrintChk | safe}}){
		if(!is.null(mqccX)){
				BSkyFormat(paste("Confidence level used for the Multivariate control chart:", round(mqccX$confidence.level, BSkyGetDecimalDigitSetting())))
				BSkyFormat("Summary for Multivariate control chart")
				summary(mqccX)
		}
		
		if(!is.null(GVcontrolX)){
				BSkyFormat("Summary for Generalized Variance control chart")
				print(GVcontrolX)
		}
	}
	
`
        };
        var objects = {
			content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) }, 
			summaryPrintChk: {
                el: new checkbox(config, {
                    label: mqccChart.t('summaryPrintChk'), 
					no: "summaryPrintChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			/*
            dataSelected: {
                el: new dstVariableList(config, {
                    label: mqccChart.t('dataSelected'),
                    no: "dataSelected",
                    filter: "Dataset",
                    extraction: "UseComma|NoPrefix|Enclosed",
					extraction: "ValueAsIs",
                    required: true,
                })
            },
            newDataSelected: {
                el: new dstVariableList(config, {
                    label: mqccChart.t('newDataSelected'),
                    no: "newDataSelected",
                    filter: "Dataset",
                    //extraction: "UseComma|Enclosed",
					extraction: "ValueAsIs",
                    required: false,
                })
            },
			*/
			variableListSelcted: {
                el: new dstVariableList(config, {
                    label: mqccChart.t('variableListSelcted'),
                    no: "variableListSelcted",
                    required: true,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "Numeric|Scale",
					style: "mt-1 ml-3",
                    extraction: "CustomFormat",
                }), r: ['{{ var | safe}}']
            },
			groupingVariable: {
                el: new dstVariable(config, {
                    label: mqccChart.t('groupingVariable'),
                    no: "groupingVariable",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					style: "ml-5",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			excludeGroups: {
                el: new input(config, {
                    no: 'excludeGroups',
                    label: mqccChart.t('excludeGroups'),
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
			groupsTobeUsedAsNewData: {
                el: new input(config, {
                    no: 'groupsTobeUsedAsNewData',
                    label: mqccChart.t('groupsTobeUsedAsNewData'),
                    placeholder: "",
                    required: false,
					filter: "character|numeric",
                    //type: "character",
					style: "ml-5 mb-4",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			confidence_level: {
                el: new input(config, {
                    no: 'confidence_level',
                    label: mqccChart.t('confidence_level'),
                    //placeholder: "",
                    required: false,
                    //type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "(1-0.00134989803156746)^p", 
					value: "(1-0.0027/2)^p",
					wrapped: ', confidence.level=c(%val%)',
					style: "mb-3",
					//width: "w-25",
                })
            },
			limitsChk: {
                el: new checkbox(config, {
                    label: mqccChart.t('limitsChk'), 
					no: "limitsChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					state:"checked",
					newline: true,
                })
            },
			pred_limitsChk: { 
                el: new checkbox(config, {
                    label: mqccChart.t('pred_limitsChk'), 
					no: "pred_limitsChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			genVarianceChk: { 
                el: new checkbox(config, {
                    label: mqccChart.t('genVarianceChk'), 
					no: "genVarianceChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
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
					objects.summaryPrintChk.el.content,
					
					objects.variableListSelcted.el.content,
					objects.groupingVariable.el.content,
					
					objects.excludeGroups.el.content,
					objects.groupsTobeUsedAsNewData.el.content, 
					
					objects.limitsChk.el.content,
					objects.pred_limitsChk.el.content,
					objects.confidence_level.el.content,
					objects.genVarianceChk.el.content
					],
            nav: {
                name: mqccChart.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: mqccChart.t('help.title'),
            r_help: "help(data,package='utils')",
            body: mqccChart.t('help.body')
        }
;
    }

}

module.exports = {
    render: () => new mqccChart().render()
}
