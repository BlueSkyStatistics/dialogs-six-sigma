/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */


var localization = {
    en: {
        title: "Multivariate Quality Control Chart (MQCC)",
		navigation: "MQCC Chart",
		
		summaryPrintChk: "Print summary in addition to chart(s)",
		
		variableListSelcted: "Data (select one or more variables) to chart",
		groupingVariable: "(Optional) Select grouping Variable if subgroups are present",
		excludeGroups: "(Optional) exclude groups (if subgroups are numeric) from computation/charting (e.g. specify as 1:10 or comma seperated as 1,4,5,7:12)",
		groupsTobeUsedAsNewData: "(Optional) New Data - groups (if subgroups are numeric) to be used as New Data to chart (e.g. specify as 1:25 or 1,4,5,7:12) - new data to plot but not included in the limit computations",
		
		limitsChk: "If control limits (Phase I) must be computed and plotted",
		pred_limitsChk: "If prediction limits (Phase II) must be computed and plotted",
		
		confidence_level: "(Optional) Confidence level: leave the deafult formula as shown (where p will be computed automatically as the number of variables selected). Otherwise specify a numeric value between 0 and 1 to be used as the confidence level to compute the probability limits",
		
		genVarianceChk: "Plot control chart for the Generalized Variance |S| for the variables selected (and a grouping variable must be selected)",
		
		help: {
            title: "Multivariate Quality Control Chart(mqcc)",
            r_help: "help(mqcc, package = qcc)",
			body: `
				<b>Description</b></br>
				mqcc function to perform multivariate statistical quality control and the generalized variance |S|
				<br/>
				<br/>
				For the detail help - use R help(mqcc, package = qcc) and help(GVcontrol, package = IAcsSPCR)
				<br/>
				<br/>
				To try this, you may load the dataset called boiler from the qcc package with Load Dataset menu by selecting qcc package and then select boiler dataset to load
				<br/>
				<br/>
				Seelct the variables from the boiler dataset, leave everything as deafult, and run the dialog
				<br/>
				<br/>
				You may also use the sample dataset file called widgets_mqcc.xlsx. Open the file in the data grid with file open menu
				<br/>
				<br/>
				To plot generalized variance |S|, the dataset must have a column with the subgroup numbers - othrwise it will not plot
				<br/>
				<br/>
				Follow the qcc tutorial at https://cran.r-project.org/web/packages/qcc/vignettes/qcc_a_quick_tour.html
				<br/>
			`
		},
		
	}
}

class mqccChart extends baseModal {
    constructor() {
        var config = {
            id: "mqccChart",
            label: localization.en.title,
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
                    label: localization.en.summaryPrintChk, 
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
                    label: localization.en.dataSelected,
                    no: "dataSelected",
                    filter: "Dataset",
                    extraction: "UseComma|NoPrefix|Enclosed",
					extraction: "ValueAsIs",
                    required: true,
                })
            },
            newDataSelected: {
                el: new dstVariableList(config, {
                    label: localization.en.newDataSelected,
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
                    label: localization.en.variableListSelcted,
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
                    label: localization.en.groupingVariable,
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
                    label: localization.en.excludeGroups,
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
                    label: localization.en.groupsTobeUsedAsNewData,
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
                    label: localization.en.confidence_level,
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
                    label: localization.en.limitsChk, 
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
                    label: localization.en.pred_limitsChk, 
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
                    label: localization.en.genVarianceChk, 
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
                name: localization.en.navigation,
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        this.help = localization.en.help;
    }

}
module.exports.item = new mqccChart().render()