/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */




class GageBiasAnalysis extends baseModal {
    static dialogId = 'GageBiasAnalysis'
    static t = baseModal.makeT(GageBiasAnalysis.dialogId)

    constructor() {
        var config = {
            id: GageBiasAnalysis.dialogId,
            label: GageBiasAnalysis.t('title'),
            modalType: "two",
            RCode:`

require(ggplot2)

{{if(options.selected.variableRespSelcted === "" && options.selected.variableBiasSelcted === "")}}
		BSkyFormat("Error: Measurement/Response or Bias variable must be specified")
{{#else}}
		{{if(options.selected.variableBiasSelcted !== "")}}
			GageBias = {{dataset.name}}[, c('{{selected.variableBiasSelcted | safe}}')] 
		{{#else}}
			GageBias = {{dataset.name}}\${{selected.variableRespSelcted | safe}} - {{dataset.name}}\${{selected.variableRefSelcted | safe}}
		{{/if}}	
		
		{{if(options.selected.variableRespSelcted !== "")}}
			GageResp = {{dataset.name}}[, c('{{selected.variableRespSelcted | safe}}')] 
		{{#else}}
			GageResp = {{dataset.name}}\${{selected.variableBiasSelcted | safe}} + {{dataset.name}}\${{selected.variableRefSelcted | safe}}
		{{/if}}		

		ggplot(data={{dataset.name}}, aes(x = {{selected.variableRefSelcted | safe}}, y = GageBias)) +
			geom_smooth(formula = y ~ x, method ="lm", alpha=1, se=FALSE, level= {{selected.alpha | safe}}, aes(color='Regression')) +  
			stat_smooth(formula = y ~ x, method = "lm", level= {{selected.alpha | safe}},
				geom ="ribbon", fill = NA, fullrange = TRUE, 
					linetype = 2, aes(color= '{{selected.alpha | safe}}% CI')) + 
			geom_point() +
			stat_summary(
				aes(fill ='Avg Bias'),
				fun = "mean",        
				geom = "point",
				size = 4,
				shape = 15,
				color='blue1'
			) +
			scale_color_manual(name='',
					breaks=c('Regression', '{{selected.alpha | safe}}% CI'),
					values=c('Regression'='red', '{{selected.alpha | safe}}% CI'='#727272')) +
			scale_fill_manual(name='',values = c('Avg Bias'= 'blue1'))+
			labs(x="{{selected.variableRefSelcted | safe}}",y="Bias",title= "Scatterplot for Gage Linearity and Bias Analysis") +
			xlab("{{selected.variableRefSelcted | safe}}") +
			ylab("Bias") + 
			theme_grey(base_size = 20) +
			theme(plot.title = element_text(size = 20)) 

		LinearRegModel1 = lm(formula = GageBias~{{selected.variableRefSelcted | safe}}, data={{dataset.name}},
			 na.action=na.exclude) %>% 
				BSkyFormat(outputTableIndex = c(3,1), coefConfInt = {{selected.alpha | safe}}, outputTableRenames=c("Gage Linearity - Coefficients"))

		mean_bias <- cbind({{dataset.name}},GageBias = GageBias) %>% 
				group_by({{selected.variableRefSelcted | safe}}) %>% 
				summarise("Avg Bias" = mean(GageBias))
		mean_bias = rbind(mean_bias, c("Overall", mean(GageBias)))

		pvals = c()

		calulatePvalues <- function()
		{
			pvalues = c()
			lapply(split(cbind({{dataset.name}},GageResp = GageResp), factor({{dataset.name}}\${{selected.variableRefSelcted | safe}})), 
				 function(x)
				 {
					 pvalues <<- c(pvalues, sprintf(paste("%.",BSkyGetDecimalDigitSetting(),"f",sep=""),(t.test(x\${{selected.variableRefSelcted | safe}}, x\$GageResp, conf.level = {{selected.alpha | safe}} ))$p.value))
				 }
			)
			invisible(return(pvalues))
		}

		pvals = calulatePvalues()
		pvals = c(pvals, sprintf(paste("%.",BSkyGetDecimalDigitSetting(),"f",sep=""),(t.test({{dataset.name}}\${{selected.variableRefSelcted | safe}}, GageResp, conf.level = {{selected.alpha | safe}}))$p.value))
		mean_bias = cbind(mean_bias, p.value = pvals)

		BSkyFormat(as.data.frame(mean_bias), outputTableRenames=c("Gage Bias"), repeatAllTableFooter = "Sample standard deviation method used for P value")
{{/if}}

`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) }, 
			/*
			boxcoxChk: {
                el: new checkbox(config, {
                    label: GageBiasAnalysis.t('boxcoxChk'), 
					no: "boxcoxChk",
                    bs_type: "valuebox",
                    //style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: " ",
					state: "checked",
					newline: true,
                })
            },
			johnsonChk: {
                el: new checkbox(config, {
                    label: GageBiasAnalysis.t('johnsonChk'), 
					no: "johnsonChk",
                    bs_type: "valuebox",
                    style: "mb-2",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: " ",
					newline: true,
                })
            },
			digits: {
                el: new inputSpinner(config, {
                    no: 'digits',
                    label: GageBiasAnalysis.t('digits'),
                    required: true,
                    min: 0,
                    max: 15,
                    step: 1,
                    value: 4,
					width: "w-25",
					style: "mb-2",
                })
            },     
			label2: { 
				el: new labelVar(config, { 
					label: GageBiasAnalysis.t('label2'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			*/
			/*
			variablePartSelcted: {
                el: new dstVariable(config, {
                    label: GageBiasAnalysis.t('variablePartSelcted'),
                    no: "variablePartSelcted",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					//filter: "Numeric|Scale",
					//style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			variableOpSelcted: {
                el: new dstVariable(config, {
                    label: GageBiasAnalysis.t('variableOpSelcted'),
                    no: "variableOpSelcted",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					//filter: "Numeric|Scale",
					//style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			*/
			variableRespSelcted: {
                el: new dstVariable(config, {
                    label: GageBiasAnalysis.t('variableRespSelcted'),
                    no: "variableRespSelcted",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					//filter: "String|Ordinal|Nominal",
					//style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			variableRefSelcted: {
                el: new dstVariable(config, {
                    label: GageBiasAnalysis.t('variableRefSelcted'),
                    no: "variableRefSelcted",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					//filter: "String|Ordinal|Nominal",
					style: "mb-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			variableBiasSelcted: {
                el: new dstVariable(config, {
                    label: GageBiasAnalysis.t('variableBiasSelcted'),
                    no: "variableBiasSelcted",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					//filter: "String|Ordinal|Nominal",
					style: "mb-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			alpha: {
                el: new input(config, {
                    no: 'alpha',
                    label: GageBiasAnalysis.t('alpha'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
					filter: "numeric",
					style: "mb-3",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "0.95",
                })
            },
			/*
			rowsTobeUsed: {
                el: new input(config, {
                    no: 'rowsTobeUsed',
                    label: GageBiasAnalysis.t('rowsTobeUsed'),
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
			*/
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [
					//objects.boxcoxChk.el.content,
					//objects.johnsonChk.el.content,
					//objects.label2.el.content,
					
					//objects.variablePartSelcted.el.content,
					//objects.variableOpSelcted.el.content,
					
					objects.variableRefSelcted.el.content,
					objects.variableRespSelcted.el.content,
					objects.variableBiasSelcted.el.content,
					
					objects.alpha.el.content,
					
					//objects.rowsTobeUsed.el.content,
					
					//objects.digits.el.content
					],
            nav: {
                name: GageBiasAnalysis.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: GageBiasAnalysis.t('help.title'),
            r_help: GageBiasAnalysis.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: GageBiasAnalysis.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new GageBiasAnalysis().render()
}
