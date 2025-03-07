/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */




class MultiVariChart extends baseModal {
    static dialogId = 'MultiVariChart'
    static t = baseModal.makeT(MultiVariChart.dialogId)

    constructor() {
        var config = {
            id: MultiVariChart.dialogId,
            label: MultiVariChart.t('title'),
            modalType: "two",
            RCode:`

require(dplyr)

require(ggplot2)

multiVariGroupingPlot <- function(data, x, y, group, color1 = "blue", color2 = "black", chart_title = "MultiVari Chart") {
	
	  data = data[,c(x,y,group)]
	  
	  #data[,1] = as.factor(data[,1])
	  #data[,3] = as.factor(data[,3])
	  
	  data <- dplyr::rename(data, "x" = all_of(x), "y" = all_of(y), "grp" = all_of(group))
	
	  means <- data %>%
		group_by(x) %>%
		summarize(y = mean(y, na.rm = TRUE))
	
	  mean_y_range <- range(means$y, na.rm = TRUE)
	  offset_mean_text_y <- (mean_y_range[2] - mean_y_range[1]) * c({{selected.pctMeanRangeAsYoffset | safe}}) 
	
	  ggplot(data, aes(x, y)) +
		geom_line(
		  aes(group = x), 
		  color = color1
		) +
		geom_point(
		  aes(shape = grp, color = grp), 
		  size = 4
		) +
		geom_line(
		  data = means, 
		  aes(group = ""),
		  color = color2,
		  linetype = "dashed"
		) +
		geom_point(
		  data = means, 
		  aes(x = x, y=y),
		  shape = 15,
		  size = 4
		) + 
		geom_text(data = means, 
		  aes(x = x, y=y + offset_mean_text_y, label = round(y,1), hjust= c({{selected.pctMeanRangeAsXoffset | safe}}), vjust=0)) + 
		annotate(geom="text",  x = Inf, y = Inf, vjust = 1, hjust = 1, label=paste("Mean values in", color2),
              color="black")+
		labs(
		  x = as.character(ensym(x)),
		  y = as.character(ensym(y)),
		  shape = as.character(ensym(group)),
		  color = as.character(ensym(group))
		) +
		{{selected.BSkyThemes | safe}} + 
		ggtitle(chart_title) +
		theme(plot.title = element_text(size = 16, face = "bold"),
		axis.text.x = element_text(angle = {{selected.angleXaxis_text | safe}}, hjust = 1)
		)
}

{{dataset.name}}_tmp = {{dataset.name}}[,c('{{selected.X_variableSelcted | safe}}', '{{selected.Y_variableRespSelcted | safe}}', '{{selected.G_variableSelcted | safe}}')]
{{dataset.name}}_tmp[,1] = as.factor({{dataset.name}}_tmp[,1])
{{dataset.name}}_tmp[,3] = as.factor({{dataset.name}}_tmp[,3])

multiVariGroupingPlot(data = {{dataset.name}}_tmp, 
					x = '{{selected.X_variableSelcted | safe}}', 
					y = '{{selected.Y_variableRespSelcted | safe}}', 
					group ='{{selected.G_variableSelcted | safe}}', 
					chart_title = "Multi-Vari Chart for {{selected.Y_variableRespSelcted | safe}} (with mean) by {{selected.X_variableSelcted | safe}}",
					) 

BSkyFormat("\n")


{{dataset.name}}_tmp\${{selected.X_variableSelcted | safe}}_{{selected.G_variableSelcted | safe}} = with({{dataset.name}}_tmp, paste({{selected.X_variableSelcted | safe}}, {{selected.G_variableSelcted | safe}}, sep='_'))
{{dataset.name}}_tmp\${{selected.X_variableSelcted | safe}}_{{selected.G_variableSelcted | safe}} = with({{dataset.name}}_tmp, factor({{selected.X_variableSelcted | safe}}_{{selected.G_variableSelcted | safe}}, levels=unlist(lapply(levels({{selected.X_variableSelcted | safe}}),function(x)paste(x,levels({{selected.G_variableSelcted | safe}}), sep='_')))))

multiVariGroupingPlot(data = {{dataset.name}}_tmp, 
					x = '{{selected.X_variableSelcted | safe}}_{{selected.G_variableSelcted | safe}}', 
					y = '{{selected.Y_variableRespSelcted | safe}}', 
					group ='{{selected.G_variableSelcted | safe}}',
					chart_title = "Multi-Vari Chart for {{selected.Y_variableRespSelcted | safe}} (with mean) by ({{selected.X_variableSelcted | safe}} and {{selected.G_variableSelcted | safe}})",
					)

{{if(options.selected.printStatChk === 'TRUE')}}
	{{dataset.name}}_tmp %>%
	dplyr::group_by({{selected.X_variableSelcted | safe}}) %>%
		dplyr::select({{selected.Y_variableRespSelcted | safe}},{{selected.X_variableSelcted | safe}}) %>%
			BSkySummaryStats(stats = c(min=TRUE, 
										max=TRUE, 
										mean=TRUE, 
										median=TRUE, 
										quantiles=FALSE) 
										) %>%
			BSkyFormat(outputTableIndex = c(2), outputTableRenames = "Stats for {{selected.Y_variableRespSelcted | safe}} by {{selected.X_variableSelcted | safe}}") 	 

	{{dataset.name}}_tmp %>%
	dplyr::group_by({{selected.G_variableSelcted | safe}},{{selected.X_variableSelcted | safe}}) %>%
		dplyr::select({{selected.Y_variableRespSelcted | safe}},{{selected.G_variableSelcted | safe}},{{selected.X_variableSelcted | safe}}) %>%
			BSkySummaryStats(stats = c(min=TRUE, 
										max=TRUE, 
										mean=TRUE, 
										median=TRUE, 
										quantiles=FALSE)
										) %>%
			BSkyFormat(outputTableIndex = c(2), outputTableRenames = "Stats for {{selected.Y_variableRespSelcted | safe}} by ({{selected.X_variableSelcted | safe}} and {{selected.G_variableSelcted | safe}})") 	
			
{{/if}}

rm({{dataset.name}}_tmp)

`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) }, 
			Y_variableRespSelcted: {
                el: new dstVariable(config, {
                    label: MultiVariChart.t('Y_variableRespSelcted'),
                    no: "Y_variableRespSelcted",
                    required: true,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "Numeric|Scale",
					//style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			X_variableSelcted: {
                el: new dstVariable(config, {
                    label: MultiVariChart.t('X_variableSelcted'),
                    no: "X_variableSelcted",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					//filter: "String|Ordinal|Nominal",
					style: "mb-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			G_variableSelcted: {
                el: new dstVariable(config, {
                    label: MultiVariChart.t('G_variableSelcted'),
                    no: "G_variableSelcted",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					//filter: "String|Ordinal|Nominal",
					style: "mb-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			printStatChk: {
                el: new checkbox(config, {
                    label: MultiVariChart.t('printStatChk'), 
					no: "printStatChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-3",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					//state: "checked",
					newline: true,
                })
            },
			pctMeanRangeAsYoffset: {
			el: new inputSpinner(config, {
			  no: 'pctMeanRangeAsYoffset',
			  label: MultiVariChart.t('pctMeanRangeAsYoffset'),
			  min: 0.01,
			  max: 2,
			  step: 0.01,
			  value: 0.05,
			  extraction: "NoPrefix|UseComma",
			  style: "mb-3"
			})
		  },
		  pctMeanRangeAsXoffset: {
			el: new inputSpinner(config, {
			  no: 'pctMeanRangeAsXoffset',
			  label: MultiVariChart.t('pctMeanRangeAsXoffset'),
			  min: -2.5,
			  max: 3.5,
			  step: 0.5,
			  value: 2.0,
			  extraction: "NoPrefix|UseComma",
			  style: "mb-3"
			})
		  },
		  angleXaxis_text: {
                el: new inputSpinner(config, {
					no: 'angleXaxis_text',
                    label: MultiVariChart.t('angleXaxis_text'),
					required: true,
                    min: 0,
                    max: 360,
                    step: 1,
                    value: 0,
                    extraction: "NoPrefix",
                    width: "w-25",
            })
		  },
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [
					objects.Y_variableRespSelcted.el.content,
					objects.X_variableSelcted.el.content,
					objects.G_variableSelcted.el.content,
					objects.printStatChk.el.content,
					objects.pctMeanRangeAsYoffset.el.content,
					objects.pctMeanRangeAsXoffset.el.content, 
					objects.angleXaxis_text.el.content,
					],
            nav: {
                name: MultiVariChart.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: MultiVariChart.t('help.title'),
            r_help: MultiVariChart.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: MultiVariChart.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new MultiVariChart().render()
}
