
var localization = {
    en: {
        title: "Multi-Vari Chart",
		navigation: "Multi-Vari Chart",
		
		Y_variableRespSelcted: "Select the variable (numerical) for measurement/response (Y-axis)",
		X_variablefSelcted: "Select the categorical variable (X-axis)",
		G_variableSelcted: "Select the grouping (categorical) variable ",
		printStatChk: "Print stats in addition to charts",
		
		help: {
            title: "Multi-Vari Chart",
            //r_help: "help(lm, package = stats)",
			body: `
				<b>Description</b></br>
				A multi-vari chart of one quantitative response variable depending on two categorical variables
				<br/>
				<br/>
				Mean values of the response variable is also plotted on the chart 
				<br/>
			`
		},
	}
}

class MultiVariChart extends baseModal {
    constructor() {
        var config = {
            id: "MultiVariChart",
            label: localization.en.title,
            modalType: "two",
            RCode:`

require(dplyr)

require(ggplot2)

multiVariGroupingPlot <- function(data, x, y, group, color1 = "blue", color2 = "orange", chart_title = "MultiVari Chart") {
	
	  data <- dplyr::rename(data, "x" = all_of(x), "y" = all_of(y), "grp" = all_of(group))
	
	  means <- data %>%
		group_by(x) %>%
		summarize(y = mean(y, na.rm = TRUE))
	
	  ggplot(data, aes(x, y)) +
		geom_line(
		  aes(group = x), 
		  color = color1
		) +
		geom_point(
		  aes(shape = grp), 
		  color = color1,
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
		  color = color2,
		  shape = 15,
		  size = 6
		) +
		theme_grey() +
		ggtitle(chart_title) +
		theme(plot.title = element_text(size = 16, face = "bold"))
}

multiVariGroupingPlot(data = {{dataset.name}}, 
					x = '{{selected.X_variablefSelcted | safe}}', 
					y = '{{selected.Y_variableRespSelcted | safe}}', 
					group ='{{selected.G_variableSelcted | safe}}', 
					chart_title = "Multi-Vari Chart for {{selected.Y_variableRespSelcted | safe}} (with mean) by {{selected.X_variablefSelcted | safe}}",
					color1 = "blue", color2 = "orange") 

BSkyFormat("\n")

{{dataset.name}}_tmp = {{dataset.name}}[,c('{{selected.X_variablefSelcted | safe}}', '{{selected.Y_variableRespSelcted | safe}}', '{{selected.G_variableSelcted | safe}}')]
{{dataset.name}}_tmp\${{selected.X_variablefSelcted | safe}}_{{selected.G_variableSelcted | safe}} = with({{dataset.name}}_tmp, paste({{selected.X_variablefSelcted | safe}}, {{selected.G_variableSelcted | safe}}, sep='_'))
multiVariGroupingPlot(data = {{dataset.name}}_tmp, 
					x = '{{selected.X_variablefSelcted | safe}}_{{selected.G_variableSelcted | safe}}', 
					y = '{{selected.Y_variableRespSelcted | safe}}', 
					group ='{{selected.G_variableSelcted | safe}}',
					chart_title = "Multi-Vari Chart for {{selected.Y_variableRespSelcted | safe}} (with mean) by ({{selected.X_variablefSelcted | safe}} and {{selected.G_variableSelcted | safe}})",
					color1 = "blue", color2 = "orange")
rm({{dataset.name}}_tmp)

{{if(options.selected.printStatChk === 'TRUE')}}
	{{dataset.name}} %>%
	dplyr::group_by({{selected.X_variablefSelcted | safe}}) %>%
		dplyr::select({{selected.Y_variableRespSelcted | safe}},{{selected.X_variablefSelcted | safe}}) %>%
			BSkySummaryStats(stats = c(min=TRUE, 
										max=TRUE, 
										mean=TRUE, 
										median=TRUE, 
										quantiles=FALSE) 
										) %>%
			BSkyFormat(outputTableIndex = c(2), outputTableRenames = "Stats for {{selected.Y_variableRespSelcted | safe}} by {{selected.X_variablefSelcted | safe}}") 	 

	{{dataset.name}} %>%
	dplyr::group_by({{selected.G_variableSelcted | safe}},{{selected.X_variablefSelcted | safe}}) %>%
		dplyr::select({{selected.Y_variableRespSelcted | safe}},{{selected.G_variableSelcted | safe}},{{selected.X_variablefSelcted | safe}}) %>%
			BSkySummaryStats(stats = c(min=TRUE, 
										max=TRUE, 
										mean=TRUE, 
										median=TRUE, 
										quantiles=FALSE)
										) %>%
			BSkyFormat(outputTableIndex = c(2), outputTableRenames = "Stats for {{selected.Y_variableRespSelcted | safe}} by ({{selected.X_variablefSelcted | safe}} and {{selected.G_variableSelcted | safe}})") 	
			
{{/if}}

`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) }, 
			Y_variableRespSelcted: {
                el: new dstVariable(config, {
                    label: localization.en.Y_variableRespSelcted,
                    no: "Y_variableRespSelcted",
                    required: true,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "Numeric|Scale",
					//style: "mt-1 ml-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			X_variablefSelcted: {
                el: new dstVariable(config, {
                    label: localization.en.X_variablefSelcted,
                    no: "X_variablefSelcted",
                    required: true,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "String|Ordinal|Nominal",
					style: "mb-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			G_variableSelcted: {
                el: new dstVariable(config, {
                    label: localization.en.G_variableSelcted,
                    no: "G_variableSelcted",
                    required: true,
                    //filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
					filter: "String|Ordinal|Nominal",
					style: "mb-3",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			printStatChk: {
                el: new checkbox(config, {
                    label: localization.en.printStatChk, 
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
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [
					objects.Y_variableRespSelcted.el.content,
					objects.X_variablefSelcted.el.content,
					objects.G_variableSelcted.el.content,
					objects.printStatChk.el.content,
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
module.exports.item = new MultiVariChart().render()