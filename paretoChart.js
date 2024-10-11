


class paretoChart extends baseModal {
    static dialogId = 'paretoChart'
    static t = baseModal.makeT(paretoChart.dialogId)

    constructor() {
        var config = {
            id: paretoChart.dialogId,
            label: paretoChart.t('title'),
            modalType: "two",
            RCode: `
require(qcc)

#Plot Pareto Chart

categoryFreq = NULL
xstats = NULL

{{if(options.selected.gpbox1 === 'long')}}
	BSkyFormat("\nInput data is in long format\n")
	{{if(options.selected.category !== '' && options.selected.counts !== '')}}
		categoryFreq = as.numeric({{dataset.name}}[,c({{selected.counts | safe}})])
		names(categoryFreq) = as.character({{dataset.name}}[,c({{selected.category | safe}})])
	{{#else}}
	BSkyFormat("\nSelect variables for both categories and counts fields\n")
	{{/if}}
{{#else}}
	BSkyFormat("\nInput data is in wide format\n")
	categoryFreq = as.numeric({{dataset.name}}[c({{selected.rownumForCounts | safe}}),])
	names(categoryFreq) = names({{dataset.name}})
{{/if}}


if(!is.null(categoryFreq))
{
	s_categoryFreq = base::sort(categoryFreq, decreasing = TRUE)
	
	if(!is.null(categoryFreq))
	{
		xstats = rbind(s_categoryFreq, cumsum(s_categoryFreq))
		
		if(!is.null(xstats))
		{
			pct = (round(s_categoryFreq/sum(s_categoryFreq), 4))*100
			xstats = rbind(xstats, pct)
			xstats = rbind(xstats, cumsum(pct))
			dimnames(xstats)[[1]] = c("Count", "Cum Count", "Percent", "Cum %")

			dummyParetoReturn = qcc::pareto.chart(categoryFreq, xlab = '{{selected.xlab | safe}}', ylab = '{{selected.ylab | safe}}', ylab2 = '{{selected.ylab2 | safe}}',
								cumperc = seq(0, 100, by = {{selected.percentStep | safe}}), main = '{{selected.mainTitle | safe}}')
		}
	}
}

if(!is.null(xstats))
{
	BSkyFormat(xstats, outputTableRenames = "Stats", decimalDigitsRounding = c({{selected.digits | safe}}))
}

`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move"}) },
			label1: { 
				el: new labelVar(config, { 
					label: paretoChart.t('label1'), 
					h: 6, 
					style: "mb-2",
				}) 
			},
			selectLongRad: {
                el: new radioButton(config, {
                    label: paretoChart.t('selectLongRad'),
                    no: "gpbox1",
                    increment: "selectLongRad",
                    value: "long",
                    state: "checked",
					//style: "mb-3",
                    extraction: "ValueAsIs",
                })
            },
			selectWideRad: {
                el: new radioButton(config, {
                    label: paretoChart.t('selectWideRad'),
                    no: "gpbox1",
                    increment: "selectWideRad",
                    value: "wide",
                    state: "",
                    extraction: "ValueAsIs",
                })
            },
			rownumForCounts: {
                el: new inputSpinner(config, {
                    no: 'rownumForCounts',
                    label: paretoChart.t('rownumForCounts'),
                    required: true,
                    min: 1,
                    max: 99999,
                    step: 1,
                    value: 1,
					style:"ml-3",
                })
            },  
			
			mainTitle: {
                el: new input(config, {
                    no: 'mainTitle',
                    label: paretoChart.t('mainTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Pareto Chart for Categories"
                })
            },
			category: {
                el: new dstVariable(config, {
                    label: paretoChart.t('category'),
                    no: "category",
                    //required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|Enclosed",
                }), r: ['{{ var | safe}}']
            },
			counts: {
                el: new dstVariable(config, {
                    label: paretoChart.t('counts'),
                    no: "counts",
                    //required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|Enclosed|Comma",
                }), r: ['{{ var | safe}}']
            },
			xlab: {
                el: new input(config, {
                    no: 'xlab',
                    label: paretoChart.t('xlab'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Categories",
                })
            },
			ylab: {
                el: new input(config, {
                    no: 'ylab',
                    label: paretoChart.t('ylab'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Category frequency",
                })
            },
			ylab2: {
                el: new input(config, {
                    no: 'ylab2',
                    label: paretoChart.t('ylab2'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Cumulative percentage",
                })
            },
			percentStep: {
                el: new input(config, {
                    no: 'percentStep',
                    label: paretoChart.t('percentStep'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style:"mb-3",
                    value: "25",
                })
            },
			digits: {
                el: new inputSpinner(config, {
                    no: 'digits',
                    label: paretoChart.t('digits'),
                    required: true,
                    min: 0,
                    max: 9,
                    step: 1,
                    value: 1,
                })
            },  
			
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [objects.label1.el.content, 
					objects.selectLongRad.el.content,
					
					objects.category.el.content,
					objects.counts.el.content,
					
					objects.selectWideRad.el.content,
					objects.rownumForCounts.el.content,
					
					objects.mainTitle.el.content, 
					objects.xlab.el.content,
					objects.ylab.el.content,
					objects.ylab2.el.content,
					objects.percentStep.el.content,
					objects.digits.el.content],
            nav: {
                name: paretoChart.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: paretoChart.t('help.title'),
            r_help: "help(data,package='utils')",
            body: paretoChart.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new paretoChart().render()
}
