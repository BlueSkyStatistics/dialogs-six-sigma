


class lossFunctionAnalysis extends baseModal {
    static dialogId = 'lossFunctionAnalysis'
    static t = baseModal.makeT(lossFunctionAnalysis.dialogId)

    constructor() {
        var config = {
            id: lossFunctionAnalysis.dialogId,
            label: lossFunctionAnalysis.t('title'),
            modalType: "two",
            RCode: `
				require(SixSigma)

				#SixSigma Loss Function Analysis

				SixSigma::ss.lfa(lfa.data = {{dataset.name}}, 
						lfa.ctq = {{selected.independent | safe}}, 
						lfa.sub = "{{selected.lfaSubTitle | safe}}", 
						lfa.Delta = {{selected.lfaDelta | safe}}, 
						lfa.Y0 = {{selected.lfaY0 | safe}},  
						lfa.L0 = {{selected.lfaL0 | safe}},  
						lfa.size = c({{selected.lfaSize | safe}}),  
						lfa.output = "{{selected.lfaOutput | safe}}" )

				`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move"}) },
			lfaSubTitle: {
                el: new input(config, {
                    no: 'lfaSubTitle',
                    label: lossFunctionAnalysis.t('lfaSubTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Six Sigma project ID or name for Loss Analysis"
                })
            },
			independent: {
                el: new dstVariable(config, {
                    label: lossFunctionAnalysis.t('independent'),
                    no: "independent",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|Enclosed",
                }), r: ['{{ var | safe}}']
            },
			lfaDelta: {
                el: new input(config, {
                    no: 'lfaDelta',
                    label: lossFunctionAnalysis.t('lfaDelta'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "0.5",
                })
            },
			lfaY0: {
                el: new input(config, {
                    no: 'lfaY0',
                    label: lossFunctionAnalysis.t('lfaY0'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "10",
                })
            },
			lfaL0: {
                el: new input(config, {
                    no: 'lfaL0',
                    label: lossFunctionAnalysis.t('lfaL0'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "0.001",
                })
            },
			lfaSize: {
                el: new input(config, {
                    no: 'lfaSize',
                    label: lossFunctionAnalysis.t('lfaSize'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			lfaOutput: {
                el: new selectVar(config, {
                    no: 'lfaOutput',
                    label: lossFunctionAnalysis.t('lfaOutput'),
                    multiple: false,
                    required: true,
                    extraction: "NoPrefix|UseComma",
					options: ["plot", "text", "both"],
                    default: "plot",
                })
            },
        };
		
        const content = {
            left: [objects.content_var.el.content],
            right: [objects.lfaSubTitle.el.content, 
					objects.independent.el.content,
					objects.lfaDelta.el.content,
					objects.lfaY0.el.content,
					objects.lfaL0.el.content,
					objects.lfaSize.el.content,
					objects.lfaOutput.el.content],
            nav: {
                name: lossFunctionAnalysis.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: lossFunctionAnalysis.t('help.title'),
            r_help: lossFunctionAnalysis.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: lossFunctionAnalysis.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new lossFunctionAnalysis().render()
}
