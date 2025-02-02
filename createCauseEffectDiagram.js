



class createCauseEffectDiagram extends baseModal {
    static dialogId = 'createCauseEffectDiagram'
    static t = baseModal.makeT(createCauseEffectDiagram.dialogId)

    constructor() {
        var config = {
            id: createCauseEffectDiagram.dialogId,
            label: createCauseEffectDiagram.t('title'),
            modalType: "two",
            RCode: `
			require(SixSigma)

				#Creating Cause and Effect Diagram
				
				{{dataset.name}}_1 = as.data.frame({{dataset.name}})
				{{dataset.name}}_1[] = lapply({{dataset.name}}_1, function(x) type.convert(as.character(x), as.is = TRUE))
				{{dataset.name}}_1[is.na({{dataset.name}}_1)] = c("")
				
				SixSigma::ss.ceDiag(effect = '{{selected.effectName | safe}}', causes.gr = c({{selected.independent | safe}}), causes = {{dataset.name}}_1[c({{selected.independent | safe}})], 
									main = '{{selected.mainTitle | safe}}', sub = '{{selected.subTitle | safe}}')

		`
        };
        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move"}) },
            effectName: {
                el: new input(config, {
                    no: 'effectName',
                    label: createCauseEffectDiagram.t('effectName'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Poor quality widgets"
                })
            },
			mainTitle: {
                el: new input(config, {
                    no: 'mainTitle',
                    label: createCauseEffectDiagram.t('mainTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Six Sigma Cause and Effect Diagram"
                })
            },
			subTitle: {
                el: new input(config, {
                    no: 'subTitle',
                    label: createCauseEffectDiagram.t('subTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Fish Bone Diagram - SixSigma Project ID - Quality issue analysis at NC Plant"
                })
            },
			independent: {
                el: new dstVariableList(config, {
                    label: createCauseEffectDiagram.t('independent'),
                    no: "independent",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|UseComma|Enclosed",
                }), r: ['{{ var | safe}}']
            },
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [objects.effectName.el.content, 
					objects.mainTitle.el.content,
					objects.subTitle.el.content,
					objects.independent.el.content],
            nav: {
                name: createCauseEffectDiagram.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: createCauseEffectDiagram.t('help.title'),
            r_help: createCauseEffectDiagram.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: createCauseEffectDiagram.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new createCauseEffectDiagram().render()
}
