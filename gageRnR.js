


class gageRnR extends baseModal {
    static dialogId = 'gageRnR'
    static t = baseModal.makeT(gageRnR.dialogId)

    constructor() {
        var config = {
            id: gageRnR.dialogId,
            label: gageRnR.t('title'),
            modalType: "two",
            RCode: `
require(SixSigma)

#SixSigma Gage R&R - Measurement System Analysis

			temp_gageRnR = SixSigma::ss.rr(
			  main = "{{selected.mainTitle | safe}}",
			  sub = "{{selected.subTitle | safe}}",
			  data = {{dataset.name}},
			  var = {{selected.measuredVar | safe}},
			  part = {{selected.part | safe}},
			  appr = {{selected.appr | safe}},
			  lsl = if(is.null(c({{selected.lsl | safe}}))) c(NA) else c({{selected.lsl | safe}}),
			  usl = if(is.null(c({{selected.usl | safe}}))) c(NA) else c({{selected.usl | safe}}),
			  tolerance = if(!is.null(c({{selected.tolerance | safe}}))) c({{selected.tolerance | safe}}) else {if(!is.null(c({{selected.usl | safe}})) && !is.null(c({{selected.lsl | safe}}))) c(c({{selected.usl | safe}})-c({{selected.lsl | safe}})) else c(NA)},
			  sigma = c({{selected.sigma | safe}}),
			  alphaLim = c({{selected.alphaLim | safe}}),
			  errorTerm = "{{selected.errorTerm | safe}}",
			  digits = BSkyGetDecimalDigitSetting(),
			  method = "crossed",
			  print_plot = TRUE,
			  signifstars = FALSE
			)
			
			for(i in 1:(length(temp_gageRnR) - 1)){
				table_name = names(temp_gageRnR)[i]
				table_name = switch(  
						i,  
						c("Anova - Complete model (with interaction)"),  
						c(paste("Anova - Reduced model (without interaction - alpha: ",c({{selected.alphaLim | safe}}),")", sep="")),  
						c("Gage R&R"),  
						c("%StudyVar %Tolerance")
				)  
				BSkyFormat(temp_gageRnR[[i]], outputTableRenames = table_name)
			}
			BSkyFormat(paste("Number of Distinct Categories:",temp_gageRnR[5]))
`
        };

        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) },
			mainTitle: {
                el: new input(config, {
                    no: 'mainTitle',
                    label: gageRnR.t('mainTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Six Sigma Gage R&R Study",
                })
            },
			subTitle: {
                el: new input(config, {
                    no: 'subTitle',
                    label: gageRnR.t('subTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "Six Sigma project ID or name for Gage R&R Study",
                })
            },
			measuredVar: {
                el: new dstVariable(config, {
                    label: gageRnR.t('measuredVar'),
                    no: "measuredVar",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|Enclosed",
                }), r: ['{{ var | safe}}']
            },
			part: {
                el: new dstVariable(config, {
                    label: gageRnR.t('part'),
                    no: "part",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|Enclosed",
                }), r: ['{{ var | safe}}']
            },
			appr: {
                el: new dstVariable(config, {
                    label: gageRnR.t('appr'),
                    no: "appr",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|Enclosed",
                }), r: ['{{ var | safe}}']
            },
			lsl: {
                el: new input(config, {
                    no: 'lsl',
                    label: gageRnR.t('lsl'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			usl: {
                el: new input(config, {
                    no: 'usl',
                    label: gageRnR.t('usl'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			sigma: {
                el: new input(config, {
                    no: 'sigma',
                    label: gageRnR.t('sigma'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "6",
                })
            },
			tolerance: {
                el: new input(config, {
                    no: 'tolerance',
                    label: gageRnR.t('tolerance'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    //value: "usl - lsl",
                })
            },
			alphaLim: {
                el: new input(config, {
                    no: 'alphaLim',
                    label: gageRnR.t('alphaLim'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "0.05",
                })
            },
			errorTerm: {
                el: new input(config, {
                    no: 'errorTerm',
                    label: gageRnR.t('errorTerm'),
                    placeholder: "",
                    required: false,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "interaction",
                })
            },
			/*
			method: {
                el: new selectVar(config, {
                    no: 'method',
                    label: gageRnR.t('method'),
                    multiple: false,
                    required: true,
                    extraction: "NoPrefix|UseComma",
					options: ["crossed", "nested"],
                    default: "crossed",
                })
            },
			*/
        };
		
        const content = {
            left: [objects.content_var.el.content],
            right: [objects.mainTitle.el.content, 
					objects.subTitle.el.content,
					objects.measuredVar.el.content,
					objects.part.el.content,
					objects.appr.el.content,
					objects.lsl.el.content,
					objects.usl.el.content,
					objects.tolerance.el.content,
					objects.sigma.el.content,
					objects.alphaLim.el.content,
					objects.errorTerm.el.content],
					//objects.method.el.content],
            nav: {
                name: gageRnR.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: gageRnR.t('help.title'),
            r_help: gageRnR.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: gageRnR.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new gageRnR().render()
}
