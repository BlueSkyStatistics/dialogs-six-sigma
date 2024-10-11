


class processCapabilityAnalysis extends baseModal {
    static dialogId = 'processCapabilityAnalysis'
    static t = baseModal.makeT(processCapabilityAnalysis.dialogId)

    constructor() {
        var config = {
            id: processCapabilityAnalysis.dialogId,
            label: processCapabilityAnalysis.t('title'),
            modalType: "two",
            RCode: `
require(SixSigma)

#SixSigma Process Capability Analysis

			temp_cp_return = SixSigma::ss.ca.cp(x = {{dataset.name}}\${{selected.measuredVar | safe}}, 
											LSL = if(is.null(c({{selected.lsl | safe}}))) c(NA) else c({{selected.lsl | safe}}), 
											USL = if(is.null(c({{selected.usl | safe}}))) c(NA) else c({{selected.usl | safe}}), 
											LT = FALSE, 
											f.na.rm = TRUE, 
											ci = FALSE, 
											alpha = 0.05)
			BSkyFormat(data.frame(Cp=temp_cp_return), outputTableRenames = "Cp value without Confidence Intervals")
			
			
			if({{selected.ciChk | safe}})
			{
				temp_cp_return = ss.ca.cp(x = {{dataset.name}}\${{selected.measuredVar | safe}}, 
												LSL = if(is.null(c({{selected.lsl | safe}}))) c(NA) else c({{selected.lsl | safe}}), 
												USL = if(is.null(c({{selected.usl | safe}}))) c(NA) else c({{selected.usl | safe}}), 
												LT = FALSE, 
												f.na.rm = TRUE, 
												ci = TRUE, 
												alpha = if(is.null(c({{selected.alphaLim | safe}}))) c(0.05) else c({{selected.alphaLim | safe}}))
				BSkyFormat(temp_cp_return, outputTableRenames = "Cp value with Confidence Intervals")
			}
			
			temp_cpk_return = ss.ca.cpk(x = {{dataset.name}}\${{selected.measuredVar | safe}}, 
											LSL = if(is.null(c({{selected.lsl | safe}}))) c(NA) else c({{selected.lsl | safe}}), 
											USL = if(is.null(c({{selected.usl | safe}}))) c(NA) else c({{selected.usl | safe}}), 
											LT = FALSE, 
											f.na.rm = TRUE, 
											ci = FALSE, 
											alpha = 0.05)
			BSkyFormat(data.frame(Cpk=temp_cpk_return), outputTableRenames = "Cpk value without Confidence Intervals")
			
			
			if({{selected.ciChk | safe}})
			{
				temp_cpk_return = ss.ca.cpk(x = {{dataset.name}}\${{selected.measuredVar | safe}}, 
												LSL = if(is.null(c({{selected.lsl | safe}}))) c(NA) else c({{selected.lsl | safe}}), 
												USL = if(is.null(c({{selected.usl | safe}}))) c(NA) else c({{selected.usl | safe}}), 
												LT = FALSE, 
												f.na.rm = TRUE, 
												ci = TRUE, 
												alpha = if(is.null(c({{selected.alphaLim | safe}}))) c(0.05) else c({{selected.alphaLim | safe}}))
				BSkyFormat(temp_cpk_return, outputTableRenames = "Cpk value with Confidence Intervals")
			}
			
			temp_cpz_return = ss.ca.z(x = {{dataset.name}}\${{selected.measuredVar | safe}}, 
											LSL = if(is.null(c({{selected.lsl | safe}}))) c(NA) else c({{selected.lsl | safe}}), 
											USL = if(is.null(c({{selected.usl | safe}}))) c(NA) else c({{selected.usl | safe}}), 
											LT = FALSE, 
											f.na.rm = TRUE)
			BSkyFormat(data.frame(Cp_Z = temp_cpz_return), outputTableRenames = "Cp Z value")
			
			if({{selected.capabilityStudyChk | safe}})
			{
				ss.study.ca(xST = {{dataset.name}}\${{selected.measuredVar | safe}}, 
							xLT = if(is.null(c({{selected.futureProcessVar | safe}}))) c(NA) else c({{selected.futureProcessVar | safe}}), 
							LSL = if(is.null(c({{selected.lsl | safe}}))) c(NA) else c({{selected.lsl | safe}}), 
							USL = if(is.null(c({{selected.usl | safe}}))) c(NA) else c({{selected.usl | safe}}), 
							Target = if(is.null(c({{selected.target | safe}}))) c(NA) else c({{selected.target | safe}}), 
							alpha = if(is.null(c({{selected.alphaLim | safe}}))) c(0.05) else c({{selected.alphaLim | safe}}), 
							f.na.rm = TRUE, 
							f.main = "{{selected.mainTitle | safe}}", 
							f.sub = "{{selected.subTitle | safe}}")
			}
			
			if({{selected.ciMeanChk | safe}})
			{
				suppressWarnings(
					ss.ci(
						  x = {{selected.measuredVar | safe}},
						  sigma2 = if(is.null(c({{selected.sigma2 | safe}}))) c(NA) else c({{selected.sigma2 | safe}}),
						  alpha = if(is.null(c({{selected.alphaLim | safe}}))) c(0.05) else c({{selected.alphaLim | safe}}),
						  data = {{dataset.name}},
						  xname = "{{selected.measuredVar | safe}}",
						  approx.z = FALSE,
						  main = "{{selected.ciMeanMainTitle | safe}}",
						  digits = BSkyGetDecimalDigitSetting(),
						  sub = "{{selected.ciMeanSubTitle | safe}}",
						  ss.col = c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE")
						)
				)
			}
`
        };

        var objects = {
            content_var: { el: new srcVariableList(config, {action: "move", scroll:true}) },
			measuredVar: {
                el: new dstVariable(config, {
                    label: processCapabilityAnalysis.t('measuredVar'),
                    no: "measuredVar",
                    required: true,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix",
                }), r: ['{{ var | safe}}']
            },
			lsl: {
                el: new input(config, {
                    no: 'lsl',
                    label: processCapabilityAnalysis.t('lsl'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			usl: {
                el: new input(config, {
                    no: 'usl',
                    label: processCapabilityAnalysis.t('usl'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
                    value: "",
                })
            },
			ciChk: {
                el: new checkbox(config, {
                    label: processCapabilityAnalysis.t('ciChk'), 
					no: "ciChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			alphaLim: {
                el: new input(config, {
                    no: 'alphaLim',
                    label: processCapabilityAnalysis.t('alphaLim'),
                    placeholder: "",
                    required: true,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style: "ml-5 mb-2",
                    value: "0.05",
                })
            },
			capabilityStudyChk: {
                el: new checkbox(config, {
                    label: processCapabilityAnalysis.t('capabilityStudyChk'), 
					no: "capabilityStudyChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			target: {
                el: new input(config, {
                    no: 'target',
                    label: processCapabilityAnalysis.t('target'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style: "ml-5",
                    value: "",
                })
            },
			futureProcessVar: {
                el: new dstVariable(config, {
                    label: processCapabilityAnalysis.t('futureProcessVar'),
                    no: "futureProcessVar",
                    required: false,
                    filter: "String|Numeric|Logical|Ordinal|Nominal|Scale",
                    extraction: "Prefix",
                }), r: ['{{ var | safe}}']
            },
			mainTitle: {
                el: new input(config, {
                    no: 'mainTitle',
                    label: processCapabilityAnalysis.t('mainTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style: "ml-5",
                    value: "Six Sigma Process Capability Analysis Study",
                })
            },
			subTitle: {
                el: new input(config, {
                    no: 'subTitle',
                    label: processCapabilityAnalysis.t('subTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style: "ml-5 mb-2",
                    value: "Six Sigma project ID or name for Process Capability Study",
                })
            },
			ciMeanChk: {
                el: new checkbox(config, {
                    label: processCapabilityAnalysis.t('ciMeanChk'), 
					no: "ciMeanChk",
                    bs_type: "valuebox",
                    style: "mt-2 mb-1",
                    extraction: "BooleanValue",
                    true_value: "TRUE",
                    false_value: "FALSE",
					newline: true,
                })
            },
			sigma2: {
                el: new input(config, {
                    no: 'sigma2',
                    label: processCapabilityAnalysis.t('sigma2'),
                    placeholder: "",
                    required: false,
                    type: "numeric",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style: "ml-5",
                    value: "",
                })
            },
			ciMeanMainTitle: {
                el: new input(config, {
                    no: 'ciMeanMainTitle',
                    label: processCapabilityAnalysis.t('ciMeanMainTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style: "ml-5",
                    value: "Confidence Interval for the Mean",
                })
            },
			ciMeanSubTitle: {
                el: new input(config, {
                    no: 'ciMeanSubTitle',
                    label: processCapabilityAnalysis.t('ciMeanSubTitle'),
                    placeholder: "",
                    required: true,
                    type: "character",
                    extraction: "TextAsIs",
					allow_spaces:true,
					style: "ml-5",
                    value: "Six Sigma project ID or name",
                })
            },
        };
        const content = {
            left: [objects.content_var.el.content],
            right: [objects.measuredVar.el.content,
					objects.lsl.el.content,
					objects.usl.el.content,
					objects.ciChk.el.content,
					objects.alphaLim.el.content,
					objects.capabilityStudyChk.el.content,
					objects.target.el.content,
					objects.futureProcessVar.el.content,
					objects.mainTitle.el.content,
					objects.subTitle.el.content,
					objects.ciMeanChk.el.content,
					objects.sigma2.el.content,
					objects.ciMeanMainTitle.el.content,
					objects.ciMeanSubTitle.el.content],
            nav: {
                name: processCapabilityAnalysis.t('navigation'),
                icon: "icon-sixsigma",
                modal: config.id
            }
        };
        super(config, objects, content);
        
        this.help = {
            title: processCapabilityAnalysis.t('help.title'),
            r_help: "help(data,package='utils')",
            body: processCapabilityAnalysis.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new processCapabilityAnalysis().render()
}
