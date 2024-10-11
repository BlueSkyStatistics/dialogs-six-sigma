

class SixSigmaOverview extends baseModal {
    static dialogId = 'SixSigmaOverview'
    static t = baseModal.makeT(SixSigmaOverview.dialogId)

    constructor() {
        var config = {
            id: SixSigmaOverview.dialogId,
            label: SixSigmaOverview.t('title'),
            modalType: "one",
        }
        var objects = {
			lbl1: { 
				el: new labelHelpSixSigma(config, { 
                    no: "lbl1",
					label: SixSigmaOverview.t('lbl1')
				}) 
			}

            // label1: {
            //     el: new labelHelp(config, {
            //         no: 'label1', 
            //         label: SixSigmaOverview.t('label1'), 
            //         h: 9
            //     }) 
            // },
        }  
        const content = {
            items: [
				objects.lbl1.el.content
				],
            nav: {
                name: SixSigmaOverview.t('navigation'),
                icon: "icon-sixsigma",
                datasetRequired: false,
                modal: config.id
            }
        }
        super(config, objects, content);
    }
}

module.exports = {
    render: () => new SixSigmaOverview().render()
}
