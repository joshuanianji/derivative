var MQ = MathQuill.getInterface(2);
// dude how does this work lol
export default class Test extends HTMLElement {
    constructor() {
        const self = super();

        self._editor = ""
    }

    connectedCallback() {
        this._editor = MQ.MathField(this, {
            // don't need backslash for words like pi, sqrt, sin, etc.
            autoCommands: 'pi sqrt degree',
            autoOperatorNames: 'sin cos ln',
            handlers: {
                edit: function () {
                    console.log(mathField.latex()); // simple API
                }
            }
        })
    }
}