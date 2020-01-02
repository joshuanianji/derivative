export { InputMath }

var MQ = MathQuill.getInterface(2)

// custom event for webcomponent to communicate with elm program

function dispatchEvent(latex) {
    var changedLatex = new CustomEvent('changedLatex', { detail: latex });
    document.dispatchEvent(changedLatex)
}


// deals with inputting math

class InputMath extends HTMLElement {
    constructor() {
        super();

        console.log("input element initiated");
    }

    connectedCallback() {

        this.innerHTML = `<span id='function-input'></span>`

        var mathField = MQ.MathField(this.querySelector("span"), {
            // don't need backslash for words like pi, sqrt, sin, etc.
            autoCommands: 'pi sqrt degree',
            autoOperatorNames: 'sin cos tan csc sec cot ln arcsin arccos arctan arccsc arcsec arccot',
            handlers: {
                edit: function () { // useful event handlers
                    console.log(mathField.latex())
                    dispatchEvent(mathField.latex())
                }
            }
        });
    }

    // through ports, elm calls javascript to reset the field and once we get that function we use the mathquill API to do so
    clear() {
        MQ(this.querySelector("span")).latex("")
    }

}

