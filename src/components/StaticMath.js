var MQ = MathQuill.getInterface(2)

// deals with displaying math in a static way

export default class StaticMath extends HTMLElement {
    constructor() {
        const self = super();

        self._latexValue = ""
        // so mathquill can identify it
        self._id = (Math.floor(Math.random() * 10000) + 1).toString();
        console.log("static math initiated - id: " + self._id)

        return self
    }

    set latexValue(value) {

        this._latexValue = value

        // delete all children
        while (this.firstChild) {
            this.removeChild(this.firstChild);
        }

        let span = document.createElement("span")
        span.id = this._id
        span.textContent = this._latexValue

        this.appendChild(span)
        MQ.StaticMath(document.getElementById(this._id))

        // var staticMath = document.getElementById(this._id)
        // staticMath.latex(this._latexValue)
    }

    get latexValue() {
        return this._latexValue
    }

    // every time the static math gets initiated i think?? or put on the DOM?
    connectedCallback() {
        // delete all children
        while (this.firstChild) {
            this.removeChild(this.firstChild);
        }

        let span = document.createElement("span")
        span.id = this._id
        span.textContent = this._latexValue

        this.appendChild(span)
        MQ.StaticMath(document.getElementById(this._id))
    }
}
