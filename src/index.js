import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import StaticMath from './components/StaticMath';
import InputMath from './components/InputMath'

window.customElements.define('mathquill-static', StaticMath);
window.customElements.define('mathquill-input', InputMath);

var app = Elm.Main.init({
    node: document.getElementById('root')
});

var MQ = MathQuill.getInterface(2);

var answerSpan = document.querySelector('#math-test');
var mathField = MQ.MathField(answerSpan, {
    // don't need backslash for words like pi, sqrt, sin, etc.
    autoCommands: 'pi sqrt degree',
    autoOperatorNames: 'sin cos tan csc sec cot ln arcsin arccos arctan arccsc arcsec arccot',
    handlers: {
        edit: function () { // useful event handlers
            // console.log(mathField.latex())
            app.ports.changedLatex.send(mathField.latex()); // simple API
        }
    }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
