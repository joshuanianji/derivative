import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import Test from './components/test';

window.customElements.define('mathquill-test', Test);

var app = Elm.Main.init({
    node: document.getElementById('root')
});

var MQ = MathQuill.getInterface(2);

var staticMaths = document.querySelectorAll('#static-math')
staticMaths.forEach(function (node) {
    console.log("Static Math initialized")
    MQ.StaticMath(node);
})


var answerSpan = document.querySelector('#math-test');
var mathField = MQ.MathField(answerSpan, {
    // don't need backslash for words like pi, sqrt, sin, etc.
    autoCommands: 'pi sqrt degree',
    autoOperatorNames: 'sin cos ln',
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
