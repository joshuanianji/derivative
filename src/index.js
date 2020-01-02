import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import StaticMath from './components/StaticMath';
import { InputMath } from './components/InputMath'

window.customElements.define('mathquill-static', StaticMath);
window.customElements.define('mathquill-input', InputMath);

var app = Elm.Main.init({
    node: document.getElementById('root')
});

app.ports.clear.subscribe(function (nothing) {
    document.querySelector("mathquill-input").clear()
});

document.addEventListener("changedLatex", function (event) {
    app.ports.changedLatex.send(event.detail); // simple API
})


/* NO IDEA WHAT THE CODE BELOW DOES LOL CREATE-ELM-APP MADE THIS FOR ME */

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
