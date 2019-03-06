import { Elm } from "../elm/src/Main.elm"

var currentTime = new Date().getTime();
var width = window.innerWidth;
var height = window.innerHeight;
var app = Elm.Main.init({flags: {currentTime: currentTime, width: width, height: height}});

app.ports.loadCaptcha.subscribe(function(sitekey) {
  grecaptcha.render(
    document.querySelector('div.g-recaptcha'),
     {
        sitekey: sitekey,
        callback: function onSubmit(val) {
            app.ports.captcha_port.send(val);
          }
     }
  );  
});