import "phoenix_html"

import socket from "./socket"

import { Elm } from "../elm/src/Main.elm"



// Strong seed for random generator //////////////// 

const crypto = window.crypto || window.msCrypto;
const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
};

const randInts = getRandomInts(5);
const seedInfo = [randInts[0], randInts.slice(1)]




var currentTime = new Date().getTime();
var width = window.innerWidth;
var height = window.innerHeight;
var app = Elm.Main.init({flags: {currentTime: currentTime, width: width, height: height, seedInfo: seedInfo}});

// Channel code //////////////////

socket.connect()

app.ports.joinChannel.subscribe(function(uuid){
  window.channel = socket.channel("bookings:locked_days", {uuid: uuid})
  
  channel.join()
  .receive("ok", resp => { console.log("Joined successfully", resp) })
  .receive("error", resp => { console.log("Unable to join", resp) })
  
  channel.on("broadcast_initial_locked_days",payload => {
    app.ports.receiveInitialLockedDays.send(payload)
  });
  
  channel.on("broadcast_locked_days",payload => {
    app.ports.receiveLockedDays.send(payload)
  });

  channel.on("new_booking",payload => {
    app.ports.broadcastRefreshAv.send(null)
  });

  channel.on("need_refresh",payload => {
    app.ports.broadcastRefreshAv.send(null)
  });

  channel.on("presence_state", payload => {
    app.ports.presenceState.send(payload)
  });

  channel.on("presence_diff",payload => {
    app.ports.presenceDiff.send(payload)
  });

});



app.ports.broadcastLockedDays.subscribe(function(lDays){
  channel.push("days_locked", {cIn: lDays.cIn, cOut: lDays.cOut});
});

app.ports.requestRefresh.subscribe(function(){
  channel.push("request_refresh", {});
});

// Captcha code ///////////////////

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

// Cookies admin code ///////////////////

app.ports.loadExternalScript.subscribe(function(url){
    var script = document.createElement("script");  
    script.src = url;  

    document.head.appendChild(script);  
});

app.ports.loadMatomo.subscribe(function(){
  // var _paq = window._paq || [];
  //   /* tracker methods like "setCustomDimension" should be called before "trackPageView" */
  //   _paq.push(['trackPageView']);
  //   _paq.push(['enableLinkTracking']);
  //   (function() {
  //     var u="//analytics.uminokirin.com/matomo/";
  //     _paq.push(['setTrackerUrl', u+'matomo.php']);
  //     _paq.push(['setSiteId', '2']);
  //     var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  //     g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
  //   })();
  loadMatomoScript();
});

app.ports.setCookieConsent.subscribe(function(prefs){
  window.localStorage.setItem('vieuxLilasCookiePrefs', JSON.stringify(prefs));
});

app.ports.loadLocalPrefs.subscribe(function(){
  var prefs = window.localStorage.getItem('vieuxLilasCookiePrefs') || "{}";
  app.ports.localPrefs.send(JSON.parse(prefs));
});

app.ports.clearLocalStorage.subscribe(function(){
  window.localStorage.clear();
  var cookies = document.cookie.split(";");

  for (var i = 0; i < cookies.length; i++) {
      var cookie = cookies[i];
      var eqPos = cookie.indexOf("=");
      var name = eqPos > -1 ? cookie.substr(0, eqPos) : cookie;
      document.cookie = name + "=;expires=Thu, 01 Jan 1970 00:00:00 GMT";
  }
})