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