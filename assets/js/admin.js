import "phoenix_html"

import socket from "./socket"

import { Elm } from "../elm/src/Admin.elm"

var currentTime = new Date().getTime();
var width = window.innerWidth;
var height = window.innerHeight;
var app = Elm.Admin.init({
	flags: {
		currentTime: currentTime,
		width: width,
		height: height
	}
});

// Channel code //////////////////

socket.connect()

let channel = socket.channel("bookings:locked_days", {uuid: "bookingsAdmin"})

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
    app.ports.broadcastRefresh.send(null)
});

channel.on("presence_state", payload => {
    app.ports.presenceState.send(payload)
});

channel.on("presence_diff",payload => {
    app.ports.presenceDiff.send(payload)
});


// Auth code /////////////////////

app.ports.toAuthLocalStorage.subscribe(function (cmd) {
	if (cmd.action === "set") {
		localStorage.setItem('gites-jwt', JSON.stringify(cmd.payload));
		app.ports.fromAuthLocalStorage.send({
			"result": "ok"
		});

	} else if (cmd.action === "get") {

		var strPayload = localStorage.getItem('gites-jwt');

		if (strPayload) {
			var payload = JSON.parse(strPayload);
			app.ports.fromAuthLocalStorage.send({
				"jwt": payload.jwt,
				"username": payload.username,
				"time": new Date().getTime()
			});
		} else {
			app.ports.fromAuthLocalStorage.send({
				"result": "error"
			})
		}

	} else if (cmd.action == "clear") {
		localStorage.clear();
		app.ports.fromAuthLocalStorage.send({
			"result": "ok"
		});
	} else {
		app.ports.fromAuthLocalStorage.send({
			"result": "error"
		});
	}
});

// Image processing code /////////

app.ports.toImageProcessor.subscribe(function(d){
    processImage(d);
});

function processImage(data) {

    var image = new Image();

    image.onload = function() {

        var canvas = document.createElement('canvas');

        if (image.height > 600) {
            var width = image.width * 600 / image.height;
            var height = 600;
        } else {
            var width = image.width;
            var height = image.height;
        }

        var ctx = canvas.getContext("2d");


        canvas.width = width;
        canvas.height = height;
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(image, 0, 0, width, height);
        var smallPic = canvas.toDataURL("image/jpeg", 0.90);


        var thumbSize = Math.min(image.width, image.height);

        if (image.width > image.height) {
            var sx = (Math.floor((image.width - image.height) / 2));
            var sy = 0;
        } else {
            var sx = 0;
            var sy = (Math.floor((image.height - image.width) / 4));
        }

        var thumbCanvas = document.createElement('canvas');
        thumbCanvas.width = 200;
        thumbCanvas.height = 200;

        var tCtx = thumbCanvas.getContext("2d");
        tCtx.clearRect(0, 0, 200, 200);


        tCtx.drawImage(image, sx, sy, thumbSize, thumbSize, 0, 0, 200, 200);
        var thumb = thumbCanvas.toDataURL("image/jpeg", 0.9);

        var result = {
            filename: data.filename,
            content: smallPic,
            thumb: thumb,
            size: fileSize(smallPic),
            width: canvas.width,
            height: canvas.height
        };

        app.ports.processedImages.send(result);
    };

    image.src = data.imageData;
}

function fileSize(src){
    var stringLength = src.length - 'data:image/jpeg;base64,'.length;

    var sizeInBytes = 4 * Math.ceil((stringLength / 3))*0.5624896334383812;
    var sizeInKb=sizeInBytes/1024;
    return Math.round(sizeInBytes);
}