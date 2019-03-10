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
            width: image.width,
            height: image.height
        };

        app.ports.processedImages.send(result);
    };

    image.src = data.imageData;
}