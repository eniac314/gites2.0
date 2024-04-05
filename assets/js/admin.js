import "phoenix_html"

import socket from "./socket"

import { Elm } from "../elm/src/Admin.elm"
import { jsPDF } from "jspdf";
import * as html2canvas from 'html2canvas'

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
var app = Elm.Admin.init({
	flags: {
		currentTime: currentTime,
		width: width,
		height: height,
        seedInfo: seedInfo
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

// saving to PDF
// window.jsPDF = window.jspdf.jsPDF;
app.ports.savePdf.subscribe(function(id){
    savePDF(id);
});

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function savePDF(id) {
  console.log(id)
  var imgs = [...document.querySelectorAll('.' + id)];
  // console.log(imgs)
  var doc = new jsPDF('p','px','a4');

  var j = 0;

  // var img = document.getElementById(id);
  // var imgs = [img];

  // app.ports.savePdfProgress.send({done: 0, total: imgs.length});
  await sleep(50);

  var promises = imgs.map(async (img, i) => { 
    var canvas = await html2canvas(img);
    // app.ports.savePdfProgress.send({done: j + 1, total: imgs.length});
    j = j + 1;
    return canvas;
  })

  var currentHeight = 0

  var canvases = await Promise.all(promises);
  var currentCanvas = 0
  
  canvases.forEach(canvas => {
    var wid = canvas.width; 
    var hgt = canvas.height; 
    var img = canvas.toDataURL("image/jpeg", 0.9);
    var hratio = hgt/wid; 
    var width = doc.internal.pageSize.width;    
    var height = width * hratio;

    if (currentCanvas > 0 && currentHeight + height + 20 > doc.internal.pageSize.height) {
      doc.addPage();
      currentHeight = 0;
    }

    doc.addImage(img,'JPEG', 0, currentHeight, width, height);

    currentHeight = currentHeight + height;
    currentCanvas = currentCanvas + 1
      
  })
  // doc.output('dataurlnewwindow',id+'.pdf');

  // window.open(URL.createObjectURL(doc.output("blob"),id+'.pdf'))

  let dataSrc = doc.output("datauristring");
  
  let win = window.open("", id +'.pdf');
  console.log(dataSrc);

  win.document.write("<html><head><title>" + id+'.pdf' + "</title></head><body width='100%' height='100%'><embed width='100%' height='100%' src=" + 
      dataSrc + "></embed></body></html>");
  
}