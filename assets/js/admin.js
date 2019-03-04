import { Elm } from "../elm/src/Admin.elm"

var currentTime = new Date().getTime();
var width = window.innerWidth;
var height = window.innerHeight;
var app = Elm.Admin.init({flags: {currentTime: currentTime, width: width, height: height}});

app.ports.toAuthLocalStorage.subscribe(function(cmd){
	if(cmd.action === "set"){
		localStorage.setItem('gites-jwt', cmd.payload);
		app.ports.fromAuthLocalStorage.send({"result":"ok"});

	} else if(cmd.action === "get"){
		var jwt = localStorage.getItem('gites-jwt');
		if(jwt){
			app.ports.fromAuthLocalStorage.send({"jwt":jwt});
		} else 
		{ app.ports.fromAuthLocalStorage.send({"result":"error"})}

	} else if(cmd.action == "clear"){
		localStorage.clear();
		app.ports.fromAuthLocalStorage.send({"result":"ok"});
	} else app.ports.fromAuthLocalStorage.send({"result":"error"});
});

