import { Elm } from "../elm/src/Admin.elm"

var currentTime = new Date().getTime();
var width = window.innerWidth;
var height = window.innerHeight;
var app = Elm.Admin.init({flags: {currentTime: currentTime, width: width, height: height}});