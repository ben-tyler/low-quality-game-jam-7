import { Elm } from "./src/Main.elm";
import {Howl, Howler} from 'howler';
var sound = new Howl({
    src: ['fast-and-intense-10115.mp3'],
    loop:true
});
var ss = new Howl({
    src: ["ff.wav"]
})

sound.play();
var app = Elm.Main.init({ node: document.getElementById("root") });

app.ports.killme.subscribe(function(message) {
    ss.play();
});