// Import stylesheets
import './style.css';

// Write Javascript code!
const appDiv = document.getElementById('app');
appDiv.innerHTML = `<h1>Master en Big Data y Data Science - VIU</h1>`;

// ejercicio de Javascript
var visualization = d3plus.viz()
  .container("#viz1")
  .data([{"year": 1991, "name":"alpha", "value": 15},
    {"year": 1992, "name":"alpha", "value": 20},])
  .type("bar")
  .id("name")
  .x("year")
  .y("value")
  .draw()