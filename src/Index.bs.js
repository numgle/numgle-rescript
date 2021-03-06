// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Express = require("express");
var NumgleConverter = require("./NumgleConverter.bs.js");

var app = Express();

app.use(Express.json());

app.get("/:value", (function (req, res) {
        var params = req.params;
        var input = params.value;
        if (input == null) {
          res.status(400).json({
                error: "Invalid input"
              });
        } else {
          res.set("content-type", "text/plain");
          res.status(200).send(NumgleConverter.convert(input));
        }
        
      }));

app.listen(8086, (function (param) {
        console.log("Server started at: http://localhost:" + String(8086));
        
      }));

var port = 8086;

exports.app = app;
exports.port = port;
/* app Not a pure module */
