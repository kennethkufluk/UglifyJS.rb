#!/usr/bin/env node
var fs = require('fs');
parser = require('./parse.js');
var js = fs.readFileSync(process.argv[2], 'utf-8');
var ast = new parser.parse(js);
//console.log((JSON.stringify(ast)));
var pro = require('./process.js');
console.log(pro.gen_code(ast));
