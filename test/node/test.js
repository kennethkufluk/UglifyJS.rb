#!/usr/bin/env node
var fs = require('fs');
parser = require('./parse.js');
var js = fs.readFileSync(process.argv[2], 'utf-8');
var ast = new parser.parse(js);
var pro = require('./process.js');
var ast = pro.ast_mangle(ast);
//console.log("MANGLED AST: ",(JSON.stringify(ast)));
console.log(pro.gen_code(ast));
