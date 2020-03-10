#!/usr/bin/env node
'use strict';

var path = require('path');
var readline = require('readline');
var execSync = require('child_process').execSync;

var rl = readline.createInterface({
    input: process.stdin,
    output: null,
    terminal: false
});

var cmd = ['.', 'node_modules', '.bin', 'purty'].join(path.sep);

var paths = [];

function purty(paths) {
    console.log(paths);
    for (let path of paths) {
        var command = [cmd, path, '--write'].join(' ');
        console.log(command);
        execSync(command, (error, stdout, stderr) => {
            if (stdout) {
                console.log(stdout);
            }
            if (stderr) {
                console.log(stderr);
            }
        });
    };
}

rl.on('line', (srcPath) => {
    paths.push(srcPath);
});

rl.on('close', () => {
    purty(paths);
});
