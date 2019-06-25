'use strict';
let webpack = require('webpack');
let path = require('path');
let exec = require('executive');
let nodeExternals = require('webpack-node-externals');
let isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');
let HtmlWebpackPlugin = require('html-webpack-plugin');
let CleanWebpackPlugin = require('clean-webpack-plugin');
let isWatch = process.argv.some(a => a === '--watch');

// TODO: We have agreed to move to spago, but not done it yet
// let spago_sources = async () =>
//   exec.quiet(
//     "psc-package sources",
//     { options: 'strict' }
//   ).then(function (res) {
//     let sources = res.stdout.split(/\r?\n/);
//     sources.pop(); // extra newline at the end of output
//     return sources;
//   });

let dist = path.join(__dirname, 'dist');
let src = path.join(__dirname, 'src');
let test = path.join(__dirname, 'test');

// kill when spago
let futured = async () => new Promise((resolve, _) => resolve([]));

module.exports = (env) =>
  // spago_sources()
  futured()
  .then(function (ps_sources) {

    ps_sources.push('src/**/*.purs');
    // TODO: testing in browser and headless
    // if (env === "browser" || env === "headless")
    //   ps_sources.push('test/Main.purs');
    let config = {
      cache: true,
      mode: 'development',
      target: "web",
      devtool: 'inline-source-map',
      devServer: {
        disableHostCheck: true,
        contentBase: dist,
        compress: true,
        port: 8000
      },
      output: {
        path: dist,
        filename: 'bundle.js'
      },
      module: {
        rules: [
          {test: /\.purs$/,
           exclude: /(node_modules)/,
           use: [
             {loader: "purs-loader",
              options: {
                src: ps_sources,
                output: dist,
                pscIde: true,
                pscIdeClientArgs: {port: 4002},
                pscIdeServerArgs: {port: 4002},
                pscArgs: {codegen: "js,sourcemaps"},
                pscPackage: true,
                bundle: false,
                watch: isWatch}},
             {loader: "source-map-loader"},
           ]},
          {test: /\.css$/,
           exclude: /(node_modules)/,
           use: ["style-loader", "css-loader"]},
          {test: /\.(png|jpg|gif|svg)$/,
           exclude: /(node_modules)/,
           use: [ "file-loader" ]},
          {test: /\.js$/,
           exclude: /(node_modules)/,
           use: ["babel-loader", "source-map-loader"]}
        ]
      },
      resolve: {
        modules: [ 'node_modules' ],
        extensions: [ '.purs', '.js']
      },
      plugins: [
      // TODO: can we put the checked-in assets in dist somewhere else
      // and move them into place so we can clean?
      //   new CleanWebpackPlugin(['dist']),
        new webpack.LoaderOptionsPlugin({debug: true})
      ],
      entry: path.join(src, "index.js")
    };
    switch(env) {
    case 'dev':
      console.log("Serving index.html from template src/index.html")
      config.plugins.push(new HtmlWebpackPlugin({
        template: path.join(src, "index.html")
      }));
      break;
    // TODO: testing environments - browser and headless
    // case 'browser':
    //   config.plugins.push(new HtmlWebpackPlugin({
    //     title: "Reactix",
    //     template: path.join(test, "browser.html")
    //   }));
    //   break;
    // case 'headless': break;
    default:
      console.log("unknown env: ", env);
    }
    return config;
  });
