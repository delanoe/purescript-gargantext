'use strict';
let webpack = require('webpack');
let path = require('path');
let exec = require('executive');
let nodeExternals = require('webpack-node-externals');
let isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');
let HtmlWebpackPlugin = require('html-webpack-plugin');
let CleanWebpackPlugin = require('clean-webpack-plugin');
let isWatch = process.argv.some(a => a === '--watch');

let dist = path.join(__dirname, 'dist');
let src = path.join(__dirname, 'src');
let test = path.join(__dirname, 'test');

module.exports = (env) =>{
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
          {test: /\.css$/,
           exclude: /(node_modules)/,
           use: ["style-loader", "css-loader"]},
          {test: /\.(png|jpg|gif|svg)$/,
           exclude: /(node_modules)/,
           use: [ "file-loader" ]},
          {test: /\.js$/,
           exclude: [/(node_modules)/, /(output)/],
           use: ["babel-loader", "source-map-loader"]}
        ]
      },
      resolve: {
        modules: [ 'node_modules' ],
        extensions: [ '.js']
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
  };
