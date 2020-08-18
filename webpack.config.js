'use strict';

const path = require('path');
const webpack = require('webpack');
const isWatch = process.argv.some(a => a === '--watch');

module.exports = {
      mode: 'development',
      devtool: 'eval-source-map',

      entry: {
            im: './loader/im.bundle.js',
            landing: './loader/landing.bundle.js',
            login: './loader/login.bundle.js'
      },

      output: {
            path: path.resolve(__dirname, 'dist'),
            filename: '[name].bundle.js'
      },

      module: {
            rules: [{
                  test: /\.purs$/,
                  use: [{
                        loader: 'purs-loader',
                        options: {
                              src: ['src/**/*.purs'],
                              spago: true,
                              watch: isWatch,
                              pscIde: true
                        }
                  }]
            }]
      },

      resolve: {
            modules: [ 'node_modules' ],
            extensions: [ '.purs', '.js']
      },

      plugins: [
            new webpack.LoaderOptionsPlugin({
                  debug: true
            }),
            function(){
                  this.plugin('done', function(stats){
                        process.stderr.write(stats.toString('errors-only'));
                  });
            }
      ]
};