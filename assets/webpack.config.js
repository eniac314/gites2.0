const path = require('path');
const glob = require('glob');
// const MiniCssExtractPlugin = require('mini-css-extract-plugin');
// const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
// const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const TerserPlugin = require("terser-webpack-plugin");
// const isProduction = process.env.NODE_ENV === 'production';

module.exports = (env, options) => {
  console.log('env:', env);
  console.log('options:', options);
  console.log('watch', options.mode === 'development');

  return {
  watch: options.mode === 'development',
  optimization: {
    minimize: true,
    minimizer: [new TerserPlugin()],
  },
  entry: {
      // app: ['./js/app.js'].concat(glob.sync('./vendor/**/*.js')),  
      main: ['./js/main.js'],
      admin: ['./js/admin.js']
      // jsPdf: ['./js/jspdf.umd.min.js'],
      // html2canvas: ['./js/html2canvas.min.js']
  },
  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, '../priv/static/js')
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader'
        }
      },
      // {
      //   test: /\.css$/,
      //   use: [MiniCssExtractPlugin.loader, 'css-loader']
      // },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [ 
        // { loader: 'elm-hot-webpack-loader' },
        { loader: 'elm-webpack-loader',
          options: {
            cwd: path.resolve(__dirname, 'elm'),
            // optimize: true
            // debug: true
          }
        }]
      }
    ]
  },
  plugins: [
    // new MiniCssExtractPlugin({ filename: '../css/app.css' }),
    new CopyWebpackPlugin({ patterns: [{ from: 'static/', to: '../' }]})
  ]
};
};
