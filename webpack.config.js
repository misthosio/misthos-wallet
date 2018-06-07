const UglifyJsPlugin = require('uglifyjs-webpack-plugin')
const webpack = require('webpack');
const HtmlWebPackPlugin = require("html-webpack-plugin");

const publicPath = '/';

const config = {
  entry: {
    main: './src/entry.bs.js'
  },
  output: {
    filename: 'static/js/[name].[chunkhash:8].js',
    publicPath: publicPath,
  },
  module: {
    rules: [
      {
        test: /\_worker\.bs\.js$/,
        use: {
          loader: 'worker-loader',
          options: {
            name: 'static/js/[hash].worker.js'
          }
        }
      },
      {
        test: /\.(js|mjs)$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader"
        }
      },
      {
        test: /\.html$/,
          use: [
            {
              loader: "html-loader",
              options: { minimize: true }
            }
          ]
      },
      {
        exclude: [ /\.js$/, /\.html$/, /\.json$/],
        loader: require.resolve('file-loader'),
        options: {
          name: 'static/media/[name].[hash:8].[ext]',
        }
      }
    ]
  },
  optimization: {
    minimizer: [
      new UglifyJsPlugin({
        uglifyOptions: {
          mangle: {
            reserved: ['BigInteger','ECPair','Point']
          },
        }
      }),
    ],
  },
  plugins: [
    new HtmlWebPackPlugin({
      inject: true,
      template: "./public/index.html",
      minify: {
        removeComments: true,
        collapseWhitespace: true,
        removeRedundantAttributes: true,
        useShortDoctype: true,
        removeEmptyAttributes: true,
        removeStyleLinkTypeAttributes: true,
        keepClosingSlash: true,
        minifyJS: true,
        minifyURLs: true,
      },
    }),
  ],
  devServer: {
    compress: true,
    clientLogLevel: 'none',
    port: 3000,
    contentBase: './public',
    publicPath: publicPath,
    historyApiFallback: {
      disableDotRule: true,
    },
    headers: {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
      "Access-Control-Allow-Headers": "X-Requested-With, content-type, Authorization",
    },
  },
  node: {
    dgram: 'empty',
    fs: 'empty',
    net: 'empty',
    tls: 'empty',
    child_process: 'empty',
  },
};

module.exports = config;
