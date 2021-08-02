const path = require('path')

module.exports = {
  mode: 'development',
  devtool: 'source-map',
  entry: './src/App',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'app.js'
  },
  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.jsx', '.svg', '.css']
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader']
      },
      {
        test: /\.svg$/i,
        type: 'asset/resource'
      },
      {
        test: /\.tsx?$/,
        exclude: /node_modules/,
        use: ['ts-loader']
      }
    ]
  }
}
