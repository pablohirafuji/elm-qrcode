# Elm QRCode demo

All commands must be run in this folder (`demo`).

## Development

```sh
elm reactor
```

Open your browser at http://localhost:8000/ and navigate to src/Main.elm.


## Build

Build the project:

```sh
node build
```


## Run standalone

Remove `"../src"` at the `source-directories` entry in `elm.json` file and install the package running:

```sh
elm install pablohirafuji/qrcode
```