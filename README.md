# README

## About

This is the official Wails Vanilla template.

## Live Development

To run in live development mode, run `wails dev` in the project directory. In another terminal, go into the `frontend`
directory and run `npm run dev`. The frontend dev server will run on http://localhost:34115. Connect to this in your
browser and connect to your application.

## Building

To build a redistributable, production mode package, use `wails build`.


# README

## About

Create GUI apps with Wails + Elm!

## Dev env requirements

- setup a [golang toolchain](https://go.dev/doc/install)
- setup a [wails toolchain](https://wails.io/docs/next/gettingstarted/installation)
- install [elm-live](https://www.elm-live.com/) globally: `npm install -g elm-live`


## Databases
https://gobuffalo.io/documentation/database/models/


$ soda generate model user title:string first_name:string last_name:string bio:text