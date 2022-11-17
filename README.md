# ADR suite

This is a Node.js command line tool for maintaining Architecture Decision Records (ADR). You can read more about ADR in the (I think) [original blog post](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions). This tool is inspired by [npryce/adr-tools](https://github.com/npryce/adr-tools).

In the end this will become an `npm` package which will provide the CLI tool as a command like `adr`.

## Status

I just started working on this and work progresses (very) slowly. There is no package yet, but the tool can be run manually (see below). What works now:

- Creating a new ADR (kind of)

## Running the tool

I wouldn't call it "using" yet, but the tool can be run after building it. Use the following command:

~~~
node dist/adr.js
~~~

## Development (or how to play around)

You will need:

* PureScript compiler (`purs`) 
* PureScript Spago build and package manager tool (`spago`)
* `npm`
* Visual Studio Code with the `PureScript IDE` extention

To set this up follow these instructions.

1. Clone the repository.

2. Run `npm install`. This should install `purs` and `spago` _locally_ to the project. It is possible to install them globally but I have multiple PureScript projects with different compiler versions. Consequently, instructions for the VS Code settings below match local installation and are **not correct** for the global installation. Generally though, global installation is more convenient because VS Code settings work out-of-the box.

3. Try compiling for the first time using

    ~~~
    npm run compile
    ~~~

    This needs to work at least once before the project can work in VS Code.

4. Adjust VS Code settings in the PureScript section (`Ctrl+,` and search for `PureScript`).

    * `Purescript: Build Command`
    
        ~~~
        npm exec spago build -- --purs-args --json-errors
        ~~~

    * `Purescript: Package Path`

        ~~~
        .spago
        ~~~

    * `Purescript: Purs Exe`

        ~~~
        ./node_modules/.bin/purs
        ~~~

5. Restart VS Code

    Everything should work from now on.

Once again, these are the steps for the set-up where PureScript and Spago are installed locally. Alternatively you can follow the getting started steps at the [official Web-site](https://www.purescript.org/) or in the [documentation](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md).