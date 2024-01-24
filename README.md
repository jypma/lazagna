# Lazagna

Lazagna is a UI framework for developing asynchronous, event-driven browser user interfaces in the Scala language, using [ScalaJS](https://www.scala-js.org/) and [ZIO](https://zio.dev/).

## Architecture

Lazagna does not use a virtual DOM. Instead, its HTML element DSL creates DOM elements directly, and relies on streams and events to perform differential updates directly. This is heavily inspired from the awesome [laminar](https://laminar.dev/) framework. However, where Laminar has its own streaming framework, Lazagna uses ZIO for its streams and concurrency.

## Naming

The framework is called "Lazagna" because of the Z in ZIO, and of course because Lasagna, being made with layers, is a [laminar](https://laminar.dev/).

# Running

At the moment, this repository contains a simple drawing application that demonstrates how to write a command pattern-based rendering pipeline. If you want to run it, then in one console run:

```sh
sbt
project client
~fastLinkJS
```

and in another console run:

```sh
cd draw-client
npm run dev
```

and in a third console run:
```sh
sbt
project server
~reStart
```

The latter will open the example at `http://localhost:5173/`, which you can open in your web browser. The example is currently fully offline, but a multi-user version with a small server backend is being planned and developed.
