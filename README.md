# Lazagna

Lazagna is a UI framework for developing asynchronous, event-driven browser user interfaces in the Scala language, using [ScalaJS](https://www.scala-js.org/) and [ZIO](https://zio.dev/).

## Architecture

Lazagna does not use a virtual DOM. Instead, its HTML element DSL creates DOM elements directly, and relies on streams and events to perform differential updates directly. This is heavily inspired from the awesome [laminar](https://laminar.dev/) framework. However, where Laminar has its own streaming framework, Lazagna uses ZIO for its streams and concurrency.

### Modifier

The most basic building block in Lazagna is a `Modifier`. This is defined as a trait with a single method:

```scala
trait Modifier {
  def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit]
}
```

As you can see, there are two things that are important to a `Modifier`:

- It is mounted into the dom tree having a parent DOM tree `Element`. This need not imply that all modifiers create child elements; they might also be affecting the parent in other ways (or not at all).
- It returns a ZIO that only has side effects when mounted (`Unit`) and can't fail (`Nothing`).

## Naming

The framework is called "Lazagna" because of the Z in ZIO, and of course because Lasagna, being made with layers, is a [laminar](https://laminar.dev/).

# Not quite ready

- Clean up the use of implicit and given, and align on having a nice one-line import for library users

# Running the "draw" example

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

# Notes

## Manual layout
- Widget has padding
- Icon (or any widget, e.g. note), moveable at will (but keep padding in tact)
  -> Push and shove moving?
- Arrows between widgets

## Automatic layout
- Band
  -> weight on distance
  -> preferred angle
  -> weight on preferred angle
- Circular layout: bands to center (first element) and between each other
- Horizontal and Vertical layout

- Band from i1 to i2. Distance between i1 and i2 is `d` distance weight `w_d`, angle weight `w_a`, and abs deviation from preferred angle `da`
  Distance d is `d = sqrt((i1.x - i2.x)^2 + (i1.y - i2.y)^2)`
  loss is `d * w_d + da * w_a`
  loss is `sqrt((i1.x - i2.x)^2 + (i1.y - i2.y)^2) * w_d + da * w_a`
  calculate
