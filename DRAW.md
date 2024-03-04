# Running the "draw" example

At the moment, this repository contains a simple real-time multi-user drawing application that demonstrates how to write a command pattern-based rendering pipeline. If you want to run it, you'll need to start several services.

## Prerequisites

You'll need `sbt` and `docker`.

## Cassandra
Storage is provided by Cassandra. During development, start cassandra locally using

```sh
pushd draw-server
docker-compose up -d
popd
```

This can take up to two minutes to fully start initially. You can leave Cassandra running in the background.

Cassandra will store its data under `draw-server/target/cassandra-data` so your precious drawings will survive restarts. Docker will make them owned by user and group `999`, though (since that's what Cassandra runs as).

## Server

In one console run:

```sh
sbt
project server
~reStart
```

## Client (sbt)

In another console run:

```sh
sbt
project client
~fastLinkJS
```

## Client (npm, vite)

In a third console run:

```sh
cd draw-client
npm install # you only need this once.
npm run dev
```

The latter will open the example at `http://localhost:5173/`, which you can open in your web browser. Multiple users (web browsers) can edit the drawing simultaneously.

# Icons

Source: https://github.com/leungwensen/svg-icon
-`npm install`
- Copy packaged SVG icon symbol collections from `dist/sprite/symbol` to `public/symbols`

# Random notes (please ignore this section)

### Timeline
- Full re-render from underlying non-pruned event store
- Snapshot the DrawingState every 1000 events
- Present a compressed actual time line (with a play button!)
- Render from snapshot and subsequent events
- Read-only view, but we can allow specific objects (or a selection) to be "rescued" back into the live editor

### Manual layout
- Widget has padding
- Icon (or any widget, e.g. note), moveable at will (but keep padding in tact)
  -> Push and shove moving?
- Arrows between widgets

### Automatic layout
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

- Other factors for loss function:
  * Style: Label: Center, keep lines same width, close to optimal width (of 2x icon?)
  * Style: Note: Justified, Top-aligned, lines exact width of note,

  * Hyphenation https://github.com/gnieh/hyphen
  * Stretch of each line (using TeX-like glue structure with badness) Glue: { size, plus, minus }. Do we need infinity  here or is big numbers enough? Or take highest order infinity that has >0.000001
  * Consider breaking lines from 0.5 stretched to 0.5 shrunk
  * Characters per line (66 optimal, 45 to 75 maxima). This includes spaces. Set glue such that line line is 33em.
  * Aspect ratio of the total text?

# Cassandra

