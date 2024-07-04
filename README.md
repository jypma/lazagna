# Lazagna

Lazagna is a UI framework for developing asynchronous, event-driven browser user interfaces in the Scala language, using [ScalaJS](https://www.scala-js.org/) and [ZIO](https://zio.dev/).

The framework is called "Lazagna" because of the Z in ZIO, and of course because Lasagna, being made with layers, is a [laminar](https://laminar.dev/).

For a nice example of a full application using Lazagna, see [draw](https://github.com/jypma/draw).

## Documentation

Lazagna does not use a virtual DOM. Instead, its HTML element DSL creates DOM elements directly, and relies on streams and events to perform differential updates directly. This is heavily inspired from the awesome [laminar](https://laminar.dev/) framework. However, where Laminar has its own streaming framework, Lazagna uses ZIO for its streams and concurrency.

### Modifier

The most basic building block in Lazagna is a `Modifier`. This is defined as a trait with a single method:

```scala
package zio.lazagna.dom

trait Modifier {
  def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit]
}
```

A `Modifier` has the following properties:

- It is mounted into the dom tree having a parent DOM tree `Element`. This need not imply that all modifiers create child elements; they might also be affecting the parent in other ways (or not at all).
- It returns a ZIO that only has side effects when mounted (`Unit`) and can't fail (`Nothing`).
- The returned ZIO is allowed to use a `Scope` in its environment. That scope is typically tied to the lifetime of the parent, so that this modifier can clean up resources together with its parent going away.

### Applying (mounting) a modifier to the DOM tree

In your application entry point, you should mount your main modifier to a DOM tree node. This involves just calling its `mount` method directly. Typically, you only have one or few of these invocations.

```scala
import org.scalajs.dom
import zio.ZIOAppDefault
import zio.lazagna.dom.Modifier

object Main extends ZIOAppDefault {
  val main: Modifier = ???

  override def run = {
    for {
      _ <- main.mount(dom.document.querySelector("#app"))
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
    } yield ExitCode.success
  }
}
```

One special thing is that you don't want your main method to exit: that would remove its `Scope` and stop all of your application.

### Elements and attributes

The `Element` class is a  `Modifier` which create elements, and allow arguments to create children. The `Attribute` class is a `Modifier` that sets attributes on its parent element. They combine as follows:

```scala
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Attribute._

val tree = div(
  div(
    `class` := "dialog",
    input(
      `type` := "text"
    )
  )
)
```

See the respective classes for which elements and attributes are currently available.

### Event handlers

In order to respond to events from any DOM `EventTarget`, the `EventsEmitter` class can be used. A ZIO will be invoked whenever an event occurs, in order to execute side effects. Events can also be directly sent to a `Hub` or `Ref`.

`EventsEmitter` is defined as follows (simplified version shown here):
```scala
package zio.scala.dom

trait EventsEmitter[+T] {
  def stream: ZStream[Scope with dom.EventTarget, Nothing, T]

  def apply[U](op: ZIO[Scope, Filtered, T] => ZIO[Scope, Filtered, U]): EventsEmitter[U]

  def -->(target: Hub[T]): Modifier

  implicit def run: Modifier
}
```

An implicit conversion will turn the `EventListener` into a `Modifier` that registers and unregisters the event handler as needed, executing any transformations and side effects chained up using `apply`.

```scala
import zio.lazagna.dom.Events._

val mouseHub: Hub[dom.MouseEvent]

input(
  `type` := "test",
  onClick(_.map(event => println(event))),
  onMouseMove --> hub
)
```

Alternatively, the events can be viewed as a `ZStream` by invoking e.g. `onClick.stream.mapZIO(...)`. An implicit conversion will turn the stream into a `Modifier`. However, the above push model is recommended as it doesn't require a background fiber for each running stream.

### Filtered

You may have noticed the `Filtered` error type above. This is a convenience filter that allows a few stream-like operations (e.g. `filter` or `drain`) on a plain ZIO that is intended for side effects only. It allows a ZIO to decide not to handle an element by emitting `Filtered` as an error (although obviously not a failure). A simplified version is defined like this:

```scala
package zio.lazagna

sealed trait Filtered {}

object Filtered {
  implicit class zioOps[R,E,T](zio: ZIO[R,E,T]) {
    /** Runs the underlying zio, but after that always fails with Filtered */
    def drain: ZIO[R, E | Filtered, Nothing]
    /** Filters the zio with the given predicate, failing with Filtered if it doesn't match */
    def filter(p: T => Boolean): ZIO[R, E | Filtered, T]
    /** Renamed from collect(), since .collect() is defined in ZIO has taking the error value as first argument. */
    def collectF[U](pf: PartialFunction[T,U]): ZIO[R, E | Filtered, U]
  }
}
```

## Usage

### Setting attribute values dynamically

An attribute can take the value from a `Hub`, `SubscriptionRef`, or directly from a `ZStream` with the correct type, using the `<--` method, and the `Consumeable` type alias.

```scala
package zio.lazagna

type Consumeable[T] = ZStream[Scope & Setup, Nothing, T]
```

The `Setup` type is similar to ZIO's built-in `Scope`, but where `Scope` allows one to register cleanup actions, `Setup` registers startup actions (executed after the ZIO itself resolves its `T`). These are typically background fibers that must be started before the rest of the flow continues (e.g. registering a subscription on a `Hub` or `SubscriptionRef`).

Implicit conversions exist from `Hub` and `SubscriptionRef` to `Consumeable`. This way, you can set an attribute that automatically follows a value:

```scala
import zio.lazagna.Consumeable.given

case class Tool(name: String)
def currentTool: Consumeable[Tool]

div(
  `class` <-- currentTool.map(t => s"main tool-${t.name}"),
)
```

Under the hood, a stream is created that updates the attribute on every value. The stream is forked into a background fiber that's tied to the `Scope` of the modifier. That way, the stream automatically stops with that scope.

### Setting element children dynamically

Various methods exist to dynamically vary the children of a parent element.

#### Changing all children at once

For instances where you want to replace all children whenever a value changes, use `Alternative.mountOne`. It completely replaces the modifier whenever `T` changes (by closing its scope).
```scala
package zio.lazagna.dom

object Alternative {
  def mountOne[T](source: Consumeable[T])(render: T => Modifier): Modifier
}
```

#### Switching between multiple mounted children

If you want to keep several alternatives mounted in the DOM tree, but only show one, use `Alternative.showOne`. This version will create all possible children up front, and use CSS to only show one at a time.
```scala
def showOne[T](source: Consumeable[T], alternatives: Map[T, Element[_]], initial: Option[T] = None): Modifier
```

#### Safely adding a child explicitly

If you want to add a child to an element explicitly at some point in time, use the `Children` class. This allows you to designate a place where these children are rendered, and then later on "inject" children there. The injected children are still tied to a `Scope`, so they will automatically disappear when that scope goes away.
```scala
package zio.lazagna.dom

trait Children {
  /** Renders the children into their actual location. This must be invoked before .child() has any effect. */
  def render: Modifier

  def child[E <: dom.Element](creator: UIO[Unit] => Element[E]): ZIO[Scope, Nothing, Unit]
}

object Children {
  def make: UIO[Children]
}
```

You use this as follows. First, make sure you create a `Children` instance using `Children.make` in central spot. Then, render that instance into your DOM tree:
```scala
div(
  childrenInstance.render
)
```

Now, you can add children at will from any other place in your code where you have a scope available. For example, an event handler:
```scala
div(
  cls := "child-client"
  onClick(_.flatMap(_ => children.child { close =>
    div(
      cls := "dialog"
      div(
        cls := "button"
        onClick(_.flatMap(_ => close))
      )
    )
  }))
)
```

The `child` function's `creator` argument receives a `UIO[Unit]` (called `close` in our example), which can be invoked to destroy the created child. The child will also automatically be destroyed when its `Scope` goes away (in our example, that's the `div` with `child-client` as CSS class).

#### Manually managing children

As a final, lowest-level approach, you can manually manage the children of an `Element` using the `children <~~ ` operator:
```scala
val operations: Consumeable[Children.ChildOp]

div(
  children <~~ operations
)
```

A selection of  `ChildOp` subclasses exist to add and remove children at specific spots, and/or rearrange them.

## Batteries included

Various DOM abstractions are included, so applications can communicate in ZIO-style.

### HTTP requests

Making an AJAX `XMLHttpRequest` request can be done through the `zio.lazagna.dom.http.Request` abstraction. For example, sending a POST request and parsing the response as JSON:

```scala
for {
  loginResp <- POST(AsDynamicJSON, s"${config.baseUrl}/users/${user}/login?password=${password}")
  token = loginResp.token.asInstanceOf[String]
} yield ???
```

Various return types exist for `Document` (XML), `Blob`, `ArrayBuffer` and `String` (see `AsXXXX` inside `Request.scala`), and you can write your own by extending `ResponseHandler[T]`.

### Web sockets

You can make websocket requests through a ZIO abstraction using the `zio.lazagna.dom.http.WebSocket` abstraction. The main entry point is defined as follows:

```scala
package zio.lazagna.dom.http.WebSocket

trait WebSocket {
  def send(text: String): IO[WebSocketError, Unit]
  def send(bytes: Array[Byte]): IO[WebSocketError, Unit]
}

object WebSocket {
  def handle(url: String)(onMessage: dom.MessageEvent => ZIO[Any, Nothing, Any], onClose: => ZIO[Any, Nothing, Any] = ZIO.unit): ZIO[Scope, Nothing, WebSocket]
}
```

This allows both sending and receiving.

### IndexedDB

There are many ways to store data client-side, but the modern variant with least size restrictions is IndexedDB. Lazagna's abstraction on this is as follows.

```scala
package zio.lazagna.dom.indexeddb

trait Database {
  def version: Version
  def objectStoreNames: Seq[String]
  def objectStore[T, TV <: js.Any, K](name: String)(using keyCodec: KeyCodec[K], valueCodec: ValueCodec[T,TV]): ObjectStore[T,K]
}

trait ObjectStore[T, K] {
  def getRange(range: Range[K], direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent, T]
  def getAll(direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent, T]

  def add(value: T, key: K): Request[Unit]
  def clear: Request[Unit]
  def delete(key: K): Request[Unit]
}

object IndexedDB {
  def open(name: String, schema: Schema): ZIO[Scope, Blocked | dom.ErrorEvent, Database]
}
```

For an example on how to interact with indexed DB, see `IndexedDBEventStore`.

### Web Locks

Since an IndexedDB instance is shared between browser tabs, it can be necessary to collaborate between tabs to decide who can write to the database. The Web Locks API can help with this, for which Lazagna provides an abstraction.

```scala
package zio.lazagna.dom.weblocks

trait Lock {
  def withExclusiveLock[R,E,A](zio: =>ZIO[R,E,A]): ZIO[R,E,A]
  def withExclusiveLockIfAvailable[R,E,A](zio: =>ZIO[R,E,A]): ZIO[R, E | LockUnavailable.type, A]
}

object Lock {
  def make(name: String): UIO[Lock]
}

```

# Full example application

An interactive whiteboarding application uses Lazagna. Read more about it [here](https://github.com/jypma/draw).

# TODO

- Clean up the use of implicit and given, and align on having a nice one-line import for library users
- Write unit tests to anchor functionality (once ZIO abstractions are confirmed, which they are not)

# Discussion points for ZIO itself

- The initial `EventsEmitter` class just created a `ZStream` for the events. This required a `Fiber` for every event handler. They turned out to be fast to create, but relatively slow to stop (about 1 second to stop 1000 fibers, on a fast desktop machine). If you have 1000 small icons to select from, that's too slow. Hence, a push-based model was introduced.
- ZIO could perhaps do with a `Filtered` class of its own, instead of or in addition to the generic `filter` error variants.
- ZStream could perhaps add a push-based stream variant, which maintains stream operation in a scope, executing a ZIO for every element. We'd have to define more precise semantics though (return type of the ZIO would have to be `Chunk[U]`, and we need a way to early close the stream to yield a value, since `EventEmitter` doesn't need that).
- Should `Setup` be part of `Scope`? Or is it not necessary here at all?

