package zio.lazagna.dom.indexeddb

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import org.scalajs.dom
import zio.ZIO
import zio.IO
import IndexedDB._
import zio.Unsafe
import org.scalajs.dom.IDBCreateObjectStoreOptions
import org.scalajs.dom.IDBCreateIndexOptions
import zio.Scope
import org.scalajs.dom.IDBRequest
import org.scalajs.dom.IDBKeyRange
import org.scalajs.dom.IDBCursorDirection
import zio.stream.ZStream
import zio.Chunk

// TODO: No more schema definition, but Database can have an optional version Int. Otherwise, it keeps track of how many ObjectStores use it (that's the version).
// When an ObjectStore starts, and the db doesn't have that ObjectStore, it causes the db to close itself (if opened), then migrate to create that ObjectStore (incrementing
// version), and then reopen.
// DatabaseImpl will have a Ref[dom.IDBDatabase].

trait Database {
  def version: Version
  def objectStoreNames: Seq[String]
  def objectStore[T, TV <: js.Any, K](name: String)(using keyCodec: KeyCodec[K], valueCodec: ValueCodec[T,TV]): ObjectStore[T,K]
}

trait KeyCodec[T] {
  def encode(t: T): js.Any
}
object KeyCodec {
  def from[T](enc: T => js.Any) = new KeyCodec[T] { override def encode(t: T) = enc(t) }

  private def cast[T] = from[T](_.asInstanceOf[js.Any])

  given string: KeyCodec[String] = cast
  given int: KeyCodec[Int] = cast
  given float: KeyCodec[Float] = cast
  given double: KeyCodec[Double] = cast
  given boolean: KeyCodec[Boolean] = cast
  given long: KeyCodec[Long] = from(_.toDouble)

  given iterable[T, S <: Iterable[T]](using codec: KeyCodec[T]): KeyCodec[S] =
    from(_.map(codec.encode).toJSArray)
  given tuple2[T1,T2](using c1: KeyCodec[T1], c2: KeyCodec[T2]): KeyCodec[(T1,T2)] =
    from(t => js.Array(c1.encode(t._1), c2.encode(t._2)))
}

trait ValueCodec[T, U <: js.Any] {
  type JsType = U
  def encode(t: T): U
  def decode(u: U): T
}

object ValueCodec {
  def from[T, U <: js.Any](enc: T => U, dec: U => T) = new ValueCodec[T,U] {
    override def encode(t: T) = enc(t)
    override def decode(u: U) = dec(u)
  }

  private def cast[T, U <: js.Any] = from[T, U](_.asInstanceOf[U], _.asInstanceOf[T])

  given string: ValueCodec[String, js.Any] = cast
  given int: ValueCodec[Int, js.Any] = cast
  given float: ValueCodec[Float, js.Any] = cast
  given double: ValueCodec[Double, js.Any] = cast
  given boolean: ValueCodec[Boolean, js.Any] = cast

  given vector[T,U <: js.Any](using codec: ValueCodec[T,U]): ValueCodec[Vector[T], js.Array[U]] =
    from(_.map(codec.encode).toJSArray, _.view.map(codec.decode).toVector)
}

/** A range of keys, used to grab a sequence of objects from a database */
case class Range[K](private[indexeddb] val idbRange: IDBKeyRange)
object Range {
  /** The bounds can be open (that is, the bounds exclude the endpoint values) or closed (that is, the bounds
    * include the endpoint values). By default, the bounds are closed.
    */
  def bound[K,KV <: js.Any](lower: K, upper: K, lowerOpen: Boolean = false, upperOpen: Boolean = false)(using codec: KeyCodec[K]): Range[K] = {
    Range(IDBKeyRange.bound(codec.encode(lower), codec.encode(upper), lowerOpen, upperOpen))
  }
}

/** A type-safe object store (currently only supporting out-of-line keys) */
trait ObjectStore[T, K] {
  private[indexeddb] def doGetRange(range: Option[Range[K]], direction: IDBCursorDirection): ZStream[Any, dom.ErrorEvent, T]
  def getRange(range: Range[K], direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent, T] = doGetRange(Some(range), direction)
  def getAll(direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent, T] = doGetRange(None, direction)

  def add(value: T, key: K): Request[Unit]
  def clear: Request[Unit]
  def delete(key: K): Request[Unit]
}

private[indexeddb] case class ObjectStoreImpl[T, TV <: js.Any, K](db: dom.IDBDatabase, objectStoreName: String)(using keyCodec: KeyCodec[K], valueCodec: ValueCodec[T,TV]) extends ObjectStore[T, K] {
  def doGetRange(range: Option[Range[K]], direction: IDBCursorDirection): ZStream[Any, dom.ErrorEvent, T] = {
    ZStream.asyncInterrupt[Any, dom.ErrorEvent, T] { cb =>
      var cancelled = false
      var count = 0
      val t = db.transaction(Seq(objectStoreName).toJSArray)
      val request = t.objectStore(objectStoreName).openCursor(range.map(_.idbRange).getOrElse(js.undefined), direction)
      request.onsuccess = { event =>
        val cursor = event.target.result
        if (!cancelled && cursor != null) {
          count += 1
          cb(ZIO.succeed(Chunk(valueCodec.decode(cursor.value.asInstanceOf[TV]))))
          cursor.continue()
        } else {
          cb(ZIO.fail(None))
        }
      }
      request.onerror = { event =>
        cb(ZIO.fail(Some(event)))
      }
      Left(ZIO.succeed {
        cancelled = true
      })
    }
  }

  def add(value: T, key: K): Request[Unit] = request {
    val t = db.transaction(Seq(objectStoreName).toJSArray, dom.IDBTransactionMode.readwrite)
    t.objectStore(objectStoreName).add(valueCodec.encode(value), keyCodec.encode(key))
  }.unit

  def clear: Request[Unit] = request {
    val t = db.transaction(Seq(objectStoreName).toJSArray, dom.IDBTransactionMode.readwrite)
    t.objectStore(objectStoreName).clear()
  }

  def delete(key: K) = request {
    val t = db.transaction(Seq(objectStoreName).toJSArray, dom.IDBTransactionMode.readwrite)
    t.objectStore(objectStoreName).delete(keyCodec.encode(key))
  }
}

private[indexeddb] case class DatabaseImpl(db: dom.IDBDatabase) extends Database {
  override def version = db.version.toInt
  override def objectStoreNames = db.objectStoreNames.toSeq
  override def objectStore[T, TV <: js.Any, K](name: String)(using keyCodec: KeyCodec[K], valueCodec: ValueCodec[T,TV]) = ObjectStoreImpl(db, name)
}

/** Schema to use for a database. On released subsequent application versions, operations must ONLY be added to this
  case class, so that indexeddb upgrades can be performed predictably. */
case class Schema(headOperation: Schema.Operation, tailOperations: Schema.Operation*) {
  val operations = headOperation +: tailOperations
  def version = operations.length
}
object Schema {
  sealed trait Operation
  case class CreateObjectStore(name: String, keyPath: Seq[String] = Seq.empty, autoIncrement: Boolean = false) extends Operation
  case class DeleteObjectStore(name: String) extends Operation
  case class CreateIndex(objectStoreName: String, name: String, keyPath: Seq[String], unique: Boolean = false, multiEntry: Boolean = false) extends Operation
  case class DeleteIndex(objectStoreName: String, name: String) extends Operation
}

object IndexedDB {

  type Effect[T] = IO[dom.DOMException, T]
  private[indexeddb] def effect[T](f: => T): Effect[T] = ZIO.attempt(f).catchAll {
    case js.JavaScriptException(x: dom.DOMException) =>
      ZIO.fail(x)
    case other =>
      ZIO.die(other)
  }

  type Request[T] = IO[dom.ErrorEvent | dom.DOMException, T]
  private[indexeddb] def request[S,A](f: => dom.IDBRequest[S,A]): Request[A] = {
    for {
      request <- effect(f)
      res <- ZIO.async[Any, dom.ErrorEvent, A] { cb =>
        request.onsuccess = { event =>
          cb(ZIO.succeed(request.result))
        }
        request.onerror = { event =>
          cb(ZIO.fail(event))
        }
      }
    } yield res
  }

  /** Version is currently modeled as an Int, because:
    - Some parts of the docs say it should be an integer number
    - Other parts refer to it as a String
    - Other parts refer to it as a "64-bit integer", which doesn't exist in JS
    - ScalaJS dom models it as a Double */
  type Version = Int

  case class Blocked(oldVersion: Version, newVersion: Version)

  def open(name: String, schema: Schema): ZIO[Scope, Blocked | dom.ErrorEvent, Database] = {
    ZIO.acquireRelease {
      ZIO.async[Any, Blocked | dom.ErrorEvent, DatabaseImpl] { cb =>
        val request = dom.window.indexedDB.getOrElse(throw new IllegalStateException("No indexeddb available!")).open(name, schema.version)

        request.onupgradeneeded = { event =>
          // TODO test what happens if our schema triggers a JS exception here. Probably should fail the ZIO? Should we close the DB as well?
          var stores = Map.empty[String, dom.IDBObjectStore]
          val db = request.result
          schema.operations.drop(event.oldVersion.toInt).foreach {
            case op:Schema.CreateObjectStore =>
              val opts =  new IDBCreateObjectStoreOptions {
                override val keyPath = if (op.keyPath.isEmpty) js.undefined else op.keyPath.toJSArray
                override val autoIncrement = op.autoIncrement
              }
              val store = db.createObjectStore(op.name, opts)
              stores = stores + (op.name -> store)
            case op:Schema.DeleteObjectStore =>
              db.deleteObjectStore(op.name)
              stores -= op.name
            case op:Schema.CreateIndex =>
              val store = stores.getOrElse(op.objectStoreName, throw new IllegalArgumentException(s"Store doesn't exist for ${op}"))
              store.createIndex(op.name, new IDBCreateIndexOptions {
                override val unique = op.unique
                override val multiEntry = op.multiEntry
              })
            case op:Schema.DeleteIndex =>
              val store = stores.getOrElse(op.objectStoreName, throw new IllegalArgumentException(s"Store doesn't exist for ${op}"))
              store.deleteIndex(op.name)
          }
        }

        request.onblocked = { event =>
          cb(ZIO.fail(Blocked(event.oldVersion.toInt, event.newVersion.toInt)))
        }

        request.onsuccess = { _ =>
          cb(ZIO.succeed(DatabaseImpl(request.result)))
        }

        request.onerror = { event =>
          dom.console.log(event)
          cb(ZIO.fail(event))
        }
      }
    } { database =>
      ZIO.succeed(database.db.close())
    }
  }
}
