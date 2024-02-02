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
import zio.Runtime

// FIXME: Only allow proper types for keys (NOT Long)
// FIXME: We need our own range type, since scalajs.dom accepts Long
// TODO: OutOfLineKeyedObjectStore, generic in K and V

trait Database {
  def version: Version

  def objectStoreNames: Seq[String]

  def transaction(
    objectStoreNames: Seq[String],
    mode: dom.IDBTransactionMode = dom.IDBTransactionMode.readonly,
    durability: dom.IDBTransactionDurability = dom.IDBTransactionDurability.default
  ): ZIO[Scope, dom.DOMException, Transaction]

  def doGetRange(objectStore: String, range: Option[IDBKeyRange], direction: IDBCursorDirection): ZStream[Any, dom.ErrorEvent, Any]
  def getRange1(objectStore: String, range: IDBKeyRange, direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent | dom.DOMException, Any] = {doGetRange(objectStore, Some(range), direction)}
  def getAll(objectStore: String, direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent | dom.DOMException, Any] = doGetRange(objectStore, None, direction)
  private[lazagna] def doAdd(objectStore: String, value: js.Any, key: Option[Any]): Request[Any]
  def add(objectStore: String, value: js.Any): Request[Any] = doAdd(objectStore, value, None)
  def add(objectStore: String, value: js.Any, key: String): Request[Unit] = doAdd(objectStore, value, Some(key)).unit
  def add(objectStore: String, value: js.Any, key: Double): Request[Unit] = doAdd(objectStore, value, Some(key)).unit
}

trait Transaction {
  def objectStore(name: String): Effect[ObjectStore]
}

// Cursor isn't directly implemented, since reactive stream elements should really be immutable.

trait ObjectStore {
  /** Returns the key of the added object. */
  private[lazagna] def doAdd(value: js.Any, key: Option[Any]): Request[Any]
  def delete(key: Any): Request[Unit]
  def deleteRange(range: IDBKeyRange): Request[Unit]
  def get(key: Any): Request[Any]
  /** Returns the key of the putted object. */
  private[lazagna] def doPut(value: js.Any, key: Option[Any]): Request[Any]
  private[lazagna] def doGetRange(range: Option[IDBKeyRange], direction: IDBCursorDirection): ZStream[Any, dom.ErrorEvent | dom.DOMException, Any]

  /** Returns the key of the added object. */
  def add(value: js.Any): Request[Any] = doAdd(value, None)
  def add(value: js.Any, key: String): Request[Unit] = doAdd(value, Some(key)).unit
  def add(value: js.Any, key: Double): Request[Unit] = doAdd(value, Some(key)).unit
  /** Returns the key of the putted object. */
  def put(value: js.Any): Request[Any] = doPut(value, None)
  /** Returns the key of the putted object. */
  def put(value: js.Any, key: String): Request[Unit] = doPut(value, Some(key)).unit
  def getRange(range: IDBKeyRange, direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent | dom.DOMException, Any] = doGetRange(Some(range), direction)
  def getAll(direction: IDBCursorDirection = IDBCursorDirection.next): ZStream[Any, dom.ErrorEvent | dom.DOMException, Any] = doGetRange(None, direction)
}

private[indexeddb] case class ObjectStoreImpl(s: dom.IDBObjectStore) extends ObjectStore {
  dom.console.log("Object store ready.")
  def doAdd(value: js.Any, key: Option[Any]) = {
    println("add:")
    dom.console.log(value)
    key.foreach(dom.console.log(_))
    request(key.map(s.add(value, _)).getOrElse(s.add(value)))
  }
  def delete(key: Any) = request(s.delete(key))
  def deleteRange(range: IDBKeyRange) = request(s.delete(range))
  def get(key: Any) = request(s.get(key))
  def doPut(value: js.Any, key: Option[Any]) = {
    println("put")
    request(key.map(s.put(value, _)).getOrElse(s.put(value)))
  }
  def doGetRange(range: Option[IDBKeyRange], direction: IDBCursorDirection) = {
    println("get range " + range)
    ZStream.unwrap( for {
      request <- effect(range.map(s.openCursor(_, direction)).getOrElse(s.openCursor(direction = direction)))
      _ = "range started"
    } yield ZStream.async[Any, dom.ErrorEvent | dom.DOMException, Any] { cb =>
      request.onsuccess = { event =>
        val cursor = event.target.result
        dom.console.log("Found: " + cursor)
        if (cursor != null) {
          cb(ZIO.succeed(Chunk(request.result.value)))
          cursor.continue()
        } else {
          cb(ZIO.fail(None))
        }
      }
      request.onerror = { event =>
        cb(ZIO.fail(Some(event)))
      }
    })
  }
}

private[indexeddb] case class TransactionImpl(t: dom.IDBTransaction) extends Transaction {
  override def objectStore(name: String) = {
    dom.console.log("Object store " + name)
    effect(ObjectStoreImpl(t.objectStore(name)))
  }
}

private[indexeddb] case class DatabaseImpl(db: dom.IDBDatabase) extends Database {
  override def version = db.version.toInt
  override def objectStoreNames = db.objectStoreNames.toSeq

  def doGetRange(objectStore: String, range: Option[IDBKeyRange], direction: IDBCursorDirection) = {
    dom.console.log("doGetRange")
    range.foreach(dom.console.log(_))
    ZStream.asyncInterrupt[Any, dom.ErrorEvent, Any] { cb =>
      var cancelled = false
      var count = 0
      val t = db.transaction(Seq(objectStore).toJSArray)
      t.oncomplete = { _ => dom.console.log("Note: stream transaction completed for range " + range) }
      val request = t.objectStore(objectStore).openCursor(range.getOrElse(js.undefined), direction)
      request.onsuccess = { event =>
        val cursor = event.target.result
        if (!cancelled && cursor != null) {
          count += 1
          cb(ZIO.succeed(Chunk(cursor.value)))
          cursor.continue()
        } else {
          println("end of stream")
          cb(ZIO.fail(None))
        }
      }
      request.onerror = { event =>
        cb(ZIO.fail(Some(event)))
      }
      Left(ZIO.succeed {
        println("Cancelled after " + count)
        cancelled = true
      })
    }
  }

  def doAdd(objectStore: String, value: js.Any, key: Option[Any]) = {
    println("add:")
    dom.console.log(value)
    key.foreach(dom.console.log(_))
    request {
      val t = db.transaction(Seq(objectStore).toJSArray, dom.IDBTransactionMode.readwrite)
      t.oncomplete = { _ => dom.console.log("Note: doAdd transaction completed here.") }
      t.objectStore(objectStore).add(value, key.getOrElse(js.undefined))
    }
  }

  override def transaction(objectStoreNames: Seq[String], mode: dom.IDBTransactionMode, durab: dom.IDBTransactionDurability) = {
    ZIO.acquireReleaseExit {
      effect {
        dom.console.log("Starting transaction")
        val t = TransactionImpl(db.transaction(objectStoreNames.toJSArray, mode, new dom.IDBTransactionOptions {
          override val durability = durab
        }))
        t.t.oncomplete = { _ => dom.console.log("Note: transaction completed here.") }
        t
      }
    } { (transaction, exit) =>
      if (exit.isSuccess) {
        ZIO.succeed {
          try {
            transaction.t.asInstanceOf[js.Dynamic].commit()
          } catch {
            case x:Throwable => dom.console.log(x)
          }
        }
      } else {
        ZIO.succeed {
          try {
            dom.console.log("Aborting transaction due to " + exit)
            transaction.t.abort()
          } catch {
            case x:Throwable => dom.console.log(x)
          }
        }
      }
    }
  }
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
