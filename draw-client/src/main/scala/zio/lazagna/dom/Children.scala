package zio.lazagna.dom

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.Consumeable.given
import zio.{Exit, Hub, Ref, Scope, UIO, ZIO}

import org.scalajs.dom
import zio.Queue

/** Tracks a set of children of differing owner scopes, and renders them into a single parent. Children can be
  * rendered into here while being owned from other places. */
trait Children {
  /** Renders the children into their actual location. This must be invoked before .child() has any effect. */
  def render: Modifier

  /** Returns a ZIO that adds a child, inserting it where [render] was invoked. The child is created and added
    * using the creator function (which receives a UIO[Unit] parameter that can be used to destroy the
    * child). The child is tied to the Scope of the returned ZIO (which is typically used as a Modifier using
    * Modifier.run). When that Scope goes away, the child is destroyed.
    */
  def child[E <: dom.Element](creator: UIO[Unit] => Element[E]): ZIO[Scope, Nothing, Unit]

  /** Runs the given child as a Modifier, adding it when that modifier is mounted. @see child() */
  def addChild[E <: dom.Element](creator: UIO[Unit] => Element[E]): Modifier = Modifier.run(child(creator))
}

/** Allows Element children to be directly added and removed by stream operations. If you're looking
  * for a way to just directly set the children (or any other Modifier), without regards for re-use, look
  * at Alternative.mountOne */
object Children {
  /** An explicit diff operation that can be sent to a children-accepting modifier */
  sealed trait ChildOp
  /** The given element will be appended as a child. It must not already exist. */
  case class Append(elmt: Element[_]) extends ChildOp
  /** The given element is added, or moved, to the position after the given element. */
  case class InsertOrMove(elmt: Element[_], after: Element[_]) extends ChildOp
  /** The given element is deleted. It must have been previously appended or inserted. */
  case class Delete(elmt: Element[_]) extends ChildOp
  /** The given DOM element is deleted. It must be the target of a previously appended or inserted Element. */
  case class DeleteDOM(elmt: dom.Element) extends ChildOp

  private[Children] case class State(
    children: Map[dom.Node, (Element[_], Scope.Closeable)] = Map.empty) {

    def append(elmt: Element[_], scope: Scope.Closeable): State = State(
      children = children + (elmt.target -> (elmt, scope))
    )

    /** Also returns the Scope to close */
    def delete(elmt: dom.Element): (Scope.Closeable, State) = {
      children.get(elmt) match {
        case None => (Scope.global, this)
        case Some((_, scope)) => (scope, State(
          children = children - elmt
        ))
      }
    }

    /** Returns the scope to use (will be an existing scope if the element was already present) */
    def insertOrMove(elmt: Element[_], after: Element[_], newScope: Scope.Closeable): (Scope.Closeable, State) = {
      children.get(elmt.target) match {
        case None =>
          // doesn't exist, just add it to the lookup map
          (newScope, State(
            children = children + (elmt.target -> (elmt, newScope))
          )
          )
        case Some((_, existingScope)) =>
          // it does exist, so reuse it in the lookup map
          (existingScope, this)
      }
    }

    def closeAll = ZIO.collectAll(children.values.map(_._2.close(Exit.unit)))
  }

  /** Directly affects the children of this parent by the given stream of child operations */
  // TEST: Close current scope when unmounted
  def <~~(content: Consumeable[ChildOp]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.acquireRelease(Ref.make(State()))(_.get.flatMap(_.closeAll)).flatMap { stateRef =>
        content.mapZIO {
          _ match {
            case Append(elmt) =>
              for {
                scope <- Scope.make
                _ <- stateRef.update {_.append(elmt, scope) }
                _ <- scope.extend(elmt.mount(parent))
              } yield ()

            case Delete(elmt) =>
              for {
                scope <- stateRef.modify(_.delete(elmt.target))
                _ <- scope.close(Exit.unit)
              } yield ()

            case DeleteDOM(elmt) =>
              for {
                scope <- stateRef.modify(_.delete(elmt))
                _ <- scope.close(Exit.unit)
              } yield ()

            case InsertOrMove(elmt, after) =>
              for {
                newScope <- Scope.make
                scope <- stateRef.modify(_.insertOrMove(elmt, after, newScope))
                isNew = scope eq newScope
                _ <- scope.extend {
                  if (isNew)
                    elmt.mount(parent, Some(after.target))
                  else
                    elmt.moveAfter(parent, after.target)
                }
              } yield ()
          }
        }.consume
      }
    }
  }

  /** Makes a receiver for child elements, which can be rendered into a different owner scope than the owner
    * scope of the actual children. */
  def make: UIO[Children] = for {
    queue <- Queue.bounded[ChildOp](100)
  } yield new Children {
    def render: Modifier = Children <~~ queue

    def child[E <: dom.Element](creator: UIO[Unit] => Element[E]): ZIO[Scope, Nothing, Unit] = {
      var element: Element[E] = null

      val destroy = ZIO.unless(element == null)(
        queue.offer(Children.Delete(element)) *>
          ZIO.succeed{element = null}
      ).unit

      val create = ZIO.unless(element != null)(queue.offer {
        element = creator(destroy)
        Children.Append(element)
      }).unit

      ZIO.acquireRelease(create)(_ => destroy).unit
    }
  }
}
