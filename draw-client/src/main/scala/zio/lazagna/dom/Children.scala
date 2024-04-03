package zio.lazagna.dom

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.{Exit, Queue, Ref, Scope, UIO, ZIO, ZLayer}

import org.scalajs.dom

import Modifier.MountPoint

type Element[+E <: dom.Element] = Modifier[E]

/** Tracks a set of children of differing owner scopes, and renders them into a single parent. Children can be
  * rendered into here while being owned from other places. */
trait Children {
  /** Renders the children into their actual location. This must be invoked before .child() has any effect. */
  def render: Modifier[Unit]

  /** Returns a ZIO that adds a child, inserting it where [render] was invoked. The child is created and added
    * using the creator function (which receives a UIO[Unit] parameter that can be used to destroy the
    * child). The child is tied to the Scope of the returned ZIO (which is typically used as a Modifier using
    * Modifier.run). When that Scope goes away, the child is destroyed.
    */
  def child[E <: dom.Element](creator: UIO[Unit] => Element[E]): ZIO[Scope, Nothing, Unit]
}

/** Allows Element children to be directly added and removed by stream operations. If you're looking
  * for a way to just directly set the children (or any other Modifier), without regards for re-use, look
  * at Alternative.mountOne */
object Children {
  /** An explicit diff operation that can be sent to a children-accepting modifier */
  sealed trait ChildOp
  /** The given element will be appended as a child. It must not already exist. */
  case class Append[E <: dom.Element](elmt: Element[E]) extends ChildOp
  /** The given element is added, or moved, to the position after the given element. */
  case class InsertOrMove[E1 <: dom.Element, E2 <: dom.Element](elmt: Element[E1], after: Element[E2]) extends ChildOp
  /** The given element is deleted. It must have been previously appended or inserted. */
  case class Delete(elmt: Element[_]) extends ChildOp

  private[Children] case class State(
    children: Map[Element[_], (dom.Element, Scope.Closeable)] = Map.empty) {

    def append(elmt: Element[_], target: dom.Element, scope: Scope.Closeable): State = State(
      children = children + (elmt  -> (target, scope))
    )

    /** Also returns the Scope to close */
    def delete(elmt: Element[_]): (Scope.Closeable, State) = {
      children.get(elmt) match {
        case None =>
          println("Warning: couldn't find child element to delete")
          dom.console.log(elmt)
          (Scope.global, this)
        case Some((_, scope)) => (scope, State(
          children = children - elmt
        ))
      }
    }

    def targetFor(elmt: Element[_]): Option[dom.Element] = children.get(elmt).map(_._1)

    def closeAll = ZIO.collectAll(children.values.map(_._2.close(Exit.unit)))
  }

  /** Directly affects the children of this parent by the given stream of child operations */
  // TEST: Close current scope when unmounted
  def <~~(content: Consumeable[ChildOp]) = Modifier { parent =>
    ZIO.acquireRelease(Ref.make(State()))(_.get.flatMap(_.closeAll)).flatMap { stateRef =>
      def append[E <: dom.Element](elmt: Element[E], after: Option[dom.Element] = None) = for {
        scope <- Scope.make
        target <- elmt.provide(ZLayer.succeed(scope), ZLayer.succeed(MountPoint(parent, after)))
        _ <- stateRef.update {_.append(elmt, target, scope) }
      } yield ()

      content.mapZIO {
        _ match {
          case Append(elmt) => append(elmt)

          case Delete(elmt) =>
            for {
              scope <- stateRef.modify(_.delete(elmt))
              _ <- scope.close(Exit.unit)
            } yield ()

          case InsertOrMove(elmt, after) =>
            for {
              afterTarget <- stateRef.get.map(_.targetFor(after))
              _ <- if (afterTarget.isEmpty) append(elmt) else for {
                elmtTarget <- stateRef.get.map(_.targetFor(elmt))
                _ <- elmtTarget.map { target =>
                  ZIO.succeed {
                    // We're moving an existing element
                    parent.insertBefore(target, afterTarget.get)
                  }
                }.getOrElse {
                  append(elmt, Some(afterTarget.get))
                }
              } yield ()
            } yield ()
        }
      }.consume
    }
  }

  /** Makes a receiver for child elements, which can be rendered into a different owner scope than the owner
    * scope of the actual children. */
  def make: UIO[Children] = for {
    queue <- Queue.bounded[ChildOp](100)
  } yield new Children {
    def render = Children <~~ queue

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
