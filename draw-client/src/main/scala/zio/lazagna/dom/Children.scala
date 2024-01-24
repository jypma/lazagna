package zio.lazagna.dom

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.{Exit, Ref, Scope, ZIO}

import org.scalajs.dom

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
  }

  def <~~(content: Consumeable[ChildOp]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      // TODO: add sentinel
      Ref.make(State()).flatMap { stateRef =>
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
}
