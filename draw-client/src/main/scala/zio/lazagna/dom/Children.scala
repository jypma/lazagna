package zio.lazagna.dom

import org.scalajs.dom
import zio.Scope
import zio.Hub
import zio.ZIO
import zio.Ref
import zio.lazagna.Consumeable
import zio.Exit

object Children {
  /*
   */
  sealed trait ChildOp
  case class Append(elmt: Element[_]) extends ChildOp
  case class InsertOrMove(elmt: Element[_], after: Element[_]) extends ChildOp
  case class Delete(elmt: Element[_]) extends ChildOp

  private[Children] case class State(
    children: Map[dom.Node, (Element[_], Scope.Closeable)] = Map.empty) {

    def append(elmt: Element[_], scope: Scope.Closeable): State = State(
      children = children + (elmt.target -> (elmt, scope))
    )

    /** Also returns the Scope to close */
    def delete(elmt: Element[_]): (Scope.Closeable, State) = {
      children.get(elmt.target) match {
        case None => (Scope.global, this)
        case Some((_, scope)) => (scope, State(
          children = children - elmt.target
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

  def <~~[H](content: Consumeable[H,ChildOp]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      // TODO: add sentinel
      Ref.make(State()).flatMap { stateRef =>
        content(_.mapZIO {
          _ match {
            case Append(elmt) =>
              for {
                scope <- Scope.make
                _ <- stateRef.update(_.append(elmt, scope))
                _ <- scope.extend(elmt.mount(parent))
              } yield ()

            case Delete(elmt) =>
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
        }).consume
      }
    }
  }

  def <--[H](content: Consumeable[H,Seq[Element[_]]]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      content(_.zipWithPrevious.map { case (prev, next) =>
        val ops = prev.map(FastDiff.diff(_, next)).getOrElse(next.zipWithIndex.map(FastDiff.Insert.apply _))
        dom.console.log("Need to " + ops)
        ops.map { _ match {
          case FastDiff.Insert(element, index) =>
            ???
          case FastDiff.Delete(index) =>
            ???
        }}
      }).consume
    }
  }
}
