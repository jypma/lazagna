package zio.lazagna.dom

import org.scalajs.dom
import zio.Scope
import zio.Hub
import zio.ZIO
import zio.Ref
import zio.lazagna.ZIOOps._
import zio.Exit

object Children {
  /*
  def <--(content: Hub[Seq[MyElement]]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      // TODO add sentinel
      ZStream.fromHubScoped(content).flatMap { in =>
        dom.console.log("Started stream")
        in.zipWithPrevious.mapZIO { case (prev, next) =>
          val ops = prev.map(FastDiff.diff(_, next)).getOrElse(next.zipWithIndex.map(FastDiff.Insert.apply _))
          dom.console.log("Need to " + ops)
          ZIO.collectAll(ops.map { _ match {
            case FastDiff.Insert(element, index) =>
              ZIO.unit
            case FastDiff.Delete(index) =>
              ZIO.unit
          }})
        }.runDrain.forkScoped
       }.unit
    }
  }
   */
  sealed trait ChildOp
  case class Append(elmt: Element) extends ChildOp
  case class InsertOrMove(elmt: Element, after: Element) extends ChildOp
  case class Delete(elmt: Element) extends ChildOp

  private[Children] case class State(
    children: Map[dom.Node, (Element, Scope.Closeable)] = Map.empty) {

    def append(elmt: Element, scope: Scope.Closeable): State = State(
      children = children + (elmt.target -> (elmt, scope))
    )

    /** Also returns the Scope to close */
    def delete(elmt: Element): (Scope.Closeable, State) = {
      children.get(elmt.target) match {
        case None => (Scope.global, this)
        case Some((_, scope)) => (scope, State(
          children = children - elmt.target
        ))
      }
    }

    // TODO: Optimize: Don't allocate a new scope if we're just moving (by adding State.hasElement(MyElement))
    def insertOrMove(elmt: Element, after: Element, newScope: Scope.Closeable): (Scope.Closeable, State) = {
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
  def <--(content: Hub[ChildOp]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      // TODO: add sentinel
      Ref.make(State()).flatMap { stateRef =>
        consume(content) {
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
        }
      }
    }
  }
}
