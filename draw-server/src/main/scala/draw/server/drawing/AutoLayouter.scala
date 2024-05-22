package draw.server.drawing

import zio.{Ref, Scope, Semaphore, ZIO}

import draw.data.AutoLayout
import draw.data.drawcommand.{DrawCommand, LayoutObjects}
import draw.data.drawevent.LinkEdited
import draw.server.drawing.Drawings.DrawingError

trait AutoLayouter {
  def start: ZIO[Scope, Nothing, Unit]
}

object AutoLayouter {
  sealed trait State
  /** No layout task is running */
  case object Idle extends State
  /** We are running layout, and are done after. */
  case object Running extends State
  /** We are running layout, and need to re-do the layout after. */
  case object RunningWithQueue extends State

  def make(drawing: Drawing): ZIO[Scope, DrawingError, Unit] = (for {
    scope <- ZIO.scope
    semaphore <- Semaphore.make(1)
    state <- Ref.make[State](Idle)
    initialSeqNr <- drawing.version
    autoLayout <- AutoLayout.make
  } yield new AutoLayouter {
    val performLayout: ZIO[Scope, Nothing, Unit] = for {
      drawState <- drawing.getState
      _ = println("Starting autolayout.")
      toMove <- autoLayout.performLayout(drawState)
      _ = println("Autolayout complete: " + toMove)
      _ <- drawing.perform(DrawCommand(LayoutObjects(toMove))).when(!toMove.isEmpty).catchAll {
        case err: DrawingError =>
          println("Can't perform autolayout: " + err)
          ZIO.unit
      }
      _ <- semaphore.withPermit {
        state.get.flatMap {
          case RunningWithQueue => state.set(Running) *> performLayout.forkScoped
          case _ => state.set(Idle)
        }
      }
    } yield ()

    val startOrQueueLayout = semaphore.withPermit {
      state.get.flatMap {
        case Idle => state.set(Running) *> performLayout.forkScoped
        case Running => state.set(RunningWithQueue)
        case RunningWithQueue => ZIO.unit
      }
    }

    override def start = drawing.eventsAfter(initialSeqNr).map(_.body).collect {
      case l:LinkEdited => l
    }.mapZIO { _ =>
      startOrQueueLayout
    }.runDrain.forkScoped.unit
  }).flatMap(_.start)
}
