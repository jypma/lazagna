package draw.server.drawing

import java.util.UUID

import zio.stream.ZStream
import zio.{IO, ZIO}

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent

import Drawings.DrawingError

trait Drawing {
  def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit]
  def events: ZStream[Any, DrawingError, DrawEvent]
  def eventsAfter(sequenceNr: Long): ZStream[Any, DrawingError, DrawEvent]
  def version: ZIO[Any, DrawingError, Long]
}

trait Drawings {
  def getDrawing(id: UUID): IO[DrawingError, Drawing]
}

object Drawings {
  case class DrawingError(message: String)

}
