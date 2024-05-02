package draw.server.drawing

import java.util.UUID

import zio.stream.ZStream
import zio.{IO, ZIO}
import zio.schema.{DeriveSchema, Schema}

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent

import Drawings.DrawingError
import draw.data.DrawingState

trait Drawing {
  def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit]
  def events: ZStream[Any, DrawingError, DrawEvent]
  def eventsAfter(sequenceNr: Long): ZStream[Any, DrawingError, DrawEvent]
  def version: ZIO[Any, DrawingError, Long]
  def getState: ZIO[Any, Nothing, DrawingState]
}

trait Drawings {
  def getDrawing(id: UUID): IO[DrawingError, Drawing]
  def list: ZStream[Any, DrawingError, UUID]
}

object Drawings {
  case class DrawingError(message: String)
  object DrawingError {
   implicit val schema: Schema[DrawingError] = DeriveSchema.gen[DrawingError]
  }

}
