package draw.server.drawing

import java.util.UUID

import zio.schema.{DeriveSchema, Schema}
import zio.stream.ZStream
import zio.{IO, ZIO}

import draw.data.DrawingState
import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent

import Drawings.DrawingError

trait Drawing {
  def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit]
  def events: ZStream[Any, DrawingError, DrawEvent]
  def eventsAfter(sequenceNr: Long): ZStream[Any, DrawingError, DrawEvent]
  def version: ZIO[Any, DrawingError, Long]
  def getState: ZIO[Any, Nothing, DrawingState]
}

trait Drawings {
  def getDrawing(id: UUID): IO[DrawingError, Drawing]
  def makeDrawing: IO[DrawingError, UUID]
  def list: ZStream[Any, DrawingError, Drawings.DrawingRef]
}

object Drawings {
  case class DrawingError(message: String)
  object DrawingError {
   implicit val schema: Schema[DrawingError] = DeriveSchema.gen[DrawingError]
  }

  case class DrawingRef(id: UUID, name: String)
  object DrawingRef {
   implicit val schema: Schema[DrawingRef] = DeriveSchema.gen[DrawingRef]
  }

}
