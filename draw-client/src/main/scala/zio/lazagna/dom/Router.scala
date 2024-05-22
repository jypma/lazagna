package zio.lazagna.dom

import java.net.URLDecoder
import java.util.UUID

import scala.util.Try

import zio.Zippable
import zio.lazagna.Consumeable._
import zio.stream.SubscriptionRef

import org.scalajs.dom

object Router {
  case class Hash(path: List[String] = Nil, params: Map[String, String] = Map.empty)

  object Hash {
    private def parsePath(s: String): List[String] = {
      s.split("/").map(p => URLDecoder.decode(p, "UTF-8")).toList
    }

    private def parseParams(s: String): Map[String, String] = {
      s.split("&").map { kv =>
        val pos = kv.indexOf("=")
        if (pos == -1) {
          (kv, "")
        } else {
          (URLDecoder.decode(kv.take(pos), "UTF-8"),
            URLDecoder.decode(kv.drop(pos + 1), "UTF-8"))
        }
      }.toMap
    }

    def parse(h: String): Hash = {
      var s = h
      if (s.startsWith("#")) s = s.substring(1)
      if (s.contains("?")) {
        val split = s.split("\\?", 2)
        Hash(parsePath(split(0)), parseParams(split(1)))
      } else {
        Hash(parsePath(s))
      }
    }

    def parse(location: dom.Location): Hash = {
      val fromLocation = parse(location.hash)
      val fromSearch = parseParams(location.search match {
        case s if s.startsWith("?") => s.drop(1)
        case s => s
      })

      Hash(path = fromLocation.path, params = fromSearch ++ fromLocation.params)
    }

    /** A Hash instance from the current javascript window location property */
    def fromLocation: Hash = parse(dom.window.location)
  }

  trait Matcher[T] { self =>
    def matches(hash: Hash): Option[T]

    def &&[U](that: Matcher[U])(implicit zippable: Zippable[T,U]) = new Matcher[zippable.Out] {
      def matches(hash: Hash): Option[zippable.Out] = {
        self.matches(hash).flatMap { t =>
          that.matches(hash).map { u =>
            zippable.zip(t, u)
          }
        }
      }
    }

    def asUUID(implicit ev: T <:< String) = new Matcher[UUID] {
      override def matches(hash: Hash): Option[UUID] = self.matches(hash).flatMap { s =>
        Try(UUID.fromString(s)).toOption
      }
    }
  }

  trait PathMatcher[T] extends Matcher[T] { parent =>
    def matchPath(hash: Hash): Option[(T, Hash)]

    def matches(hash: Hash) = matchPath(hash).map(_._1)

    def /(path: String) = new PathMatcher[T] {
      private def selfMatches(h: Hash): Option[Hash] = {
        Option.when(h.path.headOption == Some(path))(h.copy(path = h.path.tail))
      }

      def matchPath(hash: Hash): Option[(T, Hash)] = {
        parent.matchPath(hash).flatMap { (t, h) =>
          selfMatches(h).map((t, _))
        }
      }
    }
  }

  def param(name: String) = new Matcher[String] {
    def matches(hash: Hash): Option[String] = hash.params.get(name)
  }

  val all = new PathMatcher[Unit] {
    def matchPath(hash: Hash): Option[(Unit, Hash)] = {
      Some((), hash)
    }
  }

  def /(path: String): PathMatcher[Unit] = all / path

  def route[T](matcher: Matcher[T])(render: T => Modifier[Any]): PartialFunction[Hash, Modifier[Any]] = {
    matcher.matches.unlift.andThen(render)
  }

  // FIXME: Instead of Modifier without R, allow the routes to have an environment.
  def router(route: PartialFunction[Hash, Modifier[Any]], routes: PartialFunction[Hash, Modifier[Any]]*): Modifier[Unit] = {
    val f = (route +: routes).reduce(_.orElse(_))
    for {
      hash <- SubscriptionRef.make(Hash.fromLocation)
      _ <- Events.onPopState(_.flatMap { event =>
        hash.set(Hash.fromLocation)
      }).run
      res <- Alternative.mountOne(hash)(f)
    } yield res
  }
}
