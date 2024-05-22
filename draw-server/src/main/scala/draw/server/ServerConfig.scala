package draw.server

import java.io.File
import java.nio.file.Paths
import java.time.Duration
import java.time.temporal.ChronoUnit

import zio.config._
import zio.config.magnolia._
import zio.config.yaml._
import zio.{ConfigProvider, ZIO, ZLayer}

case class CassandraConfig(
  hostname: String = "127.0.0.1",
  port: Int = 9042,
  keyspace: String = "draw"
)

case class GithubConfig(
  clientId: String,
  secret: String,
  ttl: Duration = Duration.of(7, ChronoUnit.DAYS)
)

case class ServerConfig(cassandra: CassandraConfig, github: GithubConfig)

object ServerConfig {
  val config = deriveConfig[ServerConfig]

  val live: ZLayer[Any, Throwable, ServerConfig] = ZLayer.fromZIO {
    val filename = Option(System.getProperty("config.yaml")).orElse(Some("config.yaml")).filter(s => new File(s).exists)
    for {
      provider <- filename.map { f =>
        ConfigProvider.fromYamlPathZIO(Paths.get(f))
      }.getOrElse {
        ZIO.succeed(ConfigProvider.defaultProvider)
      }
      res <- provider.load(config)
    } yield res
  }
}
