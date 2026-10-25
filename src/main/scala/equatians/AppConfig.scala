package equatians

import com.typesafe.config.{Config, ConfigFactory}
import equatians.model.{Family, WorldConfig}
import equatians.sim.*

import scala.jdk.CollectionConverters.*

/** Параметры начальной популяции. */
final case class PopulationConfig(count: Int, seed: Long)

/** Параметры окна визуализации. */
final case class UiConfig(speedSteps: Vector[Int], frameBudgetMs: Int, trailLength: Int):
  require(speedSteps.nonEmpty, "ui.speed-steps не должен быть пустым")

/** Все глобальные параметры эксперимента, прочитанные из `application.conf`. */
final case class AppConfig(
    world: WorldConfig,
    population: PopulationConfig,
    genesis: GenesisConfig,
    ui: UiConfig
)

object AppConfig:

  /** Загружает конфиг стандартным способом Typesafe Config:
    * `application.conf` из ресурсов, переопределяемый через `-Dconfig.file=...` и `-Dключ=значение`.
    */
  def load(): AppConfig = fromConfig(ConfigFactory.load().getConfig("equatians"))

  /** Разбирает секцию `equatians { ... }`. Бросает исключение с путём к ключу, если чего-то нет. */
  def fromConfig(c: Config): AppConfig =
    AppConfig(
      world = WorldConfig(
        cols = c.getInt("world.cols"),
        rows = c.getInt("world.rows"),
        sectorSize = c.getDouble("world.sector-size")
      ),
      population = PopulationConfig(
        count = c.getInt("population.count"),
        seed = c.getLong("population.seed")
      ),
      genesis = genesis(c.getConfig("genesis")),
      ui = UiConfig(
        speedSteps = c.getIntList("ui.speed-steps").asScala.map(_.toInt).toVector,
        frameBudgetMs = c.getInt("ui.frame-budget-ms"),
        trailLength = c.getInt("ui.trail-length")
      )
    )

  private def genesis(c: Config): GenesisConfig =
    def range(cfg: Config, path: String): GeneRange =
      cfg.getDoubleList(path).asScala.map(_.toDouble).toList match
        case List(lo, hi) => GeneRange(lo, hi)
        case other        => throw IllegalArgumentException(s"$path: ожидается [min, max], получено $other")

    def polynomial(cfg: Config, degree: Int): Vector[GeneRange] =
      (1 to degree).map(i => range(cfg, s"a$i")).toVector

    val f = c.getConfig("functions")
    GenesisConfig(
      families = c.getStringList("families").asScala.map(family).toVector,
      movement = MovementRanges(
        period = range(c, "movement.period"),
        span = range(c, "movement.span"),
        speed = range(c, "movement.speed")
      ),
      functions = FunctionRanges(
        linear = polynomial(f.getConfig("linear"), 1),
        quadratic = polynomial(f.getConfig("quadratic"), 2),
        cubic = polynomial(f.getConfig("cubic"), 3),
        logarithmic = FunctionRanges.Logarithmic(range(f, "logarithmic.a"), range(f, "logarithmic.b")),
        sinusoidal = FunctionRanges.Sinusoidal(range(f, "sinusoidal.a"), range(f, "sinusoidal.k")),
        exponential = FunctionRanges.Exponential(range(f, "exponential.a"), range(f, "exponential.b"))
      )
    )

  private def family(name: String): Family =
    Family.values
      .find(_.toString.equalsIgnoreCase(name))
      .getOrElse(throw IllegalArgumentException(
        s"неизвестное семейство '$name', доступны: ${Family.values.map(_.toString.toLowerCase).mkString(", ")}"
      ))
