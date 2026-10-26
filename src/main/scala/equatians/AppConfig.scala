package equatians

import com.typesafe.config.{Config, ConfigFactory}
import equatians.model.*
import equatians.sim.*

import scala.jdk.CollectionConverters.*

/** Параметры начальной популяции. */
final case class PopulationConfig(count: Int, seed: Long)

/** Параметры статистики секторов: интервал переписи, предел числа снимков, частота перерисовки графиков. */
final case class StatsConfig(sampleInterval: Int, maxSamples: Int, refreshMs: Int):
  require(sampleInterval > 0 && maxSamples >= 4, "ui.stats: sample-interval > 0, max-samples ≥ 4")

/** Параметры окна визуализации. */
final case class UiConfig(speedSteps: Vector[Int], frameBudgetMs: Int, trailLength: Int, stats: StatsConfig):
  require(speedSteps.nonEmpty, "ui.speed-steps не должен быть пустым")

/** Все глобальные параметры эксперимента, прочитанные из `application.conf`. */
final case class AppConfig(
    world: WorldConfig,
    population: PopulationConfig,
    rules: Rules,
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
        sectorSize = c.getDouble("world.sector-size"),
        passage = c.getDouble("world.passage")
      ),
      population = PopulationConfig(
        count = c.getInt("population.count"),
        seed = c.getLong("population.seed")
      ),
      rules = rules(c.getConfig("rules")),
      genesis = genesis(c.getConfig("genesis")),
      ui = UiConfig(
        speedSteps = c.getIntList("ui.speed-steps").asScala.map(_.toInt).toVector,
        frameBudgetMs = c.getInt("ui.frame-budget-ms"),
        trailLength = c.getInt("ui.trail-length"),
        stats = StatsConfig(
          sampleInterval = c.getInt("ui.stats.sample-interval"),
          maxSamples = c.getInt("ui.stats.max-samples"),
          refreshMs = c.getInt("ui.stats.refresh-ms")
        )
      )
    )

  private def rules(c: Config): Rules =
    Rules(
      perceptionRadius = c.getDouble("perception-radius"),
      logSize = c.getInt("log-size"),
      graveyardSize = c.getInt("graveyard-size"),
      sizePerSqrtHealth = c.getDouble("body.size-per-sqrt-health"),
      meatPerHealth = c.getDouble("body.meat-per-health"),
      energy = EnergyRules(
        maxSatiety = c.getDouble("energy.max-satiety"),
        reservePerHealth = c.getDouble("energy.reserve-per-health"),
        reserveDensity = c.getDouble("energy.reserve-density"),
        baseMetabolism = c.getDouble("energy.base-metabolism"),
        movementCost = c.getDouble("energy.movement-cost"),
        starvationDamage = c.getDouble("energy.starvation-damage")
      ),
      healing = HealingRules(
        rate = c.getDouble("healing.rate"),
        cost = c.getDouble("healing.cost")
      ),
      forage = ForageRules(
        reproductionRate = c.getDouble("forage.reproduction-rate"),
        awakeningRate = c.getDouble("forage.awakening-rate"),
        spawnRate = c.getDouble("forage.spawn-rate"),
        concentration = c.getDouble("forage.concentration"),
        initialConcentration = c.getDouble("forage.initial-concentration"),
        speed = c.getDouble("forage.speed"),
        health = c.getDouble("forage.health"),
        reserve = c.getDouble("forage.reserve"),
        satietyThreshold = c.getDouble("forage.satiety-threshold"),
        probability = c.getDouble("forage.probability")
      ),
      scavenging = ScavengeRules(
        probability = c.getDouble("scavenging.probability"),
        satietyThreshold = c.getDouble("scavenging.satiety-threshold"),
        eatReach = c.getDouble("scavenging.eat-reach")
      ),
      reproduction = ReproductionRules(
        maturityAge = c.getInt("reproduction.maturity-age"),
        cooldown = c.getInt("reproduction.cooldown"),
        satietyThreshold = c.getDouble("reproduction.satiety-threshold"),
        reserveThreshold = c.getDouble("reproduction.reserve-threshold"),
        healthThreshold = c.getDouble("reproduction.health-threshold"),
        probability = c.getDouble("reproduction.probability"),
        reach = c.getDouble("reproduction.reach"),
        bodyCostPerHealth = c.getDouble("reproduction.body-cost-per-health"),
        offspringSatiety = c.getDouble("reproduction.offspring-satiety"),
        maxLitter = c.getInt("reproduction.max-litter")
      ),
      mutation = MutationRules(
        rate = c.getDouble("mutation.rate"),
        relativeSigma = c.getDouble("mutation.relative-sigma"),
        absoluteSigma = c.getDouble("mutation.absolute-sigma"),
        familyRate = c.getDouble("mutation.family-rate")
      ),
      behaviour = BehaviourRules(
        decisionInterval = c.getInt("behaviour.decision-interval"),
        hungerThreshold = c.getDouble("behaviour.hunger-threshold"),
        maxChaseProbability = c.getDouble("behaviour.max-chase-probability"),
        fleeSatietyThreshold = c.getDouble("behaviour.flee-satiety-threshold"),
        fleeReserveThreshold = c.getDouble("behaviour.flee-reserve-threshold"),
        fleeProbability = c.getDouble("behaviour.flee-probability"),
        loseTargetFactor = c.getDouble("behaviour.lose-target-factor")
      ),
      attack = AttackRules(
        maxReach = c.getDouble("attack.max-reach"),
        costFactor = c.getDouble("attack.cost-factor"),
        cooldown = c.getInt("attack.cooldown"),
        referenceHealth = c.getDouble("attack.reference-health")
      )
    )

  private def genesis(c: Config): GenesisConfig =
    GenesisConfig(
      health = range(c, "health"),
      reserve = range(c, "reserve"),
      litterSize = range(c, "litter-size"),
      movement = MovementGenesis(
        families = families(c, "movement.families"),
        period = range(c, "movement.period"),
        span = range(c, "movement.span"),
        speed = range(c, "movement.speed"),
        functions = functions(c.getConfig("movement.functions"))
      ),
      attack = AttackGenesis(
        families = families(c, "attack.families"),
        functions = functions(c.getConfig("attack.functions"))
      )
    )

  private def functions(f: Config): FunctionRanges =
    def polynomial(name: String, degree: Int): Vector[GeneRange] =
      (0 to degree).map(i => range(f, s"$name.a$i")).toVector
    FunctionRanges(
      constant = range(f, "constant.c"),
      linear = polynomial("linear", 1),
      quadratic = polynomial("quadratic", 2),
      cubic = polynomial("cubic", 3),
      logarithmic = FunctionRanges.Logarithmic(
        range(f, "logarithmic.a"), range(f, "logarithmic.b"), range(f, "logarithmic.c")),
      sinusoidal = FunctionRanges.Sinusoidal(
        range(f, "sinusoidal.a"), range(f, "sinusoidal.k"), range(f, "sinusoidal.phi"), range(f, "sinusoidal.c")),
      exponential = FunctionRanges.Exponential(
        range(f, "exponential.a"), range(f, "exponential.b"), range(f, "exponential.c"))
    )

  private def range(cfg: Config, path: String): GeneRange =
    cfg.getDoubleList(path).asScala.map(_.toDouble).toList match
      case List(lo, hi) => GeneRange(lo, hi)
      case other        => throw IllegalArgumentException(s"$path: ожидается [min, max], получено $other")

  private def families(cfg: Config, path: String): Vector[Family] =
    cfg.getStringList(path).asScala.map(family).toVector

  private def family(name: String): Family =
    Family.values
      .find(_.toString.equalsIgnoreCase(name))
      .getOrElse(throw IllegalArgumentException(
        s"неизвестное семейство '$name', доступны: ${Family.values.map(_.toString.toLowerCase).mkString(", ")}"
      ))
