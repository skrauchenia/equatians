package equatians.sim

import equatians.model.*
import equatians.model.MathFunction.*

import scala.util.Random

/** Начальное заселение мира случайными организмами. */
object Genesis:

  /** Создаёт мир с `count` организмами в случайных секторах и точках.
    * Результат детерминирован: одинаковые параметры и `seed` дают одинаковый мир.
    */
  def randomWorld(world: WorldConfig, rules: Rules, genesis: GenesisConfig, count: Int, seed: Long): World =
    val rnd = Random(seed)
    val sectors = world.sectors.toVector
    val organisms = Vector.tabulate(count) { id =>
      val health = genesis.health.sample(rnd)
      val reserveShare = genesis.reserve.sample(rnd)
      val o = Organism(
        id = id,
        movement = randomMovement(genesis.movement, rnd),
        attack = randomAttack(genesis.attack, rules.attack, rnd),
        sector = sectors(rnd.nextInt(sectors.size)),
        pos = Vec2(rnd.nextDouble() * world.sectorSize, rnd.nextDouble() * world.sectorSize),
        health = health,
        maxHealth = health,
        satiety = rules.energy.maxSatiety,
        reserve = reserveShare * rules.energy.reservePerHealth * health,
        litterSize = genesis.litterSize.sample(rnd).round.toInt.max(1).min(rules.reproduction.maxLitter)
      )
      o.logged(0, Narrative.born(o, rules), rules.logSize)
    }
    val plants = for
      sector <- world.sectors.toVector
      _ <- 0 until rules.plants.initialPerSector
    yield (sector, Vec2(rnd.nextDouble() * world.sectorSize, rnd.nextDouble() * world.sectorSize))
    World(
      world, rules, genesis, organisms, seed,
      plants = plants.zipWithIndex.map { case ((s, p), i) => Plant(i, s, p, rules.plants.food) },
      nextPlantId = plants.size,
      nextId = count
    )

  /** Случайный орган движения: семейство из `genesis.families`, гены из настроенных диапазонов. */
  def randomMovement(genesis: MovementGenesis, rnd: Random): MovementOrgan =
    MovementOrgan(
      f = randomFunction(pick(genesis.families, rnd), genesis.functions, rnd),
      period = genesis.period.sample(rnd).round.toInt.max(1),
      span = genesis.span.sample(rnd),
      speed = genesis.speed.sample(rnd)
    )

  /** Случайный орган атаки. Функции с D(0) ≤ 0 (не бьют даже в упор) перегенерируются. */
  def randomAttack(genesis: AttackGenesis, rules: AttackRules, rnd: Random): AttackOrgan =
    val family = pick(genesis.families, rnd)
    Iterator
      .continually(randomFunction(family, genesis.functions, rnd))
      .take(1000)
      .find(g => AttackOrgan.positive(g(0)) > 0)
      .map(AttackOrgan(_, rules.maxReach, rules.costFactor))
      .getOrElse(throw IllegalArgumentException(
        s"genesis.attack.functions.${family.toString.toLowerCase}: диапазоны почти не дают D(0) > 0"
      ))

  /** Случайная функция заданного семейства с коэффициентами из `ranges`. */
  def randomFunction(family: Family, ranges: FunctionRanges, rnd: Random): MathFunction =
    def polynomial(rs: Vector[GeneRange]) = Polynomial(rs.map(_.sample(rnd)))
    family match
      case Family.Linear      => polynomial(ranges.linear)
      case Family.Quadratic   => polynomial(ranges.quadratic)
      case Family.Cubic       => polynomial(ranges.cubic)
      case Family.Logarithmic =>
        val r = ranges.logarithmic
        Logarithmic(r.a.sample(rnd), r.b.sample(rnd), r.c.sample(rnd))
      case Family.Sinusoidal =>
        val r = ranges.sinusoidal
        Sinusoidal(r.a.sample(rnd), r.k.sample(rnd), r.phi.sample(rnd), r.c.sample(rnd))
      case Family.Exponential =>
        val r = ranges.exponential
        Exponential(r.a.sample(rnd), r.b.sample(rnd), r.c.sample(rnd))

  private def pick[A](xs: Vector[A], rnd: Random): A = xs(rnd.nextInt(xs.size))
