package equatians.sim

import equatians.model.*
import equatians.model.MathFunction.*

import scala.util.Random

/** Начальное заселение мира случайными организмами. */
object Genesis:

  /** Создаёт мир с `count` организмами в случайных секторах и точках.
    * Результат детерминирован: одинаковые параметры и `seed` дают одинаковый мир.
    */
  def randomWorld(world: WorldConfig, genesis: GenesisConfig, count: Int, seed: Long): World =
    val rnd = Random(seed)
    val sectors = world.sectors.toVector
    val organisms = Vector.tabulate(count) { id =>
      Organism(
        id = id,
        movement = randomMovement(genesis, rnd),
        sector = sectors(rnd.nextInt(sectors.size)),
        pos = Vec2(rnd.nextDouble() * world.sectorSize, rnd.nextDouble() * world.sectorSize)
      )
    }
    World(world, organisms)

  /** Случайный орган движения: семейство из `genesis.families`, гены из настроенных диапазонов. */
  def randomMovement(genesis: GenesisConfig, rnd: Random): MovementOrgan =
    val m = genesis.movement
    MovementOrgan(
      f = randomFunction(genesis.families(rnd.nextInt(genesis.families.size)), genesis.functions, rnd),
      period = m.period.sample(rnd).round.toInt.max(1),
      span = m.span.sample(rnd),
      speed = m.speed.sample(rnd)
    )

  /** Случайная функция заданного семейства. Свободный член всегда из [0, 2π) — это начальный курс. */
  def randomFunction(family: Family, ranges: FunctionRanges, rnd: Random): MathFunction =
    def angle = rnd.nextDouble() * 2 * math.Pi
    def polynomial(rs: Vector[GeneRange]) = Polynomial(angle +: rs.map(_.sample(rnd)))
    family match
      case Family.Linear      => polynomial(ranges.linear)
      case Family.Quadratic   => polynomial(ranges.quadratic)
      case Family.Cubic       => polynomial(ranges.cubic)
      case Family.Logarithmic =>
        Logarithmic(ranges.logarithmic.a.sample(rnd), ranges.logarithmic.b.sample(rnd), angle)
      case Family.Sinusoidal =>
        Sinusoidal(ranges.sinusoidal.a.sample(rnd), ranges.sinusoidal.k.sample(rnd), angle, angle)
      case Family.Exponential =>
        Exponential(ranges.exponential.a.sample(rnd), ranges.exponential.b.sample(rnd), angle)
