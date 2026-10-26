package equatians.stats

import equatians.model.*

/** Числовой признак организма, который имеет смысл усреднять по популяции. */
enum Trait(val title: String, val value: (Organism, Rules) => Double):
  case Speed         extends Trait("скорость", (o, _) => o.movement.speed)
  case MaxHealth     extends Trait("макс. здоровье", (o, _) => o.maxHealth)
  case Litter        extends Trait("ген помёта", (o, _) => o.litterSize)
  case Generation    extends Trait("поколение", (o, _) => o.generation)
  case Period        extends Trait("период движения", (o, _) => o.movement.period)
  case Span          extends Trait("span движения", (o, _) => o.movement.span)
  case Satiety       extends Trait("сытость, %", (o, r) => 100 * o.satietyFraction(r))
  case Reserve       extends Trait("жир, %", (o, r) => 100 * o.reserveFraction(r))
  case Health        extends Trait("здоровье, %", (o, _) => 100 * o.health / o.maxHealth)
  case AttackRange   extends Trait("дальность удара", (o, _) => o.attack.range)
  case AttackCost    extends Trait("стоимость удара", (o, _) => o.attack.cost)
  case ContactDamage extends Trait("урон в упор", (o, r) => o.strikeDamage(0, r))

/** Сумма признаков по группе организмов — суммы, а не средние, чтобы группы можно было складывать.
  *
  * @param count сколько организмов в группе
  */
final case class Aggregate(count: Int, sums: Map[Trait, Double]):
  /** Среднее значение признака или `None` для пустой группы. */
  def mean(t: Trait): Option[Double] = Option.when(count > 0)(sums.getOrElse(t, 0.0) / count)

  /** Объединение двух групп. */
  def +(o: Aggregate): Aggregate =
    Aggregate(count + o.count, Trait.values.iterator.map(t => t -> (sums.getOrElse(t, 0.0) + o.sums.getOrElse(t, 0.0))).toMap)

object Aggregate:
  val Empty: Aggregate = Aggregate(0, Map.empty)

  /** Суммы всех признаков по организмам `os`. */
  def of(os: Iterable[Organism], rules: Rules): Aggregate =
    Aggregate(os.size, Trait.values.iterator.map(t => t -> os.iterator.map(t.value(_, rules)).sum).toMap)

/** Перепись одного сектора (или всего мира).
  *
  * @param species численность по видам, включая корм (семейство «константная»)
  * @param active  признаки активных организмов
  * @param forage  признаки корма
  * @param attackFamilies численность активных организмов по семейству функции урона
  * @param corpses сколько трупов лежит
  */
final case class SectorCensus(
    species: Map[Family, Int],
    active: Aggregate,
    forage: Aggregate,
    attackFamilies: Map[Family, Int],
    corpses: Int
):
  def +(o: SectorCensus): SectorCensus = SectorCensus(
    merge(species, o.species),
    active + o.active,
    forage + o.forage,
    merge(attackFamilies, o.attackFamilies),
    corpses + o.corpses
  )

  private def merge(a: Map[Family, Int], b: Map[Family, Int]) =
    (a.keySet ++ b.keySet).iterator.map(f => f -> (a.getOrElse(f, 0) + b.getOrElse(f, 0))).toMap

object SectorCensus:
  val Empty: SectorCensus = SectorCensus(Map.empty, Aggregate.Empty, Aggregate.Empty, Map.empty, 0)

/** Подсчёт текущего населения. */
object Census:

  /** Перепись каждого сектора мира; пустые сектора тоже присутствуют. */
  def of(world: World): Map[SectorId, SectorCensus] =
    val bySector = world.organisms.groupBy(_.sector)
    val corpses = world.corpses.groupMapReduce(_.sector)(_ => 1)(_ + _)
    world.config.sectors.iterator.map { s =>
      val os = bySector.getOrElse(s, Vector.empty)
      val (forage, active) = os.partition(_.forage)
      s -> SectorCensus(
        species = os.groupMapReduce(_.species)(_ => 1)(_ + _),
        active = Aggregate.of(active, world.rules),
        forage = Aggregate.of(forage, world.rules),
        attackFamilies = active.groupMapReduce(_.attack.g.family)(_ => 1)(_ + _),
        corpses = corpses.getOrElse(s, 0)
      )
    }.toMap
