package equatians.model

import equatians.model.MathFunction.*

/** Значение гена: число или семейство функции. */
enum GeneValue:
  case Num(v: Double)
  case Fam(f: Family)

  /** Короткая запись для таблиц и подписей. */
  def show: String = this match
    case Num(v) => Latex.number(v)
    case Fam(f) => f.title

  /** Числовое значение, если ген числовой. */
  def numeric: Option[Double] = this match
    case Num(v) => Some(v)
    case Fam(_) => None

/** Изменение гена при рождении.
  *
  * @param before значение до мутации; `None` — ген появился (например, новый коэффициент после смены семейства)
  */
final case class GeneChange(before: Option[GeneValue], after: GeneValue):
  def show: String = s"${before.fold("—")(_.show)} → ${after.show}"

/** Запись родословной: кто, когда и от кого родился, с какими генами и что в них мутировало.
  *
  * @param origin    ген → id родителя, от которого он унаследован (пусто у созданных миром)
  * @param mutations ген → как он изменился при рождении
  * @param died      тик смерти, если организм умер
  * @param cause     последняя запись журнала перед смертью
  */
final case class LineageRecord(
    id: Int,
    born: Long,
    parents: Vector[Int],
    generation: Int,
    species: Family,
    genes: Vector[(String, GeneValue)],
    origin: Map[String, Int],
    mutations: Map[String, GeneChange],
    died: Option[Long] = None,
    cause: Option[String] = None
):
  /** Значение гена по имени. */
  def gene(key: String): Option[GeneValue] = genes.collectFirst { case (k, v) if k == key => v }

  /** Корм ли это (функция движения — константа). */
  def forage: Boolean = species == Family.Constant

/** Наследуемая часть организма: всё, что передаётся потомкам и мутирует. */
final case class Genotype(movement: MovementOrgan, attack: MathFunction, maxHealth: Double, litterSize: Int, aggression: Double):
  /** Гены в постоянном порядке: движение, удар, тело. */
  def genes: Vector[(String, GeneValue)] = Genes.from(this)

object Genotype:
  def of(o: Organism): Genotype = Genotype(o.movement, o.attack.g, o.sizeGene, o.litterSize, o.aggression)

/** Имена и значения генов организма. Имена совпадают с теми, что генетика пишет в журнал о мутациях. */
object Genes:
  val MovementFamily = "функция движения"
  val AttackFamily = "функция урона"
  val Period = "период"
  val Span = "span"
  val Speed = "скорость"
  val MaxHealth = "макс. здоровье"
  val Litter = "помёт"
  val Aggression = "агрессивность"

  /** Все гены организма в постоянном порядке: движение, удар, тело. */
  def of(o: Organism): Vector[(String, GeneValue)] = from(Genotype.of(o))

  /** Все гены генотипа в постоянном порядке: движение, удар, тело. */
  def from(g: Genotype): Vector[(String, GeneValue)] =
    Vector(MovementFamily -> GeneValue.Fam(g.movement.f.family)) ++
      coefficients("θ", g.movement.f).map((k, v) => k -> GeneValue.Num(v)) ++
      Vector(
        Period -> GeneValue.Num(g.movement.period),
        Span -> GeneValue.Num(g.movement.span),
        Speed -> GeneValue.Num(g.movement.speed),
        AttackFamily -> GeneValue.Fam(g.attack.family)
      ) ++
      coefficients("D", g.attack).map((k, v) => k -> GeneValue.Num(v)) ++
      Vector(
        MaxHealth -> GeneValue.Num(g.maxHealth),
        Litter -> GeneValue.Num(g.litterSize),
        Aggression -> GeneValue.Num(g.aggression)
      )

  /** Ген семейства функции, к которой относится коэффициент `key` (θ — движение, D — удар). */
  def familyOf(key: String): Option[String] =
    if key.startsWith("θ: ") then Some(MovementFamily)
    else if key.startsWith("D: ") then Some(AttackFamily)
    else None

  /** Функция того же семейства, что `f`, с коэффициентами `vs` в порядке [[coefficients]]. */
  def rebuild(f: MathFunction, vs: Vector[Double]): MathFunction = f match
    case Constant(_)           => Constant(vs(0))
    case Polynomial(_)         => Polynomial(vs)
    case Logarithmic(_, _, _)  => Logarithmic(vs(0), vs(1), vs(2))
    case Sinusoidal(_, _, _, _) => Sinusoidal(vs(0), vs(1), vs(2), vs(3))
    case Exponential(_, _, _)  => Exponential(vs(0), vs(1), vs(2))

  /** Коэффициенты функции как гены; `prefix` — имя функции (θ или D). */
  def coefficients(prefix: String, f: MathFunction): Vector[(String, Double)] = f match
    case Constant(c)              => Vector(s"$prefix: c" -> c)
    case Polynomial(cs)           => cs.zipWithIndex.map((c, i) => s"$prefix: a$i" -> c)
    case Logarithmic(a, b, c)     => Vector(s"$prefix: a" -> a, s"$prefix: b" -> b, s"$prefix: c" -> c)
    case Sinusoidal(a, k, phi, c) => Vector(s"$prefix: a" -> a, s"$prefix: k" -> k, s"$prefix: φ" -> phi, s"$prefix: c" -> c)
    case Exponential(a, b, c)     => Vector(s"$prefix: a" -> a, s"$prefix: b" -> b, s"$prefix: c" -> c)
