package equatians.sim

import equatians.model.*
import equatians.model.MathFunction.*

import scala.util.Random

/** Наследование: скрещивание генов двух родителей и мутации.
  *
  * Гены организма: функция движения (её коэффициенты), период, span и скорость органа движения,
  * функция урона, максимальное здоровье и размер помёта.
  *
  *  - Скрещивание: каждый числовой ген (и каждый коэффициент функции) потомок берёт от случайного родителя.
  *    Если у родителей функции разных семейств (бывает у функции урона), функция берётся целиком от одного.
  *  - Точечная мутация: с вероятностью `mutation.rate` к гену прибавляется нормальный шум
  *    с σ = relative-sigma · |ген| + absolute-sigma.
  *  - Макромутация: с вероятностью `mutation.family-rate` функция меняет семейство. Для функции движения
  *    это значит, что потомок — новый вид. Полином меняет степень с сохранением общих коэффициентов,
  *    остальные переходы генерируют функцию нового семейства из диапазонов генезиса.
  */
object Genetics:

  /** Потомок и человекочитаемый список его мутаций. */
  final case class Offspring(organism: Organism, mutations: Vector[String], newSpecies: Boolean)

  /** Создаёт потомка двух родителей в точке `pos` сектора родителя `a`.
    * Рождается с полным здоровьем, сытостью `offspring-satiety` и без жира.
    */
  def offspring(id: Int, a: Organism, b: Organism, pos: Vec2, now: Long, world: World, rnd: Random): Offspring =
    val rules = world.rules
    val mut = Mutator(rnd, rules.mutation)
    def pick[A](x: A, y: A): A = if rnd.nextBoolean() then x else y

    val movementFn = mut.family(
      "функция движения",
      mut.function("θ", crossFunction(a.movement.f, b.movement.f, rnd)),
      world.genesis.movement.families,
      world.genesis.movement.functions
    )
    val attackFn =
      val crossed = crossFunction(a.attack.g, b.attack.g, rnd)
      val mutated = mut.family("функция урона", mut.function("D", crossed), world.genesis.attack.families, world.genesis.attack.functions)
      // Мутация, после которой организм не бьёт даже в упор, нежизнеспособна — оставляем скрещенную функцию.
      if AttackOrgan.positive(mutated(0)) > 0 then mutated else crossed

    val movement = MovementOrgan(
      f = movementFn,
      period = mut.gene("период", pick(a.movement.period, b.movement.period).toDouble).round.toInt.max(1),
      span = mut.gene("span", pick(a.movement.span, b.movement.span)).max(0.1),
      speed = mut.gene("скорость", pick(a.movement.speed, b.movement.speed)).max(0.05)
    )
    val maxHealth = mut.gene("макс. здоровье", pick(a.maxHealth, b.maxHealth)).max(5)
    val litter = mut.step("помёт", pick(a.litterSize, b.litterSize), 1, rules.reproduction.maxLitter)

    val child = Organism(
      id = id,
      movement = movement,
      attack = AttackOrgan(attackFn, rules.attack.maxReach, rules.attack.costFactor),
      sector = a.sector,
      pos = pos,
      health = maxHealth,
      maxHealth = maxHealth,
      satiety = rules.reproduction.offspringSatiety * rules.energy.maxSatiety,
      reserve = 0,
      litterSize = litter,
      generation = math.max(a.generation, b.generation) + 1,
      parents = Some((a.id, b.id)),
      lastReproductionTick = now
    )
    Offspring(child, mut.notes, newSpecies = child.species != a.species)

  /** Скрещивание функций: одного семейства — покоэффициентно, разных — целиком от случайного родителя. */
  def crossFunction(f1: MathFunction, f2: MathFunction, rnd: Random): MathFunction =
    def pick(x: Double, y: Double) = if rnd.nextBoolean() then x else y
    (f1, f2) match
      case (Polynomial(c1), Polynomial(c2)) if c1.size == c2.size =>
        Polynomial(c1.indices.map(i => pick(c1(i), c2(i))).toVector)
      case (Logarithmic(a1, b1, c1), Logarithmic(a2, b2, c2)) =>
        Logarithmic(pick(a1, a2), pick(b1, b2), pick(c1, c2))
      case (Sinusoidal(a1, k1, p1, c1), Sinusoidal(a2, k2, p2, c2)) =>
        Sinusoidal(pick(a1, a2), pick(k1, k2), pick(p1, p2), pick(c1, c2))
      case (Exponential(a1, b1, c1), Exponential(a2, b2, c2)) =>
        Exponential(pick(a1, a2), pick(b1, b2), pick(c1, c2))
      case _ => if rnd.nextBoolean() then f1 else f2

  /** Применяет мутации и копит их описания для журнала. */
  private final class Mutator(rnd: Random, m: MutationRules):
    private val log = Vector.newBuilder[String]

    /** Описания всех случившихся мутаций. */
    def notes: Vector[String] = log.result()

    /** Точечная мутация числового гена. */
    def gene(name: String, v: Double): Double =
      if rnd.nextDouble() >= m.rate then v
      else
        val mutated = v + rnd.nextGaussian() * (m.relativeSigma * math.abs(v) + m.absoluteSigma)
        log += s"$name ${Latex.number(v)} → ${Latex.number(mutated)}"
        mutated

    /** Мутация целого гена на ±1 в пределах [lo, hi]. */
    def step(name: String, v: Int, lo: Int, hi: Int): Int =
      if rnd.nextDouble() >= m.rate then v
      else
        val mutated = (v + (if rnd.nextBoolean() then 1 else -1)).max(lo).min(hi)
        if mutated != v then log += s"$name $v → $mutated"
        mutated

    /** Точечные мутации каждого коэффициента функции; `prefix` — имя функции в журнале. */
    def function(prefix: String, f: MathFunction): MathFunction = f match
      case Polynomial(cs) => Polynomial(cs.zipWithIndex.map((c, i) => gene(s"$prefix: a$i", c)))
      case Logarithmic(a, b, c) =>
        Logarithmic(gene(s"$prefix: a", a), gene(s"$prefix: b", b).max(0), gene(s"$prefix: c", c))
      case Sinusoidal(a, k, phi, c) =>
        Sinusoidal(gene(s"$prefix: a", a), gene(s"$prefix: k", k), gene(s"$prefix: φ", phi), gene(s"$prefix: c", c))
      case Exponential(a, b, c) =>
        Exponential(gene(s"$prefix: a", a), gene(s"$prefix: b", b), gene(s"$prefix: c", c))

    /** Макромутация: смена семейства на случайное другое из `families`. */
    def family(name: String, f: MathFunction, families: Vector[Family], ranges: FunctionRanges): MathFunction =
      val others = families.filter(_ != f.family)
      if others.isEmpty || rnd.nextDouble() >= m.familyRate then f
      else
        val target = others(rnd.nextInt(others.size))
        val mutated = (f, polynomialDegree(target)) match
          case (Polynomial(cs), Some(degree)) =>
            Polynomial(cs.take(degree + 1).padTo(degree + 1, 0.0).zipWithIndex.map { (c, i) =>
              if i < cs.size then c else rnd.nextGaussian() * m.absoluteSigma * 2
            })
          case _ => Genesis.randomFunction(target, ranges, rnd)
        log += s"$name: ${f.family.title} → ${target.title}"
        mutated

  private def polynomialDegree(f: Family): Option[Int] = f match
    case Family.Linear    => Some(1)
    case Family.Quadratic => Some(2)
    case Family.Cubic     => Some(3)
    case _                => None
