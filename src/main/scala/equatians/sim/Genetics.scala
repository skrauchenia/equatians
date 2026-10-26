package equatians.sim

import equatians.model.*
import equatians.model.MathFunction.*

import scala.util.Random

/** Наследование: скрещивание генов двух родителей, почкование корма и мутации.
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
  *    Константа становится полиномом с тем же свободным членом. Остальные переходы генерируют функцию
  *    нового семейства из диапазонов генезиса.
  *  - Почкование корма: клон получает все гены одного родителя с теми же точечными мутациями, а смена семейства
  *    функции движения происходит с вероятностью `forage.awakening-rate` — тогда клон «пробуждается»
  *    в активный организм.
  */
object Genetics:

  /** Потомок и всё о его наследовании.
    *
    * @param mutations  описания мутаций для журнала
    * @param newSpecies другого ли он вида, чем (первый) родитель
    * @param origin     ген → id родителя, от которого ген унаследован
    * @param changes    ген → как он изменился относительно унаследованного значения
    */
  final case class Offspring(
      organism: Organism,
      mutations: Vector[String],
      newSpecies: Boolean,
      origin: Map[String, Int],
      changes: Map[String, GeneChange]
  )

  /** Создаёт потомка двух родителей в точке `pos` сектора родителя `a`.
    * Рождается с полным здоровьем, сытостью `offspring-satiety` и без жира.
    */
  def offspring(id: Int, a: Organism, b: Organism, pos: Vec2, now: Long, world: World, rnd: Random): Offspring =
    val rules = world.rules
    val inh = Inheritor(rnd, a.id, b.id)
    val crossed = Genotype(
      movement = MovementOrgan(
        f = inh.function("θ", Genes.MovementFamily, a.movement.f, b.movement.f),
        period = inh.pick(Genes.Period, a.movement.period, b.movement.period),
        span = inh.pick(Genes.Span, a.movement.span, b.movement.span),
        speed = inh.pick(Genes.Speed, a.movement.speed, b.movement.speed)
      ),
      attack = inh.function("D", Genes.AttackFamily, a.attack.g, b.attack.g),
      maxHealth = inh.pick(Genes.MaxHealth, a.sizeGene, b.sizeGene),
      litterSize = inh.pick(Genes.Litter, a.litterSize, b.litterSize),
      aggression = inh.pick(Genes.Aggression, a.aggression, b.aggression)
    )
    val mut = Mutator(rnd, rules.mutation)
    val g = mutate(crossed, mut, world, rules.mutation.familyRate, minHealth = 5)
    val child = body(id, g, a.sector, pos, now, world.rules).copy(
      satiety = rules.reproduction.offspringSatiety * rules.energy.maxSatiety,
      reserve = 0,
      generation = math.max(a.generation, b.generation) + 1,
      parents = Vector(a.id, b.id)
    )
    Offspring(child, mut.notes, child.species != a.species, completeOrigin(inh.origin, g), diff(crossed, g))

  /** Отпочковывает клона корма `p` в точке `pos`: гены родителя с мутациями, без скрещивания.
    *
    * Если функция движения клона сменила семейство (вероятность `forage.awakening-rate`), он больше не корм:
    * рождается активным, с сытостью `offspring-satiety`. Иначе — корм без сытости. Жир у обоих —
    * `forage.reserve`, но не больше ёмкости.
    */
  def bud(id: Int, p: Organism, pos: Vec2, now: Long, world: World, rnd: Random): Offspring =
    val rules = world.rules
    val inherited = Genotype.of(p)
    val mut = Mutator(rnd, rules.mutation)
    val g = mutate(inherited, mut, world, rules.forage.awakeningRate, minHealth = 5)
    val awakened = g.movement.f.family != Family.Constant
    val grown = body(id, g, p.sector, pos, now, rules)
    val child = grown.copy(
      satiety = if awakened then rules.reproduction.offspringSatiety * rules.energy.maxSatiety else 0,
      reserve = math.min(rules.forage.reserve, grown.maxReserve(rules)),
      generation = p.generation + 1,
      parents = Vector(p.id)
    )
    Offspring(child, mut.notes, awakened, g.genes.map((k, _) => k -> p.id).toMap, diff(inherited, g))

  /** Мутации всех генов генотипа; `familyRate` — вероятность смены семейства функции движения. */
  private def mutate(g: Genotype, mut: Mutator, world: World, familyRate: Double, minHealth: Double): Genotype =
    val rules = world.rules
    Genotype(
      movement = MovementOrgan(
        f = mutateMovement(mut, g.movement.f, world, familyRate),
        period = mut.gene(Genes.Period, g.movement.period.toDouble).round.toInt.max(1),
        span = mut.gene(Genes.Span, g.movement.span).max(0.1),
        speed = mut.gene(Genes.Speed, g.movement.speed).max(0.05)
      ),
      attack = mutateAttack(mut, g.attack, world),
      maxHealth = mut.gene(Genes.MaxHealth, g.maxHealth).max(minHealth),
      litterSize = mut.step(Genes.Litter, g.litterSize, 1, rules.reproduction.maxLitter),
      aggression = mut.gene(Genes.Aggression, g.aggression).max(0).min(1)
    )

  /** Организм с генами `g`, полным здоровьем, без сытости и жира. У корма тело — `forage.health`. */
  private def body(id: Int, g: Genotype, sector: SectorId, pos: Vec2, now: Long, rules: Rules): Organism =
    // Корм всегда маленький, а его ген здоровья спит до пробуждения.
    val forage = g.movement.f.family == Family.Constant
    val size = if forage then rules.forage.health else g.maxHealth
    Organism(
      id = id,
      movement = g.movement,
      attack = AttackOrgan(g.attack, rules.attack.maxReach, rules.attack.costFactor),
      sector = sector,
      pos = pos,
      health = size,
      maxHealth = size,
      satiety = 0,
      reserve = 0,
      litterSize = g.litterSize,
      lastReproductionTick = now,
      healthGene = Option.when(forage)(g.maxHealth),
      aggression = g.aggression
    )

  /** Чем итоговые гены отличаются от унаследованных; появившиеся гены — с `before = None`. */
  def diff(inherited: Genotype, result: Genotype): Map[String, GeneChange] =
    val before = inherited.genes.toMap
    result.genes.collect { case (k, v) if !before.get(k).contains(v) => k -> GeneChange(before.get(k), v) }.toMap

  /** Гены, появившиеся при смене семейства, наследуются от того же родителя, что и семейство функции. */
  private def completeOrigin(origin: Map[String, Int], g: Genotype): Map[String, Int] =
    g.genes.flatMap { (k, _) =>
      origin.get(k).orElse(Genes.familyOf(k).flatMap(origin.get)).map(k -> _)
    }.toMap

  /** Точечные мутации функции движения и смена её семейства с вероятностью `familyRate`. */
  private def mutateMovement(mut: Mutator, f: MathFunction, world: World, familyRate: Double): MathFunction =
    val g = world.genesis.movement
    mut.family(Genes.MovementFamily, mut.function("θ", f), g.families, g.functions, familyRate)

  /** Мутации функции урона. Мутация, после которой организм не бьёт даже в упор, нежизнеспособна —
    * тогда остаётся исходная функция.
    */
  private def mutateAttack(mut: Mutator, g: MathFunction, world: World): MathFunction =
    val a = world.genesis.attack
    val mutated = mut.family(Genes.AttackFamily, mut.function("D", g), a.families, a.functions, world.rules.mutation.familyRate)
    if AttackOrgan.positive(mutated(0)) > 0 then mutated else g

  /** Скрещивание с учётом происхождения: каждый ген берётся от случайного родителя, и это запоминается. */
  private final class Inheritor(rnd: Random, a: Int, b: Int):
    private val from = scala.collection.mutable.Map.empty[String, Int]

    /** Ген → id родителя, от которого он взят. */
    def origin: Map[String, Int] = from.toMap

    /** Значение гена `key` от случайного родителя. */
    def pick[A](key: String, x: A, y: A): A =
      if rnd.nextBoolean() then { from(key) = a; x }
      else { from(key) = b; y }

    /** Скрещивание функций: одного семейства — покоэффициентно, разных — целиком от случайного родителя. */
    def function(prefix: String, familyKey: String, f1: MathFunction, f2: MathFunction): MathFunction =
      val (c1, c2) = (Genes.coefficients(prefix, f1), Genes.coefficients(prefix, f2))
      if f1.family == f2.family && c1.size == c2.size then
        pick(familyKey, (), ())
        Genes.rebuild(f1, c1.zip(c2).map { case ((k, x), (_, y)) => pick(k, x, y) })
      else
        val chosen = pick(familyKey, f1, f2)
        val source = from(familyKey)
        Genes.coefficients(prefix, chosen).foreach((k, _) => from(k) = source)
        chosen

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
      case Constant(c)    => Constant(gene(s"$prefix: c", c))
      case Polynomial(cs) => Polynomial(cs.zipWithIndex.map((c, i) => gene(s"$prefix: a$i", c)))
      case Logarithmic(a, b, c) =>
        Logarithmic(gene(s"$prefix: a", a), gene(s"$prefix: b", b).max(0), gene(s"$prefix: c", c))
      case Sinusoidal(a, k, phi, c) =>
        Sinusoidal(gene(s"$prefix: a", a), gene(s"$prefix: k", k), gene(s"$prefix: φ", phi), gene(s"$prefix: c", c))
      case Exponential(a, b, c) =>
        Exponential(gene(s"$prefix: a", a), gene(s"$prefix: b", b), gene(s"$prefix: c", c))

    /** Макромутация: с вероятностью `rate` смена семейства на случайное другое из `families`. */
    def family(name: String, f: MathFunction, families: Vector[Family], ranges: FunctionRanges, rate: Double): MathFunction =
      val others = families.filter(_ != f.family)
      if others.isEmpty || rnd.nextDouble() >= rate then f
      else
        val target = others(rnd.nextInt(others.size))
        val mutated = (f, polynomialDegree(target)) match
          case (Polynomial(cs), Some(degree)) => grow(cs, degree)
          case (Constant(c), Some(degree))    => grow(Vector(c), degree)
          case _ => Genesis.randomFunction(target, ranges, rnd)
        log += s"$name: ${f.family.title} → ${target.title}"
        mutated

    /** Полином степени `degree` с теми же младшими коэффициентами; новые — небольшой шум. */
    private def grow(cs: Vector[Double], degree: Int): Polynomial =
      Polynomial(cs.take(degree + 1).padTo(degree + 1, 0.0).zipWithIndex.map { (c, i) =>
        if i < cs.size then c else rnd.nextGaussian() * m.absoluteSigma * 2
      })

  private def polynomialDegree(f: Family): Option[Int] = f match
    case Family.Linear    => Some(1)
    case Family.Quadratic => Some(2)
    case Family.Cubic     => Some(3)
    case _                => None
