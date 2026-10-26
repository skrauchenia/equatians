package equatians.sim

import equatians.model.*
import equatians.model.MathFunction.Constant

import scala.util.Random

/** Корм — организмы с функцией движения-константой, возобновляемая еда мира.
  *
  * Корм появляется двумя путями: изредка сам по себе (чтобы опустевший сектор ожил) и почкованием —
  * каждый корм с вероятностью reproduction-rate · (1 − n / max-per-sector) за тик отпочковывает клона.
  * У корма есть «спящие» гены — период, span, функция урона: пока функция движения константа, они не работают,
  * но наследуются и мутируют, и пригодятся, если клон пробудится в активный организм.
  */
object Forage:

  /** Корм с заданными органами: курс — константа, сытости нет, жир — `forage.reserve` (не больше ёмкости). */
  def create(id: Int, sector: SectorId, pos: Vec2, movement: MovementOrgan, attack: AttackOrgan, health: Double, tick: Long, rules: Rules): Organism =
    Organism(
      id = id,
      movement = movement,
      attack = attack,
      sector = sector,
      pos = pos,
      health = health,
      maxHealth = health,
      satiety = 0,
      reserve = math.min(rules.forage.reserve, rules.energy.reservePerHealth * health)
    ).logged(tick, Narrative.sprouted, rules.logSize)

  /** Корм со случайными генами: курс из [0, 2π), скорость и здоровье из `forage`,
    * спящие период, span и функция урона — из диапазонов генезиса.
    */
  def random(id: Int, sector: SectorId, pos: Vec2, rules: Rules, genesis: GenesisConfig, rnd: Random, tick: Long): Organism =
    val g = genesis.movement
    val movement = MovementOrgan(
      f = Constant(rnd.nextDouble() * 2 * math.Pi),
      period = g.period.sample(rnd).round.toInt.max(1),
      span = g.span.sample(rnd),
      speed = rules.forage.speed
    )
    val attack = Genesis.randomAttack(genesis.attack, rules.attack, rnd)
    create(id, sector, pos, movement, attack, rules.forage.health, tick, rules)

  /** Корм, появившийся сам по себе: в каждом секторе с вероятностью `spawn-rate`, если там меньше предела.
    * Случайность детерминирована сидом и тиком; id выдаются начиная с `firstId`.
    */
  def spawn(organisms: Vector[Organism], firstId: Int, world: World): Vector[Organism] =
    val f = world.rules.forage
    val size = world.config.sectorSize
    val counts = organisms.filter(_.forage).groupMapReduce(_.sector)(_ => 1)(_ + _)
    val sprouts = world.config.sectors.zipWithIndex.filter { (sector, i) =>
      counts.getOrElse(sector, 0) < f.maxPerSector && Rng.uniform(world.seed, world.tick, -(i + 1), 7) < f.spawnRate
    }
    sprouts.zipWithIndex.map { case ((sector, i), k) =>
      val rnd = Random(Rng.long(world.seed, world.tick, -(i + 1), 8))
      val pos = Vec2(rnd.nextDouble() * size, rnd.nextDouble() * size)
      random(firstId + k, sector, pos, world.rules, world.genesis, rnd, world.tick)
    }.toVector

  /** Почкование: каждый живой корм с вероятностью reproduction-rate · (1 − n / max-per-sector)
    * отпочковывает клона в паре единиц от себя. n пересчитывается по ходу, так что предел не превышается.
    *
    * @return организмы (у родителей — запись в журнале) и отпочковавшиеся клоны с id начиная с `firstId`
    */
  def bud(organisms: Vector[Organism], firstId: Int, world: World): (Vector[Organism], Vector[Genetics.Offspring]) =
    val f = world.rules.forage
    val size = world.config.sectorSize
    val start = (organisms.filter(_.forage).groupMapReduce(_.sector)(_ => 1)(_ + _), Vector.empty[Genetics.Offspring])
    val (_, buds) = organisms.filter(_.forage).foldLeft(start) { case ((counts, acc), p) =>
      val n = counts.getOrElse(p.sector, 0)
      val chance = f.reproductionRate * (1 - n.toDouble / f.maxPerSector)
      if Rng.uniform(world.seed, world.tick, p.id, 12) >= chance then (counts, acc)
      else
        val rnd = Random(Rng.long(world.seed, world.tick, p.id, 13))
        val q = p.pos + Vec2.polar(1 + rnd.nextDouble() * 2, rnd.nextDouble() * 2 * math.Pi)
        val pos = Vec2(q.x.max(0).min(size), q.y.max(0).min(size))
        val k = Genetics.bud(firstId + acc.size, p, pos, world.tick, world, rnd)
        // Пробудившийся клон — уже не корм и не занимает место корма в секторе.
        val grown = if k.organism.forage then counts.updated(p.sector, n + 1) else counts
        (grown, acc :+ k)
    }
    val byParent = buds.groupBy(_.organism.parents.head)
    val noted = organisms.map { o =>
      byParent.get(o.id).fold(o) { ks =>
        val p = ks.foldLeft(o)((x, k) => x.logged(world.tick, Narrative.buddedOff(k), world.rules.logSize))
        p.copy(offspringCount = p.offspringCount + ks.size)
      }
    }
    (noted, buds)
