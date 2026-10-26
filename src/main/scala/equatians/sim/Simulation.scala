package equatians.sim

import equatians.model.*
import equatians.model.Behaviour.*

import scala.util.Random

/** Правила одного тика мира.
  *
  * Порядок фаз:
  *  1. решение — каждый организм, видя снимок мира на начало тика, выбирает поведение;
  *     падаль в поле зрения проверяется каждый тик и важнее охоты и обычного движения;
  *     готовый к размножению сначала ищет готового партнёра своего вида, потом смотрит на чужаков;
  *  2. движение — обычное (по функции), к цели, к падали, к партнёру или от угрозы;
  *  3. метаболизм — оплата шага сытостью, затем жиром, затем здоровьем; потом отхил;
  *  4. удары — все преследователи, у которых цель в зоне удара, бьют одновременно;
  *  5. реакция — раненые решают, отвечать ли нападающему или бежать;
  *  6. еда — дошедшие до падали или растения едят, ближние первыми;
  *  7. смерть — убитого ест нанёсший наибольший урон, остаток еды остаётся трупом;
  *  8. размножение — дошедшие друг до друга готовые партнёры рождают помёт;
  *  9. рост — в секторах появляются новые растения.
  */
object Simulation:

  /** Труп с меньшим остатком еды не оставляется — нечего есть. */
  val CorpseMinReserve = 0.01

  /** Удар, решённый на этом тике. */
  private final case class Strike(attacker: Int, target: Int, distance: Double, damage: Double, sizeFactor: Double, cost: Double)

  /** Снимок мира на начало тика и вспомогательные операции над ним. */
  private final class Context(val world: World):
    val now: Long = world.tick
    val rules: Rules = world.rules
    val byId: Map[Int, Organism] = world.organisms.iterator.map(o => o.id -> o).toMap
    val bySector: Map[SectorId, Vector[Organism]] = world.organisms.groupBy(_.sector)
    val corpseById: Map[Int, Corpse] = world.corpses.iterator.map(c => c.id -> c).toMap
    val corpsesBySector: Map[SectorId, Vector[Corpse]] = world.corpses.groupBy(_.sector)
    val plantById: Map[Int, Plant] = world.plants.iterator.map(p => p.id -> p).toMap
    val plantsBySector: Map[SectorId, Vector[Plant]] = world.plants.groupBy(_.sector)

    /** Запись в журнал организма с текущим тиком. */
    def note(o: Organism, text: String): Organism = o.logged(now, text, rules.logSize)

    /** Случайное число [0, 1) для организма `o`; `salt` различает броски внутри тика. */
    def roll(o: Organism, salt: Int): Double = Rng.uniform(world.seed, now, o.id, salt)

  /** Состояние мира через один тик. */
  def step(world: World): World =
    val ctx = Context(world)
    val moved = world.organisms.map(o => metabolise(move(decide(o, ctx), ctx), ctx))
    val movedById = moved.iterator.map(o => o.id -> o).toMap
    val strikes = moved.flatMap(strikeOf(_, movedById, ctx))
    val afterFight = react(strikes, applyStrikes(strikes, movedById, ctx), ctx)
    val (fed, corpsesLeft, plantsLeft) = eatFood(afterFight, ctx)
    val (resolved, newCorpses) = resolveDeaths(strikes, fed, ctx)
    val (alive, dead) = world.organisms.map(o => resolved(o.id)).partition(_.alive)
    val (parents, newborns) = reproduce(alive, ctx)
    val plants = growPlants(plantsLeft, ctx)
    world.copy(
      organisms = parents ++ newborns,
      tick = world.tick + 1,
      graveyard = (world.graveyard ++ dead).takeRight(world.rules.graveyardSize),
      corpses = corpsesLeft ++ newCorpses,
      plants = plants,
      nextPlantId = world.nextPlantId + plants.size - plantsLeft.size,
      nextId = world.nextId + newborns.size,
      births = world.births + newborns.size
    )

  /** Вероятность того, что организм выберет охоту: 0 на пороге голода, растёт линейно к нулевой сытости. */
  def chaseProbability(o: Organism, rules: Rules): Double =
    val b = rules.behaviour
    val sat = o.satietyFraction(rules)
    if sat >= b.hungerThreshold then 0.0
    else b.maxChaseProbability * (1 - sat / b.hungerThreshold)

  /** Выбор поведения при встрече с чужим `other` по броску `roll`:
    * голодный — охотится с вероятностью [[chaseProbability]], сытый с запасом жира — отдаляется
    * с вероятностью `flee-probability`, иначе продолжает обычное движение.
    */
  def choose(o: Organism, other: Int, roll: Double, rules: Rules): Behaviour =
    val b = rules.behaviour
    val p = chaseProbability(o, rules)
    if p > 0 then if roll < p then Chase(other) else Cruise
    else if o.satietyFraction(rules) >= b.fleeSatietyThreshold && o.reserveFraction(rules) >= b.fleeReserveThreshold then
      if roll < b.fleeProbability then Flee(other) else Cruise
    else Cruise

  /** Ближайший организм другого вида в своём секторе в радиусе восприятия. */
  private def nearestStranger(o: Organism, ctx: Context): Option[(Organism, Double)] =
    ctx.bySector
      .getOrElse(o.sector, Vector.empty)
      .iterator
      .filter(n => n.id != o.id && n.species != o.species)
      .map(n => n -> o.distanceTo(n))
      .filter(_._2 <= ctx.rules.perceptionRadius)
      .minByOption(_._2)

  /** Ближайший готовый к размножению организм своего вида в своём секторе в радиусе восприятия. */
  private def nearestMate(o: Organism, ctx: Context): Option[(Organism, Double)] =
    ctx.bySector
      .getOrElse(o.sector, Vector.empty)
      .iterator
      .filter(n => n.id != o.id && n.species == o.species && n.readyToMate(ctx.now, ctx.rules))
      .map(n => n -> o.distanceTo(n))
      .filter(_._2 <= ctx.rules.perceptionRadius)
      .minByOption(_._2)

  /** Ближайший источник еды (труп или растение) в своём секторе в радиусе восприятия. */
  private def nearestFood[F <: FoodSource](o: Organism, bySector: Map[SectorId, Vector[F]], ctx: Context): Option[(F, Double)] =
    bySector
      .getOrElse(o.sector, Vector.empty)
      .iterator
      .map(c => c -> (c.pos - o.pos).length)
      .filter(_._2 <= ctx.rules.perceptionRadius)
      .minByOption(_._2)

  /** Фаза 1: проверить, существуют ли ещё цель/угроза/падаль/партнёр; каждый тик — не появилась ли падаль;
    * раз в `decision-interval` тиков — поискать пару, а если не вышло, бросить кубик относительно чужака.
    */
  private def decide(o: Organism, ctx: Context): Organism =
    val r = ctx.rules
    def within(id: Int, radius: Double) =
      ctx.byId.get(id).filter(x => x.sector == o.sector && o.distanceTo(x) <= radius)

    val kept = o.behaviour match
      case Chase(t) if within(t, r.perceptionRadius * r.behaviour.loseTargetFactor).isEmpty =>
        val text = if ctx.byId.contains(t) then Narrative.targetLost(t) else Narrative.targetGone(t)
        ctx.note(o.copy(behaviour = Cruise), text)
      case Flee(f) if within(f, r.perceptionRadius).isEmpty =>
        ctx.note(o.copy(behaviour = Cruise), Narrative.escaped(f))
      case Scavenge(c) if !ctx.corpseById.contains(c) =>
        ctx.note(o.copy(behaviour = Cruise), Narrative.carrionGone(c))
      case Graze(p) if !ctx.plantById.contains(p) =>
        ctx.note(o.copy(behaviour = Cruise), Narrative.plantGone(p))
      case Mate(p) if within(p, r.perceptionRadius * r.behaviour.loseTargetFactor).isEmpty =>
        ctx.note(o.copy(behaviour = Cruise), Narrative.mateLost(p))
      case Mate(p) if !o.readyToMate(ctx.now, r) =>
        ctx.note(o.copy(behaviour = Cruise), Narrative.mateGaveUp(p))
      case _ => o

    // Падаль важнее растений, растения — важнее охоты и обычного движения. Идущий к растению отвлечётся на падаль.
    val sat = kept.satietyFraction(r)
    val carrion = kept.behaviour match
      case Cruise | Chase(_) | Graze(_) if sat < r.scavenging.satietyThreshold =>
        nearestFood(kept, ctx.corpsesBySector, ctx).filter(_ => ctx.roll(kept, 3) < r.scavenging.probability)
      case _ => None
    val plant = kept.behaviour match
      case Cruise | Chase(_) if carrion.isEmpty && sat < r.plants.satietyThreshold =>
        nearestFood(kept, ctx.plantsBySector, ctx).filter(_ => ctx.roll(kept, 6) < r.plants.probability)
      case _ => None

    (carrion, plant) match
      case (Some((c, dist)), _) => ctx.note(kept.copy(behaviour = Scavenge(c.id)), Narrative.spotCarrion(c, dist, r, kept))
      case (_, Some((p, dist))) => ctx.note(kept.copy(behaviour = Graze(p.id)), Narrative.spotPlant(p, dist, r, kept))
      case _ => decideSocial(o, kept, ctx)

  /** Решения о партнёре и чужаках — раз в `decision-interval` тиков, если организм не занят едой или парой. */
  private def decideSocial(o: Organism, kept: Organism, ctx: Context): Organism =
    val r = ctx.rules
    kept.behaviour match
      case Scavenge(_) | Graze(_) | Mate(_) => kept
      case _ if o.age % r.behaviour.decisionInterval != 0 => kept
      case _ =>
        val partner =
          if !kept.readyToMate(ctx.now, r) then None
          else nearestMate(kept, ctx).filter(_ => ctx.roll(kept, 4) < r.reproduction.probability)
        partner match
          case Some((p, dist)) => ctx.note(kept.copy(behaviour = Mate(p.id)), Narrative.seekMate(p, dist))
          case None            => meetStranger(kept, ctx)

  /** Решение относительно ближайшего чужака: охота, бегство или обычное движение. */
  private def meetStranger(kept: Organism, ctx: Context): Organism =
    val r = ctx.rules
    nearestStranger(kept, ctx) match
      case None => kept
      case Some((other, dist)) =>
        val next = choose(kept, other.id, ctx.roll(kept, 1), r)
        if next == kept.behaviour then kept
        else
          val text = (next, kept.behaviour) match
            case (Chase(_), _)      => Narrative.startChase(kept, other, dist, r)
            case (Flee(_), _)       => Narrative.startFlee(kept, other, dist, r)
            case (Cruise, Chase(t)) => Narrative.stopChase(t)
            case (Cruise, Flee(f))  => Narrative.stopFlee(f)
            case _                  => ""
          ctx.note(kept.copy(behaviour = next), text)

  /** Шаг длины не больше `speed` к точке `target`, останавливаясь на расстоянии `stopAt` от неё. */
  private def approach(o: Organism, target: Vec2, stopAt: Double): Vec2 =
    val d = target - o.pos
    val dist = d.length
    val len = (dist - stopAt).max(0).min(o.movement.speed)
    if dist > 0 then d * (len / dist) else Vec2.Zero

  /** Фаза 2: шаг. Преследователь останавливается на половине своей дальности удара,
    * к падали идёт вплотную. Отражение от стен меняет флаги зеркал только в обычном движении.
    */
  private def move(o: Organism, ctx: Context): Organism =
    val size = ctx.world.config.sectorSize
    val wanted = o.behaviour match
      case Cruise      => o.cruiseDisplacement
      case Chase(t)    => ctx.byId.get(t).fold(Vec2.Zero)(target => approach(o, target.pos, o.attack.range * 0.5))
      case Scavenge(c) => ctx.corpseById.get(c).fold(Vec2.Zero)(corpse => approach(o, corpse.pos, 0))
      case Graze(p)    => ctx.plantById.get(p).fold(Vec2.Zero)(plant => approach(o, plant.pos, 0))
      case Mate(p) =>
        ctx.byId.get(p).fold(Vec2.Zero)(partner => approach(o, partner.pos, o.radius(ctx.rules) + partner.radius(ctx.rules)))
      case Flee(f) =>
        ctx.byId.get(f).fold(Vec2.Zero) { threat =>
          val d = o.pos - threat.pos
          if d.length > 0 then d * (o.movement.speed / d.length) else o.cruiseDisplacement
        }
    val target = o.pos + wanted
    val (x, flipX) = reflect(target.x, size)
    val (y, flipY) = reflect(target.y, size)
    val cruising = o.behaviour == Cruise
    val pos = Vec2(x, y)
    o.copy(
      pos = pos,
      age = o.age + 1,
      lastStep = pos - o.pos,
      mirrorX = o.mirrorX ^ (cruising && flipX),
      mirrorY = o.mirrorY ^ (cruising && flipY)
    )

  /** Отражает координату `v` в отрезок [0, size]; второй элемент — было ли отражение. */
  private def reflect(v: Double, size: Double): (Double, Boolean) =
    if v < 0 then (math.min(-v, size), true)
    else if v > size then (math.max(2 * size - v, 0.0), true)
    else (v, false)

  /** Расход сытости за тик: r · (base + movement-cost · шаг²) — крупным и быстрым дороже. */
  def metabolicCost(o: Organism, rules: Rules): Double =
    val step = o.lastStep.length
    o.radius(rules) * (rules.energy.baseMetabolism + rules.energy.movementCost * step * step)

  /** Фаза 3: оплатить расход (неоплаченное бьёт по здоровью), затем подлечиться на остаток энергии. */
  private def metabolise(o: Organism, ctx: Context): Organism =
    val r = ctx.rules
    val (paid, unpaid) = o.pay(metabolicCost(o, r), r)
    val after = if unpaid > 0 then paid.copy(health = paid.health - unpaid * r.energy.starvationDamage) else paid
    val noted =
      if o.satiety > 0 && after.satiety == 0 then
        ctx.note(after, if after.reserve > 0 then Narrative.burningReserve else Narrative.starvingNoReserve)
      else if o.reserve > 0 && after.reserve == 0 && after.satiety == 0 then
        ctx.note(after, Narrative.reserveExhausted)
      else after
    val healed = noted.heal(r)
    if noted.health < noted.maxHealth && healed.health >= healed.maxHealth then ctx.note(healed, Narrative.fullyHealed)
    else healed

  /** Фаза 4: удар, если организм преследует чужака, тот в зоне удара, перезарядка прошла
    * и хватает энергии оплатить удар. Урон масштабируется размером нападающего.
    */
  private def strikeOf(o: Organism, byId: Map[Int, Organism], ctx: Context): Option[Strike] =
    o.behaviour match
      case Chase(t) =>
        byId.get(t).flatMap { target =>
          val d = o.distanceTo(target)
          val damage = o.strikeDamage(d, ctx.rules)
          val ready = ctx.now - o.lastAttackTick >= ctx.rules.attack.cooldown
          val canAfford = o.energy(ctx.rules) >= o.attack.cost
          Option.when(target.sector == o.sector && target.species != o.species && ready && canAfford && damage > 0)(
            Strike(o.id, t, d, damage, o.sizeFactor(ctx.rules), o.attack.cost)
          )
        }
      case _ => None

  /** Применяет все удары тика одновременно: списывает стоимость, наносит урон, пишет журналы обеим сторонам. */
  private def applyStrikes(strikes: Vector[Strike], byId: Map[Int, Organism], ctx: Context): Map[Int, Organism] =
    strikes.foldLeft(byId) { (m, s) =>
      val a = m(s.attacker)
      val t = m(s.target)
      val hp = t.health - s.damage
      val a2 = ctx.note(
        a.pay(s.cost, ctx.rules)._1.copy(lastAttackTick = ctx.now),
        Narrative.strike(t, s.distance, s.damage, s.sizeFactor, s.cost, hp)
      )
      val t2 = ctx.note(t.copy(health = hp), Narrative.struck(a.id, s.distance, s.damage, t.health, hp))
      m.updated(a.id, a2).updated(t.id, t2)
    }

  /** Фаза 5: выживший после удара решает, что делать с самым опасным нападающим:
    * голодный может ответить (по той же вероятности охоты), иначе спасается бегством.
    */
  private def react(strikes: Vector[Strike], m: Map[Int, Organism], ctx: Context): Map[Int, Organism] =
    strikes.groupBy(_.target).toVector.sortBy(_._1).foldLeft(m) { case (acc, (targetId, hits)) =>
      val t = acc(targetId)
      val attacker = hits.maxBy(_.damage).attacker
      if !t.alive || t.behaviour == Chase(attacker) || t.behaviour == Flee(attacker) then acc
      else
        val fight = ctx.roll(t, 2) < chaseProbability(t, ctx.rules)
        val next = if fight then Chase(attacker) else Flee(attacker)
        val text = if fight then Narrative.fightBack(attacker) else Narrative.runAway(attacker)
        acc.updated(targetId, ctx.note(t.copy(behaviour = next), text))
    }

  /** Фаза 6: живые организмы, дошедшие до своей еды (падали или растения), едят — ближние первыми.
    *
    * @param target  id источника, к которому идёт организм, если он идёт есть именно такой еды
    * @param reduce  тот же источник с новым остатком еды
    * @param describe запись в журнал о съеденном
    * @return организмы после еды и оставшиеся источники (в исходном порядке)
    */
  private def feed[F <: FoodSource](
      m: Map[Int, Organism],
      sources: Vector[F],
      target: Behaviour => Option[Int],
      reduce: (F, Double) => F,
      describe: (F, Meal) => String,
      ctx: Context
  ): (Map[Int, Organism], Vector[F]) =
    val r = ctx.rules
    val byId = sources.iterator.map(f => f.id -> f).toMap
    val eaters = m.valuesIterator
      .filter(_.alive)
      .flatMap(o => target(o.behaviour).flatMap(id => byId.get(id).map(f => (o.id, id, (f.pos - o.pos).length))))
      .filter((id, _, dist) => dist <= m(id).radius(r) + r.scavenging.eatReach)
      .toVector
      .sortBy((id, _, dist) => (dist, id))
    val (fed, left) = eaters.foldLeft((m, byId)) { case ((orgs, fs), (id, f, _)) =>
      fs.get(f) match
        case None => (orgs, fs)
        case Some(source) =>
          val meal = orgs(id).eat(source.food, r)
          val rest = source.food - meal.eaten
          val o = ctx.note(meal.organism.copy(behaviour = Cruise), describe(source, meal))
          (orgs.updated(id, o), if rest > CorpseMinReserve then fs.updated(f, reduce(source, rest)) else fs - f)
    }
    (fed, sources.flatMap(f => left.get(f.id)))

  /** Падаль и растения едятся по одним правилам. */
  private def eatFood(m: Map[Int, Organism], ctx: Context): (Map[Int, Organism], Vector[Corpse], Vector[Plant]) =
    val (afterCarrion, corpses) = feed[Corpse](
      m, ctx.world.corpses,
      { case Scavenge(c) => Some(c); case _ => None },
      (c, food) => c.copy(food = food),
      Narrative.ateCarrion,
      ctx
    )
    val (afterPlants, plants) = feed[Plant](
      afterCarrion, ctx.world.plants,
      { case Graze(p) => Some(p); case _ => None },
      (p, food) => p.copy(food = food),
      Narrative.atePlant,
      ctx
    )
    (afterPlants, corpses, plants)

  /** Фаза 9: в каждом секторе с вероятностью `spawn-rate` вырастает растение, если их там меньше предела. */
  private def growPlants(plants: Vector[Plant], ctx: Context): Vector[Plant] =
    val p = ctx.rules.plants
    val size = ctx.world.config.sectorSize
    val counts = plants.groupMapReduce(_.sector)(_ => 1)(_ + _)
    val sprouts = ctx.world.config.sectors.zipWithIndex.flatMap { (sector, i) =>
      val salt = -(i + 1)
      def roll(k: Int) = Rng.uniform(ctx.world.seed, ctx.now, salt, k)
      Option.when(counts.getOrElse(sector, 0) < p.maxPerSector && roll(7) < p.spawnRate)(
        (sector, Vec2(roll(8) * size, roll(9) * size))
      )
    }
    plants ++ sprouts.zipWithIndex.map { case ((sector, pos), i) =>
      Plant(ctx.world.nextPlantId + i, sector, pos, p.food)
    }

  /** Фаза 7: погибшего от ударов ест тот, кто нанёс наибольший урон (если сам жив).
    * Еда — мясо тела плюс оставшийся жир; несъеденное остаётся трупом, который может доесть кто угодно.
    * Умерший от голода тоже оставляет труп — из одного мяса.
    *
    * @return организмы после фазы и новые трупы
    */
  private def resolveDeaths(strikes: Vector[Strike], m: Map[Int, Organism], ctx: Context): (Map[Int, Organism], Vector[Corpse]) =
    val deadIds = m.valuesIterator.filterNot(_.alive).map(_.id).toVector.sorted
    val deadSet = deadIds.toSet
    deadIds.foldLeft((m, Vector.empty[Corpse])) { case ((acc, corpses), id) =>
      val victim = acc(id)
      val food = victim.bodyFood(ctx.rules)
      def corpse(left: Double) =
        Option.when(left > CorpseMinReserve)(Corpse(id, victim.species, victim.sector, victim.pos, left, ctx.now))
      strikes.filter(_.target == id).maxByOption(_.damage).map(_.attacker) match
        case None => (acc.updated(id, ctx.note(victim, Narrative.starved)), corpses ++ corpse(food))
        case Some(killer) =>
          val withVictim = acc.updated(id, ctx.note(victim, Narrative.killedBy(killer)))
          if deadSet(killer) then (withVictim, corpses ++ corpse(food))
          else
            val meal = withVictim(killer).eat(food, ctx.rules)
            val fed = meal.organism
            val calm = if fed.behaviour == Chase(id) then fed.copy(behaviour = Cruise) else fed
            (withVictim.updated(killer, ctx.note(calm, Narrative.ate(victim, food, meal))), corpses ++ corpse(food - meal.eaten))
    }

  /** Можно ли спариться прямо сейчас: оба живы и готовы, один вид и сектор, тела почти касаются. */
  private def canMate(a: Organism, b: Organism, ctx: Context): Boolean =
    val r = ctx.rules
    b.alive && a.sector == b.sector && a.species == b.species &&
      a.readyToMate(ctx.now, r) && b.readyToMate(ctx.now, r) &&
      a.distanceTo(b) <= a.radius(r) + b.radius(r) + r.reproduction.reach

  /** Фаза 8: каждый, кто идёт к партнёру и дошёл до него, спаривается (организм — не больше раза за тик).
    *
    * @return родители после размножения и новорождённые
    */
  private def reproduce(alive: Vector[Organism], ctx: Context): (Vector[Organism], Vector[Organism]) =
    val start = (alive.iterator.map(o => o.id -> o).toMap, Set.empty[Int], Vector.empty[Organism], ctx.world.nextId)
    val (byId, _, newborns, _) = alive.sortBy(_.id).foldLeft(start) { case (state @ (orgs, mated, born, nextId), o0) =>
      val o = orgs(o0.id)
      o.behaviour match
        case Mate(p) if !mated(o.id) && !mated(p) =>
          orgs.get(p).filter(canMate(o, _, ctx)) match
            case None => state
            case Some(partner) =>
              val (a, b, kids) = mate(o, partner, nextId, ctx)
              (orgs.updated(a.id, a).updated(b.id, b), mated + a.id + b.id, born ++ kids, nextId + kids.size)
        case _ => state
    }
    (alive.map(o => byId(o.id)), newborns)

  /** Спаривание пары: помёт — среднее генов родителей, но рождается столько детей, сколько оба могут оплатить.
    * Ребёнок стоит тела (body-cost-per-health · его макс. здоровье) плюс сытости, с которой он рождается;
    * родители платят поровну.
    *
    * @return оба родителя после оплаты и новорождённые с id начиная с `firstId`
    */
  private def mate(a: Organism, b: Organism, firstId: Int, ctx: Context): (Organism, Organism, Vector[Organism]) =
    val r = ctx.rules
    val size = ctx.world.config.sectorSize
    val rnd = Random(Rng.long(ctx.world.seed, ctx.now, a.id, 5))
    val wanted = math.round((a.litterSize + b.litterSize) / 2.0).toInt.max(1)
    val mid = (a.pos + b.pos) * 0.5
    val candidates = Vector.tabulate(wanted) { i =>
      val p = mid + Vec2.polar(rnd.nextDouble() * 3, rnd.nextDouble() * 2 * math.Pi)
      val pos = Vec2(p.x.max(0).min(size), p.y.max(0).min(size))
      Genetics.offspring(firstId + i, a, b, pos, ctx.now, ctx.world, rnd)
    }
    def cost(k: Genetics.Offspring) = r.reproduction.bodyCostPerHealth * k.organism.maxHealth + k.organism.satiety
    val budget = math.min(a.energy(r), b.energy(r))
    val kids = candidates.take(candidates.scanLeft(0.0)(_ + cost(_) / 2).tail.takeWhile(_ <= budget).size)
    val share = kids.map(cost).sum / 2
    val ids = kids.map(_.organism.id)
    def parent(o: Organism, partner: Organism) =
      val paid = o.pay(share, r)._1.copy(
        behaviour = Cruise,
        lastReproductionTick = ctx.now,
        offspringCount = o.offspringCount + kids.size
      )
      ctx.note(paid, Narrative.mated(partner.id, ids, wanted, share))
    val born = kids.map(k => ctx.note(k.organism, Narrative.bornChild(k, a, b, r)))
    (parent(a, b), parent(b, a), born)
