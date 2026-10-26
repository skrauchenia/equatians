package equatians

import equatians.model.*
import equatians.model.MathFunction.*
import equatians.sim.{Genesis, Genetics, Simulation}

import scala.util.Random

class ModelSuite extends munit.FunSuite:

  private val cfg = AppConfig.load()
  /** Правила без роста растений, чтобы сценарии не зависели от случайной еды. */
  private val rules = cfg.rules.copy(plants = cfg.rules.plants.copy(spawnRate = 0))

  /** Организм в секторе (0, 0) с прямолинейным движением на восток и сильным ударом в упор. */
  private def organism(
      id: Int,
      f: MathFunction = Polynomial(Vector(0.0, 0.0)),
      pos: Vec2 = Vec2(50, 50),
      health: Double = 40,
      maxHealth: Double = -1,
      satiety: Double = 100,
      reserve: Double = 0,
      speed: Double = 1.0,
      attack: AttackOrgan = AttackOrgan(Polynomial(Vector(10.0, -12.0)), 15, 0.05),
      age: Long = 0,
      litter: Int = 1
  ): Organism =
    val max = if maxHealth < 0 then health else maxHealth
    Organism(id, MovementOrgan(f, 10, 1.0, speed), attack, SectorId(0, 0), pos, health, max, satiety, reserve,
      age = age, litterSize = litter)

  private def world(os: Organism*): World =
    World(WorldConfig(1, 1, 100), rules, cfg.genesis, os.toVector, seed = 1, nextId = 100)

  private val noMutation = rules.copy(mutation = rules.mutation.copy(rate = 0, familyRate = 0))

  /** Двое взрослых сытых организмов одного вида рядом друг с другом. */
  private def couple(satiety: Double = 100, reserve: Double = 40, litter: Int = 1, age: Long = 1000) =
    Seq(
      organism(0, satiety = satiety, reserve = reserve, litter = litter, age = age),
      organism(1, pos = Vec2(52, 50), satiety = satiety, reserve = reserve, litter = litter, age = age, speed = 0.5)
    )

  private val sine = Sinusoidal(1, 1, 0, 0)

  /** Удар, убивающий почти кого угодно в радиусе ~12. */
  private val deadly = AttackOrgan(Polynomial(Vector(200.0, -240.0)), 15, 0.05)

  private def run(w: World, ticks: Int): World = Iterator.iterate(w)(Simulation.step).drop(ticks).next()

  test("polynomial evaluates with coefficients in ascending order"):
    val f = Polynomial(Vector(1.0, -2.0, 3.0)) // 1 - 2x + 3x²
    assertEqualsDouble(f(2.0), 9.0, 1e-9)
    assertEquals(f.family, Family.Quadratic)

  test("movement argument cycles over the period"):
    val organ = MovementOrgan(Polynomial(Vector(0.0, 1.0)), period = 10, span = 2.0, speed = 1.0)
    assertEqualsDouble(organ.argument(5), 1.0, 1e-9)
    assertEqualsDouble(organ.argument(15), 1.0, 1e-9)

  test("cruise step length equals organ speed away from walls"):
    val moved = Simulation.step(world(organism(0, speed = 1.5))).organisms.head.pos
    assertEqualsDouble(math.hypot(moved.x - 50, moved.y - 50), 1.5, 1e-9)

  test("organism bounces off the sector wall and stays inside"):
    val o = Simulation.step(world(organism(0, pos = Vec2(99.5, 50)))).organisms.head
    assertEqualsDouble(o.pos.x, 99.5, 1e-9)
    assert(o.mirrorX)
    assert(o.cruiseDisplacement.x < 0)

  test("bigger and faster organisms spend more satiety"):
    def spent(health: Double, speed: Double) =
      100 - Simulation.step(world(organism(0, health = health, speed = speed))).organisms.head.satiety
    assert(spent(60, 0.5) > spent(20, 0.5))
    assert(spent(20, 1.5) > spent(20, 0.5))

  test("reserve is burned at its energy density once satiety is empty"):
    val before = organism(0, satiety = 0, reserve = 10)
    val after = Simulation.step(world(before)).organisms.head
    val cost = Simulation.metabolicCost(after, rules)
    assertEqualsDouble(after.reserve, 10 - cost / rules.energy.reserveDensity, 1e-9)
    assertEqualsDouble(after.health, 40.0, 1e-9)
    val emptied = Simulation.step(world(organism(0, satiety = 0.001, reserve = 10))).organisms.head
    assert(emptied.log.exists(_.text.contains("расходует жир")))

  test("starving organism loses health and eventually dies"):
    val end = run(world(organism(0, health = 5, satiety = 0, reserve = 0)), 20000)
    assert(end.organisms.isEmpty)
    assert(end.graveyard.head.log.last.text.contains("умер от голода"))
    assertEqualsDouble(end.corpses.head.food, rules.meatPerHealth * 5, 1e-9)

  test("eating fills satiety first and puts the surplus into reserve"):
    val meal = organism(0, satiety = 90, reserve = 0).eat(5, rules)
    assertEqualsDouble(meal.toSatiety, 10.0, 1e-9)
    assertEqualsDouble(meal.toReserve, (5 * rules.energy.reserveDensity - 10) / rules.energy.reserveDensity, 1e-9)
    assertEqualsDouble(meal.eaten, 5.0, 1e-9)
    assertEqualsDouble(meal.organism.satiety, rules.energy.maxSatiety, 1e-9)

  test("reserve capacity is proportional to max health and caps eating"):
    val small = organism(0, health = 10, satiety = 100)
    val big = organism(1, health = 60, satiety = 100)
    assertEqualsDouble(big.maxReserve(rules) / small.maxReserve(rules), 6.0, 1e-9)
    val meal = small.eat(1000, rules)
    assertEqualsDouble(meal.organism.reserve, small.maxReserve(rules), 1e-9)
    assertEqualsDouble(meal.eaten, small.maxReserve(rules), 1e-9)

  test("bigger organisms hit harder, and wounds keep their weight class"):
    val small = organism(0, health = 10)
    val big = organism(1, health = 90)
    assertEqualsDouble(big.strikeDamage(3, rules) / small.strikeDamage(3, rules), 3.0, 1e-9)
    val wounded = big.copy(health = 5)
    assertEqualsDouble(wounded.strikeDamage(3, rules), big.strikeDamage(3, rules), 1e-9)
    assertEqualsDouble(wounded.radius(rules), big.radius(rules), 1e-9)

  test("wounded organism heals at a satiety cost, but not without energy"):
    val wounded = organism(0, health = 20, maxHealth = 40, speed = 0.0)
    val healed = Simulation.step(world(wounded)).organisms.head
    assertEqualsDouble(healed.health, 20 + rules.healing.rate, 1e-9)
    val metabolism = Simulation.metabolicCost(healed, rules)
    assertEqualsDouble(healed.satiety, 100 - metabolism - rules.healing.rate * rules.healing.cost, 1e-6)
    val broke = organism(0, health = 20, maxHealth = 40, satiety = 0, reserve = 0)
    assert(Simulation.step(world(broke)).organisms.head.health < 20)

  test("attack organ: range is the first root, cost is the integral"):
    val organ = AttackOrgan(Polynomial(Vector(1.0, -2.0)), maxReach = 10, costFactor = 0.1) // D(u) = 1 - 2u
    assertEqualsDouble(organ.range, 5.0, 1e-6)
    assertEqualsDouble(organ.cost, 0.1 * 10 * 0.25, 1e-4)
    assertEqualsDouble(organ.damage(2), 0.6, 1e-9)
    assertEqualsDouble(organ.damage(6), 0.0, 1e-9)

  test("organisms of the same species never attack each other"):
    val a = organism(0, satiety = 0, reserve = 40)
    val b = organism(1, satiety = 0, reserve = 40, pos = Vec2(53, 50))
    val end = run(world(a, b), 300)
    assertEquals(end.organisms.size, 2)
    assert(end.organisms.forall(_.log.forall(e => !e.text.contains("ударил"))))

  test("hungry predator chases, kills and eats a stranger"):
    val predator = organism(0, satiety = 0, reserve = 40)
    val prey = organism(1, f = sine, pos = Vec2(53, 50), health = 5, reserve = 5)
    val end = run(world(predator, prey), 100)
    assertEquals(end.organisms.map(_.id), Vector(0))
    val p = end.organisms.head
    assert(p.log.exists(_.text.contains("решил преследовать #1")))
    assert(p.log.exists(_.text.contains("убил #1 и съел")))
    assert(p.satiety > 0)
    assert(end.graveyard.head.log.last.text.contains("погиб от удара #0"))

  test("uneaten fat stays as carrion that another organism eats"):
    val predator = organism(0, satiety = 0, reserve = 40, attack = deadly)
    val prey = organism(1, f = sine, pos = Vec2(53, 50), health = 60, reserve = 50)
    val scavenger = organism(2, pos = Vec2(60, 50), satiety = 50, speed = 0.5)
    val afterKill = Simulation.step(world(predator, prey, scavenger))
    assertEquals(afterKill.corpses.map(_.id), Vector(1))
    // Хищник платил за удар из жира (сытость была 0), так что съел 100 сытости и восполнил потраченный жир.
    val eaten = (100.0 + deadly.cost) / rules.energy.reserveDensity
    val food = rules.meatPerHealth * 60 + 50
    assertEqualsDouble(afterKill.corpses.head.food, food - eaten, 0.1)
    val end = run(afterKill, 60)
    assert(end.organisms.find(_.id == 2).get.log.exists(_.text.contains("съел падаль #1")))

  test("carrion takes priority over chasing a live stranger"):
    val hungry = organism(0, satiety = 0, reserve = 40)
    val stranger = organism(1, f = sine, pos = Vec2(55, 50))
    val corpse = Corpse(9, Family.Cubic, SectorId(0, 0), Vec2(45, 50), 10, 0)
    val end = run(world(hungry, stranger).copy(corpses = Vector(corpse)), 5)
    val log = end.organisms.find(_.id == 0).get.log
    assert(log.exists(_.text.contains("заметил падаль #9")))
    assert(!log.exists(_.text.contains("решил преследовать")))

  test("genesis spawns organisms with full health and satiety"):
    val w = Genesis.randomWorld(WorldConfig(3, 2, 100), rules, cfg.genesis, count = 50, seed = 3)
    w.organisms.foreach { o =>
      assertEqualsDouble(o.health, o.maxHealth, 1e-9)
      assertEqualsDouble(o.satiety, rules.energy.maxSatiety, 1e-9)
      assert(o.reserve <= o.maxReserve(rules))
    }

  test("random world keeps its invariants over many ticks"):
    val start = Genesis.randomWorld(WorldConfig(3, 2, 100), rules, cfg.genesis, count = 100, seed = 7)
    val end = run(start, 3000)
    assertEquals(end.tick, 3000L)
    assertEquals(end.organisms.map(_.id).distinct.size, end.organisms.size)
    assert(end.organisms.size <= 100 + end.births)
    assert(end.corpses.forall(_.food > 0))
    end.organisms.foreach { o =>
      assert(o.pos.x >= 0 && o.pos.x <= 100 && o.pos.y >= 0 && o.pos.y <= 100, o.id)
      assert(o.health > 0 && o.satiety >= 0 && o.reserve >= 0, o.id)
      assert(o.satiety <= rules.energy.maxSatiety + 1e-9 && o.reserve <= o.maxReserve(rules) + 1e-9, o.id)
      assert(o.health <= o.maxHealth + 1e-9, o.id)
    }

  test("crossover without mutation takes every gene from one of the parents"):
    val a = organism(0, f = Polynomial(Vector(1.0, 2.0, 3.0)), health = 30, litter = 1, speed = 0.5)
    val b = organism(1, f = Polynomial(Vector(-1.0, -2.0, -3.0)), health = 50, litter = 4, speed = 1.5)
    val w = world(a, b).copy(rules = noMutation)
    for seed <- 0 until 20 do
      val k = Genetics.offspring(100, a, b, Vec2(50, 50), 0, w, Random(seed))
      val child = k.organism
      assert(k.mutations.isEmpty)
      val Polynomial(cs) = child.movement.f: @unchecked
      cs.zipWithIndex.foreach((c, i) => assert(c == (i + 1) || c == -(i + 1), cs))
      assert(Set(30.0, 50.0)(child.maxHealth) && Set(1, 4)(child.litterSize) && Set(0.5, 1.5)(child.movement.speed))
      assertEquals(child.generation, 1)
      assertEquals(child.parents, Some((0, 1)))
      assertEqualsDouble(child.health, child.maxHealth, 1e-9)

  test("family macro-mutation creates a new species"):
    val (a, b) = (organism(0), organism(1))
    val always = rules.copy(mutation = rules.mutation.copy(familyRate = 1))
    val k = Genetics.offspring(100, a, b, Vec2(50, 50), 0, world(a, b).copy(rules = always), Random(1))
    assert(k.newSpecies)
    assertNotEquals(k.organism.species, a.species)
    assert(k.mutations.exists(_.contains("функция движения")))

  test("ready couple of the same species mates and parents pay for the offspring"):
    val end = run(world(couple()*), 40)
    assertEquals(end.births, 1L)
    val child = end.organisms.find(_.id == 100).get
    assertEquals(child.parents, Some((0, 1)))
    assertEquals(child.generation, 1)
    assert(child.log.head.text.startsWith("родился от #0 × #1"))
    val parents = end.organisms.filter(_.id < 2)
    assert(parents.forall(p => p.offspringCount == 1 && p.log.exists(_.text.contains("спарился"))))
    assert(parents.forall(_.energy(rules) < 100 + 40 * rules.energy.reserveDensity - 50))

  test("immature organisms do not mate"):
    assertEquals(run(world(couple(age = 0)*), 200).births, 0L)

  test("litter is limited by what both parents can afford"):
    // Энергия каждого ≈ 62 + 8·4 = 94, ребёнок стоит 3·40 + 50 = 170, т.е. 85 на родителя: хватает на одного из пяти.
    val end = run(world(couple(satiety = 62, reserve = 8, litter = 5)*).copy(rules = noMutation), 40)
    assertEquals(end.births, 1L)
    assert(end.organisms.find(_.id == 0).get.log.exists(_.text.contains("хотели 5, хватило сил на 1")))

  test("config refuses bodies cheaper than the meat they leave"):
    intercept[IllegalArgumentException](
      rules.copy(reproduction = rules.reproduction.copy(bodyCostPerHealth = 1.0))
    )

  test("plants sprout in every sector over time, up to the limit"):
    val withPlants = cfg.rules.copy(plants = cfg.rules.plants.copy(spawnRate = 1, maxPerSector = 5))
    val w = World(WorldConfig(2, 1, 100), withPlants, cfg.genesis, Vector.empty, seed = 1)
    val end = run(w, 50)
    assertEquals(end.plants.groupMapReduce(_.sector)(_ => 1)(_ + _), Map(SectorId(0, 0) -> 5, SectorId(1, 0) -> 5))
    assertEquals(end.plants.map(_.id).distinct.size, 10)

  test("hungry organism walks to a plant and eats it"):
    val hungry = organism(0, satiety = 30)
    val plant = Plant(0, SectorId(0, 0), Vec2(56, 50), rules.plants.food)
    val end = run(world(hungry).copy(plants = Vector(plant), nextPlantId = 1), 40)
    val o = end.organisms.head
    assert(o.log.exists(_.text.startsWith("съел растение")), o.log)
    assert(end.plants.isEmpty)
    assert(o.satiety > 30)

  test("default config file loads"):
    assertEquals(cfg.genesis.movement.families.toSet, Family.values.toSet)
    assertEquals(cfg.genesis.attack.functions.cubic.size, 4)

  test("latex: powers, signs, unit coefficients and zero terms"):
    assertEquals(Polynomial(Vector(1.0, -3.0, 1.0)).latex, "x^{2} - 3x + 1")
    assertEquals(Polynomial(Vector(0.0, 0.0, 0.0, -1.0)).latex, "-x^{3}")
    assertEquals(Polynomial(Vector(2.5, 0.001, 1.5)).latex, "1.5x^{2} + 2.5")
    assertEquals(Polynomial(Vector(0.0, 0.0)).latex, "0")

  test("latex: transcendental families"):
    assertEquals(Logarithmic(-2, 0.5, 1).latex, "-2\\ln\\left(1 + 0.5|x|\\right) + 1")
    assertEquals(Sinusoidal(1, 3, -1.25, 0).latex, "\\sin\\left(3x - 1.25\\right)")
    assertEquals(Exponential(0.5, -0.8, 2).latex, "0.5e^{-0.8x} + 2")
