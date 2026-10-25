package equatians

import equatians.model.*
import equatians.model.MathFunction.*
import equatians.sim.Genesis

class ModelSuite extends munit.FunSuite:

  test("polynomial evaluates with coefficients in ascending order"):
    val f = Polynomial(Vector(1.0, -2.0, 3.0)) // 1 - 2x + 3x²
    assertEqualsDouble(f(2.0), 9.0, 1e-9)
    assertEquals(f.family, Family.Quadratic)

  test("movement argument cycles over the period"):
    val organ = MovementOrgan(Polynomial(Vector(0.0, 1.0)), period = 10, span = 2.0, speed = 1.0)
    assertEqualsDouble(organ.argument(5), 1.0, 1e-9)
    assertEqualsDouble(organ.argument(15), 1.0, 1e-9)

  test("step length equals organ speed away from walls"):
    val organ = MovementOrgan(Polynomial(Vector(0.7, 0.0)), period = 10, span = 1.0, speed = 1.5)
    val o = Organism(0, organ, SectorId(0, 0), Vec2(50, 50))
    val moved = o.step(100).pos
    assertEqualsDouble(math.hypot(moved.x - 50, moved.y - 50), 1.5, 1e-9)

  test("organism bounces off the sector wall and stays inside"):
    val east = MovementOrgan(Polynomial(Vector(0.0, 0.0)), period = 10, span = 1.0, speed = 1.0)
    val o = Organism(0, east, SectorId(0, 0), Vec2(99.5, 50)).step(100)
    assertEqualsDouble(o.pos.x, 99.5, 1e-9)
    assert(o.mirrorX)
    assert(o.displacement.x < 0)

  test("random world keeps every organism inside its sector for many ticks"):
    val cfg = AppConfig.load()
    val start = Genesis.randomWorld(WorldConfig(3, 2, 100), cfg.genesis, count = 100, seed = 7)
    val end = Iterator.iterate(start)(_.step).drop(5000).next()
    assertEquals(end.tick, 5000L)
    end.organisms.foreach { o =>
      assert(o.pos.x >= 0 && o.pos.x <= 100 && o.pos.y >= 0 && o.pos.y <= 100, o)
    }

  test("default config file loads"):
    val cfg = AppConfig.load()
    assertEquals(cfg.genesis.families.toSet, Family.values.toSet)
    assertEquals(cfg.genesis.functions.cubic.size, 3)

  test("latex: powers, signs, unit coefficients and zero terms"):
    assertEquals(Polynomial(Vector(1.0, -3.0, 1.0)).latex, "x^{2} - 3x + 1")
    assertEquals(Polynomial(Vector(0.0, 0.0, 0.0, -1.0)).latex, "-x^{3}")
    assertEquals(Polynomial(Vector(2.5, 0.001, 1.5)).latex, "1.5x^{2} + 2.5")
    assertEquals(Polynomial(Vector(0.0, 0.0)).latex, "0")

  test("latex: transcendental families"):
    assertEquals(Logarithmic(-2, 0.5, 1).latex, "-2\\ln\\left(1 + 0.5|x|\\right) + 1")
    assertEquals(Sinusoidal(1, 3, -1.25, 0).latex, "\\sin\\left(3x - 1.25\\right)")
    assertEquals(Exponential(0.5, -0.8, 2).latex, "0.5e^{-0.8x} + 2")
