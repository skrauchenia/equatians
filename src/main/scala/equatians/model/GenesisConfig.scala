package equatians.model

import scala.util.Random

/** Замкнутый диапазон [min, max] для случайного выбора гена. */
final case class GeneRange(min: Double, max: Double):
  require(min <= max, s"пустой диапазон [$min, $max]")

  /** Равномерно выбирает число из диапазона. */
  def sample(rnd: Random): Double = min + rnd.nextDouble() * (max - min)

/** Диапазоны коэффициентов для каждого семейства функций.
  *
  * Для полиномов — диапазоны a0..an по возрастанию степени.
  */
final case class FunctionRanges(
    linear: Vector[GeneRange],
    quadratic: Vector[GeneRange],
    cubic: Vector[GeneRange],
    logarithmic: FunctionRanges.Logarithmic,
    sinusoidal: FunctionRanges.Sinusoidal,
    exponential: FunctionRanges.Exponential
)

object FunctionRanges:
  /** a·ln(1 + b·|x|) + c */
  final case class Logarithmic(a: GeneRange, b: GeneRange, c: GeneRange)
  /** a·sin(k·x + φ) + c */
  final case class Sinusoidal(a: GeneRange, k: GeneRange, phi: GeneRange, c: GeneRange)
  /** a·e^(b·x) + c */
  final case class Exponential(a: GeneRange, b: GeneRange, c: GeneRange)

/** Гены органа движения. `families` задаёт, какие виды вообще появляются в мире. */
final case class MovementGenesis(
    families: Vector[Family],
    period: GeneRange,
    span: GeneRange,
    speed: GeneRange,
    functions: FunctionRanges
):
  require(families.nonEmpty, "нужно хотя бы одно семейство в genesis.movement.families")

/** Гены органа атаки. */
final case class AttackGenesis(families: Vector[Family], functions: FunctionRanges):
  require(families.nonEmpty, "нужно хотя бы одно семейство в genesis.attack.families")

/** Параметры начального заселения мира. Организмы рождаются с полным здоровьем и 100% сытостью.
  *
  * @param health  максимальное здоровье
  * @param reserve начальный жир как доля от его максимума
  * @param litterSize ген размера помёта (округляется до целого)
  */
final case class GenesisConfig(
    health: GeneRange,
    reserve: GeneRange,
    litterSize: GeneRange,
    movement: MovementGenesis,
    attack: AttackGenesis
)
