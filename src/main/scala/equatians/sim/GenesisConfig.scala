package equatians.sim

import equatians.model.Family

import scala.util.Random

/** Замкнутый диапазон [min, max] для случайного выбора гена. */
final case class GeneRange(min: Double, max: Double):
  require(min <= max, s"пустой диапазон [$min, $max]")

  /** Равномерно выбирает число из диапазона. */
  def sample(rnd: Random): Double = min + rnd.nextDouble() * (max - min)

/** Диапазоны генов органа движения. */
final case class MovementRanges(period: GeneRange, span: GeneRange, speed: GeneRange)

/** Диапазоны коэффициентов для каждого семейства функций.
  *
  * Для полиномов — диапазоны a1..an по возрастанию степени (свободный член a0 не настраивается).
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
  final case class Logarithmic(a: GeneRange, b: GeneRange)
  final case class Sinusoidal(a: GeneRange, k: GeneRange)
  final case class Exponential(a: GeneRange, b: GeneRange)

/** Параметры начального заселения мира. */
final case class GenesisConfig(families: Vector[Family], movement: MovementRanges, functions: FunctionRanges):
  require(families.nonEmpty, "нужно хотя бы одно семейство в genesis.families")
