package equatians.model

import scala.math.BigDecimal.RoundingMode

/** Семейство функции. Пока что именно семейство определяет «вид» организма. */
enum Family(val title: String):
  case Linear      extends Family("линейная")
  case Quadratic   extends Family("квадратичная")
  case Cubic       extends Family("кубическая")
  case Logarithmic extends Family("логарифмическая")
  case Sinusoidal  extends Family("синусоидальная")
  case Exponential extends Family("экспоненциальная")

/** Функция f: ℝ → ℝ — «генетический материал» органа. */
sealed trait MathFunction:
  /** Значение функции в точке `x`. */
  def apply(x: Double): Double

  /** Семейство, к которому относится функция. */
  def family: Family

  /** Правая часть формулы в нотации LaTeX, например `-0.5x^{2} + 3`. */
  def latex: String

object MathFunction:

  /** a0 + a1·x + a2·x² + ...; коэффициенты по возрастанию степени. */
  final case class Polynomial(coeffs: Vector[Double]) extends MathFunction:
    require(coeffs.size >= 2 && coeffs.size <= 4, "поддерживаются степени 1..3")

    /** Вычисление по схеме Горнера. */
    def apply(x: Double): Double = coeffs.foldRight(0.0)((c, acc) => acc * x + c)

    /** Семейство определяется степенью полинома. */
    def family: Family = coeffs.size match
      case 2 => Family.Linear
      case 3 => Family.Quadratic
      case _ => Family.Cubic

    def latex: String =
      Latex.sum(coeffs.zipWithIndex.reverse.map {
        case (c, 0) => c -> ""
        case (c, 1) => c -> "x"
        case (c, i) => c -> s"x^{$i}"
      })

  /** a·ln(1 + b·|x|) + c. Модуль делает функцию определённой на всей оси. */
  final case class Logarithmic(a: Double, b: Double, c: Double) extends MathFunction:
    def apply(x: Double): Double = a * math.log1p(b * math.abs(x)) + c
    def family: Family = Family.Logarithmic
    def latex: String =
      val arg = Latex.sum(Seq(1.0 -> "", b -> "|x|"))
      Latex.sum(Seq(a -> s"\\ln\\left($arg\\right)", c -> ""))

  /** a·sin(k·x + φ) + c */
  final case class Sinusoidal(a: Double, k: Double, phi: Double, c: Double) extends MathFunction:
    def apply(x: Double): Double = a * math.sin(k * x + phi) + c
    def family: Family = Family.Sinusoidal
    def latex: String =
      val arg = Latex.sum(Seq(k -> "x", phi -> ""))
      Latex.sum(Seq(a -> s"\\sin\\left($arg\\right)", c -> ""))

  /** a·e^(b·x) + c */
  final case class Exponential(a: Double, b: Double, c: Double) extends MathFunction:
    def apply(x: Double): Double = a * math.exp(b * x) + c
    def family: Family = Family.Exponential
    def latex: String =
      Latex.sum(Seq(a -> s"e^{${Latex.sum(Seq(b -> "x"))}}", c -> ""))

/** Сборка формул в LaTeX с обычными для математики упрощениями. */
object Latex:

  /** Число с точностью до двух знаков без хвостовых нулей: 1.50 → `1.5`, 2.00 → `2`. */
  def number(v: Double): String =
    BigDecimal(v).setScale(2, RoundingMode.HALF_UP).bigDecimal.stripTrailingZeros.toPlainString

  /** Сумма слагаемых «коэффициент · тело», где пустое тело означает константу.
    *
    * Слагаемые, округляющиеся до нуля, опускаются; коэффициент 1 не пишется;
    * знак выносится в операцию: `(1, x²), (-3, x), (1, "")` → `x^{2} - 3x + 1`.
    */
  def sum(terms: Seq[(Double, String)]): String =
    val parts = terms.flatMap { (c, body) =>
      val magnitude = number(c.abs)
      val text =
        if body.isEmpty then magnitude
        else if magnitude == "1" then body
        else magnitude + body
      Option.when(magnitude != "0")((c < 0, text))
    }
    if parts.isEmpty then "0"
    else
      parts.zipWithIndex.map {
        case ((negative, t), 0) => if negative then s"-$t" else t
        case ((negative, t), _) => if negative then s" - $t" else s" + $t"
      }.mkString
