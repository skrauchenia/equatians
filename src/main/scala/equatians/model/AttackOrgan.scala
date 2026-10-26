package equatians.model

/** Орган атаки. Одна функция D задаёт сразу три характеристики удара.
  *
  * Аргумент — нормированная дистанция u = d / maxReach ∈ [0, 1].
  *  - урон на дистанции d равен D(u) (отрицательные значения считаются нулём);
  *  - дальность — первый ноль D на (0, 1], умноженный на maxReach; если нуля нет — maxReach;
  *  - стоимость удара в сытости = costFactor · ∫₀^дальность D, т.е. «площадь» оружия:
  *    сильный и дальнобойный удар дороже слабого и короткого.
  *
  * Генезис гарантирует D(0) > 0, иначе организм не смог бы ударить даже в упор.
  */
final case class AttackOrgan(g: MathFunction, maxReach: Double, costFactor: Double):

  /** Доля maxReach, на которой урон впервые падает до нуля. */
  val reachFraction: Double = AttackOrgan.firstRoot(g)

  /** Дальность удара в единицах мира. */
  val range: Double = maxReach * reachFraction

  /** Стоимость одного удара в единицах сытости. */
  val cost: Double = costFactor * maxReach * AttackOrgan.integral(g, reachFraction)

  /** Урон по цели на дистанции `distance`; 0 за пределами дальности. */
  def damage(distance: Double): Double =
    if distance > range then 0.0 else AttackOrgan.positive(g(distance / maxReach))

  /** Формула урона в LaTeX. */
  def damageLatex: String = s"D(u) = ${g.latex}"

  /** Формула аргумента функции урона в LaTeX. */
  def argumentLatex: String = s"u = \\frac{d}{${Latex.number(maxReach)}}"

object AttackOrgan:
  private val Samples = 1000

  /** Значение, обрезанное снизу нулём; бесконечности и NaN тоже дают 0. */
  def positive(v: Double): Double = if v.isFinite && v > 0 then v else 0.0

  /** Первый ноль `positive(g)` на (0, 1]: сначала сканирование сеткой, потом бисекция. */
  def firstRoot(g: MathFunction): Double =
    if positive(g(0)) == 0 then 0.0
    else
      (1 to Samples).find(i => positive(g(i.toDouble / Samples)) == 0) match
        case None => 1.0
        case Some(i) =>
          var lo = (i - 1).toDouble / Samples
          var hi = i.toDouble / Samples
          for _ <- 0 until 40 do
            val mid = (lo + hi) / 2
            if positive(g(mid)) > 0 then lo = mid else hi = mid
          lo

  /** ∫₀^upTo positive(g(u)) du методом трапеций. */
  def integral(g: MathFunction, upTo: Double): Double =
    if upTo <= 0 then 0.0
    else
      val n = 400
      val h = upTo / n
      val inner = (1 until n).map(i => positive(g(i * h))).sum
      h * (inner + (positive(g(0)) + positive(g(upTo))) / 2)
