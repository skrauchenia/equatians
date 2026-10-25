package equatians.model

/** Орган движения.
  *
  * Организм ходит как «черепашка»: каждый тик делает шаг длины `speed`
  * в направлении θ = f(x) радиан. Аргумент x — фаза жизненного цикла органа:
  * за `period` тиков он равномерно пробегает отрезок [0, span), потом начинается заново.
  *
  * Поэтому форма траектории — это прямое следствие формы функции:
  * линейная f даёт дуги окружностей, квадратичная — спирали Эйлера,
  * синус — «змейку», логарифм — быстрый разворот и почти прямой ход.
  */
final case class MovementOrgan(f: MathFunction, period: Int, span: Double, speed: Double):
  require(period > 0, "period должен быть положительным")

  /** Аргумент функции x для организма возраста `age` тиков. */
  def argument(age: Long): Double = (age % period).toDouble / period * span

  /** Курс θ = f(x) в радианах, без учёта отражений от стен. */
  def heading(age: Long): Double = f(argument(age))

  /** Шаг за тик: вектор длины `speed` по курсу. Если f(x) не конечно — организм стоит. */
  def displacement(age: Long): Vec2 =
    val theta = heading(age)
    if theta.isFinite then Vec2.polar(speed, theta) else Vec2.Zero

  /** Формула курса в LaTeX: `\theta(x) = ...`. */
  def headingLatex: String = s"\\theta(x) = ${f.latex}"

  /** Формула аргумента в LaTeX, где t — возраст организма в тиках. */
  def argumentLatex: String =
    s"x = \\frac{t \\;\\mathrm{mod}\\; $period}{$period} \\cdot ${Latex.number(span)}"
