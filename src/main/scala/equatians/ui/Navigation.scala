package equatians.ui

/** История переходов как в браузере: «назад», «вперёд», переход с отбрасыванием того, что было впереди. */
final class Navigation[A]:
  private var items: Vector[A] = Vector.empty
  private var position = -1

  /** Начинает историю заново с `a`. */
  def reset(a: A): Unit =
    items = Vector(a)
    position = 0

  /** Переход к `a`; всё, что было «впереди», забывается. Повторный переход к текущему ничего не меняет. */
  def go(a: A): Unit =
    if !current.contains(a) then
      items = items.take(position + 1) :+ a
      position = items.size - 1

  /** Шаг назад (−1) или вперёд (+1); `None`, если идти некуда. */
  def step(delta: Int): Option[A] =
    val target = position + delta
    Option.when(target >= 0 && target < items.size) {
      position = target
      items(position)
    }

  def current: Option[A] = items.lift(position)
  def first: Option[A] = items.headOption
  def canBack: Boolean = position > 0
  def canForward: Boolean = position < items.size - 1

  /** Вся история и индекс текущего элемента — для строки «путь». */
  def trail: (Vector[A], Int) = (items, position)
