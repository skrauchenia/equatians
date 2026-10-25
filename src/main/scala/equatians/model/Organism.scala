package equatians.model

/** Организм.
  *
  * `pos` — локальные декартовы координаты внутри сектора: (0, 0) — левый нижний угол,
  * ось y направлена вверх. Переход в другой сектор пока не реализован: граница сектора —
  * стена, от которой организм отражается. Отражение запоминается флагами `mirrorX`/`mirrorY`,
  * потому что направление задаёт функция, а не текущая скорость.
  */
final case class Organism(
    id: Int,
    movement: MovementOrgan,
    sector: SectorId,
    pos: Vec2,
    age: Long = 0,
    mirrorX: Boolean = false,
    mirrorY: Boolean = false
):
  /** Вид организма — семейство функции его органа движения. */
  def family: Family = movement.f.family

  /** Текущее направление движения с учётом отражений от стен. */
  def heading: Double =
    val d = displacement
    math.atan2(d.y, d.x)

  /** Шаг, который организм сделает на текущем тике, с учётом отражений от стен. */
  def displacement: Vec2 =
    val d = movement.displacement(age)
    Vec2(if mirrorX then -d.x else d.x, if mirrorY then -d.y else d.y)

  /** Состояние через один тик. Если шаг выводит за границу сектора,
    * организм зеркально отражается от неё, а соответствующий флаг отражения переключается.
    */
  def step(sectorSize: Double): Organism =
    val target = pos + displacement
    val (x, flipX) = reflect(target.x, sectorSize)
    val (y, flipY) = reflect(target.y, sectorSize)
    copy(
      pos = Vec2(x, y),
      age = age + 1,
      mirrorX = mirrorX ^ flipX,
      mirrorY = mirrorY ^ flipY
    )

  /** Отражает координату `v` в отрезок [0, size]; второй элемент — было ли отражение. */
  private def reflect(v: Double, size: Double): (Double, Boolean) =
    if v < 0 then (math.min(-v, size), true)
    else if v > size then (math.max(2 * size - v, 0.0), true)
    else (v, false)
