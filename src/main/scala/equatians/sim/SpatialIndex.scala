package equatians.sim

import equatians.model.Vec2

/** Объекты одного сектора, разложенные по квадратным ячейкам со стороной `cell` — для поиска ближайшего.
  *
  * Поиск в радиусе не больше `cell` смотрит только 3×3 ячейки вокруг точки, а не весь сектор.
  * Результат такой же, как у перебора всех объектов по порядку: из равноудалённых выбирается
  * стоящий раньше в исходном векторе.
  *
  * @param items объекты в исходном порядке
  * @param pos   положение объекта
  * @param cell  сторона ячейки; поиск дальше неё не поддерживается
  */
final class SpatialIndex[A](items: Vector[A], pos: A => Vec2, cell: Double):
  require(cell > 0, "сторона ячейки должна быть положительной")

  private val positions: Array[Vec2] = items.iterator.map(pos).toArray

  /** Ячейка → индексы объектов в ней по возрастанию. */
  private val cells: Map[(Int, Int), Array[Int]] =
    positions.indices.groupBy(i => key(positions(i))).view.mapValues(_.toArray.sorted).toMap

  private def key(p: Vec2): (Int, Int) = ((p.x / cell).floor.toInt, (p.y / cell).floor.toInt)

  /** Ближайший к `p` объект в радиусе `radius`, удовлетворяющий `accept`, и расстояние до него. */
  def nearest(p: Vec2, radius: Double, accept: A => Boolean = (_: A) => true): Option[(A, Double)] =
    require(radius <= cell, s"радиус поиска $radius больше ячейки индекса $cell")
    val (cx, cy) = key(p)
    val r2 = radius * radius
    var best = -1
    var bestD2 = Double.PositiveInfinity
    var dx = -1
    while dx <= 1 do
      var dy = -1
      while dy <= 1 do
        cells.get((cx + dx, cy + dy)).foreach { ids =>
          var k = 0
          while k < ids.length do
            val i = ids(k)
            val q = positions(i)
            val ex = q.x - p.x
            val ey = q.y - p.y
            val d2 = ex * ex + ey * ey
            if d2 <= r2 && (d2 < bestD2 || (d2 == bestD2 && i < best)) && accept(items(i)) then
              best = i
              bestD2 = d2
            k += 1
        }
        dy += 1
      dx += 1
    Option.when(best >= 0)(items(best) -> math.sqrt(bestD2))
