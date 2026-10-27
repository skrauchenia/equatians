package equatians.model

/** Двумерный вектор / точка в декартовых координатах. */
final case class Vec2(x: Double, y: Double):
  /** Покомпонентная сумма. */
  def +(o: Vec2): Vec2 = Vec2(x + o.x, y + o.y)

  /** Покомпонентная разность. */
  def -(o: Vec2): Vec2 = Vec2(x - o.x, y - o.y)

  /** Умножение на скаляр. */
  def *(k: Double): Vec2 = Vec2(x * k, y * k)

  /** Евклидова длина. Через `sqrt`, а не `math.hypot`: тот в разы медленнее, а переполнение здесь невозможно. */
  def length: Double = math.sqrt(x * x + y * y)

object Vec2:
  val Zero: Vec2 = Vec2(0, 0)

  /** Вектор заданной длины, повёрнутый на `angle` радиан от оси x против часовой стрелки. */
  def polar(length: Double, angle: Double): Vec2 =
    Vec2(length * math.cos(angle), length * math.sin(angle))
