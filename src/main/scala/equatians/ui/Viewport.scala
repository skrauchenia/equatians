package equatians.ui

import java.awt.geom.AffineTransform

/** Масштаб и сдвиг карты поверх вписанного в окно вида.
  *
  * Точка вписанного вида p попадает на экран в zoom · p + pan. При zoom = 1 и pan = 0 карта вписана в окно.
  */
final class Viewport(minZoom: Double = 0.3, maxZoom: Double = 40):
  private var z = 1.0
  private var px = 0.0
  private var py = 0.0

  /** Текущий масштаб: 1 — вписано в окно. */
  def zoom: Double = z

  /** Экранная точка → точка вписанного вида. */
  def toView(x: Double, y: Double): (Double, Double) = ((x - px) / z, (y - py) / z)

  /** Меняет масштаб в `factor` раз, оставляя на месте то, что под экранной точкой (x, y). */
  def zoomAt(factor: Double, x: Double, y: Double): Unit =
    val (vx, vy) = toView(x, y)
    z = (z * factor).max(minZoom).min(maxZoom)
    px = x - vx * z
    py = y - vy * z

  /** Сдвигает карту на (dx, dy) экранных пикселей. */
  def panBy(dx: Double, dy: Double): Unit =
    px += dx
    py += dy

  /** Возвращает вписанный вид. */
  def reset(): Unit =
    z = 1.0
    px = 0.0
    py = 0.0

  /** Преобразование для `Graphics2D`: из вписанного вида на экран. */
  def transform: AffineTransform = AffineTransform(z, 0, 0, z, px, py)
