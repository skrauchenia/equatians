package equatians.ui

import java.awt.*
import javax.swing.JComponent

/** Горизонтальная шкала с подписью поверх — для здоровья, сытости и жира. */
final class StatBar(color: Color) extends JComponent:
  private var fraction = 0.0
  private var text = ""

  setPreferredSize(Dimension(320, 18))
  setMaximumSize(Dimension(Int.MaxValue, 18))

  /** Задаёт заполненность (0..1, обрезается) и подпись. */
  def set(fraction: Double, text: String): Unit =
    this.fraction = fraction.max(0).min(1)
    this.text = text
    repaint()

  override def paintComponent(g0: Graphics): Unit =
    val g = g0.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    val w = getWidth
    val h = getHeight
    g.setColor(Color(225, 225, 230))
    g.fillRect(0, 0, w, h)
    g.setColor(color)
    g.fillRect(0, 0, (w * fraction).round.toInt, h)
    g.setColor(Color(60, 60, 70))
    g.drawRect(0, 0, w - 1, h - 1)
    g.setFont(getFont.deriveFont(11f))
    g.setColor(Color.BLACK)
    g.drawString(text, 6, h - 5)
