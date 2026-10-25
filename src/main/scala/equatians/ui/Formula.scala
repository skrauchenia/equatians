package equatians.ui

import org.scilab.forge.jlatexmath.{ParseException, TeXConstants, TeXFormula}

import java.awt.{Color, Font}
import javax.swing.{Icon, JLabel}

/** Отрисовка LaTeX-формул в Swing через jlatexmath. */
object Formula:

  /** Иконка с отрендеренной формулой; размер `size` — в пунктах. */
  def icon(latex: String, size: Float = 17f, color: Color = Color.BLACK): Icon =
    val icon = TeXFormula(latex).createTeXIcon(TeXConstants.STYLE_DISPLAY, size)
    icon.setForeground(color)
    icon

  /** Ставит формулу в метку. Если LaTeX не разобрался — показывает исходник, а не падает. */
  def render(label: JLabel, latex: String): Unit =
    try
      label.setIcon(icon(latex))
      label.setText(null)
    catch
      case _: ParseException =>
        label.setIcon(null)
        label.setFont(Font(Font.MONOSPACED, Font.PLAIN, 12))
        label.setText(latex)
