package equatians.ui

import equatians.model.*

import java.awt.*
import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.geom.{Ellipse2D, Line2D, Path2D, Rectangle2D}
import javax.swing.JPanel
import scala.collection.mutable

/** Цвета визуализации. Цвета видов подобраны так, чтобы различаться и при дальтонизме. */
object Palette:
  val Background: Color = Color(14, 15, 19)
  val Sector: Color = Color(26, 28, 35)
  val SectorBorder: Color = Color(70, 74, 88)
  val Grid: Color = Color(38, 41, 50)
  val Label: Color = Color(120, 125, 140)

  val byFamily: Map[Family, Color] = Map(
    Family.Linear      -> Color(86, 180, 233),
    Family.Quadratic   -> Color(230, 159, 0),
    Family.Cubic       -> Color(213, 94, 0),
    Family.Logarithmic -> Color(0, 158, 115),
    Family.Sinusoidal  -> Color(204, 121, 167),
    Family.Exponential -> Color(240, 228, 66)
  )

/** Рисует сектора мира и организмы в них. Следы хранятся только здесь, в модели их нет. */
final class WorldView(initial: World, trailLength: Int) extends JPanel:
  private var world: World = initial
  private val trails = mutable.Map.empty[Int, mutable.ArrayDeque[Vec2]]

  /** Рисовать ли следы организмов. */
  var showTrails: Boolean = true

  /** id выбранного кликом организма. */
  var selectedId: Option[Int] = None

  /** Вызывается после клика: с организмом под курсором или `None`, если клик мимо. */
  var onSelect: Option[Organism] => Unit = _ => ()

  setBackground(Palette.Background)
  setPreferredSize(Dimension(960, 660))

  addMouseListener(new MouseAdapter:
    override def mouseClicked(e: MouseEvent): Unit =
      val picked = organismAt(e.getX, e.getY)
      selectedId = picked.map(_.id)
      onSelect(picked)
      repaint()
  )

  /** Показывает новое состояние мира и дописывает текущие позиции в следы. */
  def show(w: World): Unit =
    world = w
    for o <- w.organisms do
      val trail = trails.getOrElseUpdate(o.id, mutable.ArrayDeque.empty)
      trail.append(o.pos)
      if trail.size > trailLength then trail.removeHead()
    repaint()

  /** Показывает другой мир с нуля: стирает следы и снимает выделение. */
  def reset(w: World): Unit =
    trails.clear()
    selectedId = None
    show(w)

  /** Выбранный организм в текущем состоянии мира. */
  def selected: Option[Organism] = selectedId.flatMap(id => world.organisms.find(_.id == id))

  /** Геометрия: где на экране сектор и как перевести локальные координаты в пиксели. */
  private final case class Layout(cell: Double, gap: Double, ox: Double, oy: Double):
    private val cfg = world.config
    val scale: Double = cell / cfg.sectorSize

    /** Прямоугольник сектора на экране. */
    def sectorRect(s: SectorId): Rectangle2D =
      Rectangle2D.Double(left(s), top(s), cell, cell)

    /** Локальная точка сектора → пиксели. Ось y переворачивается: в модели она смотрит вверх. */
    def toScreen(s: SectorId, p: Vec2): (Double, Double) =
      (left(s) + p.x * scale, top(s) + (cfg.sectorSize - p.y) * scale)

    private def left(s: SectorId) = ox + s.col * (cell + gap)
    private def top(s: SectorId) = oy + (cfg.rows - 1 - s.row) * (cell + gap)

  /** Вписывает сетку секторов в текущий размер панели, сохраняя их квадратными. */
  private def currentLayout: Layout =
    val cfg = world.config
    val gap = 18.0
    val margin = 20.0
    val cell = math.min(
      (getWidth - 2 * margin - gap * (cfg.cols - 1)) / cfg.cols,
      (getHeight - 2 * margin - gap * (cfg.rows - 1)) / cfg.rows
    ).max(10)
    val totalW = cell * cfg.cols + gap * (cfg.cols - 1)
    val totalH = cell * cfg.rows + gap * (cfg.rows - 1)
    Layout(cell, gap, (getWidth - totalW) / 2, (getHeight - totalH) / 2)

  /** Ближайший к точке экрана организм в радиусе 12 пикселей. */
  private def organismAt(x: Int, y: Int): Option[Organism] =
    val l = currentLayout
    world.organisms
      .map { o =>
        val (sx, sy) = l.toScreen(o.sector, o.pos)
        o -> math.hypot(sx - x, sy - y)
      }
      .filter(_._2 < 12)
      .minByOption(_._2)
      .map(_._1)

  override def paintComponent(g0: Graphics): Unit =
    super.paintComponent(g0)
    val g = g0.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val l = currentLayout
    world.config.sectors.foreach(drawSector(g, l, _))
    if showTrails then world.organisms.foreach(drawTrail(g, l, _))
    world.organisms.foreach(drawOrganism(g, l, _))

  /** Фон сектора, сетка по четвертям, рамка и подпись с координатами сектора. */
  private def drawSector(g: Graphics2D, l: Layout, s: SectorId): Unit =
    val r = l.sectorRect(s)
    g.setColor(Palette.Sector)
    g.fill(r)
    g.setColor(Palette.Grid)
    g.setStroke(BasicStroke(1f))
    val size = world.config.sectorSize
    for i <- 1 until 4 do
      val (x, _) = l.toScreen(s, Vec2(size * i / 4, 0))
      val (_, y) = l.toScreen(s, Vec2(0, size * i / 4))
      g.draw(Line2D.Double(x, r.getMinY, x, r.getMaxY))
      g.draw(Line2D.Double(r.getMinX, y, r.getMaxX, y))
    g.setColor(Palette.SectorBorder)
    g.setStroke(BasicStroke(1.5f))
    g.draw(r)
    g.setColor(Palette.Label)
    g.setFont(g.getFont.deriveFont(11f))
    g.drawString(s"(${s.col}, ${s.row})", (r.getMinX + 6).toFloat, (r.getMinY + 15).toFloat)

  /** След организма — ломаная по его последним позициям; у выбранного ярче. */
  private def drawTrail(g: Graphics2D, l: Layout, o: Organism): Unit =
    trails.get(o.id).filter(_.size > 1).foreach { trail =>
      val path = Path2D.Double()
      val (x0, y0) = l.toScreen(o.sector, trail.head)
      path.moveTo(x0, y0)
      trail.iterator.drop(1).foreach { p =>
        val (x, y) = l.toScreen(o.sector, p)
        path.lineTo(x, y)
      }
      val c = Palette.byFamily(o.family)
      val alpha = if selectedId.contains(o.id) then 200 else 70
      g.setColor(Color(c.getRed, c.getGreen, c.getBlue, alpha))
      g.setStroke(BasicStroke(1.2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      g.draw(path)
    }

  /** Организм — точка цвета вида и чёрточка по курсу; выбранный обведён. */
  private def drawOrganism(g: Graphics2D, l: Layout, o: Organism): Unit =
    val (x, y) = l.toScreen(o.sector, o.pos)
    val r = 4.0
    val h = o.heading
    g.setColor(Palette.byFamily(o.family))
    g.fill(Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))
    g.setStroke(BasicStroke(1.5f))
    g.draw(Line2D.Double(x, y, x + 9 * math.cos(h), y - 9 * math.sin(h)))
    if selectedId.contains(o.id) then
      g.setColor(Color.WHITE)
      g.draw(Ellipse2D.Double(x - r - 4, y - r - 4, 2 * r + 8, 2 * r + 8))
