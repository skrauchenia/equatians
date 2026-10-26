package equatians.ui

import equatians.model.*

import java.awt.*
import java.awt.event.{MouseAdapter, MouseEvent, MouseWheelEvent}
import java.awt.geom.{Ellipse2D, Line2D, Path2D, Rectangle2D}
import javax.swing.JPanel
import scala.collection.mutable

/** Цвета визуализации. Цвета видов подобраны так, чтобы различаться и при дальтонизме. */
object Palette:
  val Background: Color = Color(14, 15, 19)
  val Sector: Color = Color(26, 28, 35)
  val SectorBorder: Color = Color(70, 74, 88)
  val SectorHighlight: Color = Color(120, 170, 255)
  val Grid: Color = Color(38, 41, 50)
  val Label: Color = Color(120, 125, 140)
  val Chase: Color = Color(235, 80, 70)
  val Flee: Color = Color(120, 170, 255)
  val Dead: Color = Color(150, 150, 160)
  val Carrion: Color = Color(170, 140, 110)
  val Scavenge: Color = Color(200, 170, 120)
  val Mate: Color = Color(255, 140, 200)

  val byFamily: Map[Family, Color] = Map(
    Family.Constant    -> Color(90, 190, 90),
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

  /** Вызывается при клике по пустому месту сектора. */
  var onSectorClick: SectorId => Unit = _ => ()

  /** Сектор, открытый в окне статистики — его рамка подсвечивается. */
  var highlightedSector: Option[SectorId] = None

  /** Масштаб и сдвиг карты: колесо мыши — масштаб вокруг курсора, перетаскивание — сдвиг. */
  val viewport: Viewport = Viewport()

  /** Вызывается после изменения масштаба или сдвига — например, чтобы обновить подпись масштаба. */
  var onViewChange: () => Unit = () => ()

  setBackground(Palette.Background)
  setPreferredSize(Dimension(960, 660))

  private val mouse = new MouseAdapter:
    private var last = (0, 0)
    private var dragged = false

    override def mousePressed(e: MouseEvent): Unit =
      last = (e.getX, e.getY)
      dragged = false

    override def mouseDragged(e: MouseEvent): Unit =
      val (dx, dy) = (e.getX - last._1, e.getY - last._2)
      if dragged || math.abs(dx) + math.abs(dy) > 3 then
        dragged = true
        setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR))
        viewport.panBy(dx, dy)
        last = (e.getX, e.getY)
        viewChanged()

    override def mouseReleased(e: MouseEvent): Unit =
      setCursor(Cursor.getDefaultCursor)

    override def mouseClicked(e: MouseEvent): Unit =
      if !dragged then
        val (x, y) = viewport.toView(e.getX, e.getY)
        val picked = organismAt(x, y)
        selectedId = picked.map(_.id)
        onSelect(picked)
        if picked.isEmpty then sectorAt(x, y).foreach(onSectorClick)
        repaint()

    override def mouseWheelMoved(e: MouseWheelEvent): Unit =
      viewport.zoomAt(math.pow(1.15, -e.getPreciseWheelRotation), e.getX, e.getY)
      viewChanged()

  addMouseListener(mouse)
  addMouseMotionListener(mouse)
  addMouseWheelListener(mouse)

  /** Приближает (factor > 1) или отдаляет карту вокруг центра окна. */
  def zoomBy(factor: Double): Unit =
    viewport.zoomAt(factor, getWidth / 2.0, getHeight / 2.0)
    viewChanged()

  /** Вписывает весь мир в окно. */
  def fitView(): Unit =
    viewport.reset()
    viewChanged()

  private def viewChanged(): Unit =
    onViewChange()
    repaint()

  /** Показывает новое состояние мира и дописывает текущие позиции в следы.
    * У корма следов нет: он ползёт по прямой, а таких организмов в мире больше всего — их следы дорого рисовать.
    */
  def show(w: World): Unit =
    world = w
    val alive = w.organisms.iterator.map(_.id).toSet
    trails.filterInPlace((id, _) => alive(id))
    for o <- w.organisms if !o.forage do
      val trail = trails.getOrElseUpdate(o.id, mutable.ArrayDeque.empty)
      trail.append(o.pos)
      if trail.size > trailLength then trail.removeHead()
    repaint()

  /** Показывает другой мир с нуля: стирает следы и снимает выделение. */
  def reset(w: World): Unit =
    trails.clear()
    selectedId = None
    show(w)

  /** Выбранный организм в текущем состоянии мира — живой или из недавно погибших. */
  def selected: Option[Organism] = selectedId.flatMap(world.find)

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

  /** Сектор под точкой вписанного вида, если она попала в сектор, а не в промежуток между ними. */
  private def sectorAt(x: Double, y: Double): Option[SectorId] =
    val l = currentLayout
    world.config.sectors.find(s => l.sectorRect(s).contains(x, y))

  /** Ближайший к точке вписанного вида организм, по которому попал клик (с запасом в 8 экранных пикселей). */
  private def organismAt(x: Double, y: Double): Option[Organism] =
    val l = currentLayout
    world.organisms
      .map { o =>
        val (sx, sy) = l.toScreen(o.sector, o.pos)
        o -> (math.hypot(sx - x, sy - y) - screenRadius(l, o))
      }
      .filter(_._2 < 8 / viewport.zoom)
      .minByOption(_._2)
      .map(_._1)

  override def paintComponent(g0: Graphics): Unit =
    super.paintComponent(g0)
    val g = g0.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    // Всё ниже рисуется во «вписанном» виде, а масштаб и сдвиг применяются одним преобразованием.
    g.transform(viewport.transform)
    val l = currentLayout
    world.config.sectors.foreach(drawSector(g, l, _))
    world.corpses.foreach(drawCarrion(g, l, _))
    if showTrails then world.organisms.foreach(drawTrail(g, l, _))
    world.organisms.foreach(drawIntent(g, l, _))
    world.organisms.foreach(drawOrganism(g, l, _))
    selected.filterNot(_.alive).foreach(drawCorpse(g, l, _))

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
    val highlighted = highlightedSector.contains(s)
    g.setColor(if highlighted then Palette.SectorHighlight else Palette.SectorBorder)
    g.setStroke(BasicStroke(if highlighted then 3f else 1.5f))
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

  /** Радиус организма на экране: по размеру тела, но не меньше 2.5 пикселя, чтобы было видно. */
  private def screenRadius(l: Layout, o: Organism): Double = (o.radius(world.rules) * l.scale).max(2.5)

  /** Линия намерения: красная к цели преследования (толстая — сразу после удара),
    * голубая пунктирная — от того, от кого организм убегает.
    */
  private def drawIntent(g: Graphics2D, l: Layout, o: Organism): Unit =
    val (x, y) = l.toScreen(o.sector, o.pos)
    def lineToPoint(sector: SectorId, p: Vec2, color: Color, stroke: BasicStroke): Unit =
      val (tx, ty) = l.toScreen(sector, p)
      g.setColor(color)
      g.setStroke(stroke)
      g.draw(Line2D.Double(x, y, tx, ty))
    def lineTo(id: Int, color: Color, stroke: BasicStroke): Unit =
      world.organisms.find(_.id == id).foreach(other => lineToPoint(other.sector, other.pos, color, stroke))
    o.behaviour match
      case Behaviour.Chase(t) =>
        val justHit = world.tick - o.lastAttackTick <= 3
        val stroke = if justHit then BasicStroke(3f) else BasicStroke(1f)
        val c = Palette.Chase
        lineTo(t, if justHit then c else Color(c.getRed, c.getGreen, c.getBlue, 140), stroke)
      case Behaviour.Flee(f) =>
        val dash = BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(4f, 4f), 0f)
        val c = Palette.Flee
        lineTo(f, Color(c.getRed, c.getGreen, c.getBlue, 140), dash)
      case Behaviour.Scavenge(c) =>
        world.corpses.find(_.id == c).foreach(corpse => lineToPoint(corpse.sector, corpse.pos, Palette.Scavenge, BasicStroke(1.5f)))
      case Behaviour.Mate(p) =>
        lineTo(p, Palette.Mate, BasicStroke(1.5f))
      case Behaviour.Cruise => ()

  /** Организм — круг цвета вида (размер по весовой категории) и чёрточка по курсу; выбранный обведён.
    * Корм рисуется одним кругом без чёрточки — его много, и курс у него постоянный.
    */
  private def drawOrganism(g: Graphics2D, l: Layout, o: Organism): Unit =
    val (x, y) = l.toScreen(o.sector, o.pos)
    val r = screenRadius(l, o)
    val h = o.heading
    g.setColor(Palette.byFamily(o.family))
    g.fill(Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))
    g.setStroke(BasicStroke(1.5f))
    if !o.forage then g.draw(Line2D.Double(x, y, x + (r + 5) * math.cos(h), y - (r + 5) * math.sin(h)))
    if selectedId.contains(o.id) then
      g.setColor(Color.WHITE)
      g.draw(Ellipse2D.Double(x - r - 4, y - r - 4, 2 * r + 8, 2 * r + 8))

  /** Падаль — бурое кольцо с полупрозрачной заливкой цвета вида; площадь пропорциональна оставшейся еде. */
  private def drawCarrion(g: Graphics2D, l: Layout, c: Corpse): Unit =
    val (x, y) = l.toScreen(c.sector, c.pos)
    val r = (world.rules.sizePerSqrtHealth * math.sqrt(c.food) * l.scale).max(2.5)
    val tint = Palette.byFamily(c.species)
    g.setColor(Color(tint.getRed, tint.getGreen, tint.getBlue, 60))
    g.fill(Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))
    g.setColor(Palette.Carrion)
    g.setStroke(BasicStroke(1f))
    g.draw(Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))

  /** Крестик на месте гибели выбранного организма. */
  private def drawCorpse(g: Graphics2D, l: Layout, o: Organism): Unit =
    val (x, y) = l.toScreen(o.sector, o.pos)
    g.setColor(Palette.Dead)
    g.setStroke(BasicStroke(2f))
    g.draw(Line2D.Double(x - 6, y - 6, x + 6, y + 6))
    g.draw(Line2D.Double(x - 6, y + 6, x + 6, y - 6))
