package equatians.ui

import equatians.model.*
import equatians.sim.Genealogy

import java.awt.*
import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.geom.{Ellipse2D, Line2D}
import javax.swing.JPanel

/** Дерево предков: столбцы слева направо — организм, родители, их родители и т.д.
  *
  * Узел — кружок цвета вида; у живых тёмная обводка, у умерших — блёклая заливка. Если выбран ген,
  * у каждого предка подписано его значение; путь наследования подсвечивается золотым, узлы на пути,
  * где ген мутировал, — красным кольцом с подписью «было → стало». Мутации гена у предков вне пути
  * подписаны бледно-красным. Клик по узлу делает его центральным, наведение показывает подсказку.
  *
  * Корм не раскрывается: над ним обычно сотни почти одинаковых клонов, поэтому у узла-корма вместо предков
  * подпись «+N поколений корма» (и сколько раз выбранный ген мутировал в этой цепочке).
  * У организмов, созданных миром (стартовое население и самопоявившийся корм), предков нет —
  * они подписаны «создан миром», чтобы было видно, где линия обрывается.
  */
final class LineageTreePanel extends JPanel:
  private val ColumnWidth = 150
  private val RowHeight = 46
  private val Radius = 8.0
  private val MaxPerLayer = 40
  private val Margin = 30

  private var lineage: Map[Int, LineageRecord] = Map.empty
  private var alive: Set[Int] = Set.empty
  private var layers: Vector[Vector[LineageRecord]] = Vector.empty
  private var hidden: Vector[Int] = Vector.empty
  private var positions: Map[Int, (Double, Double)] = Map.empty
  private var trace: Vector[Genealogy.TraceStep] = Vector.empty
  private var forageChains: Map[Int, Vector[LineageRecord]] = Map.empty
  private var gene: Option[String] = None

  /** Вызывается при клике по узлу. */
  var onFocus: Int => Unit = _ => ()

  setBackground(Color(250, 250, 252))
  setToolTipText("")
  addMouseListener(new MouseAdapter:
    override def mouseClicked(e: MouseEvent): Unit = nodeAt(e.getX, e.getY).foreach(onFocus)
  )

  /** Показывает предков организма `id` до глубины `depth` и (необязательно) выбранный ген и его путь. */
  def show(
      lineage: Map[Int, LineageRecord],
      alive: Set[Int],
      id: Int,
      depth: Int,
      gene: Option[String] = None,
      trace: Vector[Genealogy.TraceStep] = Vector.empty
  ): Unit =
    this.lineage = lineage
    this.alive = alive
    this.gene = gene
    this.trace = trace
    val all = Genealogy.layers(lineage, id, depth)
    layers = all.map(_.take(MaxPerLayer))
    forageChains = layers.flatten.filter(_.forage).map(r => r.id -> Genealogy.forageAncestors(lineage, r.id)).toMap
    hidden = all.map(l => (l.size - MaxPerLayer).max(0))
    positions = layers.zipWithIndex.flatMap { (layer, col) =>
      layer.zipWithIndex.map((r, row) => r.id -> (Margin + col * ColumnWidth.toDouble, Margin + row * RowHeight.toDouble))
    }.toMap
    val rows = layers.map(_.size + 1).maxOption.getOrElse(1)
    setPreferredSize(Dimension(Margin * 2 + layers.size * ColumnWidth + 60, Margin * 2 + rows * RowHeight))
    revalidate()
    repaint()

  private def nodeAt(x: Int, y: Int): Option[Int] =
    positions.collectFirst { case (id, (nx, ny)) if math.hypot(nx - x, ny - y) <= Radius + 3 => id }

  override def getToolTipText(e: MouseEvent): String =
    nodeAt(e.getX, e.getY).flatMap(lineage.get).fold(null) { r =>
      val status = r.died.fold(if alive(r.id) then "жив" else "—")(t => s"умер на тике $t" + r.cause.fold("")(c => s": $c"))
      val origin = r.parents match
        case Vector()  => "создан миром"
        case Vector(p) => s"отпочковался от #$p"
        case ps        => s"родители ${ps.map(p => s"#$p").mkString(" × ")}"
      val chain = forageChains.get(r.id).filter(_.nonEmpty).fold("") { c =>
        s"<br>над ним ${c.size} поколений корма, до #${c.last.id} (тик ${c.last.born})"
      }
      s"<html><b>#${r.id}</b> — ${if r.forage then "корм" else r.species.title}, поколение ${r.generation}<br>" +
        s"родился на тике ${r.born}, $origin<br>$status$chain</html>"
    }

  override def paintComponent(g0: Graphics): Unit =
    super.paintComponent(g0)
    val g = g0.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setFont(Font(Font.SANS_SERIF, Font.PLAIN, 11))
    val traceIds = trace.map(_.record.id).toSet
    val traceEdges = trace.flatMap(s => s.from.map(s.record.id -> _)).toSet

    // Подписи столбцов.
    g.setColor(Color(120, 120, 130))
    layers.indices.foreach { col =>
      val title = col match
        case 0 => "организм"
        case 1 => "родители"
        case 2 => "деды"
        case n => s"предки, поколение −$n"
      g.drawString(title, (Margin + col * ColumnWidth - 10).toFloat, 14f)
    }

    // Рёбра: от ребёнка к родителям.
    for
      layer <- layers
      r <- layer
      p <- r.parents
      (cx, cy) <- positions.get(r.id)
      (px, py) <- positions.get(p)
    do
      val onTrace = traceEdges((r.id, p))
      g.setColor(if onTrace then Color(220, 160, 0) else Color(200, 200, 210))
      g.setStroke(BasicStroke(if onTrace then 3f else 1f))
      g.draw(Line2D.Double(cx + Radius, cy, px - Radius, py))

    // Узлы.
    val stepById = trace.map(s => s.record.id -> s).toMap
    for layer <- layers; r <- layer; (x, y) <- positions.get(r.id) do
      val base = Palette.byFamily(r.species)
      val fill = if alive(r.id) then base else Color(base.getRed, base.getGreen, base.getBlue, 110)
      g.setColor(fill)
      g.fill(Ellipse2D.Double(x - Radius, y - Radius, 2 * Radius, 2 * Radius))
      if alive(r.id) then
        g.setColor(Color(40, 40, 50))
        g.setStroke(BasicStroke(1.5f))
        g.draw(Ellipse2D.Double(x - Radius, y - Radius, 2 * Radius, 2 * Radius))
      val step = stepById.get(r.id)
      step.foreach { s =>
        val mutated = s.change.isDefined
        g.setColor(if mutated then Color(210, 40, 40) else Color(220, 160, 0))
        g.setStroke(BasicStroke(2.5f))
        g.draw(Ellipse2D.Double(x - Radius - 4, y - Radius - 4, 2 * Radius + 8, 2 * Radius + 8))
      }
      g.setColor(Color(40, 40, 50))
      g.drawString(s"#${r.id}", (x + Radius + 6).toFloat, (y - 1).toFloat)
      g.setColor(Color(120, 120, 130))
      val (detail, color) = gene match
        case None => (s"пок. ${r.generation}", Color(120, 120, 130))
        case Some(key) =>
          val change = r.mutations.get(key)
          val text = change.fold(r.gene(key).fold("—")(_.show))(c => s"Δ ${c.show}")
          val color = (step.isDefined, change.isDefined) match
            case (true, true)   => Color(190, 30, 30)
            case (true, false)  => Color(150, 100, 0)
            case (false, true)  => Color(215, 120, 120)
            case (false, false) => Color(120, 120, 130)
          (text, color)
      g.setColor(color)
      g.drawString(detail, (x + Radius + 6).toFloat, (y + 11).toFloat)
      if r.parents.isEmpty then
        g.setColor(Color(90, 110, 160))
        g.setFont(g.getFont.deriveFont(Font.ITALIC))
        g.drawString("создан миром", (x + Radius + 6).toFloat, (y + 23).toFloat)
        g.setFont(g.getFont.deriveFont(Font.PLAIN))
      forageChains.get(r.id).filter(_.nonEmpty).foreach { chain =>
        val mutated = gene.fold(0)(key => chain.count(_.mutations.contains(key)))
        val note = s"+${chain.size} пок. корма" + gene.fold("")(_ => s", мутаций: $mutated")
        g.setColor(if mutated > 0 then Color(190, 30, 30) else Color(90, 150, 90))
        g.drawString(note, (x + Radius + 6).toFloat, (y + 23).toFloat)
      }

    // Сколько предков не поместилось в столбец.
    g.setColor(Color(120, 120, 130))
    hidden.zipWithIndex.foreach { (n, col) =>
      if n > 0 then g.drawString(s"… ещё $n", (Margin + col * ColumnWidth - 4).toFloat, (Margin + MaxPerLayer * RowHeight).toFloat)
    }
