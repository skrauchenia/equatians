package equatians.ui

import equatians.model.*
import equatians.sim.Genealogy

import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}

import java.awt.{BasicStroke, BorderLayout, Color, Dimension, Font}
import java.awt.geom.Ellipse2D
import javax.swing.*
import javax.swing.table.DefaultTableModel

/** Окно родословной: гены организма, дерево предков и путь наследования выбранного гена.
  *
  * Клик по строке гена подсвечивает в дереве, через каких предков ген пришёл, где мутировал,
  * и строит график его значения по поколениям. Клик по узлу дерева открывает родословную этого предка;
  * «Назад» / «Вперёд» (и Backspace) ходят по истории переходов, «К исходному» — к организму,
  * с которого окно открыли.
  */
final class LineageWindow(world: () => World):
  private val TreeDepth = 8

  private val frame = JFrame("Родословная")
  private val header = JLabel()
  private val genesModel = new DefaultTableModel(Array[AnyRef]("Ген", "Значение", "От кого", "Мутация при рождении"), 0):
    override def isCellEditable(row: Int, column: Int): Boolean = false
  private val genesTable = JTable(genesModel)
  private val tree = LineageTreePanel()
  private val chartPanel = ChartPanel(null)
  private val traceText = JTextArea()

  private val backButton = JButton("← Назад")
  private val forwardButton = JButton("Вперёд →")
  private val homeButton = JButton("К исходному")
  private val path = JLabel()

  private val history = Navigation[Int]()

  private var focus: Option[LineageRecord] = None
  private var lineage: Map[Int, LineageRecord] = Map.empty
  private var alive: Set[Int] = Set.empty

  locally {
    genesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    genesTable.setRowHeight(22)
    genesTable.getColumnModel.getColumn(0).setPreferredWidth(120)
    genesTable.getColumnModel.getColumn(3).setPreferredWidth(150)
    genesTable.getSelectionModel.addListSelectionListener(e => if !e.getValueIsAdjusting then showTrace())
    tree.onFocus = id => navigate(id)
    backButton.addActionListener(_ => move(-1))
    forwardButton.addActionListener(_ => move(1))
    homeButton.addActionListener(_ => history.first.foreach(navigate))
    val root = frame.getRootPane
    root.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("BACK_SPACE"), "back")
    root.getActionMap.put("back", new AbstractAction:
      def actionPerformed(e: java.awt.event.ActionEvent): Unit = move(-1))

    traceText.setEditable(false)
    traceText.setFont(Font(Font.MONOSPACED, Font.PLAIN, 11))
    traceText.setLineWrap(true)
    traceText.setWrapStyleWord(true)

    val refresh = JButton("Обновить")
    refresh.addActionListener(_ => focus.foreach(r => display(r.id)))
    val nav = JPanel(java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 4, 0))
    Seq(backButton, forwardButton, homeButton).foreach(nav.add)
    nav.add(path)
    val top = JPanel(BorderLayout(10, 4))
    top.setBorder(BorderFactory.createEmptyBorder(8, 10, 6, 10))
    top.add(nav, BorderLayout.NORTH)
    top.add(header, BorderLayout.CENTER)
    top.add(refresh, BorderLayout.EAST)

    val left = JPanel(BorderLayout(0, 4))
    left.add(JLabel(" Гены (клик — проследить ген по предкам):"), BorderLayout.NORTH)
    left.add(JScrollPane(genesTable), BorderLayout.CENTER)
    left.setPreferredSize(Dimension(470, 0))

    val traceScroll = JScrollPane(traceText)
    traceScroll.setBorder(BorderFactory.createTitledBorder("Путь гена"))
    val bottom = JSplitPane(JSplitPane.HORIZONTAL_SPLIT, chartPanel, traceScroll)
    bottom.setResizeWeight(0.55)
    val treeScroll = JScrollPane(tree)
    treeScroll.setBorder(BorderFactory.createTitledBorder("Предки (наведите — подробности, клик — перейти)"))
    val right = JSplitPane(JSplitPane.VERTICAL_SPLIT, treeScroll, bottom)
    right.setResizeWeight(0.6)

    frame.getContentPane.setLayout(BorderLayout())
    frame.getContentPane.add(top, BorderLayout.NORTH)
    frame.getContentPane.add(JSplitPane(JSplitPane.HORIZONTAL_SPLIT, left, right), BorderLayout.CENTER)
    frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
    frame.setPreferredSize(Dimension(1400, 860))
    frame.pack()
  }

  /** Открывает родословную организма `id` с чистой историей переходов. */
  def show(id: Int): Unit =
    history.reset(id)
    display(id)

  /** Переход к предку: всё, что было «впереди» в истории, отбрасывается. */
  private def navigate(id: Int): Unit =
    if !history.current.contains(id) then
      history.go(id)
      display(id)

  /** Шаг по истории назад (−1) или вперёд (+1). */
  private def move(delta: Int): Unit = history.step(delta).foreach(display)

  /** Показывает родословную организма `id` по текущему состоянию мира. */
  private def display(id: Int): Unit =
    val w = world()
    lineage = w.lineage
    alive = w.organisms.iterator.map(_.id).toSet
    focus = lineage.get(id)
    focus match
      case None =>
        header.setText(s"<html>Запись о #$id не сохранилась.</html>")
        genesModel.setRowCount(0)
        tree.show(lineage, alive, id, TreeDepth)
      case Some(r) =>
        header.setText(headerText(r))
        genesModel.setRowCount(0)
        r.genes.foreach { (key, value) =>
          val from = r.origin.get(key).fold(if r.parents.isEmpty then "создан миром" else "—")(p => s"#$p")
          val change = r.mutations.get(key).fold("")(_.show)
          genesModel.addRow(Array[AnyRef](key, value.show, from, change))
        }
        tree.show(lineage, alive, id, TreeDepth)
        chartPanel.setChart(null)
        traceText.setText("Выберите ген в таблице, чтобы увидеть, от каких предков он пришёл и где мутировал.")
    frame.setTitle(s"Родословная #$id")
    backButton.setEnabled(history.canBack)
    forwardButton.setEnabled(history.canForward)
    homeButton.setEnabled(!history.current.exists(history.first.contains))
    val (trail, at) = history.trail
    path.setText(trail.zipWithIndex.map((h, i) => if i == at then s"<b>#$h</b>" else s"#$h").mkString("<html>путь: ", " → ", "</html>"))
    if !frame.isVisible then
      frame.setLocationByPlatform(true)
      frame.setVisible(true)
    frame.toFront()

  private def headerText(r: LineageRecord): String =
    val kind = if r.forage then "корм" else r.species.title
    val origin = r.parents match
      case Vector()  => "создан миром"
      case Vector(p) => s"отпочковался от #$p"
      case ps        => s"родители ${ps.map(p => s"#$p").mkString(" × ")}"
    val status = if alive(r.id) then "жив" else r.died.fold("—")(t => s"умер на тике $t")
    val mutations = if r.mutations.isEmpty then "без мутаций" else s"мутаций при рождении: ${r.mutations.size}"
    s"<html><b>#${r.id}</b> — $kind, поколение ${r.generation}, родился на тике ${r.born}, $origin; $status; $mutations." +
      r.cause.fold("")(c => s"<br>Последняя запись: $c") + "</html>"

  /** Подсвечивает путь выбранного гена в дереве, строит график и текстовый список шагов. */
  private def showTrace(): Unit =
    val row = genesTable.getSelectedRow
    focus.filter(_ => row >= 0).foreach { r =>
      val key = genesModel.getValueAt(row, 0).toString
      val steps = Genealogy.trace(lineage, r.id, key)
      tree.show(lineage, alive, r.id, TreeDepth, Some(key), steps)
      chartPanel.setChart(traceChart(key, steps))
      traceText.setText(traceDescription(key, steps))
      traceText.setCaretPosition(0)
    }

  /** Список шагов пути гена. Длинные цепочки корма (клонов) сворачиваются: первый и последний корм
    * показываются, промежуточные — одной строкой со сводкой.
    */
  private def traceDescription(key: String, steps: Vector[Genealogy.TraceStep]): String =
    val mutations = steps.count(_.change.isDefined)
    val head = s"Ген «$key»: ${steps.size} шагов по предкам, мутаций на пути — $mutations.\n\n"
    def line(s: Genealogy.TraceStep): String =
      val r = s.record
      val value = s.value.fold("—")(_.show)
      val change = s.change.fold("")(c => s"  ⚡ мутация ${c.show}")
      val from = s.from.fold("")(p => s"  ← от #$p")
      val end = s.end.fold("")(e => s"  ■ $e")
      s"#${r.id} (пок. ${r.generation}, ${if r.forage then "корм" else r.species.title}, тик ${r.born}): $value$change$from$end"
    def collapsed(run: Vector[Genealogy.TraceStep]): String =
      val mutated = run.filter(_.change.isDefined)
      val values = run.flatMap(_.value).map(_.show)
      s"  ⋮ ещё ${run.size} поколений корма (#${run.head.record.id} … #${run.last.record.id}): " +
        s"${values.lastOption.getOrElse("—")} → ${values.headOption.getOrElse("—")}, мутаций ${mutated.size}" +
        (if mutated.nonEmpty then mutated.take(5).map(s => s"#${s.record.id}: ${s.change.get.show}").mkString(" (", "; ", if mutated.size > 5 then "; …)" else ")") else "")
    val runs = steps.foldLeft(Vector.empty[Vector[Genealogy.TraceStep]]) { (acc, s) =>
      acc.lastOption match
        case Some(run) if run.head.record.forage == s.record.forage => acc.init :+ (run :+ s)
        case _                                                     => acc :+ Vector(s)
    }
    head + runs.flatMap { run =>
      if !run.head.record.forage || run.size <= 3 then run.map(line)
      else Vector(line(run.head), collapsed(run.slice(1, run.size - 1)), line(run.last))
    }.mkString("\n")

  /** Значение гена вдоль пути наследования по поколениям; мутации — красные точки. Для семейств — без графика. */
  private def traceChart(key: String, steps: Vector[Genealogy.TraceStep]): JFreeChart =
    val values = XYSeries("значение", true, true)
    val mutations = XYSeries("мутации", true, true)
    steps.foreach { s =>
      s.value.flatMap(_.numeric).foreach { v =>
        values.add(s.record.generation.toDouble, v)
        if s.change.isDefined then mutations.add(s.record.generation.toDouble, v)
      }
    }
    val ds = XYSeriesCollection()
    ds.addSeries(values)
    ds.addSeries(mutations)
    val title = if values.getItemCount == 0 then s"«$key» — не числовой ген, см. путь справа" else s"«$key» по поколениям предков"
    val chart = ChartFactory.createXYLineChart(title, "поколение", key, ds, PlotOrientation.VERTICAL, true, true, false)
    val r = XYLineAndShapeRenderer()
    r.setSeriesPaint(0, Color(70, 110, 200))
    r.setSeriesStroke(0, BasicStroke(1.6f))
    r.setSeriesShapesVisible(0, false)
    r.setSeriesPaint(1, Color(210, 40, 40))
    r.setSeriesLinesVisible(1, false)
    r.setSeriesShape(1, Ellipse2D.Double(-4, -4, 8, 8))
    chart.getXYPlot.setRenderer(r)
    chart.getTitle.setFont(Font(Font.SANS_SERIF, Font.BOLD, 12))
    chart.setBackgroundPaint(Color.WHITE)
    chart
