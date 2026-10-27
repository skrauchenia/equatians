package equatians.ui

import equatians.AppConfig
import equatians.model.*
import equatians.sim.{Genesis, Narrative, Simulation}
import equatians.stats.{History, Scope}

import java.awt.*
import java.awt.event.{ActionEvent, KeyEvent}
import java.util.Locale
import javax.swing.*

object Main:
  /** Точка входа: читает конфиг и открывает окно симулятора в потоке Swing. */
  def main(args: Array[String]): Unit =
    val config = AppConfig.load()
    SwingUtilities.invokeLater(() => SimulatorApp(config).start())

/** Окно симулятора: мир слева, панель управления и карточка организма справа.
  *
  * Скорость задаётся в тиках за кадр. Кадр ~60 раз в секунду, но на симуляцию в кадре
  * отводится не больше `ui.frame-budget-ms` (или времени отрисовки прошлого кадра, если оно больше) —
  * если не успеваем, реальная скорость (TPS) будет ниже заданной.
  */
final class SimulatorApp(config: AppConfig):
  private val SpeedSteps = config.ui.speedSteps
  private val FrameBudgetNanos = config.ui.frameBudgetMs * 1_000_000L
  private val rules = config.rules

  private var seed = config.population.seed
  /** Геометрия мира: из конфига, но её можно поменять в панели — тогда создаётся новый мир. */
  private var worldConfig = config.world
  private var world = createWorld()
  private var running = true

  private val view = WorldView(world, config.ui.trailLength)
  private val history = History(config.ui.stats.sampleInterval, config.ui.stats.maxSamples)
  private val lineageWindow = LineageWindow(() => world)
  private val lineageButton = JButton("Родословная (L)")
  private val statsWindow = SectorStatsWindow(
    () => world,
    history,
    config.ui.stats.refreshMs,
    scope => view.highlightedSector = scope match
      case Scope.Sector(id) => Some(id)
      case Scope.Whole      => None
  )
  private val frame = JFrame("Equatians")

  private val tickLabel = JLabel()
  private val tpsLabel = JLabel()
  private val speedLabel = JLabel()
  private val speedSlider = JSlider(0, SpeedSteps.size - 1, 0)
  private val pauseButton = JButton()
  private val trailsBox = JCheckBox("Следы (T)", true)
  private val legendLabels = Family.values.map(f => f -> JLabel()).toMap
  private val deathsLabel = JLabel()

  private val infoTitle = JLabel()
  private val healthBar = StatBar(Color(220, 90, 90))
  private val satietyBar = StatBar(Color(120, 200, 120))
  private val reserveBar = StatBar(Color(240, 200, 90))
  private val headingFormula = JLabel()
  private val argumentFormula = JLabel()
  private val damageFormula = JLabel()
  private val attackInfo = JLabel()
  private val infoState = JLabel()
  private val logArea = JTextArea()

  /** Для какого организма уже отрендерены формулы — чтобы не пересобирать их каждый кадр. */
  private var formulasFor: Option[Int] = None
  /** Что сейчас показано в журнале — чтобы не перезаписывать текст без изменений. */
  private var logShown: Option[(Int, Int, Option[LogEntry])] = None

  private var ticksSinceMeasure = 0L
  private var lastMeasure = System.nanoTime()

  /** Текущая выбранная скорость в тиках за кадр. */
  def ticksPerFrame: Int = SpeedSteps(speedSlider.getValue)

  /** Собирает окно, вешает клавиши и запускает таймер кадров. Вызывать в потоке Swing. */
  def start(): Unit =
    view.onSelect = _ => refreshInfo()
    view.onSectorClick = id => statsWindow.show(Scope.Sector(id))
    view.onViewChange = () => zoomLabel.setText(s"${(view.viewport.zoom * 100).round}%")
    history.reset(world)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.getContentPane.setLayout(BorderLayout())
    frame.getContentPane.add(view, BorderLayout.CENTER)
    frame.getContentPane.add(sidePanel, BorderLayout.EAST)
    bindKeys()
    refreshAll()
    frame.pack()
    frame.setSize(1340, 900)
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    Timer(16, _ => onFrame()).start()

  /** Новый случайный мир по текущему сиду и параметрам из конфига. */
  private def createWorld(): World =
    Genesis.randomWorld(worldConfig, rules, config.genesis, config.population.count, seed)

  /** Один кадр: прогнать сколько успеем тиков, перерисовать, раз в полсекунды обновить TPS. */
  private def onFrame(): Unit =
    if running then
      // Если кадр рисуется дольше бюджета, симуляции даётся столько же, сколько отрисовке:
      // иначе на большой скорости почти всё время уходило бы на картинку, а не на тики.
      val deadline = System.nanoTime() + FrameBudgetNanos.max(view.lastPaintNanos)
      var done = 0
      while done < ticksPerFrame && System.nanoTime() < deadline do
        world = Simulation.step(world)
        history.record(world)
        done += 1
      ticksSinceMeasure += done
      view.show(world)
    val now = System.nanoTime()
    if now - lastMeasure >= 500_000_000L then
      val tps = ticksSinceMeasure * 1e9 / (now - lastMeasure)
      tpsLabel.setText(String.format(Locale.ROOT, "Реально: %.0f тиков/с", tps))
      ticksSinceMeasure = 0
      lastMeasure = now
    refreshAll()

  /** Пауза / продолжение. */
  private def togglePause(): Unit = running = !running

  /** Ставит на паузу и делает ровно один тик. */
  private def stepOnce(): Unit =
    running = false
    world = Simulation.step(world)
    history.record(world)
    view.show(world)

  /** Пересоздаёт мир со следующим сидом. */
  private def newWorld(): Unit =
    seed += 1
    world = createWorld()
    history.reset(world)
    view.reset(world)
    if statsWindow.isVisible then statsWindow.refresh()
    refreshAll()

  /** Сдвигает ползунок скорости на `delta` ступеней в пределах `ui.speed-steps`. */
  private def changeSpeed(delta: Int): Unit =
    speedSlider.setValue((speedSlider.getValue + delta).max(0).min(SpeedSteps.size - 1))

  /** Включает / выключает следы. */
  private def toggleTrails(): Unit =
    trailsBox.setSelected(!trailsBox.isSelected)
    view.showTrails = trailsBox.isSelected

  /** Обновляет все подписи панели под текущее состояние. */
  private def refreshAll(): Unit =
    tickLabel.setText(s"Тик: ${world.tick}   (сид $seed)")
    speedLabel.setText(s"Скорость: ×$ticksPerFrame тиков/кадр")
    pauseButton.setText(if running then "Пауза (Space)" else "Пуск (Space)")
    val counts = world.organisms.groupMapReduce(_.family)(_ => 1)(_ + _)
    legendLabels.foreach { (f, l) =>
      val name = if f == Family.Constant then s"корм (${f.title})" else f.title
      l.setText(s"$name: ${counts.getOrElse(f, 0)}")
    }
    val (forage, active) = world.organisms.partition(_.forage)
    val deaths = config.population.count + world.births + world.awakenings - active.size
    val maxGeneration = active.map(_.generation).maxOption.getOrElse(0)
    deathsLabel.setText(
      s"<html>Живых: ${active.size}, рождений: ${world.births}, погибло: $deaths<br>" +
        s"Макс. поколение: $maxGeneration, падаль: ${world.corpses.size}<br>" +
        s"Корм: ${forage.size}, отпочковалось ${world.buds}, пробудилось ${world.awakenings}</html>"
    )
    refreshInfo()

  private val colsSpinner = JSpinner(SpinnerNumberModel(config.world.cols, 1, 20, 1))
  private val rowsSpinner = JSpinner(SpinnerNumberModel(config.world.rows, 1, 20, 1))
  private val sizeSpinner = JSpinner(SpinnerNumberModel(config.world.sectorSize.round.toInt, 20, 1000, 10))
  private val passageSpinner = JSpinner(SpinnerNumberModel((config.world.passage * 100).round.toInt, 0, 100, 5))
  private val zoomLabel = JLabel("100%")

  /** Две строки из `rows`, без растягивания по ширине панели (иначе BoxLayout раздувает их по высоте). */
  private def stacked(rows: JComponent*): JComponent =
    val box = JPanel()
    box.setLayout(BoxLayout(box, BoxLayout.Y_AXIS))
    rows.foreach { r =>
      r.setAlignmentX(Component.LEFT_ALIGNMENT)
      box.add(r)
    }
    box.setMaximumSize(box.getPreferredSize)
    box

  /** Строка из компонентов слева направо. */
  private def line(cs: JComponent*): JPanel =
    val row = JPanel(FlowLayout(FlowLayout.LEFT, 3, 1))
    cs.foreach(row.add)
    row

  /** Сетка, размер сектора и ширина проходов; «Применить» создаёт новый мир с этой геометрией.
    * Несколько строк — в одну по ширине панели не помещается.
    */
  private def worldControls: JComponent =
    val apply = JButton("Применить — новый мир")
    apply.setToolTipText("Создать новый мир с такой сеткой секторов, размером сектора и шириной проходов")
    passageSpinner.setToolTipText("Ширина прохода между соседними секторами, % стороны сектора (0 — без проходов)")
    apply.setFocusable(false)
    apply.addActionListener(_ => applyGeometry())
    stacked(
      line(JLabel("Сетка"), colsSpinner, JLabel("×"), rowsSpinner, JLabel(", сектор"), sizeSpinner),
      line(JLabel("Проход между секторами, %"), passageSpinner),
      line(apply)
    )

  /** Масштаб: −, +, вписать и текущее значение. Колесо мыши и перетаскивание работают на самой карте. */
  private def zoomControls: JComponent =
    def button(title: String, tip: String)(action: => Unit) =
      val b = JButton(title)
      b.setToolTipText(tip)
      b.setFocusable(false)
      b.addActionListener(_ => action)
      b
    val box = stacked(
      line(JLabel("Масштаб:"), zoomLabel, JLabel("(колесо мыши, перетаскивание)")),
      line(
        button("−", "Отдалить")(view.zoomBy(1 / 1.25)),
        button("+", "Приблизить")(view.zoomBy(1.25)),
        button("Вписать (F)", "Показать весь мир")(view.fitView())
      )
    )
    box

  /** Создаёт новый мир с геометрией из панели и вписывает его в окно. */
  private def applyGeometry(): Unit =
    worldConfig = WorldConfig(
      cols = committed(colsSpinner),
      rows = committed(rowsSpinner),
      sectorSize = committed(sizeSpinner).toDouble,
      passage = committed(passageSpinner) / 100.0
    )
    view.fitView()
    newWorld()

  /** Значение спиннера с учётом текста, набранного в поле, но ещё не принятого.
    *
    * `JSpinner` принимает набранное только по Enter или при потере фокуса, а кнопка «Применить» фокус
    * не забирает, так что без `commitEdit` применялось бы прежнее значение. Неразборчивый текст
    * отбрасывается, и в поле возвращается последнее принятое значение.
    */
  private def committed(s: JSpinner): Int =
    try s.commitEdit()
    catch case _: java.text.ParseException => s.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setValue(s.getValue)
    s.getValue.asInstanceOf[Int]

  /** Открывает родословную выбранного организма. */
  private def openLineage(): Unit = view.selected.foreach(o => lineageWindow.show(o.id))

  /** Текущее занятие организма для заголовка карточки. */
  private def activity(o: Organism): String =
    if !o.alive then if o.forage then "съеден" else "погиб"
    else if o.forage then "корм: ползёт по прямой, почкуется"
    else
      o.behaviour match
        case Behaviour.Cruise   => "обычное движение"
        case Behaviour.Chase(t) => s"преследует #$t"
        case Behaviour.Flee(f)  => s"отдаляется от #$f"
        case Behaviour.Scavenge(c) => s"идёт к падали #$c"
        case Behaviour.Mate(p)     => s"идёт к партнёру #$p"

  /** Карточка выбранного организма: формулы рендерятся при смене выбора, остальное — каждый кадр. */
  private def refreshInfo(): Unit =
    val selected = view.selected
    if selected.map(_.id) != formulasFor then
      formulasFor = selected.map(_.id)
      selected match
        case Some(o) =>
          Formula.render(headingFormula, o.movement.headingLatex)
          Formula.render(argumentFormula, o.movement.argumentLatex)
          Formula.render(damageFormula, s"${o.attack.damageLatex}, \\quad ${o.attack.argumentLatex}")
          val a = o.attack
          def dmg(d: Double) = Narrative.num(o.strikeDamage(d, rules))
          attackInfo.setText(
            s"<html>скорость: <b>${Latex.number(o.movement.speed)}</b> ед./тик<br>" +
              s"урон: в упор <b>${dmg(0)}</b>, на ½ дальности <b>${dmg(a.range / 2)}</b> " +
              s"(×${String.format(Locale.ROOT, "%.2f", o.sizeFactor(rules))} за размер)<br>" +
              s"дальность ${Narrative.num(a.range)}, стоимость ${Narrative.num(a.cost)}, " +
              s"перезарядка ${rules.attack.cooldown} т.<br>" +
              s"агрессивность: <b>${Latex.number(o.aggression)}</b>" +
              (if o.forage then " (спящий ген)" else "") + "</html>"
          )
          lineageButton.setEnabled(true)
        case None =>
          Seq(headingFormula, argumentFormula, damageFormula).foreach(_.setIcon(null))
          attackInfo.setText("")
          lineageButton.setEnabled(false)

    selected match
      case None =>
        infoTitle.setText("<html>Кликните по организму,<br>чтобы увидеть его функции и журнал.</html>")
        Seq(healthBar, satietyBar, reserveBar).foreach(_.set(0, ""))
        infoState.setText("")
      case Some(o) =>
        val e = rules.energy
        infoTitle.setText(s"<html><b>Организм #${o.id}</b> — ${o.family.title} · ${activity(o)}</html>")
        healthBar.set(o.health / o.maxHealth, s"здоровье ${Narrative.num(o.health.max(0))} / ${Narrative.num(o.maxHealth)}")
        satietyBar.set(o.satietyFraction(rules), s"сытость ${Narrative.num(o.satiety)} / ${Narrative.num(e.maxSatiety)}")
        reserveBar.set(
          o.reserveFraction(rules),
          s"жир ${Narrative.num(o.reserve)} / ${Narrative.num(o.maxReserve(rules))}  (×${Narrative.num(e.reserveDensity)} к сытости)"
        )
        val m = o.movement
        val origin = o.parents match
          case Vector()     => "создан миром"
          case Vector(p)    => s"отпочковался от #$p"
          case ps           => s"родители ${ps.map(id => s"#$id").mkString(" × ")}"
        val ready = if o.readyToMate(world.tick, rules) then "готов к размножению" else "не готов к размножению"
        infoState.setText(
          s"<html>поколение ${o.generation}, $origin<br>" +
            s"ген помёта ${o.litterSize}, детей ${o.offspringCount}, $ready<br>" +
            s"сектор (${o.sector.col}, ${o.sector.row}), позиция (${Narrative.num(o.pos.x)}, ${Narrative.num(o.pos.y)})<br>" +
            s"возраст t = ${o.age}, x = ${Narrative.num(m.argument(o.age))}, радиус ${Narrative.num(o.radius(rules))}<br>" +
            s"расход ${String.format(Locale.ROOT, "%.3f", Simulation.metabolicCost(o, rules))} / тик, " +
            s"отхил до ${Narrative.num(rules.healing.rate)} / тик за ${Narrative.num(rules.healing.cost)} сытости</html>"
        )
    refreshLog(selected)

  /** Журнал выбранного организма; прокручивается к последней записи при каждом изменении. */
  private def refreshLog(selected: Option[Organism]): Unit =
    val key = selected.map(o => (o.id, o.log.size, o.log.lastOption))
    if key != logShown then
      logShown = key
      val text = selected.fold("")(_.log.map(e => s"[${e.tick}] ${e.text}").mkString("\n"))
      logArea.setText(text)
      logArea.setCaretPosition(text.length)

  /** Правая панель: сверху управление, легенда и карточка организма, снизу — журнал. */
  private def sidePanel: JComponent =
    val top = JPanel()
    top.setLayout(BoxLayout(top, BoxLayout.Y_AXIS))

    def add(c: JComponent, gap: Int = 4): Unit =
      c.setAlignmentX(Component.LEFT_ALIGNMENT)
      c.setFocusable(false)
      top.add(c)
      top.add(Box.createVerticalStrut(gap))

    def button(title: String)(action: => Unit): JButton =
      val b = JButton(title)
      b.addActionListener(_ => { action; refreshAll() })
      b

    add(tickLabel)
    add(tpsLabel)
    add(speedLabel)
    speedSlider.setPaintTicks(true)
    speedSlider.setSnapToTicks(true)
    speedSlider.setMajorTickSpacing(1)
    speedSlider.addChangeListener(_ => refreshAll())
    add(speedSlider)
    val buttons = JPanel(GridLayout(1, 3, 4, 0))
    pauseButton.addActionListener(_ => { togglePause(); refreshAll() })
    Seq(pauseButton, button("Тик (N)")(stepOnce()), button("Новый (R)")(newWorld())).foreach { b =>
      b.setFocusable(false)
      buttons.add(b)
    }
    add(buttons)
    val statsButton = button("Статистика мира (S)")(statsWindow.show(Scope.Whole))
    statsButton.setToolTipText("Клик по пустому месту сектора — статистика сектора")
    add(statsButton)
    trailsBox.addActionListener(_ => view.showTrails = trailsBox.isSelected)
    add(trailsBox, 6)
    add(worldControls, 4)
    add(zoomControls, 10)

    add(JLabel("Виды (функция движения):"))
    val legend = JPanel(GridLayout(0, 2, 6, 2))
    Family.values.foreach { f =>
      val l = legendLabels(f)
      l.setIcon(ColorIcon(Palette.byFamily(f)))
      legend.add(l)
    }
    add(legend)
    add(deathsLabel, 12)

    add(infoTitle, 6)
    Seq(healthBar, satietyBar, reserveBar).foreach(add(_, 3))
    add(headingFormula, 2)
    add(argumentFormula, 2)
    add(damageFormula, 2)
    add(attackInfo)
    add(infoState)
    lineageButton.setEnabled(false)
    lineageButton.addActionListener(_ => openLineage())
    add(lineageButton)

    logArea.setEditable(false)
    logArea.setLineWrap(true)
    logArea.setWrapStyleWord(true)
    logArea.setFont(Font(Font.MONOSPACED, Font.PLAIN, 11))
    val logScroll = JScrollPane(logArea)
    logScroll.setBorder(BorderFactory.createTitledBorder("Журнал"))

    val panel = JPanel(BorderLayout(0, 6))
    panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10))
    panel.setPreferredSize(Dimension(400, 0))
    panel.add(top, BorderLayout.NORTH)
    panel.add(logScroll, BorderLayout.CENTER)
    panel

  /** Горячие клавиши, работающие при любом фокусе внутри окна. */
  private def bindKeys(): Unit =
    val root = frame.getRootPane
    def bind(key: Int, name: String)(action: => Unit): Unit =
      root.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(key, 0), name)
      root.getActionMap.put(name, new AbstractAction:
        def actionPerformed(e: ActionEvent): Unit = { action; refreshAll() })
    bind(KeyEvent.VK_SPACE, "pause")(togglePause())
    bind(KeyEvent.VK_N, "step")(stepOnce())
    bind(KeyEvent.VK_R, "reset")(newWorld())
    bind(KeyEvent.VK_T, "trails")(toggleTrails())
    bind(KeyEvent.VK_S, "stats")(statsWindow.show(Scope.Whole))
    bind(KeyEvent.VK_L, "lineage")(openLineage())
    bind(KeyEvent.VK_F, "fit")(view.fitView())
    bind(KeyEvent.VK_EQUALS, "faster")(changeSpeed(1))
    bind(KeyEvent.VK_PLUS, "faster2")(changeSpeed(1))
    bind(KeyEvent.VK_MINUS, "slower")(changeSpeed(-1))

/** Кружок цвета вида для легенды. */
private final class ColorIcon(color: Color) extends Icon:
  def getIconWidth: Int = 12
  def getIconHeight: Int = 12
  def paintIcon(c: Component, g: Graphics, x: Int, y: Int): Unit =
    g.setColor(color)
    g.fillOval(x, y, 12, 12)
