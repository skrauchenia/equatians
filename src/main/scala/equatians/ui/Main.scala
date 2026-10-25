package equatians.ui

import equatians.AppConfig
import equatians.model.*
import equatians.sim.Genesis

import java.awt.*
import java.awt.event.{ActionEvent, KeyEvent}
import java.util.Locale
import javax.swing.*

object Main:
  /** Точка входа: читает конфиг и открывает окно симулятора в потоке Swing. */
  def main(args: Array[String]): Unit =
    val config = AppConfig.load()
    SwingUtilities.invokeLater(() => SimulatorApp(config).start())

/** Окно симулятора: мир слева, панель управления справа.
  *
  * Скорость задаётся в тиках за кадр. Кадр ~60 раз в секунду, но на симуляцию в кадре
  * отводится не больше `ui.frame-budget-ms` — если не успеваем, реальная скорость (TPS)
  * будет ниже заданной.
  */
final class SimulatorApp(config: AppConfig):
  private val SpeedSteps = config.ui.speedSteps
  private val FrameBudgetNanos = config.ui.frameBudgetMs * 1_000_000L

  private var seed = config.population.seed
  private var world = createWorld()
  private var running = true

  private val view = WorldView(world, config.ui.trailLength)
  private val frame = JFrame("Equatians")

  private val tickLabel = JLabel()
  private val tpsLabel = JLabel()
  private val speedLabel = JLabel()
  private val speedSlider = JSlider(0, SpeedSteps.size - 1, 0)
  private val pauseButton = JButton()
  private val trailsBox = JCheckBox("Следы (T)", true)
  private val legendLabels = Family.values.map(f => f -> JLabel()).toMap

  private val infoTitle = JLabel()
  private val headingFormula = JLabel()
  private val argumentFormula = JLabel()
  private val infoState = JLabel()
  /** Для какого организма уже отрендерены формулы — чтобы не пересобирать их каждый кадр. */
  private var formulasFor: Option[Int] = None

  private var ticksSinceMeasure = 0L
  private var lastMeasure = System.nanoTime()

  /** Текущая выбранная скорость в тиках за кадр. */
  def ticksPerFrame: Int = SpeedSteps(speedSlider.getValue)

  /** Собирает окно, вешает клавиши и запускает таймер кадров. Вызывать в потоке Swing. */
  def start(): Unit =
    view.onSelect = _ => refreshInfo()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.getContentPane.setLayout(BorderLayout())
    frame.getContentPane.add(view, BorderLayout.CENTER)
    frame.getContentPane.add(sidePanel, BorderLayout.EAST)
    bindKeys()
    refreshAll()
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    Timer(16, _ => onFrame()).start()

  /** Новый случайный мир по текущему сиду и параметрам из конфига. */
  private def createWorld(): World =
    Genesis.randomWorld(config.world, config.genesis, config.population.count, seed)

  /** Один кадр: прогнать сколько успеем тиков, перерисовать, раз в полсекунды обновить TPS. */
  private def onFrame(): Unit =
    if running then
      val deadline = System.nanoTime() + FrameBudgetNanos
      var done = 0
      while done < ticksPerFrame && System.nanoTime() < deadline do
        world = world.step
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
    world = world.step
    view.show(world)

  /** Пересоздаёт мир со следующим сидом. */
  private def newWorld(): Unit =
    seed += 1
    world = createWorld()
    view.reset(world)
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
    legendLabels.foreach((f, l) => l.setText(s"${f.title}: ${counts.getOrElse(f, 0)}"))
    refreshInfo()

  /** Карточка выбранного организма: формулы рендерятся при смене выбора, состояние — каждый кадр. */
  private def refreshInfo(): Unit =
    val selected = view.selected
    if selected.map(_.id) != formulasFor then
      formulasFor = selected.map(_.id)
      selected match
        case Some(o) =>
          infoTitle.setText(s"<html><b>Организм #${o.id}</b> — ${o.family.title}</html>")
          Formula.render(headingFormula, o.movement.headingLatex)
          Formula.render(argumentFormula, o.movement.argumentLatex)
        case None =>
          infoTitle.setText("<html>Кликните по организму,<br>чтобы увидеть его функцию.</html>")
          headingFormula.setIcon(null)
          argumentFormula.setIcon(null)
    infoState.setText(selected.fold("") { o =>
      val m = o.movement
      def f(v: Double) = Latex.number(v)
      s"""<html>шаг: ${f(m.speed)} за тик<br><br>
         |сектор: (${o.sector.col}, ${o.sector.row})<br>
         |позиция: (${f(o.pos.x)}, ${f(o.pos.y)})<br>
         |возраст t: ${o.age}<br>
         |x = ${f(m.argument(o.age))}<br>
         |курс: ${f(math.toDegrees(o.heading))}°</html>""".stripMargin
    })

  /** Правая панель: счётчики, управление скоростью, легенда видов и карточка организма. */
  private def sidePanel: JComponent =
    val panel = JPanel()
    panel.setLayout(BoxLayout(panel, BoxLayout.Y_AXIS))
    panel.setBorder(BorderFactory.createEmptyBorder(12, 12, 12, 12))
    panel.setPreferredSize(Dimension(360, 0))

    def add(c: JComponent): Unit =
      c.setAlignmentX(Component.LEFT_ALIGNMENT)
      c.setFocusable(false)
      panel.add(c)
      panel.add(Box.createVerticalStrut(6))

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
    pauseButton.addActionListener(_ => { togglePause(); refreshAll() })
    add(pauseButton)
    add(button("Один тик (N)")(stepOnce()))
    add(button("Новый мир (R)")(newWorld()))
    trailsBox.addActionListener(_ => view.showTrails = trailsBox.isSelected)
    add(trailsBox)

    panel.add(Box.createVerticalStrut(10))
    add(JLabel("Виды (функция движения):"))
    Family.values.foreach { f =>
      val l = legendLabels(f)
      l.setIcon(ColorIcon(Palette.byFamily(f)))
      add(l)
    }

    panel.add(Box.createVerticalStrut(14))
    Seq(infoTitle, headingFormula, argumentFormula, infoState).foreach(add)
    panel.add(Box.createVerticalGlue())
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
