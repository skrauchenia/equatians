package equatians.ui

import equatians.model.*
import equatians.stats.*

import org.jfree.chart.{ChartPanel, JFreeChart}

import java.awt.{BorderLayout, Dimension, GridLayout}
import javax.swing.*

/** Окно статистики сектора (или всего мира). Одно на приложение: клик по другому сектору переключает его.
  *
  * Графики перестраиваются раз в `refreshMs`, и только на открытой вкладке.
  *
  * @param world         текущее состояние мира
  * @param history       история снимков
  * @param onScopeChange вызывается при смене области — чтобы подсветить выбранный сектор на карте
  */
final class SectorStatsWindow(
    world: () => World,
    history: History,
    refreshMs: Int,
    onScopeChange: Scope => Unit
):
  import StatsCharts.Input

  /** Вкладка: название, раскладка сетки и построители графиков. */
  private final case class Tab(title: String, rows: Int, cols: Int, charts: Vector[Input => JFreeChart]):
    val panels: Vector[ChartPanel] = charts.map(_ => ChartPanel(null))

  private val tabs = Vector(
    Tab("Население", 2, 2, Vector(
      StatsCharts.composition, StatsCharts.populationTotals, StatsCharts.currentSpecies, StatsCharts.lifeRates)),
    Tab("Атака", 2, 2, Vector(
      StatsCharts.attackScatter, StatsCharts.attackFamilies, StatsCharts.attackTrends,
      StatsCharts.traitHistogram(_, Trait.ContactDamage))),
    Tab("Признаки", 2, 4, Vector(
      StatsCharts.traitTrend(_, Trait.Speed, withForage = true),
      StatsCharts.traitTrend(_, Trait.MaxHealth, withForage = true),
      StatsCharts.traitTrend(_, Trait.Litter, withForage = false),
      StatsCharts.traitTrend(_, Trait.Generation, withForage = false),
      StatsCharts.traitTrend(_, Trait.Period, withForage = false),
      StatsCharts.traitTrend(_, Trait.Span, withForage = false),
      StatsCharts.traitHistogram(_, Trait.Speed),
      StatsCharts.traitHistogram(_, Trait.MaxHealth))),
    Tab("Энергия и корм", 2, 2, Vector(
      StatsCharts.vitals, StatsCharts.forageRates,
      StatsCharts.traitHistogram(_, Trait.Satiety), StatsCharts.traitHistogram(_, Trait.Reserve)))
  )

  private val frame = JFrame("Статистика")
  private val scopes: Vector[Scope] = Scope.Whole +: world().config.sectors.toVector.map(Scope.Sector(_))
  private val scopeBox = JComboBox[String](scopes.map(_.title).toArray)
  private val summary = JLabel()
  private val tabbed = JTabbedPane()
  private val timer = Timer(refreshMs, _ => refresh())

  locally {
    tabs.foreach { tab =>
      val grid = JPanel(GridLayout(tab.rows, tab.cols, 6, 6))
      tab.panels.foreach(grid.add)
      tabbed.addTab(tab.title, grid)
    }
    tabbed.addChangeListener(_ => refresh())
    scopeBox.addActionListener { _ =>
      onScopeChange(scope)
      refresh()
    }
    val top = JPanel(BorderLayout(10, 0))
    top.setBorder(BorderFactory.createEmptyBorder(8, 10, 4, 10))
    top.add(scopeBox, BorderLayout.WEST)
    top.add(summary, BorderLayout.CENTER)
    frame.getContentPane.setLayout(BorderLayout())
    frame.getContentPane.add(top, BorderLayout.NORTH)
    frame.getContentPane.add(tabbed, BorderLayout.CENTER)
    frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
    frame.setPreferredSize(Dimension(1280, 820))
    frame.pack()
  }

  /** Выбранная область. */
  def scope: Scope = scopes(scopeBox.getSelectedIndex.max(0))

  /** Открыто ли окно. */
  def isVisible: Boolean = frame.isVisible

  /** Показывает статистику области `s`, открывая окно при необходимости. */
  def show(s: Scope): Unit =
    scopeBox.setSelectedIndex(scopes.indexOf(s).max(0))
    if !frame.isVisible then
      frame.setLocationByPlatform(true)
      frame.setVisible(true)
    frame.toFront()
    timer.start()
    refresh()

  /** Перестраивает сводку и графики открытой вкладки. */
  def refresh(): Unit =
    if !frame.isVisible then timer.stop()
    else
      val w = world()
      val input = Input(history.snapshot, w, scope)
      frame.setTitle(s"Статистика — ${scope.title}")
      summary.setText(summaryText(input, w))
      tabs.lift(tabbed.getSelectedIndex).foreach { tab =>
        tab.panels.zip(tab.charts).foreach((panel, build) => panel.setChart(build(input)))
      }

  private def summaryText(in: Input, w: World): String =
    val (forage, active) = in.organisms.partition(_.forage)
    val events = in.samples.lastOption.fold(SectorEvents.Zero)(_.eventsOf(in.scope))
    val corpses = in.scope match
      case Scope.Sector(id) => w.corpses.count(_.sector == id)
      case Scope.Whole      => w.corpses.size
    val maxGen = active.map(_.generation).maxOption.getOrElse(0)
    s"<html>Тик ${w.tick}. Активных <b>${active.size}</b> (видов ${active.map(_.species).distinct.size}), " +
      s"корма <b>${forage.size}</b>, падали $corpses, макс. поколение $maxGen.<br>" +
      s"С начала: рождений ${events.births}, пробуждений ${events.awakenings}, смертей ${events.deaths}, " +
      s"корма появилось ${events.forageBorn}, съедено ${events.forageEaten}. " +
      s"Снимков ${in.samples.size}, раз в ${history.currentInterval} тиков.</html>"
