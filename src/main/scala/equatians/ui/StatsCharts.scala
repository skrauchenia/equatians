package equatians.ui

import equatians.model.*
import equatians.stats.*

import org.jfree.chart.{ChartFactory, JFreeChart}
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.renderer.category.BarRenderer
import org.jfree.chart.renderer.xy.{StackedXYAreaRenderer2, XYLineAndShapeRenderer}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.statistics.HistogramDataset
import org.jfree.data.xy.{DefaultTableXYDataset, XYSeries, XYSeriesCollection}

import java.awt.{BasicStroke, Color, Font, Paint}
import java.awt.geom.Ellipse2D

/** Построение графиков статистики. Ничего не знает об окне — графики можно отрисовать и в картинку.
  *
  * Временные графики строятся по истории снимков, «текущие» — по живой популяции мира прямо сейчас.
  */
object StatsCharts:

  /** Данные для графиков: история, текущий мир и выбранная область. */
  final case class Input(samples: Vector[Sample], world: World, scope: Scope):
    /** Организмы в выбранной области сейчас. */
    val organisms: Vector[Organism] = scope match
      case Scope.Sector(id) => world.organisms.filter(_.sector == id)
      case Scope.Whole      => world.organisms

    val active: Vector[Organism] = organisms.filterNot(_.forage)
    val censuses: Vector[(Long, SectorCensus)] = samples.map(s => s.tick -> s.censusOf(scope))

    /** Виды активных организмов, встречавшиеся в истории или живущие сейчас, в порядке перечисления. */
    val activeSpecies: Vector[Family] =
      val seen = censuses.iterator.flatMap(_._2.species.keys).toSet ++ active.map(_.species)
      Family.values.toVector.filter(f => f != Family.Constant && seen(f))

  private val TickLabel = "тик"

  /** Окно сглаживания частоты событий, в тиках. */
  private val RateWindow = 1000

  // ---------- Население ----------

  /** Состав активного населения по видам во времени (накопленные области). */
  def composition(in: Input): JFreeChart =
    val ds = DefaultTableXYDataset()
    in.activeSpecies.foreach { f =>
      val s = XYSeries(f.title, true, false)
      in.censuses.foreach((t, c) => s.add(t.toDouble, c.species.getOrElse(f, 0).toDouble))
      ds.addSeries(s)
    }
    val chart = ChartFactory.createStackedXYAreaChart(
      "Состав активного населения", TickLabel, "организмов", ds, PlotOrientation.VERTICAL, true, true, false)
    val renderer = StackedXYAreaRenderer2()
    in.activeSpecies.zipWithIndex.foreach((f, i) => renderer.setSeriesPaint(i, Palette.byFamily(f)))
    chart.getXYPlot.setRenderer(renderer)
    style(chart)

  /** Активные, корм и падаль во времени. */
  def populationTotals(in: Input): JFreeChart =
    lines("Численность", "организмов", in.censuses, Seq(
      ("активные", Color(70, 110, 200), c => Some(c.active.count.toDouble)),
      ("корм", Palette.byFamily(Family.Constant), c => Some(c.forage.count.toDouble)),
      ("падаль", Palette.Carrion, c => Some(c.corpses.toDouble))
    ))

  /** Численность по видам прямо сейчас, включая корм. */
  def currentSpecies(in: Input): JFreeChart =
    val counts = in.organisms.groupMapReduce(_.species)(_ => 1)(_ + _)
    bars("Сейчас: численность по видам", "организмов",
      Family.values.toVector.filter(counts.contains).map(f => (label(f), Palette.byFamily(f), counts(f).toDouble)))

  // ---------- Атака ----------

  /** Семейства функции урона у активных организмов прямо сейчас. */
  def attackFamilies(in: Input): JFreeChart =
    val counts = in.active.groupMapReduce(_.attack.g.family)(_ => 1)(_ + _)
    bars("Сейчас: семейства функции урона D(u)", "организмов",
      Family.values.toVector.filter(counts.contains).map(f => (f.title, Palette.byFamily(f), counts(f).toDouble)))

  /** Каждая особь — точка «дальность удара × урон в упор», цвет — вид. */
  def attackScatter(in: Input): JFreeChart =
    val ds = XYSeriesCollection()
    val species = in.active.map(_.species).distinct.sortBy(_.ordinal)
    species.foreach { f =>
      val s = XYSeries(f.title, false, true)
      in.active.filter(_.species == f).foreach(o => s.add(o.attack.range, o.strikeDamage(0, in.world.rules)))
      ds.addSeries(s)
    }
    val chart = ChartFactory.createScatterPlot(
      "Сейчас: особи — дальность × урон в упор", "дальность удара", "урон в упор (с учётом размера)", ds)
    val r = chart.getXYPlot.getRenderer
    species.zipWithIndex.foreach { (f, i) =>
      r.setSeriesPaint(i, Palette.byFamily(f))
      r.setSeriesShape(i, Ellipse2D.Double(-4, -4, 8, 8))
    }
    style(chart)

  /** Средние характеристики удара во времени. */
  def attackTrends(in: Input): JFreeChart =
    lines("Удар: средние по активным", "значение", in.censuses, Seq(
      ("дальность", Color(200, 80, 60), _.active.mean(Trait.AttackRange)),
      ("урон в упор", Color(120, 60, 160), _.active.mean(Trait.ContactDamage)),
      ("стоимость", Color(60, 140, 90), _.active.mean(Trait.AttackCost))
    ))

  // ---------- Признаки ----------

  /** Средний признак во времени у активных и (если имеет смысл) у корма. */
  def traitTrend(in: Input, t: Trait, withForage: Boolean): JFreeChart =
    val series = Seq[(String, Color, SectorCensus => Option[Double])](("активные", Color(70, 110, 200), _.active.mean(t))) ++
      (if withForage then Seq(("корм", Palette.byFamily(Family.Constant), (c: SectorCensus) => c.forage.mean(t))) else Nil)
    lines(s"Среднее: ${t.title}", t.title, in.censuses, series)

  /** Распределение признака у активных организмов прямо сейчас. */
  def traitHistogram(in: Input, t: Trait): JFreeChart =
    val ds = HistogramDataset()
    val values = in.active.map(t.value(_, in.world.rules)).toArray
    if values.nonEmpty then ds.addSeries("активные", values, 12)
    val chart = ChartFactory.createHistogram(
      s"Сейчас: распределение — ${t.title}", t.title, "организмов", ds, PlotOrientation.VERTICAL, false, true, false)
    chart.getXYPlot.getRenderer match
      case r: org.jfree.chart.renderer.xy.XYBarRenderer =>
        r.setBarPainter(org.jfree.chart.renderer.xy.StandardXYBarPainter())
        r.setShadowVisible(false)
        r.setSeriesPaint(0, Color(70, 110, 200))
      case r => r.setSeriesPaint(0, Color(70, 110, 200))
    style(chart)

  // ---------- Энергия и жизнь ----------

  /** Средние запасы и здоровье активных во времени, в процентах. */
  def vitals(in: Input): JFreeChart =
    lines("Запасы и здоровье активных", "%", in.censuses, Seq(
      ("сытость", Color(90, 170, 90), _.active.mean(Trait.Satiety)),
      ("жир", Color(220, 170, 40), _.active.mean(Trait.Reserve)),
      ("здоровье", Color(210, 80, 80), _.active.mean(Trait.Health))
    ))

  /** События активных на 1000 тиков: рождения, пробуждения, смерти. */
  def lifeRates(in: Input): JFreeChart =
    rates("Активные: события на 1000 тиков", in, Seq(
      ("рождения", Color(70, 110, 200), _.births),
      ("пробуждения", Color(160, 90, 200), _.awakenings),
      ("смерти", Color(60, 60, 60), _.deaths)
    ))

  /** Корм на 1000 тиков: сколько появилось и сколько съели. */
  def forageRates(in: Input): JFreeChart =
    rates("Корм: события на 1000 тиков", in, Seq(
      ("появилось", Palette.byFamily(Family.Constant), _.forageBorn),
      ("съедено", Color(200, 80, 60), _.forageEaten)
    ))

  // ---------- общие построители ----------

  private def lines(
      title: String,
      yLabel: String,
      points: Vector[(Long, SectorCensus)],
      series: Seq[(String, Color, SectorCensus => Option[Double])]
  ): JFreeChart =
    val ds = XYSeriesCollection()
    series.foreach { (name, _, f) =>
      val s = XYSeries(name, true, false)
      points.foreach((t, c) => f(c).foreach(v => s.add(t.toDouble, v)))
      ds.addSeries(s)
    }
    lineChart(title, yLabel, ds, series.map(_._2))

  /** Частота событий на 1000 тиков, сглаженная скользящим окном: для каждого снимка берётся разность
    * накопительных счётчиков с самым поздним снимком, отстоящим хотя бы на `RateWindow` тиков (или с первым).
    */
  private def rates(title: String, in: Input, series: Seq[(String, Color, SectorEvents => Long)]): JFreeChart =
    val events = in.samples.map(s => s.tick -> s.eventsOf(in.scope))
    val windows = events.indices.drop(1).map { i =>
      val t = events(i)._1
      val j = events.lastIndexWhere(_._1 <= t - RateWindow, i - 1).max(0)
      (events(j), events(i))
    }
    val ds = XYSeriesCollection()
    series.foreach { (name, _, f) =>
      val s = XYSeries(name, true, false)
      windows.foreach { case ((t0, e0), (t1, e1)) =>
        if t1 > t0 then s.add(t1.toDouble, (f(e1) - f(e0)) * 1000.0 / (t1 - t0))
      }
      ds.addSeries(s)
    }
    lineChart(title, "событий / 1000 тиков", ds, series.map(_._2))

  private def lineChart(title: String, yLabel: String, ds: XYSeriesCollection, colors: Seq[Color]): JFreeChart =
    val chart = ChartFactory.createXYLineChart(title, TickLabel, yLabel, ds, PlotOrientation.VERTICAL, true, true, false)
    val r = XYLineAndShapeRenderer(true, false)
    colors.zipWithIndex.foreach { (c, i) =>
      r.setSeriesPaint(i, c)
      r.setSeriesStroke(i, BasicStroke(1.6f))
    }
    chart.getXYPlot.setRenderer(r)
    style(chart)

  /** Столбчатая диаграмма, у каждого столбца свой цвет. */
  private def bars(title: String, valueLabel: String, items: Vector[(String, Color, Double)]): JFreeChart =
    val ds = DefaultCategoryDataset()
    items.foreach((name, _, v) => ds.addValue(v, "", name))
    val chart = ChartFactory.createBarChart(title, "", valueLabel, ds, PlotOrientation.VERTICAL, false, true, false)
    val colors = items.map(_._2)
    val renderer = new BarRenderer:
      override def getItemPaint(row: Int, column: Int): Paint = colors.lift(column).getOrElse(Color.GRAY)
    renderer.setShadowVisible(false)
    renderer.setBarPainter(org.jfree.chart.renderer.category.StandardBarPainter())
    chart.getCategoryPlot.setRenderer(renderer)
    style(chart)

  private def label(f: Family): String = if f == Family.Constant then "корм" else f.title

  /** Единый неброский стиль: белый фон, светлая сетка, компактный заголовок. */
  private def style(chart: JFreeChart): JFreeChart =
    chart.getTitle.setFont(Font(Font.SANS_SERIF, Font.BOLD, 12))
    chart.setBackgroundPaint(Color.WHITE)
    val plot = chart.getPlot
    plot.setBackgroundPaint(Color(250, 250, 252))
    plot match
      case xy: XYPlot =>
        xy.setDomainGridlinePaint(Color(220, 220, 225))
        xy.setRangeGridlinePaint(Color(220, 220, 225))
      case _ => ()
    chart
