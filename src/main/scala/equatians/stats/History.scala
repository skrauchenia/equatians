package equatians.stats

import equatians.model.*

import scala.collection.mutable.ArrayBuffer

/** Что отображать: один сектор или весь мир. */
enum Scope:
  case Sector(id: SectorId)
  case Whole

  def title: String = this match
    case Sector(id) => s"сектор (${id.col}, ${id.row})"
    case Whole      => "весь мир"

/** Снимок мира на тике `tick`: перепись и накопительные счётчики событий по секторам. */
final case class Sample(tick: Long, census: Map[SectorId, SectorCensus], events: Map[SectorId, SectorEvents]):

  /** Перепись в выбранной области. */
  def censusOf(scope: Scope): SectorCensus = scope match
    case Scope.Sector(id) => census.getOrElse(id, SectorCensus.Empty)
    case Scope.Whole      => census.valuesIterator.foldLeft(SectorCensus.Empty)(_ + _)

  /** Накопительные события в выбранной области. */
  def eventsOf(scope: Scope): SectorEvents = scope match
    case Scope.Sector(id) => events.getOrElse(id, SectorEvents.Zero)
    case Scope.Whole      => events.valuesIterator.foldLeft(SectorEvents.Zero)(_ + _)

object Sample:
  /** Снимок текущего состояния мира. */
  def of(world: World): Sample = Sample(world.tick, Census.of(world), world.events)

/** История снимков для графиков. Пишется раз в `interval` тиков.
  *
  * Память ограничена: когда снимков становится больше `maxSamples`, каждый второй выбрасывается,
  * а интервал удваивается. События хранятся накопительно, поэтому прореживание их не теряет —
  * частота событий за любой промежуток считается по разности соседних снимков.
  */
final class History(initialInterval: Int, maxSamples: Int):
  private val samples = ArrayBuffer.empty[Sample]
  private var interval = initialInterval

  /** Текущий интервал между снимками, в тиках. */
  def currentInterval: Int = interval

  /** Все снимки по возрастанию тика. */
  def snapshot: Vector[Sample] = samples.toVector

  /** Вызывать после каждого тика: снимает перепись, если подошёл её тик. */
  def record(world: World): Unit =
    if world.tick % interval == 0 && samples.lastOption.forall(_.tick < world.tick) then
      samples += Sample.of(world)
      if samples.size > maxSamples then thin()

  /** Забывает историю (например, при создании нового мира). */
  def reset(world: World): Unit =
    samples.clear()
    interval = initialInterval
    samples += Sample.of(world)

  /** Оставляет каждый второй снимок (первый и последний сохраняются) и удваивает интервал. */
  private def thin(): Unit =
    val kept = samples.zipWithIndex.collect { case (s, i) if i % 2 == 0 || i == samples.size - 1 => s }
    samples.clear()
    samples ++= kept
    interval *= 2
