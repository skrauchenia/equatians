package equatians.model

/** Координаты сектора в сетке мира: col — слева направо, row — снизу вверх. */
final case class SectorId(col: Int, row: Int)

/** Геометрия мира: сетка `cols × rows` квадратных секторов со стороной `sectorSize`.
  *
  * @param passage ширина прохода в середине каждой общей стены соседних секторов, доля стороны (0 — без проходов)
  */
final case class WorldConfig(cols: Int, rows: Int, sectorSize: Double, passage: Double = 0):
  require(cols > 0 && rows > 0, "в мире должен быть хотя бы один сектор")
  require(sectorSize > 0, "sector-size должен быть положительным")
  require(passage >= 0 && passage <= 1, "world.passage должен быть в [0, 1]")

  /** Есть ли такой сектор в мире. */
  def contains(s: SectorId): Boolean = s.col >= 0 && s.col < cols && s.row >= 0 && s.row < rows

  /** Половина ширины прохода в единицах мира. */
  def passageHalfWidth: Double = sectorSize * passage / 2

  /** Попадает ли координата вдоль стены в проём прохода (проём — в середине стены). */
  def inPassage(along: Double): Boolean = passage > 0 && math.abs(along - sectorSize / 2) <= passageHalfWidth

  /** Если точка `target` (локальные координаты сектора `from`) вышла за стену в месте прохода и за стеной есть
    * сектор — этот сектор и та же точка в его координатах. Иначе `None`: стена глухая, организм отражается.
    */
  def crossing(from: SectorId, target: Vec2): Option[(SectorId, Vec2)] =
    val s = sectorSize
    val step =
      if target.x > s && inPassage(target.y) then Some((1, 0, Vec2(target.x - s, target.y)))
      else if target.x < 0 && inPassage(target.y) then Some((-1, 0, Vec2(target.x + s, target.y)))
      else if target.y > s && inPassage(target.x) then Some((0, 1, Vec2(target.x, target.y - s)))
      else if target.y < 0 && inPassage(target.x) then Some((0, -1, Vec2(target.x, target.y + s)))
      else None
    step.flatMap { (dc, dr, p) =>
      val next = SectorId(from.col + dc, from.row + dr)
      Option.when(contains(next))(next -> Vec2(p.x.max(0).min(s), p.y.max(0).min(s)))
    }

  /** Все сектора мира, построчно снизу вверх. */
  def sectors: Seq[SectorId] =
    for
      row <- 0 until rows
      col <- 0 until cols
    yield SectorId(col, row)

/** Накопительные счётчики событий в секторе с начала мира.
  *
  * @param births      рождений от скрещивания
  * @param awakenings  пробуждений корма в активные организмы
  * @param forageBorn  нового корма (почкование и самопоявление)
  * @param deaths      смертей активных организмов (в бою и от голода)
  * @param forageEaten съеденного корма
  * @param arrived     активных организмов, пришедших через проходы
  * @param left        активных организмов, ушедших через проходы
  */
final case class SectorEvents(
    births: Long = 0,
    awakenings: Long = 0,
    forageBorn: Long = 0,
    deaths: Long = 0,
    forageEaten: Long = 0,
    arrived: Long = 0,
    left: Long = 0
):
  def +(o: SectorEvents): SectorEvents = SectorEvents(
    births + o.births,
    awakenings + o.awakenings,
    forageBorn + o.forageBorn,
    deaths + o.deaths,
    forageEaten + o.forageEaten,
    arrived + o.arrived,
    left + o.left
  )

  def -(o: SectorEvents): SectorEvents = this + o.scaled(-1)

  private def scaled(k: Long) =
    SectorEvents(births * k, awakenings * k, forageBorn * k, deaths * k, forageEaten * k, arrived * k, left * k)

object SectorEvents:
  val Zero: SectorEvents = SectorEvents()

/** Неизменяемое состояние мира на тике `tick`.
  *
  * Следующее состояние считает `equatians.sim.Simulation.step`.
  *
  * @param seed      сид мира; вместе с тиком и id организма даёт все случайные решения
  * @param graveyard последние погибшие организмы — чтобы можно было посмотреть их журнал
  * @param genesis   диапазоны генов генезиса — нужны мутации, меняющей семейство функции
  * @param corpses   трупы с недоеденной едой
  * @param nextId    id для следующего организма (родившегося или кормового)
  * @param births    сколько организмов родилось с начала мира (без корма)
  * @param forageSpawned сколько корма появилось в мире само по себе (включая генезис)
  * @param buds      сколько корма отпочковалось (включая пробудившихся)
  * @param awakenings сколько клонов корма пробудилось в активные организмы
  * @param events    накопительные счётчики событий по секторам — для статистики
  * @param lineage   родословная: записи о живых, недавно погибших и всех их предках (см. `sim.Genealogy`)
  */
final case class World(
    config: WorldConfig,
    rules: Rules,
    genesis: GenesisConfig,
    organisms: Vector[Organism],
    seed: Long,
    tick: Long = 0,
    graveyard: Vector[Organism] = Vector.empty,
    corpses: Vector[Corpse] = Vector.empty,
    nextId: Int = 0,
    births: Long = 0,
    forageSpawned: Long = 0,
    buds: Long = 0,
    awakenings: Long = 0,
    events: Map[SectorId, SectorEvents] = Map.empty,
    lineage: Map[Int, LineageRecord] = Map.empty
):
  /** Организмы, находящиеся в секторе `sector`. */
  def organismsIn(sector: SectorId): Vector[Organism] = organisms.filter(_.sector == sector)

  /** Организм по id — среди живых, а если его нет — среди недавно погибших. */
  def find(id: Int): Option[Organism] =
    organisms.find(_.id == id).orElse(graveyard.findLast(_.id == id))
