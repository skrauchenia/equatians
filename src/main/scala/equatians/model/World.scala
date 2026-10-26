package equatians.model

/** Координаты сектора в сетке мира: col — слева направо, row — снизу вверх. */
final case class SectorId(col: Int, row: Int)

/** Геометрия мира: сетка `cols × rows` квадратных секторов со стороной `sectorSize`. */
final case class WorldConfig(cols: Int, rows: Int, sectorSize: Double):
  require(cols > 0 && rows > 0, "в мире должен быть хотя бы один сектор")
  require(sectorSize > 0, "sector-size должен быть положительным")

  /** Все сектора мира, построчно снизу вверх. */
  def sectors: Seq[SectorId] =
    for
      row <- 0 until rows
      col <- 0 until cols
    yield SectorId(col, row)

/** Неизменяемое состояние мира на тике `tick`.
  *
  * Следующее состояние считает `equatians.sim.Simulation.step`.
  *
  * @param seed      сид мира; вместе с тиком и id организма даёт все случайные решения
  * @param graveyard последние погибшие организмы — чтобы можно было посмотреть их журнал
  * @param genesis   диапазоны генов генезиса — нужны мутации, меняющей семейство функции
  * @param corpses   трупы с недоеденной едой
  * @param plants    растения
  * @param nextPlantId id для следующего растения
  * @param nextId    id для следующего родившегося организма
  * @param births    сколько организмов родилось с начала мира
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
    plants: Vector[Plant] = Vector.empty,
    nextPlantId: Int = 0,
    nextId: Int = 0,
    births: Long = 0
):
  /** Организмы, находящиеся в секторе `sector`. */
  def organismsIn(sector: SectorId): Vector[Organism] = organisms.filter(_.sector == sector)

  /** Организм по id — среди живых, а если его нет — среди недавно погибших. */
  def find(id: Int): Option[Organism] =
    organisms.find(_.id == id).orElse(graveyard.findLast(_.id == id))
