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
  * @param corpses   трупы с недоеденным жиром
  */
final case class World(
    config: WorldConfig,
    rules: Rules,
    organisms: Vector[Organism],
    seed: Long,
    tick: Long = 0,
    graveyard: Vector[Organism] = Vector.empty,
    corpses: Vector[Corpse] = Vector.empty
):
  /** Организмы, находящиеся в секторе `sector`. */
  def organismsIn(sector: SectorId): Vector[Organism] = organisms.filter(_.sector == sector)

  /** Организм по id — среди живых, а если его нет — среди недавно погибших. */
  def find(id: Int): Option[Organism] =
    organisms.find(_.id == id).orElse(graveyard.findLast(_.id == id))
