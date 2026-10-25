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

/** Неизменяемое состояние мира на тике `tick`. */
final case class World(config: WorldConfig, organisms: Vector[Organism], tick: Long = 0):
  /** Следующее состояние: каждый организм делает один шаг. */
  def step: World =
    copy(organisms = organisms.map(_.step(config.sectorSize)), tick = tick + 1)

  /** Организмы, находящиеся в секторе `sector`. */
  def organismsIn(sector: SectorId): Vector[Organism] = organisms.filter(_.sector == sector)
