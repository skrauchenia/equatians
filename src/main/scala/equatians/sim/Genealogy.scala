package equatians.sim

import equatians.model.*

import scala.annotation.tailrec

/** Реестр родословной мира: записи о всех организмах, у которых есть живые (или недавно погибшие) потомки.
  *
  * Записи появляются при рождении и отмечаются при смерти. Раз в [[PruneInterval]] тиков реестр
  * прореживается: остаются живые, недавно погибшие (кладбище) и все их предки, остальное забывается.
  * Поэтому линия любого живого организма прослеживается до самого начала мира, а память не растёт без конца.
  */
object Genealogy:

  /** Как часто прореживать реестр, в тиках. */
  val PruneInterval = 1000

  /** Шаг прослеживания гена: организм, значение гена у него, мутация (если была) и от кого ген пришёл.
    *
    * @param from родитель, от которого ген унаследован; `None` — линия на этом обрывается
    * @param end  почему линия обрывается: организм создан миром, ген появился при смене семейства,
    *             или запись о предке не сохранилась
    */
  final case class TraceStep(record: LineageRecord, value: Option[GeneValue], change: Option[GeneChange], from: Option[Int], end: Option[String])

  /** Запись о родившемся (или созданном миром) организме. */
  def record(o: Organism, born: Long, origin: Map[String, Int] = Map.empty, changes: Map[String, GeneChange] = Map.empty): LineageRecord =
    LineageRecord(o.id, born, o.parents, o.generation, o.species, Genes.of(o), origin, changes)

  /** Отмечает смерть: тик и последняя запись журнала. */
  def markDead(lineage: Map[Int, LineageRecord], dead: Iterable[Organism], tick: Long): Map[Int, LineageRecord] =
    dead.foldLeft(lineage) { (acc, o) =>
      acc.get(o.id).fold(acc)(r => acc.updated(o.id, r.copy(died = Some(tick), cause = o.log.lastOption.map(_.text))))
    }

  /** Оставляет записи организмов `keep` и всех их предков. */
  def prune(lineage: Map[Int, LineageRecord], keep: Iterable[Int]): Map[Int, LineageRecord] =
    @tailrec
    def closure(frontier: List[Int], seen: Set[Int]): Set[Int] = frontier match
      case Nil => seen
      case id :: rest =>
        if seen(id) then closure(rest, seen)
        else closure(lineage.get(id).fold(rest)(_.parents.toList ++ rest), seen + id)
    val kept = closure(keep.toList, Set.empty)
    lineage.filter((id, _) => kept(id))

  /** Прослеживает ген `key` от организма `id` вверх по предкам: на каждом шаге — от какого родителя ген пришёл.
    * Линия идёт, пока у гена есть родитель-источник; не длиннее `limit` шагов.
    */
  def trace(lineage: Map[Int, LineageRecord], id: Int, key: String, limit: Int = 100000): Vector[TraceStep] =
    @tailrec
    def loop(current: Int, acc: Vector[TraceStep]): Vector[TraceStep] =
      lineage.get(current) match
        case None => acc
        case Some(r) =>
          val change = r.mutations.get(key)
          val appeared = change.exists(_.before.isEmpty)
          val from = if appeared then None else r.origin.get(key)
          val end =
            if appeared then Some("ген появился при смене семейства функции")
            else if r.parents.isEmpty then Some("создан миром")
            else if from.exists(p => !lineage.contains(p)) then Some("запись о предке не сохранилась")
            else if from.isEmpty then Some("происхождение гена неизвестно")
            else None
          val step = TraceStep(r, r.gene(key), change, from, end)
          from match
            case Some(p) if end.isEmpty && acc.size < limit => loop(p, acc :+ step)
            case _                                          => acc :+ step
    loop(id, Vector.empty)

  /** Корм-предки корма `id` подряд: родитель, его родитель и т.д., пока это корм (клоны с одним родителем).
    * Сам `id` не входит; ближайший предок — первый.
    */
  def forageAncestors(lineage: Map[Int, LineageRecord], id: Int): Vector[LineageRecord] =
    @tailrec
    def loop(current: LineageRecord, acc: Vector[LineageRecord]): Vector[LineageRecord] =
      current.parents match
        case Vector(p) =>
          lineage.get(p) match
            case Some(parent) if parent.forage => loop(parent, acc :+ parent)
            case _                             => acc
        case _ => acc
    lineage.get(id).filter(_.forage).fold(Vector.empty)(loop(_, Vector.empty))

  /** Предки организма по уровням: 0 — сам организм, 1 — родители, 2 — их родители и т.д.
    * Каждый предок попадает на уровень, где встречается впервые (при близкородственном скрещивании
    * один предок бывает и дедом, и прадедом).
    *
    * Корм не раскрывается: его предки — длинная цепочка почти одинаковых клонов, поэтому показывается
    * только ближайший корм, а длину цепочки над ним даёт [[forageAncestors]].
    */
  def layers(lineage: Map[Int, LineageRecord], id: Int, depth: Int): Vector[Vector[LineageRecord]] =
    def parentsShown(i: Int) = lineage.get(i).filterNot(_.forage).toVector.flatMap(_.parents)
    Iterator
      .iterate((Vector(id), Set(id))) { (level, seen) =>
        val next = level.flatMap(parentsShown).distinct.filterNot(seen)
        (next, seen ++ next)
      }
      .map(_._1.flatMap(lineage.get))
      .take(depth + 1)
      .takeWhile(_.nonEmpty)
      .toVector
