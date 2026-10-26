package equatians.model

/** Что организм делает прямо сейчас. */
enum Behaviour:
  /** Обычное движение по функции органа движения. */
  case Cruise
  /** Преследует организм `target` и бьёт его, когда тот в зоне удара. */
  case Chase(target: Int)
  /** Удаляется от организма `from`. */
  case Flee(from: Int)
  /** Идёт есть падаль `corpse`. */
  case Scavenge(corpse: Int)
  /** Идёт к партнёру своего вида, чтобы размножиться. */
  case Mate(partner: Int)

/** Труп, который может доесть любой организм.
  *
  * @param id   id погибшего организма
  * @param food сколько еды осталось, в единицах жира (мясо тела плюс несъеденный жир)
  */
final case class Corpse(id: Int, species: Family, sector: SectorId, pos: Vec2, food: Double, diedAt: Long)

/** Запись в журнале организма. */
final case class LogEntry(tick: Long, text: String)

/** Организм.
  *
  * `pos` — локальные декартовы координаты внутри сектора: (0, 0) — левый нижний угол,
  * ось y направлена вверх. Переход в другой сектор пока не реализован: граница сектора —
  * стена, от которой организм отражается. Отражение запоминается флагами `mirrorX`/`mirrorY`,
  * потому что в обычном движении направление задаёт функция, а не текущая скорость.
  *
  * @param health    текущее здоровье. При health ≤ 0 организм мёртв
  * @param maxHealth максимальное здоровье — весовая категория: от него зависят размер, урон и ёмкость жира
  * @param satiety   сытость — ею оплачиваются движение и удары
  * @param reserve   жир — тратится, когда сытость на нуле; одна единица = `reserveDensity` сытости.
  *                  Ёмкость жира пропорциональна `maxHealth`
  * @param lastStep  фактический шаг на прошлом тике — для отрисовки курса и расчёта расхода
  * @param litterSize ген: желаемое число детей за одно размножение
  * @param generation поколение: 0 у созданных генезисом, у потомка — максимум родителей плюс один
  * @param parents   id родителей: двое при скрещивании, один при почковании корма, никого у созданных миром
  * @param lastReproductionTick тик последнего размножения — для перерыва между ними
  * @param offspringCount сколько всего детей родил организм
  * @param healthGene спящий ген макс. здоровья у корма: тело корма всегда маленькое (`forage.health`),
  *                   а этот ген наследуется, мутирует и включается, когда клон пробуждается.
  *                   У активных организмов `None` — их ген и есть `maxHealth`
  * @param aggression ген агрессивности 0..1: чем выше, тем охотнее организм нападает на чужаков, реже убегает
  *                   и чаще предпочитает драку еде. У корма спящий
  */
final case class Organism(
    id: Int,
    movement: MovementOrgan,
    attack: AttackOrgan,
    sector: SectorId,
    pos: Vec2,
    health: Double,
    maxHealth: Double,
    satiety: Double,
    reserve: Double,
    age: Long = 0,
    mirrorX: Boolean = false,
    mirrorY: Boolean = false,
    behaviour: Behaviour = Behaviour.Cruise,
    lastAttackTick: Long = Long.MinValue / 2,
    lastStep: Vec2 = Vec2.Zero,
    log: Vector[LogEntry] = Vector.empty,
    litterSize: Int = 1,
    generation: Int = 0,
    parents: Vector[Int] = Vector.empty,
    lastReproductionTick: Long = Long.MinValue / 2,
    offspringCount: Int = 0,
    healthGene: Option[Double] = None,
    aggression: Double = 0
):
  /** Семейство функции органа движения. */
  def family: Family = movement.f.family

  /** Тип организма. Пока он определяется только функцией движения; свои своих не атакуют. */
  def species: Family = family

  /** Ген макс. здоровья: у корма — спящий, у остальных совпадает с `maxHealth`. */
  def sizeGene: Double = healthGene.getOrElse(maxHealth)

  /** Корм — организм с функцией движения-константой: ползёт по прямой и служит едой.
    * Не ест, не голодает, не лечится, не убегает и не дерётся; размножается делением.
    */
  def forage: Boolean = species == Family.Constant

  /** Жив ли организм. */
  def alive: Boolean = health > 0

  /** Радиус тела: r = k·√maxHealth — площадь пропорциональна максимальному здоровью («весовой категории»).
    * Ранения размер не уменьшают.
    */
  def radius(rules: Rules): Double = rules.sizePerSqrtHealth * math.sqrt(maxHealth)

  /** Доля сытости от максимума, 0..1. */
  def satietyFraction(rules: Rules): Double = satiety / rules.energy.maxSatiety

  /** Ёмкость жира: пропорциональна максимальному здоровью — крупные запасают больше. */
  def maxReserve(rules: Rules): Double = rules.energy.reservePerHealth * maxHealth

  /** Доля жира от максимума, 0..1. */
  def reserveFraction(rules: Rules): Double = reserve / maxReserve(rules)

  /** Сколько еды (в единицах жира) даст тело после смерти: мясо по весовой категории плюс оставшийся жир. */
  def bodyFood(rules: Rules): Double = rules.meatPerHealth * maxHealth + reserve

  /** Множитель урона за весовую категорию: √(maxHealth / reference-health).
    * Зависит от максимального здоровья, поэтому раненый крупный организм бьёт так же сильно.
    */
  def sizeFactor(rules: Rules): Double = math.sqrt(maxHealth / rules.attack.referenceHealth)

  /** Урон, который организм нанесёт на дистанции `distance`, с учётом своего размера. */
  def strikeDamage(distance: Double, rules: Rules): Double = attack.damage(distance) * sizeFactor(rules)

  /** Вся доступная энергия в единицах сытости: сытость плюс жир по курсу. */
  def energy(rules: Rules): Double = satiety + reserve * rules.energy.reserveDensity

  /** Готов ли к размножению на тике `now`: взрослый, перерыв прошёл, сытость, жир и здоровье выше порогов. */
  def readyToMate(now: Long, rules: Rules): Boolean =
    val r = rules.reproduction
    alive && !forage && age >= r.maturityAge && now - lastReproductionTick >= r.cooldown &&
      satietyFraction(rules) >= r.satietyThreshold && reserveFraction(rules) >= r.reserveThreshold &&
      health / maxHealth >= r.healthThreshold

  /** Расстояние до другого организма (имеет смысл только внутри одного сектора). */
  def distanceTo(other: Organism): Double = (other.pos - pos).length

  /** Шаг обычного движения на текущем тике, с учётом отражений от стен. */
  def cruiseDisplacement: Vec2 =
    val d = movement.displacement(age)
    Vec2(if mirrorX then -d.x else d.x, if mirrorY then -d.y else d.y)

  /** Направление движения: по фактическому прошлому шагу, а до первого шага — по органу. */
  def heading: Double =
    val d = if lastStep.length > 0 then lastStep else cruiseDisplacement
    math.atan2(d.y, d.x)

  /** Списывает `amount` сытости: сначала из сытости, остаток — из жира по курсу.
    *
    * @return организм после оплаты и неоплаченный остаток (в единицах сытости)
    */
  def pay(amount: Double, rules: Rules): (Organism, Double) =
    val density = rules.energy.reserveDensity
    val fromSatiety = math.min(amount, satiety)
    val reserveNeeded = (amount - fromSatiety) / density
    val fromReserve = math.min(reserveNeeded, reserve)
    val unpaid = (reserveNeeded - fromReserve) * density
    (copy(satiety = satiety - fromSatiety, reserve = reserve - fromReserve), unpaid)

  /** Съедает до `food` единиц жира: сначала по курсу заполняется сытость, излишек уходит в жир.
    * Сколько не влезло — остаётся несъеденным (например, на трупе).
    */
  def eat(food: Double, rules: Rules): Meal =
    val e = rules.energy
    val energyGained = food.max(0) * e.reserveDensity
    val toSatiety = math.min(energyGained, (e.maxSatiety - satiety).max(0))
    val toReserve = math.min((energyGained - toSatiety) / e.reserveDensity, (maxReserve(rules) - reserve).max(0))
    Meal(copy(satiety = satiety + toSatiety, reserve = reserve + toReserve), toSatiety, toReserve,
      eaten = toSatiety / e.reserveDensity + toReserve)

  /** Восстанавливает до `rules.healing.rate` здоровья за тик, пока хватает энергии оплатить. */
  def heal(rules: Rules): Organism =
    val missing = maxHealth - health
    if missing <= 0 || !alive then this
    else
      val h = rules.healing
      val amount = Seq(h.rate, missing, energy(rules) / h.cost).min
      if amount <= 0 then this
      else pay(amount * h.cost, rules)._1.copy(health = health + amount)

  /** Добавляет запись в журнал, храня не больше `limit` последних записей. */
  def logged(tick: Long, text: String, limit: Int): Organism =
    val appended = log :+ LogEntry(tick, text)
    copy(log = if appended.size > limit then appended.drop(appended.size - limit) else appended)

/** Результат еды.
  *
  * @param organism  организм после еды
  * @param toSatiety прибавка сытости
  * @param toReserve прибавка жира
  * @param eaten     сколько жира еды реально съедено
  */
final case class Meal(organism: Organism, toSatiety: Double, toReserve: Double, eaten: Double)
