package equatians.model

/** Экономика сытости и жира.
  *
  * @param reservePerHealth ёмкость жира на единицу максимального здоровья
  * @param reserveDensity сколько единиц сытости даёт одна единица жира
  * @param baseMetabolism расход за тик на единицу радиуса тела, даже стоя на месте
  * @param movementCost   добавка к расходу на единицу радиуса за каждый квадрат длины шага
  * @param starvationDamage сколько здоровья стоит единица расхода, которую нечем оплатить
  */
final case class EnergyRules(
    maxSatiety: Double,
    reservePerHealth: Double,
    reserveDensity: Double,
    baseMetabolism: Double,
    movementCost: Double,
    starvationDamage: Double
):
  require(maxSatiety > 0 && reservePerHealth > 0 && reserveDensity > 0, "ёмкости и плотность жира должны быть > 0")

/** Восстановление здоровья: `rate` единиц за тик по цене `cost` сытости за единицу. */
final case class HealingRules(rate: Double, cost: Double)

/** Растения — возобновляемая еда.
  *
  * @param spawnRate        вероятность за тик, что в секторе вырастет растение
  * @param maxPerSector     предел числа растений в секторе
  * @param initialPerSector сколько растений в секторе при генезисе
  * @param food             еда в одном растении, в единицах жира
  * @param satietyThreshold растения интересны, пока доля сытости ниже порога
  * @param probability      вероятность за тик пойти к замеченному растению
  */
final case class PlantRules(
    spawnRate: Double,
    maxPerSector: Int,
    initialPerSector: Int,
    food: Double,
    satietyThreshold: Double,
    probability: Double
):
  require(spawnRate >= 0 && spawnRate <= 1, "plants.spawn-rate должен быть в [0, 1]")

/** Поедание падали.
  *
  * @param probability      вероятность за тик пойти к замеченной падали
  * @param satietyThreshold падаль интересна, только пока доля сытости ниже порога
  * @param eatReach         расстояние от края тела, с которого можно есть
  */
final case class ScavengeRules(probability: Double, satietyThreshold: Double, eatReach: Double)

/** Размножение.
  *
  * @param maturityAge       возраст, с которого можно размножаться
  * @param cooldown          перерыв между размножениями
  * @param probability       вероятность пойти к готовому партнёру при очередном решении
  * @param reach             расстояние между краями тел, на котором происходит спаривание
  * @param bodyCostPerHealth стоимость тела потомка в сытости на единицу его макс. здоровья
  * @param offspringSatiety  сытость новорождённого как доля от максимума — тоже оплачивается родителями
  * @param maxLitter         верхняя граница гена размера помёта
  */
final case class ReproductionRules(
    maturityAge: Int,
    cooldown: Int,
    satietyThreshold: Double,
    reserveThreshold: Double,
    healthThreshold: Double,
    probability: Double,
    reach: Double,
    bodyCostPerHealth: Double,
    offspringSatiety: Double,
    maxLitter: Int
):
  require(maxLitter >= 1, "max-litter должен быть ≥ 1")

/** Мутации при рождении.
  *
  * @param rate          вероятность мутации каждого числового гена
  * @param relativeSigma доля |гена| в σ нормального шума
  * @param absoluteSigma постоянная часть σ, чтобы мутировали и нулевые гены
  * @param familyRate    вероятность смены семейства функции (для движения — новый вид)
  */
final case class MutationRules(rate: Double, relativeSigma: Double, absoluteSigma: Double, familyRate: Double)

/** Параметры выбора поведения при встрече с чужим организмом. */
final case class BehaviourRules(
    decisionInterval: Int,
    hungerThreshold: Double,
    maxChaseProbability: Double,
    fleeSatietyThreshold: Double,
    fleeReserveThreshold: Double,
    fleeProbability: Double,
    loseTargetFactor: Double
):
  require(decisionInterval > 0, "decision-interval должен быть > 0")
  require(hungerThreshold > 0, "hunger-threshold должен быть > 0")

/** Общие для всех органов атаки константы.
  *
  * @param maxReach  максимально возможная дальность удара; аргумент функции урона — d / maxReach
  * @param costFactor множитель стоимости удара относительно ∫ урона по дальности
  * @param cooldown  минимум тиков между ударами
  * @param referenceHealth здоровье, при котором множитель урона за размер равен 1
  */
final case class AttackRules(maxReach: Double, costFactor: Double, cooldown: Int, referenceHealth: Double)

/** Законы мира, одинаковые для всех организмов.
  *
  * @param sizePerSqrtHealth радиус тела на √(макс. здоровье)
  * @param meatPerHealth     еда (в единицах жира), которую даёт тело погибшего на единицу его макс. здоровья
  */
final case class Rules(
    perceptionRadius: Double,
    logSize: Int,
    graveyardSize: Int,
    sizePerSqrtHealth: Double,
    meatPerHealth: Double,
    energy: EnergyRules,
    healing: HealingRules,
    plants: PlantRules,
    scavenging: ScavengeRules,
    reproduction: ReproductionRules,
    mutation: MutationRules,
    behaviour: BehaviourRules,
    attack: AttackRules
):
  require(
    reproduction.bodyCostPerHealth >= meatPerHealth * energy.reserveDensity,
    s"reproduction.body-cost-per-health (${reproduction.bodyCostPerHealth}) должен быть не меньше " +
      s"meat-per-health · reserve-density (${meatPerHealth * energy.reserveDensity}): " +
      "иначе выгодно рожать детей и есть их падаль"
  )
