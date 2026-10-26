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

/** Поедание падали.
  *
  * @param probability      вероятность за тик пойти к замеченной падали
  * @param satietyThreshold падаль интересна, только пока доля сытости ниже порога
  * @param eatReach         расстояние от края тела, с которого можно есть
  */
final case class ScavengeRules(probability: Double, satietyThreshold: Double, eatReach: Double)

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
    scavenging: ScavengeRules,
    behaviour: BehaviourRules,
    attack: AttackRules
)
