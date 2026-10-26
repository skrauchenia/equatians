package equatians.sim

import equatians.model.*

import java.util.Locale

/** Тексты записей журнала. Собраны в одном месте, чтобы правила тика не тонули в строках. */
object Narrative:

  /** Число с одним знаком после запятой. */
  def num(v: Double): String = String.format(Locale.ROOT, "%.1f", v)

  /** Доля в процентах. */
  def pct(fraction: Double): String = s"${(fraction * 100).round}%"

  /** Краткая сводка запасов: сытость и жир в процентах. */
  def vitals(o: Organism, rules: Rules): String =
    s"сытость ${pct(o.satietyFraction(rules))}, жир ${pct(o.reserveFraction(rules))}"

  def born(o: Organism, rules: Rules): String =
    s"появился: здоровье ${num(o.health)}, ${vitals(o, rules)}; " +
      s"удар: дальность ${num(o.attack.range)}, стоимость ${num(o.attack.cost)}"

  def startChase(o: Organism, target: Organism, dist: Double, rules: Rules): String =
    s"решил преследовать #${target.id} (${target.family.title}) в ${num(dist)}: ${vitals(o, rules)}"

  def startFlee(o: Organism, threat: Organism, dist: Double, rules: Rules): String =
    s"решил отдалиться от #${threat.id} (${threat.family.title}) в ${num(dist)}: ${vitals(o, rules)}"

  def stopChase(target: Int): String = s"прекратил преследование #$target, обычное движение"
  def stopFlee(threat: Int): String = s"перестал убегать от #$threat, обычное движение"
  def targetGone(target: Int): String = s"цель #$target исчезла, обычное движение"
  def targetLost(target: Int): String = s"потерял #$target из виду, обычное движение"
  def escaped(threat: Int): String = s"оторвался от #$threat, обычное движение"

  def strike(target: Organism, dist: Double, damage: Double, sizeFactor: Double, cost: Double, hpAfter: Double): String =
    s"ударил #${target.id} с ${num(dist)}: урон ${num(damage)} (×${String.format(Locale.ROOT, "%.2f", sizeFactor)} за размер), " +
      s"стоимость ${num(cost)} " +
      s"(здоровье #${target.id}: ${num(target.health)} → ${num(hpAfter.max(0))})"

  def struck(attacker: Int, dist: Double, damage: Double, hpBefore: Double, hpAfter: Double): String =
    s"получил удар от #$attacker с ${num(dist)}: −${num(damage)} здоровья (${num(hpBefore)} → ${num(hpAfter.max(0))})"

  def fightBack(attacker: Int): String = s"отвечает на нападение: атакует #$attacker"
  def runAway(attacker: Int): String = s"спасается от #$attacker"

  def ate(victim: Organism, food: Double, meal: Meal): String =
    val left = food - meal.eaten
    s"убил #${victim.id} и съел ${num(meal.eaten)} из ${num(food)} еды (мясо + жир): " +
      s"+${num(meal.toSatiety)} сытости, +${num(meal.toReserve)} жира" +
      (if left > Simulation.CorpseMinReserve then s"; на трупе осталось ${num(left)}" else "")

  def spotCarrion(c: Corpse, dist: Double, rules: Rules, o: Organism): String =
    s"заметил падаль #${c.id} (еды ${num(c.food)}) в ${num(dist)} — идёт есть; ${vitals(o, rules)}"

  def carrionGone(c: Int): String = s"падаль #$c исчезла, обычное движение"

  def ateCarrion(c: Corpse, meal: Meal): String =
    val left = c.food - meal.eaten
    s"съел падаль #${c.id}: ${num(meal.eaten)} еды → +${num(meal.toSatiety)} сытости, +${num(meal.toReserve)} жира" +
      (if left > Simulation.CorpseMinReserve then s"; осталось ${num(left)}" else "; ничего не осталось")

  def seekMate(partner: Organism, dist: Double): String = s"ищет пару: идёт к #${partner.id} в ${num(dist)}"
  def mateLost(partner: Int): String = s"потерял партнёра #$partner из виду, обычное движение"
  def mateGaveUp(partner: Int): String = s"больше не готов к размножению, оставил #$partner"

  def mated(partner: Int, children: Vector[Int], wanted: Int, share: Double): String =
    if children.isEmpty then s"спарился с #$partner, но сил не хватило даже на одного потомка"
    else
      s"спарился с #$partner: потомство ${children.map(id => s"#$id").mkString(", ")}" +
        (if children.size < wanted then s" (хотели $wanted, хватило сил на ${children.size})" else "") +
        s"; потрачено ${num(share)} сытости"

  def bornChild(k: Genetics.Offspring, a: Organism, b: Organism, rules: Rules): String =
    val o = k.organism
    s"родился от #${a.id} × #${b.id}, поколение ${o.generation}: макс. здоровье ${num(o.maxHealth)}, " +
      s"${vitals(o, rules)}, помёт ${o.litterSize}; " +
      (if k.mutations.isEmpty then "без мутаций" else s"мутации: ${k.mutations.mkString("; ")}") +
      (if k.newSpecies then s" — НОВЫЙ ВИД (${o.family.title})" else "")

  def spotForage(f: Organism, dist: Double, rules: Rules, o: Organism): String =
    s"заметил корм #${f.id} в ${num(dist)} — охотится; ${vitals(o, rules)}"

  def sprouted: String = "появился как корм"

  def buddedOff(k: Genetics.Offspring): String =
    s"отпочковал #${k.organism.id}" + (if k.newSpecies then s" — тот пробудился (${k.organism.family.title})" else "")

  def budded(k: Genetics.Offspring, parent: Organism): String =
    val o = k.organism
    val head =
      if k.newSpecies then s"ПРОБУДИЛСЯ из корма #${parent.id}: новый активный организм (${o.family.title}), поколение ${o.generation}"
      else s"отпочковался от корма #${parent.id}, поколение ${o.generation}"
    head + s": макс. здоровье ${num(o.maxHealth)}, скорость ${num(o.movement.speed)}; " +
      (if k.mutations.isEmpty then "без мутаций" else s"мутации: ${k.mutations.mkString("; ")}")

  def fullyHealed: String = "здоровье полностью восстановлено"

  def killedBy(killer: Int): String = s"погиб от удара #$killer"
  def starved: String = "умер от голода"

  def burningReserve: String = "сытость на нуле — расходует жир"
  def starvingNoReserve: String = "сытость на нуле, жира нет — голодает, теряет здоровье"
  def reserveExhausted: String = "жир кончился — голодает, теряет здоровье"
