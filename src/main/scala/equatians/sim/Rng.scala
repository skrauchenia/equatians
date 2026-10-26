package equatians.sim

/** Детерминированный «бросок кубика» без изменяемого состояния.
  *
  * Число зависит только от (сид мира, тик, id организма, соль), поэтому результат шага
  * не зависит от порядка обхода организмов, а один и тот же мир всегда развивается одинаково.
  */
object Rng:

  /** Равномерное число из [0, 1). `salt` различает разные броски одного организма на одном тике. */
  def uniform(seed: Long, tick: Long, id: Int, salt: Int): Double =
    (long(seed, tick, id, salt) >>> 11).toDouble / (1L << 53).toDouble

  /** 64-битное хэш-число тех же аргументов — например, сид для `scala.util.Random`. */
  def long(seed: Long, tick: Long, id: Int, salt: Int): Long =
    var z = seed * 0x9E3779B97F4A7C15L + tick * 0xBF58476D1CE4E5B9L + id.toLong * 0x94D049BB133111EBL + salt
    z = (z ^ (z >>> 30)) * 0xBF58476D1CE4E5B9L
    z = (z ^ (z >>> 27)) * 0x94D049BB133111EBL
    z ^ (z >>> 31)
