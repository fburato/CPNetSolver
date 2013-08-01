/**
 * File: ClassicCounter.scala
 * Package: solver
 * Autore: Francesco Burato
 * Creazione: 31/lug/2013
 */
package solver

/**
 * @author Francesco Burato
 *
 */
class ClassicCounter(val size: Int) extends Counter {
  if (size < 0)
    throw new Exception("Cannot initialize Counter with negative number of sleeve")
  private val dimensions = new Array[Int](size)
  private val values = new Array[Int](size)
  private var unsettedSleeves = size
  private var myEnd = false

  def end = myEnd

  for (i <- 0 until size) {
    values(i) = 0
    dimensions(i) = -1 //segnalo la ghiera i-esima come non inizializzata
  }

  // interfaccia pubblica
  def setSleeve(s: Int, sleeveSize: Int): Boolean =
    if (s < 0 || s >= size || sleeveSize < 0)
      // se gli indici non sono valori validi per l'inizializzazione restituisco falso
      throw new Exception("Cannot initialize sleeve " + s + " with size " + sleeveSize)
    else if (dimensions(s) != -1)
      init
    else {
      //indici validi e la ghiera non è stata ancora impostata
      dimensions(s) = sleeveSize
      unsettedSleeves -= 1
      init
    }

  def init: Boolean = unsettedSleeves == 0

  def getSleeveSize(i: Int): Int =
    if (i < 0 || i >= size || !init)
      -1
    else
      dimensions(i)

  def apply(i: Int): Int =
    if (i < 0 || i >= size || !init)
      -1
    else
      values(i)

  def reset: Unit =
    for (i <- 0 until size)
      values(i) = 0

  def ++(): Int =
    if (init && !end) {
      var reminder = 1
      var i = 0
      while (i < size && reminder > 0) {
        values(i) = (values(i) + 1) % dimensions(i)
        if (values(i) != 0)
          reminder = 0
        i += 1
      }
      i -= 1
      if (i == 0 && values(i) == dimensions(i)-1) {
        // se l'ultimo aggiornamento che ho fatto è stato alla ghiera meno
        // significativa potrei essere all'ultimo valore... controllo
        var stillMax = true
        while(i < size-1 && stillMax) {
          stillMax = values(size-1-i) == dimensions(size-1-i)-1
          i += 1
        }
        if(stillMax)
          myEnd = true
      }
      0
    } else -1

}