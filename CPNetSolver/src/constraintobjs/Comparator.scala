/**
 * File: Comparator.scala
 * Package: constraintobjs
 * Autore: Francesco Burato
 * Creazione: 28/giu/2013
 */
package constraintobjs

/**
 * Comparatore di elementi all'interno di un ordinamento
 * @author Francesco Burato
 *
 */
trait Comparator {
  /**
   * Aggiuge al comparatore attuale un'assegnamento di variabile a valore.
   * Restituisce true iff il comparatore è stabile (non è necessario specificare altre 
   * informazioni per confrontare due valori)
   */
  def put(varName : String, value : String) : Boolean
  /**
   * Restituisce Some(true) iff il comparatore è stabile e il valore value1
   * associato alla variabile varName è minore del valore value2 associato alla
   * medesima variabile.
   * 
   * Restituisce None se il comparatore non è stabile
   */
  def isMinor(value1 : String, value2 : String) : Option[Boolean]
  /**
   * Reimposta completamente il comparatore per poterlo riutilizzare
   */
  def reset() : Unit
}