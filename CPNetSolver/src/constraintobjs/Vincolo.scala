/**
 * File: Vincolo.scala
 * Package: constraintobjs
 * Autore: Francesco Burato
 * Creazione: 28/giu/2013
 */
package constraintobjs

/**
 * Trait per la rappresentazione di Vincoli generici
 * @author Francesco Burato
 *
 */
trait Vincolo {
  /**
   * Inserimento di una tupla al vincolo.
   */
  def insert(t : Array[String])
  
  /**
   * Proiezione dei valori ammessi della variabile varName dal vincolo corrente
   */
  def proiezione(varName : String) : List[String]
  
  /**
   * Costruizione di un nuovo vincolo in cui la variabile varName assume come unico
   * valore value
   */
  def riduzione(varName : String, value : String) : Vincolo
}