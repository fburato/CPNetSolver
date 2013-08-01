/**
 * File: Counter.scala
 * Package: solver
 * Autore: Francesco Burato
 * Creazione: 31/lug/2013
 */
package solver

/**
 * Contatore generico con funzionamento a ghiera.
 * @author Francesco Burato
 *
 */
trait Counter {
  
  /**
   * Segnala che il contatore è inizializzato
   */
  def init : Boolean
  /**
   * Segnala se il contatore è arrivato al valore massimo rappresentabile
   */
  def end: Boolean
  /**
   * Riporta il contatore allo stato iniziale
   */
  def reset: Unit
  /**
   * Incrementa di 1 il contatore e restituisce l'indice della ghiera aggiornata
   */
  def ++(): Int
  /**
   * Restituisce il valore della ghiera i-esima oppure -1 se i è fuori dal range delle ghiere
   */
  def apply(i: Int): Int
  
  /**
   * Imposta la ghiera s-esima alla dimensione size e restituisce true sse 
   * tutte le ghiere sono state impostate
   */
  def setSleeve(s : Int, size : Int) : Boolean
  
  /**
   * Restituisce la dimensione della ghiera i-esima oppure -1 se la ghiera non è stata
   * impostata
   */
  def getSleeveSize(i : Int) : Int
  
  /**
   * Dimensione del contatore in numero di ghiere
   */
  def size : Int
  
  override def toString: String =
    if (init) {
      var between = "("
      var res = ""
      for (i <- 0 until size) {
        res = res + between + this(size - 1 - i)
        between = ","
      }
      res + ")"
    } else "()"
}