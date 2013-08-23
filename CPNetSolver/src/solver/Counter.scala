/**
 * CPNetSolver
 * Copyright (C) 2013  Francesco Burato, Simone Carriero
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see [http://www.gnu.org/licenses/].
 * 
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