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