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
 * File: VariationCounter.scala
 * Package: solver
 * Autore: Francesco Burato
 * Creazione: 31/lug/2013
 */
package solver

/**
 * Contatore a ghiera per il calcolo dei valori diversi strettamente maggiori di un
 * altro contatore
 * @author Francesco Burato
 *
 */
class VariationCounter(private val starter: Counter) extends Counter {
  // costruttore di classe
  if (!starter.init)
    throw new Exception("Cannot instantiate VariationCounter from uninitialized counter")
  val size = starter.size
  private val startingValues = new Array[Int](size)
  private val dimensions = new Array[Int](size)
  private val currentValues = new Array[Int](size)
  private var lastUpdated = 0
  private var leftMost = -1
  private var myEnd = false
  val init = true
  for (i <- 0 until size) {
    dimensions(i) = starter.getSleeveSize(i)
    startingValues(i) = starter(i)
    currentValues(i) = starter(i)
  }
  findLast()
  //imposto l'ultimo valore degli incrementi
  private def findLast() {
    var i = size -1
    while(i>=0 && startingValues(i)==dimensions(i)-1)
      //cerco il valore più significativo che sia massimale 
      i-=1
    if(i>=0) {
      // se tale valore esiste allora il massimo si ottiene mettendo
      // al max della ghiera il valore aggiornato
      leftMost = i
    } else {
      // se tale valore non esiste allora tutti i valori sono massimali a meno del punto
      // fisso quindi il contatore è già al massimo
      myEnd = true
    }
  }
  // fine costruttore di classe
  def setSleeve(s: Int, i: Int) = false

  def getSleeveSize(i: Int): Int =
    if (i < 0 || i >= size)
      -1
    else
      dimensions(i)

  def apply(i: Int): Int =
    if (i < 0 || i >= size)
      -1
    else
      currentValues(i)

  def reset: Unit = {
    for (i <- 0 until size)
      currentValues(i) = startingValues(i)
    lastUpdated = 0
  }
  
  def ++(): Int = 
    if(!end){
      while(lastUpdated<= leftMost && currentValues(lastUpdated)==dimensions(lastUpdated)-1) {
        currentValues(lastUpdated) = startingValues(lastUpdated)
        lastUpdated+=1
      }
      currentValues(lastUpdated) += 1
      if(lastUpdated == leftMost && currentValues(lastUpdated) == dimensions(lastUpdated)-1)
        myEnd = true
      lastUpdated
    } else -1
    
  def end = myEnd
}