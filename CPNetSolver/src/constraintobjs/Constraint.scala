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
 * File: Constraint.scala
 * Package: constraintobjs
 * Author: Simone Carriero
 * Creation: 28/06/2013
 */
package constraintobjs

import scala.collection.mutable.{Buffer, ListBuffer}

/**
 * Represents a constraint.
 * @author Simone Carriero
 */
class Constraint(val vars:     Array[String],
                 val accepted: Buffer[Array[String]]) {

  def this(vars: Array[String]) = this(vars, new ListBuffer)

  /**
   * Inserts an accepted instantiation.
   * If the length of the instantiation is different from the number
   * of variables an Exception is thrown.
   */
  def insert(instantiation: Array[String]): Unit = {
    if (instantiation.length != vars.length)
      throw new Exception("Constraint.insert error: the length of the " +
      		"instantiation is different from the number of variables")
    
    accepted += instantiation
  }
  
  /**
   * Returns a Set containing all the values that the variable
   * "variable" could take.
   * If the variable does not exist an Exception is thrown.
   */
  def projection(variable: String): Set[String] = {
    val index = vars.indexOf(variable)
    if (index == -1)
      throw new Exception("Constraint.projection error: the variable" +
          variable + " does not exist")

    val allValues = accepted map { inst => inst(index) }
    allValues.toSet
  }
  
  /**
   * Returns a new Constraint that accepts only the instantiations
   * for which "variable" = "value"
   */  
  def reduction(variable: String, value: String): Constraint  = {
    val index = vars.indexOf(variable)
    val newAccepted = accepted filter { inst => inst(index) == value }
    new Constraint(vars, newAccepted)
  }
  
  override def toString : String = {
    "(" + vars.mkString(",") + ")->{" + accepted.map {case x : Array[String] => "(" + x.mkString(",") +") "} + "}"
  }
}

