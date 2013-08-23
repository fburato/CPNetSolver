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
 * File: Domain.scala
 * Package: constraintobjs
 * Author: Simone Carriero
 * Creation: 30/06/2013
 */
package constraintobjs

import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

/**
 * Companion object.
 */
object Domain {
  
  val domains: mutable.HashMap[String, Domain] = new mutable.HashMap[String, Domain]

  def addDomain(domain: Domain) {
    domains get domain.variable match {
      case None => domains += (domain.variable -> domain)
      case _ =>
    }
  }
  
  def apply(variable: String): Option[Domain] = domains get variable

  def reset() : Unit = domains.clear
}

/**
 * Represents a domain.
 * @author Simone Carriero
 */
class Domain(val variable: String, val accepted: Set[String]) {

  /**
   * The list of constraints involving the variable "variable".
   */
  val constraints: Buffer[Constraint] = new ListBuffer[Constraint]

  /**
   * Adds a constraint to the list of constraints.
   * If the constraint to be added doesn't involves the variable "variable"
   * an Exception is thrown.
   */
  def addConstraint(c: Constraint): Unit = {  
    if ( !(c.vars contains variable) )
      throw new Exception("Domain.addConstraint error: adding to the domain of " +
          variable + " a constraint in which " + variable + " is not involved.")

    constraints += c
  }
  
  /**
   * Tests if some element is contained in this domain.
   */
  def contains(elem: String): Boolean = accepted.contains(elem)
  
}

