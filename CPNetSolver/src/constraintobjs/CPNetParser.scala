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
 * File: CPNetParser.scala
 * Package: constraintobjs
 * Autore: Simone Carriero
 * Creazione: 26/07/2013
 */
package constraintobjs

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable
import solver.FCSolver
import java.io.FileReader
import java.io.Reader
import scala.collection.mutable.ListBuffer

/**
 * Singleton parser
 * @author Simone Carriero
 */
object CPNetParser extends RegexParsers {

  /**
   * Parses a string formatted like in the following example and
   * adds domains and orders to the objects Domain and Ordini
   * 
   * Whitespaces are always allowed.
   * If the string is malformed, an Exception is thrown.
   * 
   * Example:
   *   val input = "var X dependsOn={}\n"+
   *               "dom={x,!x}\n"+
   *               ":x>!x\n"+
   *               "var Y dependsOn={}\n"+
   *               "dom={y,!y}\n"+
   *               ":y>!y\n"+
   *               "var Z dependsOn={X,Y}\n"+
   *               "dom={z,!z}\n"+
   *               "x,y:z>!z\n"+
   *               "x,!y:!z>z\n"+
   *               "!x,y:!z>z\n"+
   *               "!x,!y:!z>z\n"
   * 
   * Pay attention to remember the \n (or a whitespace) when
   * you use concatenation of string like in the example 
   */
  def parse(input: String) = {
    currentVarDependencies = None
    currentVariable = ""
    OrderSavingList.clear()
    parseAll(variables, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) => throw new Exception("Error while parsing variable " +
          currentVariable + ": " + msg)
    }
    processOrders
    addConstraints
  }
  
  /**
   * Parses a java.io.Reader representing a file formatted
   * like in the following example and adds domains and
   * orders to the objects Domain and Ordini.
   * Whitespaces are always allowed.
   * If the string is malformed, an Exception is thrown.
   * 
   * Example:
   *   var X dependsOn={}
   *   dom={x,!x}
   *   :x>!x
   *   var Y dependsOn={}
   *   dom={y,!y}
   *   :y>!y
   *   var Z dependsOn={X,Y}
   *   dom={z,!z}
   *   x,y:z>!z
   *   x,!y:!z>z
   *   !x,y:!z>z
   *   !x,!y:!z>z
   */
  def parse(input: Reader) = {
    currentVarDependencies = None
    currentVariable = ""
    OrderSavingList.clear()
    parseAll(variables, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) => throw new Exception("Error while parsing variable " +
          currentVariable + ": " + msg)
    }
    processOrders
    addConstraints
  }
  
  private def addConstraints = {
    for (i <- Domain.domains) {
      val variable = i._1
      val domain = i._2      
      val ord = Ordini(variable).getOrElse( throw new Exception("order of variable " + variable + " not found") )
      val c = ord.getConstraints.getOrElse( throw new Exception("error while calculating constraints of variable " + variable) )
      //add to every domain the constraint talks about the constraint itself
      for ( v <- c.vars) 
        Domain(v).getOrElse(throw new Exception("order of variable " + v + " not found")).addConstraint(c)
    }
  }

  private def variables = variable*

  private def variable = varDeclaration ~ domain ~ orders

  private def varDeclaration = "var" ~ literal ~ dependencies  ^^ {
    case "var" ~ literal ~ dependencies => {
      currentVariable = literal
      currentVarDependencies = Some(dependencies.toVector)
    }
  }  
  
  private def dependencies = "dependsOn" ~ "=" ~ "{" ~ repsep(literal, ",") ~ "}" ^^ {
    case "dependsOn" ~ "=" ~ "{" ~ dependencies ~ "}" => dependencies 
  }
  
  private def domain = "dom" ~ "=" ~ "{" ~ values ~ "}" ^^ {
    case "dom" ~ "=" ~ "{" ~ values ~ "}" => {
      Domain.addDomain(new Domain(currentVariable, values.toSet)) 
    }
  }
  
  private def orders = order*
  
  private def order = repsep(literal, ",") ~ ":" ~ repsep(literal, ">") ^^ {
    case assignment ~ ":" ~ order => {
      // saving order, to be processed later
      OrderSavingList += new OrderSaving(assignment,order,currentVariable,currentVarDependencies)
    }  
  }
    
  private def literal = "[a-zA-Z!]*".r
  
  private def values = repsep(literal, ",")

  private def createOrderMap(order: List[String]): Map[String,Int] = {
    val map = new mutable.HashMap[String,Int]
    var counter = order.size - 1
    for (o <- order) {
      map += (o -> counter)
      counter = counter - 1
    }
    map.toMap

  }
  
  private def checkOrderConsistency(variable: String, order: List[String]): Unit = {
      val domain = Domain(variable).get
      if( order exists {x => !domain.contains(x) } )
        throw new Exception("orders of variable " + variable + " contain values out of its domain" )
  }

  private def processOrders = {
    for (o <- OrderSavingList) {
      checkOrderConsistency(o.variable, o.order)

      val map = createOrderMap(o.order)
        
      if (o.varDependencies.get(0) == "") { //independent var
        val ord = new Ordini(o.variable)
        ord.add(map)
        Ordini.addOrdini(ord)
      }
        
      else { //dependent var
        Ordini(o.variable) match {
          case Some(ord) => {
            ord.add(o.assignment.toArray, map)
          }
          case None => {
            val ord = new Ordini(o.variable, o.varDependencies.get)
            ord.add(o.assignment.toArray, map)
            Ordini.addOrdini(ord)
          }
        }
      }
    }
  }
  
  private var currentVariable: String = ""
  private var currentVarDependencies: Option[Vector[String]] = None
  
  // used to save order while parsing, because order can't be created before the creation of all the domains
  private val OrderSavingList = new ListBuffer[OrderSaving]
  private class OrderSaving(val assignment: List[String],
                            val order: List[String],
                            val variable: String,
                            val varDependencies: Option[Vector[String]]);
  
}