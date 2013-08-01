/**
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
    parseAll(variables, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) => throw new Exception("Error while parsing variable " +
          currentVariable + ": " + msg)
    }
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
    parseAll(variables, input) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) => throw new Exception("Error while parsing variable " +
          currentVariable + ": " + msg)
    }
    addConstraints
  }
  
  private def addConstraints = {
    for (i <- Domain.domains) {
      val variable = i._1
      val domain = i._2      
      val ord = Ordini(variable).getOrElse( throw new Exception("order of variable " + variable + " not found") )
      val c = ord.getConstraints.getOrElse( throw new Exception("error while calculating constraints of variable " + variable) )
      domain.addConstraint(c)
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
      
      checkOrderConsistency(order)

      val map = createOrderMap(order)
        
      if (currentVarDependencies.get(0) == "") { //independent var
        val ord = new Ordini(currentVariable)
        ord.add(map)
        Ordini.addOrdini(ord)
      }
        
      else { //dependent var
        Ordini(currentVariable) match {
          case Some(ord) => {
            ord.add(assignment.toArray, map)
          }
          case None => {
            val ord = new Ordini(currentVariable, currentVarDependencies.get)
            ord.add(assignment.toArray, map)
            Ordini.addOrdini(ord)
          }
        }
      }
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
  
  private def checkOrderConsistency(order: List[String]): Unit = {
      val domain = Domain(currentVariable).get
      if( order exists {x => !domain.contains(x) } )
        throw new Exception("orders of variable " + currentVariable + " contain values out of its domain" )
  }

  private var currentVariable: String = ""
  private var currentVarDependencies: Option[Vector[String]] = None
  
}