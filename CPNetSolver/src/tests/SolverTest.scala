/**
 * File: SolverTest.scala
 * Package: tests
 * Autore: Francesco Burato
 * Creazione: 02/lug/2013
 */
package tests

import org.scalatest.{FunSuite, BeforeAndAfter}
import constraintobjs._
import solver.FCSolver
import scala.collection.mutable.HashMap
/**
 * @author Francesco Burato
 *
 */
class SolverTest extends FunSuite with BeforeAndAfter {
  before{
    Domain.reset
    Domain.addDomain(new Domain("x",Set("x","!x")))
    Domain.addDomain(new Domain("y",Set("y","!y")))
    Domain.addDomain(new Domain("z",Set("z","!z")))
  }
  
  test("Solver with independent variables should choose the best value as solution") {
    val ord1 = new Ordini("x")
    assert(ord1.add(Map("x"->0,"!x"->1)))
    val ord2 = new Ordini("y")
    assert(ord2.add(Map("y"->0,"!y"->1)))
    val ord3 = new Ordini("z")
    assert(ord3.add(Map("z"->0,"!z"->1)))
    Domain("x") match {
      case None => assert(false)
      case Some(x) => x.addConstraint(ord1.getConstraints match {
        case None => throw new Exception()
        case Some(x) => x})
    }
    Domain("y") match {
      case None => assert(false)
      case Some(x) => x.addConstraint(ord2.getConstraints match {
        case None => throw new Exception()
        case Some(x) => x})
    }
    Domain("z") match {
      case None => assert(false)
      case Some(x) => x.addConstraint(ord3.getConstraints match {
        case None => throw new Exception()
        case Some(x) => x})
    }
    val solutions = FCSolver.solve()
    assert(solutions.size === 1)
    solutions.foreach {v => v foreach {s => print(s + " ")}}
  }
  
  test("Solver with dependent variables and no cycle should find the best solution") {
    val ord1 = new Ordini("x")
    assert(ord1.add(Map("x"->1,"!x"->0)))
    val ord2 = new Ordini("y", Vector("x"))
    assert(ord2.add(Array("x"),Map("y"->0,"!y"->1)))
    assert(ord2.add(Array("!x"),Map("y"->1,"!y"->0)))
    val ord3 = new Ordini("z",Vector("y"))
    assert(ord3.add(Array("y"),Map("z"->0,"!z"->1)))
    assert(ord3.add(Array("!y"),Map("z"->1,"!z"->0)))
    ord1.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    ord2.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    ord3.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    val solutions = FCSolver.solve()
    assert(solutions.size === 1)
    solutions.foreach {v => v foreach {s => print(s + " ")}}
  }
  
  test("Solver with dependent variables and cycle should find the best solution") {
    val ord3 = new Ordini("z")
    assert(ord3.add(Map("z"->1,"!z"->0)))
    val ord1 = new Ordini("x",Vector("y"))
    assert(ord1.add(Array("y"),Map("x"->1,"!x"->0)))
    assert(ord1.add(Array("!y"),Map("x"->0,"!x"->1)))
    val ord2 = new Ordini("y", Vector("x"))
    assert(ord2.add(Array("x"),Map("y"->1,"!y"->0)))
    assert(ord2.add(Array("!x"),Map("y"->0,"!y"->1)))
    ord1.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    ord2.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    ord3.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    val solutions = FCSolver.solve()
    assert(solutions.size === 2)
    solutions.foreach {v => print("["); v foreach {s => print(s + " ")} ;println("]")}
  }
  test("Solver with dependent variables, many values and cycle should find the best solution") {
    Domain.reset
    Domain.addDomain(new Domain("x",Set("x1","x2","x3")))
    Domain.addDomain(new Domain("y",Set("y1","y2","y3")))
    Domain.addDomain(new Domain("z",Set("z1","z2")))
    val ordx = new Ordini("x",Vector("z"))
    val ordy = new Ordini("y",Vector("x"))
    val ordz = new Ordini("z",Vector("x","y"))
    ordx.add(Array("z1"),Map("x1"->1,"x2"->0,"x3"->2))
    ordx.add(Array("z2"),Map("x1"->0,"x2"->2,"x3"->1))
    ordy.add(Array("x1"),Map("y1"->1,"y2"->0,"y3"->2))
    ordy.add(Array("x2"),Map("y1"->0,"y2"->2,"y3"->1))
    ordy.add(Array("x3"),Map("y1"->2,"y2"->0,"y3"->1))
    ordz.add(Array("x1","y1"),Map("z1"->1,"z2"->0))
    ordz.add(Array("x1","y2"),Map("z1"->0,"z2"->1))
    ordz.add(Array("x1","y3"),Map("z1"->0,"z2"->1))
    ordz.add(Array("x2","y1"),Map("z1"->1,"z2"->0))
    ordz.add(Array("x2","y2"),Map("z1"->0,"z2"->1))
    ordz.add(Array("x2","y3"),Map("z1"->0,"z2"->1))
    ordz.add(Array("x3","y1"),Map("z1"->0,"z2"->1))
    ordz.add(Array("x3","y2"),Map("z1"->1,"z2"->0))
    ordz.add(Array("x3","y3"),Map("z1"->1,"z2"->0))
    ordx.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    ordy.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    ordz.getConstraints match {
      case None => assert(false)
      case Some(x) => x.vars foreach {v => 
        val t = x 
        Domain(v).get.addConstraint(x)
      }
    }
    val solutions = FCSolver.solve()
    assert(solutions.size === 1)
    solutions.foreach {v => print("["); v foreach {s => print(s + " ")} ;println("]")}
  }
}