/**
 * File: OrdiniTest.scala
 * Package: tests
 * Autore: Francesco Burato
 * Creazione: 01/lug/2013
 */
package tests

import org.scalatest.{FunSuite, BeforeAndAfter}
import constraintobjs._
import scala.collection.mutable.HashMap
/**
 * Test di unità su Ordini e Comparator
 * @author Francesco Burato
 *
 */
class OrdiniTest extends FunSuite with BeforeAndAfter {
  before{
    Domain.addDomain(new Domain("x",Set("a","b","c")))
    Domain.addDomain(new Domain("y",Set("a","b")))
  }
  
  test("Independent variable should work"){
    val ord = new Ordini("x")
    ord.add(Map("a"->0,"b"->1,"c"->2))
    ord.getConstraints match {
      case None => assert(false)
      case Some(x) => assert(x.accepted.size===1)
    }
  }
  
  test("Dependent variable with uncomplete order should not work") {
    val ord = new Ordini("y",Vector("x"))
    ord.add(Array("b"),Map("a"->0,"b"->1))
    ord.getConstraints match {
      case None => assert(true)
      case _ => assert(false)
    }
  }
  
  test("Dependent variable with complete order should work") {
    val ord = new Ordini("y",Vector("x"))
    assert(ord.add(Array("a"),Map("a"->0,"b"->1)))
    assert(ord.add(Array("b"),Map("a"->1,"b"->0)))
    assert(ord.add(Array("c"),Map("a"->2,"b"->1)))
    ord.getConstraints match {
      case None => assert(false)
      case Some(x) => 
        assert(x.accepted.size === 3)
    }
  }
  
  test("Comparator with independent should work") {
    val ord = new Ordini("y")
    ord.add(Map("a"->0,"b"->1))
    val comp = ord.comparator
    // deve restituire true
    assert(comp.put("x","a"))
    comp.isMinor("a", "b") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("b", "a") match {
      case None => assert(false)
      case Some(x) => assert(!x)
    }
    comp.isMinor("b", "b") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("a", "a") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
  }
  test("Comparator with dependent should work") {
    val ord = new Ordini("y",Vector("x"))
    assert(ord.add(Array("a"),Map("a"->0,"b"->1)))
    assert(ord.add(Array("b"),Map("a"->1,"b"->0)))
    assert(ord.add(Array("c"),Map("a"->2,"b"->1)))
    val comp = ord.comparator
    // deve restituire true
    assert(! comp.put("y","a"))
    assert(comp.put("x","a"))
    comp.isMinor("a", "b") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("b", "a") match {
      case None => assert(false)
      case Some(x) => assert(!x)
    }
    comp.isMinor("b", "b") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("a", "a") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.reset
    assert(comp.put("x","b"))
    comp.isMinor("a", "b") match {
      case None => assert(false)
      case Some(x) => assert(!x)
    }
    comp.isMinor("b", "a") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("b", "b") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("a", "a") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.reset
    assert(comp.put("x","c"))
    comp.isMinor("a", "b") match {
      case None => assert(false)
      case Some(x) => assert(!x)
    }
    comp.isMinor("b", "a") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("b", "b") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.isMinor("a", "a") match {
      case None => assert(false)
      case Some(x) => assert(x)
    }
    comp.reset
  }
}