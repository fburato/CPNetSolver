/**
 * File: CountersTest.scala
 * Package: tests
 * Autore: Francesco Burato
 * Creazione: 31/lug/2013
 */
package tests

import org.scalatest.{FunSuite, BeforeAndAfter}
import solver._
/**
 * Test di funzionamento dei contatori
 * @author Francesco Burato
 *
 */
class CountersTest extends FunSuite with BeforeAndAfter{
  test("Classic counter should refuse uninitialized access") {
    val c = new ClassicCounter(4)
    assert((c++) == -1)
    assert((c getSleeveSize 0) == -1)
    assert((c(0)) == -1)
  }
  
  test("Classic counter should refuse partially initialized access"){
    val c = new ClassicCounter(4)
    c.setSleeve(0, 3)
    c.setSleeve(1, 7)
    assert(!c.init)
    assert((c++) == -1)
    assert((c getSleeveSize 0) == -1)
    assert((c(0)) == -1)
  }
  test("Classic counter should accept initialized access"){
    val c = new ClassicCounter(4)
    c.setSleeve(0, 3)
    c.setSleeve(1, 7)
    c.setSleeve(2, 6)
    c.setSleeve(3, 4)
    assert(c.init)
    assert((c++) == 0)
    assert((c getSleeveSize 0) == 3)
    assert((c(0)) == 1)
  }
  
  test("Classic counter should update every element") {
    val c = new ClassicCounter(3)
    c.setSleeve(0, 2)
    c.setSleeve(1, 2)
    c.setSleeve(2, 2)
    for(i <- 1 to 6)
      c++;
    assert(c(0) == 0)
    assert(c(1) == 1)
    assert(c(2) == 1)
    assert(! c.end)
    c++;
    assert(c.end)
  }
  
  test("Classic counter should update every element in variable context") {
    val c = new ClassicCounter(3)
    c.setSleeve(0, 3)
    c.setSleeve(1, 7)
    c.setSleeve(2, 6)
    for(i <- 1 to 77){
      println(c + " " + (i-1))
      c++;
    }
    println(c + " " + 77)
    assert(c(0) == 2)
    assert(c(1) == 4)
    assert(c(2) == 3)
  }
  test("Classic counter should handle correctly termination in variable context") {
    val c = new ClassicCounter(3)
    c.setSleeve(0, 3)
    c.setSleeve(1, 7)
    c.setSleeve(2, 6)
    for(i <- 1 to 124){
      c++;
    }
    assert(c(0) == 1)
    assert(c(1) == 6)
    assert(c(2) == 5)
    assert(!c.end)
    c++;
    assert(c.end)
  }
}