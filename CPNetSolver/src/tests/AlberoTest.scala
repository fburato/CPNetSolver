/**
 * File: AlberoTest.scala
 * Package: tests
 * Autore: Francesco Burato
 * Creazione: 29/giu/2013
 */
package tests

import org.scalatest.{FunSuite, BeforeAndAfter}
import constraintobjs._
import scala.collection.mutable.HashMap
/**
 * @author Francesco Burato
 *
 */
class AlberoTest extends FunSuite with BeforeAndAfter{
  var root : AlberoOrdini = _
  var ordine : Map[String,Int] = _
  before {
    root = new InternoOrdine(NilOrdine)
    val l = new HashMap[String,Int]
    l += ("hello" -> 1)
    l += ("bello" -> 2)
    l += ("caco" -> 3)
    ordine = l.toMap
  }
  
  test("Root should not have sons and leaves"){
    assert(root.getSons === Nil)
    assert(root.leaves === 0)
  }
  
  test("LeafNode should update root") {
    val son = LeafOrdine(root,ordine)
    assert(root.getSons === son::Nil )
    assert(root.leaves === 1)
  }
  
  test("Inner nodes should propagate the leaves") {
    val inner1 = InternoOrdine(root,"a")
    assert(root.leaves === 0)
    val inner2 = InternoOrdine(root,"b")
    assert(root.leaves === 0)
    val leaf1 = LeafOrdine(inner1,ordine)
    assert(root.leaves === 1)
    val leaf2 = LeafOrdine(inner2,ordine)
    assert(root.getSons === inner1 :: inner2 :: Nil)
    assert(inner1.getSons === leaf1 :: Nil)
    assert(inner2.getSons === leaf2 :: Nil)
    assert(root.leaves === 2)
  }
}