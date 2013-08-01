/**
 * File: POGTest2.scala
 * Package: tests
 * Autore: Francesco Burato
 * Creazione: 31/lug/2013
 */
package tests

import swing._
import gui.GraphPanel
import solver._
import constraintobjs._
import edu.uci.ics.jung.graph.{ Graph, DirectedSparseGraph }
/**
 * Test di verifica disegno soluzioni con flip peggiorativi con cicli
 * @author Francesco Burato
 *
 */
object POGTest2 extends SimpleSwingApplication {
  val graphPanel = new GraphPanel
  def top = new MainFrame {
    title = "My Test"
    contents = {
      graphPanel.paintGraph(initGraph)
      graphPanel.panel
    }
  }
  
  def initGraph : Graph[String,String] = {
    Domain.addDomain(new Domain("x",Set("x","!x")))
    Domain.addDomain(new Domain("y",Set("y","!y")))
    Domain.addDomain(new Domain("z",Set("z","!z")))
    val ordx = new Ordini("x",Vector("z"))
    val ordy = new Ordini("y",Vector("x"))
    val ordz = new Ordini("z",Vector("y"))
    ordx.add(Array("z"),Map("x"->1,"!x"->0))
    ordx.add(Array("!z"),Map("x"->0,"!x"->1))
    ordy.add(Array("x"),Map("y"->1,"!y"->0))
    ordy.add(Array("!x"),Map("y"->0,"!y"->1))
    ordz.add(Array("y"),Map("z"->1,"!z"->0))
    ordz.add(Array("!y"),Map("z"->0,"!z"->1))
    Ordini.addOrdini(ordx)
    Ordini.addOrdini(ordy)
    Ordini.addOrdini(ordz)
    val pog = new POGCreator
    pog.getGraph
  }
}