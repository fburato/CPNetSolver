/**
 * File: POGTest.scala
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
 * Test di verifica disegno soluzioni con flip peggiorativi indipendenti
 * @author Francesco Burato
 *
 */
object POGTest extends SimpleSwingApplication {
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
    val ord = new Ordini("x")
    ord.add(Map("x"->0,"!x"->1))
    Ordini.addOrdini(ord)
    val ord1 = new Ordini("y")
    ord1.add(Map("y"->1,"!y"->0))
    Ordini.addOrdini(ord1)
    val pog = new POGCreator
    pog.getGraph
  }
}