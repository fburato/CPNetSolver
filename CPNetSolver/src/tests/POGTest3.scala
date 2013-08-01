/**
 * File: POGTest3.scala
 * Package: tests
 * Autore: Francesco Burato
 * Creazione: 01/ago/2013
 */
package tests

import swing._
import gui.GraphPanel
import solver._
import constraintobjs._
import edu.uci.ics.jung.graph.{ Graph, DirectedSparseGraph }
/**
 * Test di verifica disegno soluzioni con flip peggiorativi con cicli e più variabili e dipendenze
 * @author Francesco Burato
 *
 */
object POGTest3 extends SimpleSwingApplication {
  val graphPanel = new GraphPanel
  def top = new MainFrame {
    title = "My Test"
    contents = {
      graphPanel.paintGraph(initGraph)
      graphPanel.panel
    }
  }
  
  def initGraph : Graph[String,String] = {
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
    Ordini.addOrdini(ordx)
    Ordini.addOrdini(ordy)
    Ordini.addOrdini(ordz)
    val pog = new POGCreator
    pog.getGraph
  }
}