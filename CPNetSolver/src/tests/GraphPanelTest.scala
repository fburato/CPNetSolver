/**
 * File: GraphPanelTest.scala
 * Package: tests
 * Autore: Francesco Burato
 * Creazione: 03/lug/2013
 */
package tests

import swing._
import gui.GraphPanel
import edu.uci.ics.jung.graph.{ Graph, DirectedSparseGraph }
/**
 * @author Francesco Burato
 *
 */
object GraphPanelTest extends SimpleSwingApplication {
  val graphPanel = new GraphPanel
  def top = new MainFrame {
    title = "My Test"
    contents = {
      graphPanel.paintGraph(initGraph)
      graphPanel.panel
    }
  }
  
  def initGraph : Graph[String,String] = {
    val g = new DirectedSparseGraph[String,String]
    g.addVertex("a")
    g.addVertex("b")
    g.addVertex("c")
    g.addEdge("edge1","a","b")
    g.addEdge("edge2","b","c")
    g.addEdge("edge3","c","a")
    g
  }
}