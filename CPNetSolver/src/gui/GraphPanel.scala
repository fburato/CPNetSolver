/**
 * File: GraphPanel.scala
 * Package: guitest
 * Autore: Francesco Burato
 * Creazione: 03/lug/2013
 */
package gui

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.util.Iterator;
import org.apache.commons.collections15.Transformer
import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.graph.{ Graph, DirectedSparseGraph }
import edu.uci.ics.jung.visualization.{ GraphZoomScrollPane, VisualizationViewer }
import edu.uci.ics.jung.visualization.control.{ DefaultModalGraphMouse, GraphMouseListener, ModalGraphMouse }
import edu.uci.ics.jung.visualization.decorators.{ EdgeShape, PickableEdgePaintTransformer, PickableVertexPaintTransformer, ToStringLabeller }
import edu.uci.ics.jung.visualization.picking.PickedState
import scala.collection.JavaConversions.asScalaIterator
import scala.swing.Panel


/**
 * Pannello per il disegno dei grafi
 * @author Francesco Burato
 *
 */

class GraphPanel {
  /**
   * Il costruttore inizializza completamente gli oggetti del pannello
   */
  private val graphMouse = new DefaultModalGraphMouse[Int, Int]
  private val graph = new DirectedSparseGraph[String, String]
  private val layout = new DAGLayout[String, String](graph)
  private val viewer = new VisualizationViewer[String, String](layout)
  initialize
  val panel = new Panel{ peer add new GraphZoomScrollPane(viewer)}
  // inizializzazione del pannello
  private def initialize = {
    graphMouse.setMode(ModalGraphMouse.Mode.PICKING)
    viewer.setGraphMouse(graphMouse)
    viewer.setBackground(Color.WHITE)

    // imposto i labeler di archi e vertici
    viewer.getRenderContext().setEdgeLabelTransformer(new Transformer[String, String] { def transform(s: String) = "" })
    viewer.getRenderContext().setVertexLabelTransformer(new Transformer[String, String] { def transform(s: String) = s })
    // imposto il look and feel di vertivi e archi
    viewer.getRenderContext().setEdgeDrawPaintTransformer(new PickableEdgePaintTransformer[String](viewer.getPickedEdgeState(), Color.black, Color.cyan))
    viewer.getRenderContext().setEdgeShapeTransformer(new EdgeShape.Line[String, String])
    viewer.getRenderContext().setVertexFillPaintTransformer(new PickableVertexPaintTransformer[String](viewer.getPickedVertexState(), Color.red, Color.yellow));
    viewer.setVertexToolTipTransformer(new ToStringLabeller[String]());
    //TODO forse non serve
    //viewer.setGraphMouse(graphMouse);

    // imposto il listener per il mouse
    viewer.addGraphMouseListener(new GraphMouseListener[String] {
      override def graphClicked(a1: String, a2: MouseEvent) {}
      override def graphPressed(a1: String, a2: MouseEvent) {}
      override def graphReleased(a1: String, a2: MouseEvent) {}
    })
  }

  //metodi dell'interfaccia pubblica
  def cleanAll() {
    clearGraph
    layout.reset
    viewer.revalidate
    viewer.repaint()
  }

  def paintGraph(g: Graph[String, String]) {
    clearGraph
    //ottengo tutti i vertici
    g.getVertices.iterator foreach {vertex => graph.addVertex(vertex) }
    // inserisco tutti gli archi
    for (vertex <- g.getVertices.iterator) {
      g.getOutEdges(vertex).iterator foreach { edge => graph.addEdge(edge, g.getEndpoints(edge))}
    }
    // Refresh
    layout.reset();
    viewer.revalidate();
    viewer.repaint();
  }

  private def clearGraph() =
    graph.getVertices.toArray foreach { vertex => graph.removeVertex(vertex.asInstanceOf[String]) }
    
}