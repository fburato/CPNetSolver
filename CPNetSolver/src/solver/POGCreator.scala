/**
 * File: POGCreator.scala
 * Package: solver
 * Autore: Francesco Burato
 * Creazione: 31/lug/2013
 */
package solver

import edu.uci.ics.jung.graph.{ Graph, DirectedSparseGraph }
import constraintobjs.{Domain,Ordini,Comparator}
import scala.collection.mutable.HashMap
/**
 * @author Francesco Burato
 * Generatore del grafo degli ordini parziali delle soluzioni.
 */
class POGCreator {
  // inizio costruttore di classe
  // inizializzo le strutture dati di appoggio
  val variables : Array[String] = new Array[String](Domain.domains.size)
  val comparators : Array[Comparator] = new Array[Comparator](Domain.domains.size)
  var edge = 0
  private val domains = new HashMap[String, Array[String]]
  init()
  private def init() {
    // popolo l'array delle variabili e degli ordini
    var i = 0
    Domain.domains.foreach { 
      case (s,d) => 
        comparators(i) = Ordini(s) match {
          case None => throw new Exception("The orders have not been completely initialized")
          case Some(x) => x.comparator
        }
        variables(i) = s
        // aggiungo tutti gli elementi accettati ai domini linearizzati
        domains += (s -> d.accepted.toArray)
        i += 1
    }
  }
  //fine costruttore di classe
  
  def getGraph() : Graph[String,String]  = {
    // inizializzo il contatore 
    var edge = 0
    val counter = new ClassicCounter(variables.size)
    for(i <- 0 until variables.size) 
      counter.setSleeve(i, domains(variables(i)).size)
    assert(counter.init)
    
    // inizializzo il grafo
    val res = new DirectedSparseGraph[String,String]
    // costruisco il grafo
    while(!counter.end) {
      processAssign(counter, res)
      counter++
    }
    // l'ultimo assegnamento non ha successori, quindi non ha senso elaborarlo
    res
  }
  
  private def processAssign(counter : Counter , graph : DirectedSparseGraph[String,String]) {
    val startingNode = nodeName(counter)
    // eseguo tutti i flip con un VariationCounter
    val variation = new VariationCounter(counter)
    var updatedVariable = variation++;
    while(updatedVariable != -1) {
      // prelevo ed azzero il comparatore
      val comparator = comparators(updatedVariable)
      val originalValue = domains(variables(updatedVariable))(counter(updatedVariable)) //assegnamento alla variabile di partenza
      val newValue = domains(variables(updatedVariable))(variation(updatedVariable)) //valore alla variabile variato
      val reachingNode = nodeName(variation)
      comparator.reset
      var i = 0
      var completed = false
      while(!completed && i < variation.size){
        //aggiungo l'assegnamento della variabile i-esima al comparatore
        completed = comparator.put(variables(i), domains(variables(i))(variation(i)))
        i+= 1
      }
      assert(completed)
      // eseguo il confronto tra i due valori di differenza
      comparator.isMinor(originalValue,newValue) match {
        case None => assert(false)
        case Some(x) => 
          if(x)
            graph.addEdge(""+edge,reachingNode,startingNode)
          else
            graph.addEdge(""+edge,startingNode,reachingNode)
      }
      edge +=1
      updatedVariable = variation++
    }
  }
  
  private def nodeName(counter : Counter) : String = {
    var res = ""
    var between = ""
    for(i <- 0 until counter.size) {
      res = res + between + domains(variables(i))(counter(i))
      between = ","
    }
    res
  }
}