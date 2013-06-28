package constraintobjs

import scala.collection.mutable.Stack
/**
 * Struttura dati ad albero per rappresentare in maniera efficiente
 * gli ordini associati ad una variabile.
 */
trait AlberoOrdini {
  def getSons: List[AlberoOrdini]
  def +=(newson: AlberoOrdini): Unit
  def parent: AlberoOrdini
  def leaves_=(i: Int): Unit
  def leaves: Int
}

/**
 * Nodo terminatore dell'albero
 */
case object NilOrdine extends AlberoOrdini {
  def getSons = Nil
  def +=(newson: AlberoOrdini) {}
  def parent = NilOrdine
  def leaves_=(i: Int) {}
  def leaves = 0
}

/**
 * Nodo contenente un ordine totale associato al percorso dalla radice
 * dell'albero fino al nodo corrente
 */
case class LeafOrdine(val parent: AlberoOrdini, val order: Map[String, Int]) extends AlberoOrdini {
  def getSons = NilOrdine :: Nil
  def leaves_=(i: Int) {}
  def leaves = 0
  def +=(newson: AlberoOrdini) {}
}
/**
 * Nodo interno generico: potrebbe essere un nodo associato ad una variabile oppure 
 * una radice
 */
case class ContainerOrdine(val parent: AlberoOrdini) extends AlberoOrdini {
  private val mySons: Stack[AlberoOrdini] = Stack()
  private var myLeaf = 0
  /**
   * Lista dei figli costruita a partire dalla coda
   */
  def getSons = {
    var list: List[AlberoOrdini] = Nil
    for (l <- mySons)
      list = l :: list
    list
  }

  /**
   * Aggiunge un nuovo figlio all'albero
   */
  def +=(newson: AlberoOrdini) {
    mySons push newson
    /*
     * Aggiornamento del numero di foglie dell'albero corrente e dei
     * nodi antenati
     */
    def update(node: AlberoOrdini): Unit = node match {
      case NilOrdine =>
      case x =>
        x.leaves = x.leaves + 1
        update(parent)
    }
    newson match {
      // aggiorna solo se il nodo aggiunto � di tipo foglia
      case LeafOrdine(parent, order) => update(this)
      case x =>
    }
  }

  def leaves_=(i: Int) {
    myLeaf = i
  }

  def leaves = myLeaf
}
case class InternoOrdine(val parent: AlberoOrdini, val varValue: String) extends AlberoOrdini{
  private val containerOrdine = new ContainerOrdine(parent)
  def getSons: List[AlberoOrdini] = containerOrdine.getSons
  def +=(newson: AlberoOrdini) = containerOrdine += newson
  def leaves_=(i: Int) = containerOrdine.leaves = i
  def leaves = containerOrdine.leaves
}
