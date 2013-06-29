/**
 * File: Ordini.scala
 * Package: constraintobjs
 * Autore: Francesco Burato
 * Creazione: 28/giu/2013
 */
package constraintobjs

import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
object Ordini {
  val listaOrdini: HashMap[String, Ordini] = new HashMap[String, Ordini]
  def addOrdini(variable: String, ord: Ordini) {
    listaOrdini get variable match {
      case None => listaOrdini += (variable -> ord)
      case _ =>
    }
  }

  def apply(variable: String): Option[Ordini] =
    listaOrdini get variable
}
/**
 * Classe di rappresentazione degli ordini associati ad una variabile.
 * Vector è una collezione di oggetti immutabili e immutabile.
 * @author Francesco Burato
 *
 */
class Ordini(val varName: String, val dependencies: Vector[String]) {
  // comparatore privato per l'ordine
  private class MyComparator extends Comparator {
    val buffer: Array[String] = new Array[String](dependencies.length)
    var setted = 0
    var order: Map[String, Int] = null
    def put(variable: String, value: String): Boolean = {
      val index = dependencies.indexOf(variable) // trovo l'indice della variabile
      if (index < 0 || index >= dependencies.length)
        false
      else {
        // indice trovato
        //TODO controllo di appartenenza di value al dominio di variable
        buffer(index) = value
        setted += 1
        if (setted == buffer.length)
          findOrder()
        else
          false
      }
    }
    // PRE: tutti i valori in buffer appartengono ai domini delle variabili 
    // ovvero è garantita l'esistenza nell'albero degli ordini
    def findOrder(): Boolean = {
      var node: AlberoOrdini = Ordini.this.root
      for (value <- buffer) {
        node = findNode(value, node.getSons) match {
          case Some(x) => x
          case None => NilOrdine // non dovrebbe mai accadere
        }
      }
      assert(node != NilOrdine)
      // node contiene il nodo interno il cui figlio è il nodo d'ordine
      val ordine = node.getSons match {
        case Nil => NilOrdine
        case x :: rest => x
      }
      // assegno l'ordine effettivo
      ordine match {
        case NilOrdine => false
        case LeafOrdine(_, ord) => order = ord; true
      }
    }
    // Se l'ordine è stato impostato allora restituisco il valore corretto, altrimenti None
    def isMinor(value1: String, value2: String): Option[Boolean] =
      if (order != null) {
        None
      } else {
        Some(order(value1) <= order(value2))
      }

    //Azzera tutti i campi del comparatore
    def reset() {
      for (i <- 0 until buffer.length)
        buffer(i) = null
      order = null
      setted = 0
    }
  }

  private val root = new InternoOrdine(NilOrdine)
  init()
  // inizializzatore dell'albero degli ordini
  private def init() {
    /*
     * Costruisce la struttura ad albero completa senza le foglie
     */
    def initNode(node: AlberoOrdini, i: Int, dep: Vector[String]): Unit =
      if (i < dep.length) {
        //TODO completare con i domini corretti
        val s: Set[String] = Set()
        for (elem <- s) {
          val v = InternoOrdine(node, elem)
          node += v
          initNode(v, i + 1, dep)
        }
      }
    initNode(root, 0, dependencies)
  }
  /**
   * Controllo di correttezza degli ordini totali
   */
  private def orderNotTotal(ord: Map[String, Int]): Boolean = {
    //TODO implementare il controllo di ordine totale
    false
  }

  /**
   * Ricerca del figlio con un valore cercato
   */
  private def findNode(value: String, list: List[AlberoOrdini]): Option[AlberoOrdini] = list match {
    case Nil => None
    case x :: rest => x match {
      case InternoOrdine(parent, assign) => if (value == assign) Some(x) else findNode(value, rest)
      case _ => None
    }
  }
  def add(assign: Array[String], ordine: Map[String, Int]): Boolean =
    // controllo di consistenza sugli assegnamenti e dell'ordine
    if (assign.length != dependencies.length || orderNotTotal(ordine))
      false
    else {
      var end: AlberoOrdini = root
      var error = false
      var i = 0
      while (!error && i < assign.length) {
        // cerco il figlio con il valore dell'assegnamento
        val next = findNode(assign(i), end.getSons)
        error = next match {
          case Some(x) => 
            if (x.leaves >= 1)
              true
            else { end = x; i += 1; false }
          // se non viene trovato allora segnalo lerrore
          case None => true
        }
      }
      if (error)
        false
      else {
        // trovato il nodo terminale a cui attaccare la foglia in end
        end += LeafOrdine(end, ordine)
        true
      }
    }
  def checkCorrectness(): Boolean = {
    //TODO calcolo del numero atteso di foglie
    val l = 0
    root.leaves == l
  }

  def getConstraints(): Option[TVincolo] =
    if (!checkCorrectness)
      None
    else {
      //TODO aggiungere la definizione corretta del vincolo
      val v: TVincolo = null
      //inserimento dei valori autorizzati
      def adder(arr: Array[String], node: AlberoOrdini, i: Int, v: TVincolo): Unit = node match {
        case InternoOrdine(_, value) =>
          // copio l'array creato fino a questo punto e assegno il valore relativo al nodo interno corrente
          for (son <- node.getSons) {
            val a = arr.clone
            a(i) = value
            adder(a, son, i + 1, v)
          }
        case LeafOrdine(_, ordine) =>
          // trovo il valore più preferito e completo l'array
          val l = ordine.reduce((c1: (String, Int), c2: (String, Int)) => if (c1._2 > c2._2) c1 else c2) //massimo della mappa
          arr(i) = l._1
          v insert arr
      }
      // per ogni figlio della radice inizio l'aggiunta delle informazioni
      for (son <- root.getSons) {
        val a = new Array[String](dependencies.length + 1)
        adder(a, son, 0, v)
      }
      Some(v)
    }
  /**
   * Alloca un nuovo comparatore per controllare le differenze nell'ordine
   */
  def comparator(): Comparator = new MyComparator()
}