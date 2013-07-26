/**
 * File: Ordini.scala
 * Package: constraintobjs
 * Autore: Francesco Burato
 * Creazione: 28/giu/2013
 */

/*
 * Bugfix:
 * 26/07/2013
 * - Bug:   restituzione None in caso di dipendenze multivariate.
 * - Causa: controllo "if (x.leaves >= 1)" errato in quanto il controllo è stato inserito per
 * verificare che non ci sia sovrascrittura di un ordine ma in realtà non tiene conto del caso
 * multivariato
 * - Soluzione: controllo aggiornato a "if (i == dependencies.length-1 && x.leaves >= 1)" in maniera
 * da realizzare la funzionalità solo quando si raggiunge il nodo terminale.
 */
package constraintobjs

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
    
  def reset() {
    listaOrdini.clear
  }
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
    if(dependencies.length == 0) {
      //la variabile è indipendente: carico l'ordine unico
      findOrder()
    }
    def put(variable: String, value: String): Boolean = {
      // in caso di inizializzazione già eseguita ritorna immediatamente true
      if(order != null)
        return true
      val index = dependencies.indexOf(variable) // trovo l'indice della variabile
      if (index < 0 || index >= dependencies.length)
        false
      else {
        // indice trovato
        val accepted = Domain(dependencies(index)) match {
          case Some(x) => x.accepted
          case None => throw new Exception("An order can only be built on a registered variable")
        }
        if (accepted contains value) {
          buffer(index) = value
          setted += 1
          if (setted == buffer.length)
            findOrder()
          else
            false
        } else
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
      if (order == null) {
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
    if (dependencies contains varName)
      throw new Exception("A variable should not depend on itself")
    def initNode(node: AlberoOrdini, i: Int, dep: Vector[String]): Unit =
      if (i < dep.length) {
        val s: Set[String] = Domain(dep(i)) match {
          case Some(x) => x.accepted
          case None => throw new Exception("A registered variable should be passed")
        }
        for (elem <- s) {
          val v = InternoOrdine(node, elem)
          initNode(v, i + 1, dep)
        }
      }
    initNode(root, 0, dependencies)
  }
  /**
   * Controllo di correttezza degli ordini totali
   */
  private def orderNotTotal(ord: Map[String, Int]): Boolean = {
    val domain = Domain(varName) match {
      case Some(x) => x.accepted
      case None => throw new Exception("An order can be built only on a registered variable")
    }
    var incrementor = 0
    val keys = ord.keys.toList
    keys.foreach((s: String) => if (domain contains s) incrementor += 1)
    domain.foreach((s: String) => if (keys contains s) incrementor -= 1)
    if (incrementor != 0)
      true
    else {
      // converto i valori ad insieme
      val values = ord.values.toSet
      // se sono tutti diversi la dimensione è uguale, altrimenti l'ordine non è totale
      values.size != ord.values.size
    }
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

  // costruttore per variabili indipendenti
  def this(v: String) = this(v, Vector())

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
            if (i == dependencies.length-1 && x.leaves >= 1)
              // all'ultimo nodo se x.leaves è positivo vuol dire che 
              // l'assegnamento è stato già effettuato, quindi l'aggiunta deve fallire
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
        LeafOrdine(end, ordine)
        true
      }
    }
  def checkCorrectness(): Boolean = {
    var inc = 1
    dependencies.foreach((s: String) => {
      val dom = Domain(s) match {
        case Some(x) => x.accepted
        case None => throw new Exception("All dependecies should be registered variables")
      }
      inc *= dom.size
    })
    root.leaves == inc
  }

  //metodo di aggiunta per variabili indipendenti
  def add(ordine: Map[String,Int]) :Boolean = add(Array[String](),ordine)
  def getConstraints(): Option[Constraint] =
    if (!checkCorrectness)
      None
    else {
      // inizializzo la dimensione del vincolo e il vincolo stesso
      // inserisco le dipendenze e la variabile attuale
      val arr = new Array[String](dependencies.size + 1)
      dependencies.copyToArray(arr)
      arr(arr.size - 1) = varName
      val v: Constraint = new Constraint(arr)
      //inserimento dei valori autorizzati
      def adder(arr: Array[String], node: AlberoOrdini, i: Int, v: Constraint): Unit = node match {
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