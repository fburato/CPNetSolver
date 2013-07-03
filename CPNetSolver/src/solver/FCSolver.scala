/**
 * File: FCSolver.scala
 * Package: solver
 * Autore: Francesco Burato
 * Creazione: 01/lug/2013
 */
package solver
import constraintobjs.{ Constraint, Domain, Ordini }
import scala.collection.mutable.{ Map, Queue, HashMap }
/**
 * Risolutore di vincoli che restituisce la lista di tutte le soluzioni
 * ottime della CPNet dati i vincoli e tutti i domini.
 * @author Francesco Burato
 *
 */
object FCSolver {
  /**
   * Risolutore totale senza parametri
   */
  def solve(): Queue[Array[(String,String)]] = {
    // costruisco gli inizializzatori
    val domains = Domain.domains
    val independents = new Queue[(String,String)]()
    var dependents : List[String] = Nil
    val constraints = new HashMap[String,List[Constraint]]
    val domainsSet = new HashMap[String,Set[String]]
    // popolo constraints, independents e domainsSet
    for(variable <- domains.keys) {
      val constList = domains(variable).constraints.toList
      // se c' un solo vincolo, che insiste su una sola variabile che accetta un solo valore allora
      // la variabile in questione  indipendente
      checkIndependence(constList) match {
        case Some(x) => independents enqueue ((variable,x.accepted(0)(0)))
        case None =>domainsSet += ((variable,domains(variable).accepted))
        constraints += ((variable,constList))
        dependents = variable :: dependents
      }
    }
    // riduco i vincoli e i domini ai soli valori determinati dall'indipendenza
    for(dep <- dependents ) 
      for((indep,value)<-independents) {
        constraints get dep match {
          case Some(x) => 
            val temp = reduceSingleConstraint(constraints(dep),indep,value)
            constraints.update(dep,temp)
          case None =>
        }
        domainsSet get dep match {
          case Some(x) => 
            val temp = rebuildSingleDomain(constraints(dep),dep,domainsSet(dep))
            domainsSet(dep) = temp
          case None =>
        }
      }
    var solution: Queue[Array[(String,String)]] = Queue()
    val supportArray = new Array[(String,String)](domains.size)
    // istanzio le variabili indipendenti
    var i = 0
    for((variable,assign) <- independents) {
      supportArray(i) = ((variable,assign))
      i += 1
    }
    // calcolo le soluzioni ricorsive
    solve(dependents.toList, domainsSet, constraints, solution, supportArray, i)
    solution
  }
  

  private def checkIndependence(l : List[Constraint]) : Option[Constraint] = l match {
    case Nil => None
    case head :: tail => if(head.vars.size == 1 && head.accepted.size == 1)
        Some(head)
      else
        checkIndependence(tail)
  }
  private def solve(vars: List[String],
    domains: Map[String, Set[String]],
    constraints: Map[String, List[Constraint]],
    solutions: Queue[Array[(String,String)]],
    support: Array[(String,String)],
    i: Int): Unit = vars match {
    case Nil => // ho assegnato tutto le varaibili quindi aggiungo l'array alla coda
      solutions enqueue support
    case head :: tail => if (!domains(head).isEmpty) {
      // per ogni assegnamento possibile ai valori della variabile in head
      for (elem <- domains(head)) {
        // ricostruisci la lista dei vincoli con la restrizione "head = elem"
        val newconstr = constraints.map { case (s, c) => (s, reduceSingleConstraint(c, head, elem)) }
        // ricostruisci i domini in base ai nuovi vincoli
        val newdomains = newconstr.map { 
          case (s, c) => 
            (s, if (checkEmptyness(c, domains(s)).isEmpty) 
                  Set[String]() 
                else 
                  rebuildSingleDomain(c, s,domains(s))
             )
        }
        val newsup = support.clone
        newsup(i) = ((head,elem))
        // risolvi il resto del problema
        solve(tail, newdomains, newconstr, solutions, newsup, i + 1)
      }
    }
  }

  private def rebuildSingleDomain(l: List[Constraint], variable: String, all : Set[String]): Set[String] = l match {
    case Nil => all
    case head :: tail =>
      head.projection(variable) intersect rebuildSingleDomain(tail, variable,all)
  }

  private def checkEmptyness(l: List[Constraint], all: Set[String]): Set[String] = l match {
    case Nil => all
    case head :: tail =>
      if (head.accepted.isEmpty)
        Set[String]()
      else
        checkEmptyness(tail, all)
  }

  private def reduceSingleConstraint(l: List[Constraint], variable: String, assign: String): List[Constraint] = l match {
    case head :: tail =>
      if (head.vars contains variable)
        head.reduction(variable, assign) :: reduceSingleConstraint(tail, variable, assign)
      else
        head :: reduceSingleConstraint(tail, variable, assign)
    case Nil => Nil
  }

}