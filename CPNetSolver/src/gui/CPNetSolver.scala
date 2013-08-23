package gui

import swing._
import swing.event._
import GridBagPanel._
import java.io.File
import constraintobjs.CPNetParser
import java.io.FileReader
import solver.POGCreator
import java.io.BufferedReader
import constraintobjs.Domain
import constraintobjs.Ordini
import solver.FCSolver
import scala.collection.mutable.Queue
import javax.swing.UIManager
import scala.swing.Alignment
import java.awt.Color
import java.awt.Font
import javax.swing.BorderFactory

object CPNetSolver extends SimpleSwingApplication {
  
  val editTextArea = new TextArea(20,20)
  object setModification extends Button("OK")  
  
  val CPNetLabel = new TextArea
  CPNetLabel.background = UIManager.getColor("Panel.background")
  CPNetLabel.editable = false
  CPNetLabel.focusable = false
  CPNetLabel.font = CPNetLabel.font.deriveFont(Font.BOLD) 
  
  val solutionsLabel = new TextArea
  solutionsLabel.background = UIManager.getColor("Panel.background")
  solutionsLabel.editable = false
  solutionsLabel.focusable = false
  solutionsLabel.font = solutionsLabel.font.deriveFont(Font.BOLD)
  
  val CPNetTextArea = new TextArea
  CPNetTextArea.background = UIManager.getColor("Panel.background")
  CPNetTextArea.editable = false
  CPNetTextArea.focusable = false

  val solutionsTextArea = new TextArea(20,20)
  solutionsTextArea.background = UIManager.getColor("Panel.background")
  solutionsTextArea.editable = false
  solutionsTextArea.focusable = false

  val menu = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Import from file") {importFromFile})
      contents += new Separator
      contents += new MenuItem(Action("Edit current CPNet") {editDialog.visible = true})
    }
    contents += new Menu("?") {
      contents += new MenuItem(Action("About") {
        Dialog.showMessage(null, "Constraint Programming class\nUniversity of Padua 2012/2013\nAuthors:\n * Francesco Burato\n * Simone Carriero\nhttp://www.github.com/fburato/CPNetSolver", "About", Dialog.Message.Info)
      })
    }
  }
  
  val graphPanel = new GraphPanel
  
  lazy val ui = new BoxPanel(Orientation.Vertical) {
    contents += CPNetLabel
    val s1 = new ScrollPane(CPNetTextArea)
    s1.border = BorderFactory.createEmptyBorder
    contents += s1
    contents += solutionsLabel
    val s2 = new ScrollPane(solutionsTextArea)
    s2.border = BorderFactory.createEmptyBorder
    contents += s2
  }
  
  def top = new MainFrame {
    title = "CPNetSolver"
    menuBar = menu
      
    val b = new BoxPanel(Orientation.Horizontal)
    b.contents += graphPanel.panel
    b.contents += ui
    contents = b
  }
  
  lazy val grid = new GridBagPanel {
    val c = new Constraints
    c.fill = Fill.Horizontal

    val s = new ScrollPane(editTextArea)
    s.border = BorderFactory.createEmptyBorder
    
    c.weightx = 0.5

    c.fill = Fill.Horizontal
    c.gridx = 0;
    c.gridy = 0;
    layout(s) = c

    c.fill = Fill.Horizontal
    c.weightx = 0.5;
    c.gridx = 0;
    c.gridy = 1;
    layout(setModification) = c
  }
  
  val editDialog = new Dialog {
    title = "Edit current CPNet"
    modal=true

    contents = grid

    listenTo(setModification)
    reactions += {
      case ButtonClicked(`setModification`) => {
        importFromString(editTextArea.text)
        this.visible = false
      }
    }
  }
 
  def fileToString(file: File) = {
    var s = ""
    val buffReader = new BufferedReader(new FileReader(file))
    var line: String = buffReader.readLine
    while (line != null) {
      s = s + line + "\n"
      line = buffReader.readLine
    }
    s
  }
  
  def cleanTextArea() = {
    editTextArea.text = ""
    CPNetLabel.text = ""
    CPNetTextArea.text = ""
    solutionsLabel.text = ""
    solutionsTextArea.text = ""
  }
  
  def setTextArea(string: String, solutions: Queue[Array[(String, String)]]) = {
    editTextArea.text = string
      
    var s = ""
    for (sol <- solutions) {
      for (v <- sol) {
        s = s + v._1 + " = " + v._2 + "\n"
      }
    }
    if (s.isEmpty){
      CPNetLabel.text = ""
      solutionsLabel.text = ""
    }
    else {
      CPNetLabel.text= "Current CPNet"
      solutionsLabel.text = "Solutions"
    }
    CPNetTextArea.text = string
    solutionsTextArea.text = s
    
  }
	  
  def choosePlainFile(title: String = ""): Option[File] = {  
    val chooser = new FileChooser()
    chooser.title = title
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      Some(chooser.selectedFile)
    }
    else None
  }
  
  def importFromString(string: String) = {
    try {
      Domain.reset
      Ordini.reset
      CPNetParser.parse(string)
      val solutions = FCSolver.solve
      val pog = new POGCreator
      graphPanel.paintGraph(pog.getGraph)
      setTextArea(string, solutions)
    }
    catch {
      case e:Exception => {
        graphPanel.cleanAll
        cleanTextArea()
        Dialog.showMessage(null, e.getMessage, "Error", Dialog.Message.Error)    
      }
    }
  }
  
  def importFromFile = {
    var x = ""
    choosePlainFile("Import from file") match {
      case None => 
      case Some(file) => {
        try {
          Domain.reset
          Ordini.reset
          CPNetParser.parse(new FileReader(file))
          val solutions = FCSolver.solve
          val pog = new POGCreator
          graphPanel.paintGraph(pog.getGraph)
          setTextArea(fileToString(file), solutions)
        }
        catch {
          case e:Exception => {
            graphPanel.cleanAll
            cleanTextArea()
            Dialog.showMessage(null, e.getMessage, "Error", Dialog.Message.Error)    
          }
        }        
      }
    }
  }
}
