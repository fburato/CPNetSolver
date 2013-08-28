/**
 * CPNetSolver
 * Copyright (C) 2013  Francesco Burato, Simone Carriero
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see [http://www.gnu.org/licenses/].
 * 
 * File: CPNetSolver.scala
 * Package: gui
 * Autore: Simone Carriero
 * Creazione: 03/lug/2013
 */
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
  
  val editDialog : Dialog = new Dialog {
    title = "Edit current CPNet"
    modal=true

    contents = grid

    listenTo(setModification)
    reactions += {
      case ButtonClicked(`setModification`) => {
        importFromString(editTextArea.text)
        //this.visible = false
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
    //editTextArea.text = ""
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
        s = s + v._1 + " = " + v._2 + " "
      }
      s = s + "\n"
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
      editDialog.visible = false
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
