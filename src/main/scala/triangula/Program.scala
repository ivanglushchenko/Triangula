package triangula

import swing._

import scala.swing.Swing._
import scala.swing.{ MainFrame, Panel }
import scala.swing.event._
import java.awt.{ Color, Graphics2D, Point, geom }

object Program extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Triangula"
    contents = SolverForBoard3By3.ui
  }
}