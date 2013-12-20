package com.kismatka.mi.gui

import scala.swing.{Dialog, Button, MainFrame, SimpleSwingApplication}
import java.awt.Dimension
import javax.swing.UIManager
import com.sun.java.swing.plaf.windows.WindowsLookAndFeel

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Hello World"
    size = new Dimension(200, 100)
    UIManager.setLookAndFeel(new WindowsLookAndFeel)
    contents = Button("Click Me") {
      Dialog.showMessage(null, "Clicked", "Clicked", Dialog.Message.Info)
    }
  }
}
