package admiraledu.submit

/*
  URL: https://cs220.cs.umass.edu/ct/

  Ensure I'm logged in.
 Fields: assignment, step, file

 */

import javafx.application.{Application, Platform}
import javafx.scene.Scene
import javafx.scene.layout.StackPane
import javafx.scene.web.WebEngine
import javafx.scene.web.WebView
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.stage.Stage

object LoginWindow {

  var redirect : String = null
  var target : String = null

  class LoginWindowApplication extends Application {

    def start(stage : Stage) {
        val root = new StackPane()
        val view = new WebView()
        val engine = view.getEngine()
        root.getChildren().add(view)
        engine.load(redirect)
        val scene = new Scene(root, 800, 600)
        stage.setScene(scene)

        engine.locationProperty().addListener(new ChangeListener[String] {

          def changed(observable : ObservableValue[_ <: String],
                      oldLoc : String, newLoc : String) : Unit = {
            if (newLoc == target) {
              Platform.runLater(new Runnable {
                def run() = {
                  engine.load(null)
                  stage.hide()
                }
              })
            }
          }
        })

        stage.show()

    }

  }

  def show(redirect : String, target : String) = {
    LoginWindow.redirect = redirect
    LoginWindow.target = target
    Application.launch(classOf[LoginWindowApplication])
    Platform.exit()
  }

}
