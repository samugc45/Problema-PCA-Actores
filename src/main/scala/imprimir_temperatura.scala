import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import akka.pattern.ask
import akka.util.Timeout
import java.awt._
import javax.swing._


class Ventana_Superior {
  private var textAreaSuperior: JTextArea = _

  def crearVentanaSuperior(): JFrame = {
    val frame = new JFrame("Ventana Superior: Información")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setSize(800, 300)
    frame.setLayout(new BorderLayout())
    textAreaSuperior = new JTextArea(10, 30)
    textAreaSuperior.setEditable(false)
    textAreaSuperior.setLineWrap(true)
    textAreaSuperior.setWrapStyleWord(true)
    frame.add(new JScrollPane(textAreaSuperior), BorderLayout.CENTER)
    frame
  }

  def escribirEnVentanaSuperior(mensaje: String): Unit = {
    SwingUtilities.invokeLater(() => {
      textAreaSuperior.append(mensaje + "\n")
      textAreaSuperior.setCaretPosition(textAreaSuperior.getDocument.getLength)
    })
  }

  def iniciar(): Unit = {
    val v = crearVentanaSuperior()
    v.setVisible(true)
  }
}

class Imprimir_temperatura(sensor: ActorRef, getObjetivo: () => Int) extends Actor {
  val ventana = new Ventana_Superior
  implicit val timeout: Timeout = Timeout(1.second)
  implicit val ec: ExecutionContext = context.system.dispatcher

  override def preStart(): Unit = {
    ventana.iniciar()
    arranca()
  }

  def imprime_informe(s: String): Unit = {
    ventana.escribirEnVentanaSuperior(s)
  }

  def arranca (): Unit = {
    val executionContext: ExecutionContext = context.system.dispatcher
    val tareaPeriodica: Cancellable = context.system.scheduler.scheduleAtFixedRate(
      initialDelay = 0.seconds,
      interval = 2.seconds,
      receiver = self,
      message = "dos segundos"
    )(executionContext) // <- lo pasas explícitamente aquí
  }


  def receive: Receive = {
    case "dos segundos" =>
      (sensor ? TemperaturaActual).mapTo[Int].foreach { actual =>
        val objetivo = getObjetivo()
        val mensaje = "Temperatura actual: " + actual + " | Temperatura objetivo: " + objetivo
        imprime_informe(mensaje)
      }
  }
}

