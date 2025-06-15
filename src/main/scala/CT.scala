import akka.actor._
import scala.swing._
import scala.swing.event._
import java.awt.{Font, Point, Dimension}
import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import akka.event.Logging

sealed trait Mensaje extends Serializable

case object TemperaturaActual extends Mensaje
case object ActualizaTemperatura extends Mensaje

case class NuevaTemperaturaObjetivo(valor: Int) extends Mensaje

class VentanaActor extends Actor with ActorLogging {
  var estado = "cerrada"

  def receive = {
    case "estado ventana" => 
      log.info(s"Ventana: Consultado estado - valor: $estado")
      sender() ! estado
      
    case "abre ventana" => 
      estado = "abierta"
      log.info("Ventana: Cambiado estado a abierta")
      
    case "cierra ventana" => 
      estado = "cerrada"
      log.info("Ventana: Cambiado estado a cerrada")
  }
}

class RadiadorActor extends Actor with ActorLogging {
  var estado = "cerrada"

  def receive = {
    case "estado radiador" => 
      log.info(s"Radiador: Consultado estado - valor: $estado")
      sender() ! estado
      
    case "abre radiador" => 
      estado = "abierta"
      log.info("Radiador: Cambiado estado a abierta")
      
    case "cierra radiador" => 
      estado = "cerrada"
      log.info("Radiador: Cambiado estado a cerrada")
  }
}

class SensorActor(ventana: ActorRef, radiador: ActorRef) extends Actor with ActorLogging {
  var temperatura = 20

  override def preStart(): Unit = {
    log.info(s"Sensor: Iniciando con temperatura inicial de $temperatura grados")
    context.system.scheduler.scheduleAtFixedRate(2.seconds, 2.seconds, self, ActualizaTemperatura)
  }

  def receive = {
    case TemperaturaActual => 
      log.info(s"Sensor: Consultada temperatura actual - valor: $temperatura")
      sender() ! temperatura

    case ActualizaTemperatura =>
      implicit val timeout: Timeout = Timeout(1.second)
      log.debug("Sensor: Iniciando ciclo de actualización de temperatura")
      
      val futuroVentana = (ventana ? "estado ventana").mapTo[String]
      val futuroRadiador = (radiador ? "estado radiador").mapTo[String]

      for {
        estadoVentana <- futuroVentana
        estadoRadiador <- futuroRadiador
      } yield {
        val temperaturaAnterior = temperatura
        
        if (estadoVentana == "abierta" && estadoRadiador == "cerrada") {
          temperatura -= 1
          log.info(s"Sensor: Temperatura disminuye de $temperaturaAnterior a $temperatura " +
                   s"(Ventana: $estadoVentana, Radiador: $estadoRadiador)")
        }
        else if (estadoVentana == "cerrada" && estadoRadiador == "abierta") {
          temperatura += 1
          log.info(s"Sensor: Temperatura aumenta de $temperaturaAnterior a $temperatura " +
                   s"(Ventana: $estadoVentana, Radiador: $estadoRadiador)")
        }
        else {
          log.info(s"Sensor: Temperatura se mantiene en $temperatura " +
                   s"(Ventana: $estadoVentana, Radiador: $estadoRadiador)")
        }
      }
  }
}

class PlacaActor(sensor: ActorRef, radiador: ActorRef, ventana: ActorRef) extends Actor with ActorLogging {
  var temperaturaObjetivo = 23

  override def preStart(): Unit = {
    log.info(s"Placa: Iniciando con temperatura objetivo de $temperaturaObjetivo grados")
    context.system.scheduler.scheduleAtFixedRate(10.seconds, 10.seconds, self, "control temperatura")
  }

  def receive = {
    case NuevaTemperaturaObjetivo(valor) =>
      val anteriorObjetivo = temperaturaObjetivo
      temperaturaObjetivo = valor
      log.info(s"Placa: Temperatura objetivo actualizada de $anteriorObjetivo a $valor")

    case "control temperatura" =>
      implicit val timeout: Timeout = Timeout(1.second)
      log.debug("Placa: Iniciando ciclo de control de temperatura")
      
      (sensor ? TemperaturaActual).mapTo[Int].foreach { temperaturaActual =>
        log.info(s"Placa: Evaluando - Temperatura actual: $temperaturaActual, Objetivo: $temperaturaObjetivo")
        
        if (temperaturaActual < temperaturaObjetivo) {
          log.info("Placa: Temperatura por debajo del objetivo - Activando calefacción")
          radiador ! "abre radiador"
          ventana ! "cierra ventana"
        } else {
          log.info("Placa: Temperatura en/por encima del objetivo - Desactivando calefacción")
          radiador ! "cierra radiador"
          ventana ! "abre ventana"
        }
      }
  }
}

class VentanaOperacion(var temperatura: Int, callback: Int => Unit) extends MainFrame {
  title = "Control de Temperatura"

  val label = new Label("Temperatura objetivo"){
    font = new Font("Arial", Font.BOLD, 12)
    horizontalAlignment = Alignment.Left
  }

  val panelLabel = new BoxPanel(Orientation.Horizontal) {
    contents += label
    contents += Swing.HGlue
  }

  val campoTemperatura = new TextField {
    text = temperatura.toString
    editable = false
    horizontalAlignment = Alignment.Center
    maximumSize = new Dimension(80, 40)
  }
  val fontGrande = new Font("Arial", Font.BOLD, 20)

  val botonMas = new Button("+") { font = fontGrande }
  val botonMenos = new Button("-") { font = fontGrande }

  val panelBotones = new BoxPanel(Orientation.Horizontal) {
    contents += botonMenos
    contents += campoTemperatura
    contents += botonMas
  }

  contents = new BoxPanel(Orientation.Vertical) {
    contents += panelLabel
    contents += panelBotones
    border = Swing.EmptyBorder(20, 20, 20, 20)
    xLayoutAlignment = 0.0
  }

  listenTo(botonMas, botonMenos)

  reactions += {
    case ButtonClicked(`botonMas`) =>
      temperatura += 1
      actualizarTemperatura()

    case ButtonClicked(`botonMenos`) =>
      temperatura -= 1
      actualizarTemperatura()
  }

  def actualizarTemperatura(): Unit = {
    campoTemperatura.text = temperatura.toString
    callback(temperatura)
  }

  visible = true
}

object CT extends App {
  val ourSystem = ActorSystem("CT")

  val ventanaActor = ourSystem.actorOf(Props[VentanaActor](), "ventana")
  val radiadorActor = ourSystem.actorOf(Props[RadiadorActor](), "radiador")
  val sensorActor = ourSystem.actorOf(Props(new SensorActor(ventanaActor, radiadorActor)), "sensor")
  val placaActor = ourSystem.actorOf(Props(new PlacaActor(sensorActor, radiadorActor, ventanaActor)), "placa")

  var temperaturaActual = 23
  val imprimirTemperaturaActor = ourSystem.actorOf(
    Props(new Imprimir_temperatura(sensorActor, () => temperaturaActual)),
    "imprimir"
  )

  val ventana_op = new VentanaOperacion(temperaturaActual, nuevaTemp => informarCT(nuevaTemp))

  def informarCT(nuevaTemp: Int): Unit = {
    temperaturaActual = nuevaTemp
    placaActor ! NuevaTemperaturaObjetivo(nuevaTemp)
  }
}

