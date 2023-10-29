package geoJson

import com.fasterxml.jackson.annotation.JsonSubTypes
import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}

import scala.language.implicitConversions

class GeometryBuildException(message: String) extends RuntimeException(message)

@JsonTypeInfo(
  use = JsonTypeInfo.Id.NAME,
  include = JsonTypeInfo.As.PROPERTY,
  property = "type"
)
@JsonSubTypes(Array(
  new JsonSubTypes.Type(value = classOf[Point], name = "Point"),
  new JsonSubTypes.Type(value = classOf[Polygon], name = "Polygon"),
  new JsonSubTypes.Type(value = classOf[LineString], name = "LineString")
))
sealed trait Geometry


case class  Point private ( var coordinates: (Double, Double)) extends Geometry{
  val `type`: String = "Point"
  def copy(): Unit = ()
}

case class Polygon private (coordinates: List[List[(Double, Double)]]) extends Geometry {
  val `type`: String = "Polygon"
  def copy(): Unit = ()
}

case class LineString private (coordinates: List[(Double, Double)]) extends Geometry {
  val `type`: String = "LineString"
  def copy(): Unit = ()
}

sealed trait WKTProcessor[T] {

  def fromWKT(s: String): Either[GeometryBuildException, T]
  def toWKT(obj: T): String
}

object Point extends WKTProcessor[Point] {
  val pattern = "^POINT\\s*\\(([0-9\\.]+)\\s+([0-9\\.]+)\\)$".r
  def apply(coordinates: (Double, Double)): Either[GeometryBuildException, Point] = {
    Right(new Point(coordinates))
  }

  override def fromWKT(s: String): Either[GeometryBuildException, Point] = {
      pattern.findFirstMatchIn(s) match {
        case Some(v) => Point((v.group(1).toDouble,v.group(2).toDouble))
        case None => Left(new GeometryBuildException(s"The `$s`  is not a WKT Point."))
      }
  }
  override def toWKT(obj: Point): String = {
    s"POINT(${obj.coordinates._1} ${obj.coordinates._2})"
  }
}

object Polygon extends WKTProcessor[Polygon] {
  def apply(coordinates: List[List[(Double, Double)]]): Either[GeometryBuildException, Polygon] = {
    coordinates match {
      case l if l.head.length >= 4 && l.head.head == l.head.last => Right(new Polygon(coordinates))
      case _ => Left(new GeometryBuildException("The polygon has invalid coordinates. The size should be more 3 and first and last should be the same."))
    }
  }

  override def fromWKT(s: String): Either[GeometryBuildException, Polygon] = {
      val pattern = "^POLYGON\\s*\\(\\(([\\d\\.\\s,-]+)\\)\\)$".r
      pattern.findFirstMatchIn(s) match {
        case Some(v) => {
          val maybeCoords = v.group(1).split(",").map(_.trim.split(" ").map(_.toDouble))
          maybeCoords.count(_.length != 2) match {
            case 0 => Polygon(List(maybeCoords.map(c => (c(0), c(1))).toList))
            case _ => Left(new GeometryBuildException(s"The $s is not a WKT polygon."))
          }
        }
        case None => Left(new GeometryBuildException(s"The $s is not a WKT polygon."))
      }
  }

  override def toWKT(obj: Polygon): String = {
    s"POLYGON(${obj.coordinates.map(lc => {
      s"(${lc.map(c=>s"${c._1} ${c._2}").mkString(", ")})"
    })})"
  }
}

object LineString extends WKTProcessor[LineString] {
  def apply(coordinates: List[(Double, Double)]): Either[GeometryBuildException, LineString] = {
    coordinates match {
      case l if l.length > 1 => Right(new LineString(coordinates))
      case _ => Left(new GeometryBuildException("The linestring has invalid coordinates. The size should be more 1."))
    }
  }

  override def fromWKT(s: String): Either[GeometryBuildException, LineString] = {
    val pattern = "^LINESTRING\\s*\\(([\\d\\.\\s,-]+)\\)$".r
    pattern.findFirstMatchIn(s) match {
      case Some(v) => {
        val maybeCoords = v.group(1).split(",").map(_.trim.split(" ").map(_.toDouble))
        maybeCoords.count(_.length != 2) match {
          case 0 => LineString(maybeCoords.map(c => (c(0), c(1))).toList)
          case _ => Left(new GeometryBuildException(s"The $s is not a WKT linestring."))
        }
      }
      case None => Left(new GeometryBuildException(s"The $s is not a WKT linestring."))
    }
  }

  override def toWKT(obj: LineString): String = {
    s"LINESTRING(${obj.coordinates.map(c=>s"${c._1} ${c._2}").mkString(",")})"
  }
}
case class Feature(geometry: Geometry,  properties: Map[String, String] = Map()){val `type`: String = "Feature"}

case class FeatureCollection(features: List[Feature]) {val `type` = "FeatureCollection"}

object FeatureCollection {
  private val jsonMapper = JsonMapper.builder().addModule(DefaultScalaModule).build() :: ClassTagExtensions

  def writeToGeoJSON(collection: FeatureCollection): String = {
    jsonMapper.writeValueAsString(collection)
  }

  def readFromGeoJSON(collection: String): FeatureCollection = {
    jsonMapper.readValue[FeatureCollection](collection)
  }

  def writeToWKT(collection: FeatureCollection): String = {
    collection.features.map { v => {
      v.geometry match {
        case Point(_) => Point.toWKT(v.geometry.asInstanceOf[Point])
        case LineString(_) => LineString.toWKT(v.geometry.asInstanceOf[LineString])
        case Polygon(_) => Polygon.toWKT(v.geometry.asInstanceOf[Polygon])
      }
    }
    }.mkString("\n")
  }

  def readFromWKT(collection: String): FeatureCollection = {
    val objects: List[Either[GeometryBuildException, Geometry]] = collection.split("\n").toList.map(line => {
      List(Point, Polygon, LineString).map(_ fromWKT line).find(_.isRight).getOrElse(Left(new GeometryBuildException("no factory")))
    })
    val (objectsWithError, objectCorrect) = objects.partition(_.isLeft)

    if (objectsWithError.nonEmpty) {
      println(s"[WARNING]: Some of the lines are incorrect. Incorrect lines:\n${
        objectsWithError.map({ case Left(v) => v.toString }).mkString("\n")
      }")
    }

    FeatureCollection(
      features = objectCorrect.map({
        case Right(value) => Feature(value)
      })
    )
  }
}
