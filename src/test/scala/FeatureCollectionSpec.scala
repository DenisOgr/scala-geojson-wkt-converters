import geoJson.{Feature, FeatureCollection, GeometryBuildException, Point}
import org.scalatest.flatspec.AnyFlatSpec

class FeatureCollectionSpec extends AnyFlatSpec {
  behavior of "FeatureCollection"

  val testPoint: FeatureCollection = FeatureCollection(List(Feature(Point((1d, 1d)) match {
    case Left(value) => throw value
    case Right(value) => value
  })))

  val testPointGeoJson = """{"features":[{"geometry":{"type":"Point","coordinates":[1.0,1.0],"type":"Point"},"properties":{},"type":"Feature"}],"type":"FeatureCollection"}"""

  val testPointWKT = "POINT(1.0 1.0)"

  it should "produce valid Geo-JSON string when write to Geo-JSON format" in {
    assert(FeatureCollection.writeToGeoJSON(testPoint) === testPointGeoJson)
  }

  it should "produce valid Scala Feature collection object when read from the Geo-JSON string" in {
    assert(FeatureCollection.readFromGeoJSON(testPointGeoJson) == testPoint)
  }

  it should "produce valid WKT string when write to WKT format" in {
    assert(FeatureCollection.writeToWKT(testPoint) == testPointWKT)
  }

  it should "produce valid Scala Feature collection object when read from the WKT string" in {
    assert(FeatureCollection.readFromWKT(testPointWKT) == testPoint)
  }
}
