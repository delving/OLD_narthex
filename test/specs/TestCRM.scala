package specs

import java.io.InputStream
import java.util

import com.hp.hpl.jena.rdf.model._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConversions._

class CRM(is: InputStream) {
  val RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val RDFS = "http://www.w3.org/2000/01/rdf-schema#"
  val CRM = "http://www.cidoc-crm.org/cidoc-crm/"

  val model = ModelFactory.createDefaultModel().read(is, null)

  def list(s: Option[Resource], p: Option[Property], o: Option[Resource]): util.List[Statement] =
    model.listStatements(s.orNull, p.orNull, o.orNull).toList

  def rdf(localName: String) = model.getProperty(RDF, localName)

  def rdfs(localName: String) = model.getProperty(RDFS, localName)

  val LABEL = rdfs("label")
  val DOMAIN = rdfs("domain")
  val RANGE = rdfs("range")
  val COMMENT = rdfs("comment")
  val SUB_CLASS_OF = rdfs("subClassOf")
  val SUB_PROPERTY_OF = rdfs("subPropertyOf")
  val CLASS = rdfs("Class")
  val LITERAL = rdfs("Literal")
  val PROPERTY = rdf("Property")
  val TYPE = rdf("type")

  def preUnderscore(resource: Resource) = {
    val name = resource.getLocalName
    name.substring(0, name.indexOf("_"))
  }

  def postUnderscore(resource: Resource) = {
    val name = resource.getLocalName
    name.substring(name.indexOf("_") + 1).replaceAll("-", "_")
  }

  def subjects(p: Property, o: Resource): List[Resource] =
    list(None, Some(p), Some(o)).map(_.getSubject).toList

  def objects(s: Resource, p: Property): List[Resource] =
    list(Some(s), Some(p), None).map(_.getObject.asResource()).toList

  def literal(s: Resource, p: Property, l: String = "en"): String =
    list(Some(s), Some(p), None)
      .map(_.getObject.asLiteral())
      .find(_.getLanguage == l)
      .map(_.getString)
      .getOrElse(postUnderscore(s))

  case class CRMClass(resource: Resource) {
    lazy val superClasses = objects(resource, SUB_CLASS_OF).map(toClass)
    lazy val subClasses = subjects(SUB_CLASS_OF, resource).map(toClass)
    lazy val properties = subjects(DOMAIN, resource).map(toProperty)
    lazy val comment = literal(resource, COMMENT)

    override def toString = postUnderscore(resource)
  }

  case class CRMProperty(resource: Resource) {
    lazy val superProperties = objects(resource, SUB_PROPERTY_OF).map(toProperty)
    lazy val subProperties = subjects(SUB_PROPERTY_OF, resource).map(toProperty)
    lazy val rangeClass = objects(resource, RANGE).filter(_ != LITERAL).map(toClass).headOption
    lazy val comment = literal(resource, COMMENT)

    override def toString = resource.getLocalName

    //    override def toString = postUnderscore(resource)
  }

  val classes = subjects(TYPE, CLASS).map(CRMClass)
  val rootClass = classes.find(_.superClasses.isEmpty).get
  val properties = subjects(TYPE, PROPERTY).map(CRMProperty)

  def toClass(resource: Resource): CRMClass = classes.find(_.resource == resource).getOrElse(throw new RuntimeException(s"No class for $resource"))

  def toProperty(resource: Resource): CRMProperty = properties.find(_.resource == resource).getOrElse(throw new RuntimeException(s"No property for $resource"))

  def findClass(localName: String) = classes.find(_.resource.getLocalName == localName).get

  def findProperty(localName: String) = properties.find(_.resource.getLocalName == localName).get
}

class TestCRM extends FlatSpec with Matchers {

  "The CRM" should "appear in JSON" in {

    val CRM = new CRM(getClass.getResource("/crm/cidoc_crm_v5.0.4_official_release.rdfs.xml").openStream())

    def countTree(crmClass: CRM.CRMClass): Int = 1 + crmClass.subClasses.map(countTree).sum

    def showProperty(crmProperty: CRM.CRMProperty, level: Int): Unit = {
      val indent = "   " * level
      val range = crmProperty.rangeClass.map(_.toString).getOrElse("Literal")
      val count = crmProperty.rangeClass.map(countTree).getOrElse(0)
      crmProperty.subProperties.foreach(showProperty(_, level + 1))
    }

    def showClass(crmClass: CRM.CRMClass, withProperties: Boolean, level: Int = 0): Unit = {
      val indent = "   " * level
      if (withProperties) crmClass.properties.foreach(showProperty(_, level + 1))
      crmClass.subClasses.foreach(showClass(_, withProperties, level + 1))
    }

  }
}
