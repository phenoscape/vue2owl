package org.nescent.vue

import scala.io.Source
import scala.xml.XML
import scala.collection.mutable
import scala.collection.JavaConversions._
import java.io.InputStreamReader
import java.io.FileInputStream
import scala.xml.Node
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.apibinding.OWLManager
import java.io.File
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLClass
import scala.None
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression

object VUE2OWL extends App {

	val factory = OWLManager.getOWLDataFactory();
	val manager = OWLManager.createOWLOntologyManager();	
	val cleanedInput = Source.fromFile(args(0), "US-ASCII").getLines.dropWhile(_.startsWith("<!--")).mkString;
	val input = XML.loadString(cleanedInput);
	def isNode(elem: Node): Boolean = {
			(elem \ "@{http://www.w3.org/2001/XMLSchema-instance}type").head.text == "node";
	}
	def isLink(elem: Node): Boolean = {
			(elem \ "@{http://www.w3.org/2001/XMLSchema-instance}type").head.text == "link";
	}
	val docIRI = (input \ "URIString").head.text;
	val ontology = manager.createOntology(IRI.create(docIRI));
	val terms: mutable.Map[String, OWLEntity] = mutable.Map();
	(input \ "child").filter(isNode(_)).foreach(elem => {
		val id = (elem \ "@ID").head.text;
		val iri = IRI.create((elem \ "URIString").head.text);
		(elem \ "@label").foreach(attrValue => manager.addAxiom(ontology, factory.getOWLAnnotationAssertionAxiom(factory.getRDFSLabel(), iri, factory.getOWLLiteral(attrValue.text))));
		val termOption = (elem \ "shape" \ "@{http://www.w3.org/2001/XMLSchema-instance}type").head.text match {
		case "roundRect" => Some(factory.getOWLClass(iri));
		case "ellipse" => Some(factory.getOWLNamedIndividual(iri));
		case _ => None;
		}
		termOption.foreach(term => {
			terms.put(id, term);
			manager.addAxiom(ontology, factory.getOWLDeclarationAxiom(term));
		});
	});
	(input \ "child").filter(isLink(_)).foreach(elem => {
		val sourceID = (elem \ "ID1").head.text;
		val targetID = (elem \ "ID2").head.text;
		val source = terms(sourceID);
		val target = terms(targetID);
		val relation = (elem \ "@label").headOption.map(attr => factory.getOWLObjectProperty(IRI.create(docIRI + "#" + attr.text)));
		val restrictionFactory = (elem \ "@strokeStyle").headOption.filter(_.text == "4") match {
		case Some(node) => factory.getOWLObjectAllValuesFrom _;
		case None => factory.getOWLObjectSomeValuesFrom _;
		}
		(source, target, relation) match {
		case (individual: OWLNamedIndividual, ontClass: OWLClass, None) => {
			manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(ontClass, individual));
		}
		case (individual: OWLNamedIndividual, ontClass: OWLClass, Some(property)) => {
			manager.addAxiom(ontology, factory.getOWLClassAssertionAxiom(restrictionFactory(property, ontClass), individual));
		}
		case (subclass: OWLClass, superclass: OWLClass, None) => {
			manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(subclass, superclass));
		}
		case (subclass: OWLClass, superclass: OWLClass, Some(property)) => {
			manager.addAxiom(ontology, factory.getOWLSubClassOfAxiom(subclass, restrictionFactory(property, superclass)));
		}
		case (subject: OWLNamedIndividual, value: OWLNamedIndividual, Some(property)) => {
			manager.addAxiom(ontology, factory.getOWLObjectPropertyAssertionAxiom(property, subject, value));
		}
		case _ => {}
		}
	});
	manager.saveOntology(ontology, IRI.create(new File(args(1))));


}