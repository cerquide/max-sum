package es.csic.iiia.mmuca

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import breeze.util.HashIndex
import breeze.util.Index

case class Participant (val name:String) 

class Good(val idx:String, val name:String) {
  override def toString = "[Index=" + idx + " name=" + name + "]"
  override def hashCode = 41 * name.hashCode() + idx.hashCode()
  override def equals(other:Any) = {
    other match {
      case g : Good => (g canEqual this) && (g.idx == this.idx) && (g.name == this.name)
      case _ => false 
    }
  } 
  def canEqual(other:Any) = other.isInstanceOf[Good]
}


object Marking {  
  def fromXML(xmlNode : xml.Node, xmlIdToGoodIdx:Map[String,GoodIdx], goodName : String) = { 
	def addGM(xmlGM : xml.Node , M: Marking) = {
      val ref = xmlIdToGoodIdx((xmlGM \ "@idRef").text)
      if (M(ref) != 0)
        println("Duplicated GoodMarking")
      else 
        M(ref) =(xmlGM \ "@quantity").text.toInt
    }
 
    var marking = new Marking(xmlIdToGoodIdx.size)
    val xmlInputGoodMarkings = xmlNode \ goodName
    xmlInputGoodMarkings.map(xmlIGM => addGM(xmlIGM,marking))
  	marking
  }
}


class Schema(val inputs:Marking, val outputs:Marking) {
  override def toString = "[Inputs= " + inputs.deep + " Outputs= " + outputs.deep + "]"
  override def hashCode = 41 * inputs.hashCode() + outputs.hashCode()
  override def equals(other:Any) = {
    other match {
      case s : Schema => (s canEqual this) && (s.inputs == this.inputs) && (s.outputs == this.outputs)
      case _ => false 
    }
  }
  def canEqual(other:Any) = other.isInstanceOf[Schema]
}

object Schema{
  def fromXML(xmlTransformation : xml.Node , xmlIdToGoodIdx:Map[String,Int]) = {
      val inputMarking = Marking.fromXML(xmlTransformation, xmlIdToGoodIdx, "InputGood")
      val outputMarking = Marking.fromXML(xmlTransformation, xmlIdToGoodIdx, "OutputGood")
      new Schema(inputMarking,outputMarking)
    }
}

class UnknownBidCombinationType extends Exception
class NonSupportedComplexAtomicBidsException extends Exception
class NonSupportedComplexXORBidsException extends Exception

abstract class Bid(bidder: Participant)
case class AtomicBid(schema : Schema, bidder : Participant, price : Double) extends Bid(bidder)
//case class XORBid(atomicBids : List[AtomicBid], bidder:Participant) extends Bid(bidder)

object AtomicBid {
  def fromXML(xmlAtomicBid : xml.Node, xmlIdToSchema : Map[String,Schema], bidder:Participant) = {
      //println( xmlAtomicBid.text)
	  //println( (xmlAtomicBid \ "@price").text)
      val price = (xmlAtomicBid \ "@price").text.toDouble
      val xmlBidTransformations = (xmlAtomicBid \ "BidTransformation")
      if (xmlBidTransformations.size > 1) {
        throw new NonSupportedComplexAtomicBidsException()
      }
      val schema = xmlIdToSchema((xmlBidTransformations \ "@idRef").text)
      val quantity = ((xmlBidTransformations \ "@quantity").text.toInt)
      if (quantity > 1) {
        throw new NonSupportedComplexAtomicBidsException()
      }
      new AtomicBid(schema, bidder, price)
    }
}   

object XORBid {
  def fromXML(xmlCombination : xml.Node, xmlIdToSchema: Map[String,Schema],bidder: Participant) = {
    val xmlAtomicBids = (xmlCombination \ "AtomicBid")
    if (xmlAtomicBids.size > 1) {
      throw new NonSupportedComplexXORBidsException()
    }
    AtomicBid.fromXML(xmlAtomicBids(0),xmlIdToSchema,bidder)
  }
}

class Problem(val goods : Index[Good] , val participants : Index[Participant], val schemas : Index[Schema], val atomicBids:Index[AtomicBid])

object Problem {
  def fromXML(fileName : String) = {
    
    val xmlProblem = xml.XML.loadFile(fileName)
  
    // First step, getting the goods and defining the good codification, 
    // assigning each good an index between 0 and the number of goods minus one
    
    var goods = new HashIndex[Good]()
    var xmlIdToGood = new HashMap[String,Good]() 
    var xmlIdToGoodIdx = new HashMap[String,GoodIdx]()
    val xmlGoods = xmlProblem \ "Good"
    def indexXmlGood(xmlGood : xml.Node) = {
      val g = new Good((xmlGood \ "@id").text, (xmlGood \ "@name").text)
      xmlIdToGood(g.idx) = g
      xmlIdToGoodIdx(g.idx) = goods.index(g)
    }
    xmlGoods.map(indexXmlGood)
    val nGoods = goods.size
    println(goods)
 
    val MStart = Marking.fromXML(xmlProblem, xmlIdToGoodIdx, "InputGoodMarking")
    println("MStart = ",MStart.deep)
    
    val MEnd = Marking.fromXML(xmlProblem, xmlIdToGoodIdx, "OutputGoodMarking")
    println("MEnd = ",MEnd.deep)
    
    //class TransformationInXML(val schema: Schema, val id:String, val name:String)
    
    var schemas = new HashIndex[Schema]()
    //var transformationsInXML = new HashSet[Transformation]()
    var xmlIdToSchema = new HashMap[String,Schema]()
 
    def loadTransformation(xmlTransformation : xml.Node) {
      val schema = Schema.fromXML(xmlTransformation,xmlIdToGoodIdx)
      schemas.index(schema)
      val xmlId = (xmlTransformation \ "@id").text
      xmlIdToSchema(xmlId) = schema
    }
    (xmlProblem \ "Transformation").map(loadTransformation)
    println("Schemas: ",schemas)
    
    var participants = new HashIndex[Participant]()
    var atomicBids = new HashIndex[AtomicBid]()
    
    def loadBidCombination(xmlBidCombination: xml.Node) {
      //println(xmlBidCombination)
      val agentName = (xmlBidCombination \ "@agentName").text
      //println(agentName)
      val participant = new Participant(agentName)
      participants.index(participant)
      val combinationType =  (xmlBidCombination \ "@type").text
      combinationType match {
        case "XOR" => 
          //println("BidType is XOR")
          val atomicBid = XORBid.fromXML(xmlBidCombination,xmlIdToSchema,participant)
          atomicBids.index(atomicBid)
        case _ =>  throw new UnknownBidCombinationType()
      } 
    }
    (xmlProblem \ "BidsCombination").map(loadBidCombination)
    println(atomicBids)
    new Problem(goods,participants,schemas,atomicBids)
  }
}