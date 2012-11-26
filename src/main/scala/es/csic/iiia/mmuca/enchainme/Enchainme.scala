package es.csic.iiia.mmuca.enchainme

import es.csic.iiia.mmuca
import es.csic.iiia.mmuca.Problem

class SchemaMediator {}

class GoodAgent(id:Any,initialState:GoodAgentState) extends DataGraphVertex(id,initialState) {}

class Enchainme {
  def solve(p:mmuca.Problem) = {
    // Create an agent for each good 
    p.goods.foreach()
    
    // Signal Collect for the amount 
  }  
}

object PeriodicAuctionApp extends App {

  Problem.fromXML("src/main/resources/88.xml")
  Problem.fromXML("src/main/resources/test.xml")

}