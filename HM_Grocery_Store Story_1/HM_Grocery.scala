package HM_Grocery_Store

import scala.xml._

object HM_Grocery extends App{
  case class Items(id : Int,name : String,uom :String,unitsize:Int,unitprice:String,quantity:String)

  case class unitprice(amount : Double,currency:String)
  case class Quantity(stock :Int,measure:String)

  def show_unitprice(n:Node) :unitprice = {
    var amount = (n\\"amount").text.toDouble
     var currency = (n \\ "currency").text
    unitprice(amount,currency)
  }

  def show_quantity(n:Node) :Quantity = {
    var stock = (n\\"stock").text.toInt
    var measure = (n \\ "measure").text
    Quantity(stock,measure)
  }

  def showItems(n:Node):Items={

    var id = (n \\ "id").text.toInt
    var name = (n \\ "name").text
    var uom = (n \\ "uom").text
    var unitsize = (n \\ "unitSize").text.toInt
    var unit_price = (n\\"unitPrice").map(show_unitprice).toList match{
      case List(unitprice(amount : Double,currency:String)) => amount + " "+ currency
      case _ => "No price is given"
    }

    var quantity = (n\\"quantity").map(show_quantity).toList match {
      case List(Quantity(stock :Int,measure:String)) => stock + " " + measure
      case _ => "No quantity given"
    }
    Items(id,name,uom,unitsize, unit_price,quantity)

  }

  val xml_file = XML.loadFile("/home/dashmeet/IdeaProjects/scala practice/src/HM_Grocery_Store/HM_Data.xml")
  var item =  (xml_file \\ "item").map(showItems)
  item.foreach(println)
}
