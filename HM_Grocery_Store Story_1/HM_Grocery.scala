package HM_Grocery_Store

import scala.xml._



object HM_Grocery extends App{
  case class Items(id : Int,name : String,uom :String,unitsize:Int,unitprice:String,quantity:String)

  case class unitprice(amount : Double,currency:String)
  case class Quantity(stock :Int,measure:String)
  var total_bill=0.0
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

                         // Story 3


  case class Item_toCart(product:Array[String],Q :Array[Int])

   def Add_Cart(): Unit = {


     println("How many items you wanna buy-: ")
     val n = readInt()
     val itemname = new Array[String](n)
     val item_quan = new Array[Int](n)
     var sum= new Array[Double](n)

     for (i <- 0 until n) {
       println("Choose any Item from Catalogue -: ")
       itemname(i) = readLine()

       println("Enter the Quantity of Item -: ")
       item_quan(i) = readInt()


       for(j <- 0 until 4){
        if(itemname(i) == item(j).name){
          itemname(i)=("Item Name-: " + item(j).name + ", " + "Price-: " + item(j).unitprice + " ,Quantity -: ")
          var total = (item(j).unitprice).split(" ")
          sum(i)= (total(0).toDouble) * item_quan(i)
          total_bill += sum(i)
        }
      }

       }

       val li = (itemname, item_quan,sum).zipped            // print items in cart
       println("Products added in Cart are -: ")
         li.toList.foreach(println)

         println("Do you want to Checkout y/n")          // Checkout Cart
          val ans = readLine()
         if(ans=="y"){
          println("Your Cart Items Are -:")
           li.toList.foreach(println)
          println("The total Amount to be paid is-:"+total_bill)
         }

 }


  val xml_file = XML.loadFile("/home/dashmeet/IdeaProjects/scala practice/src/HM_Grocery_Store/HM_Data.xml")
  var item =  (xml_file \\ "item").map(showItems)
  println("                        Welcome to H&M Grocery Store                      ")

  println("H&M's Catalogue -: ")
  item.foreach(println)
 
   Add_Cart


}
