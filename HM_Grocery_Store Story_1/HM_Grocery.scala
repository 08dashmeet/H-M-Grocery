package HM_Grocery_Store
import java.util

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.util.control.Breaks._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.xml._

class HM_Grocery  {
  case class Items(id : Int,name : String,uom :String,unitsize:Int,unitprice:String,quantity:String)
  case class Category(Category_id:String,Category_name:String)
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

  def Show_Cat(n:Node): (String,String)={
    var cat_id = (n \\"@id").text
    var cat_name = (n  \\"@name").text

    (cat_id ,cat_name)
  }


                         // Story 3

 var itemname = ListBuffer[String]()
  var iname = ListBuffer[String]()
  val item_quan = ListBuffer[Int]()
  var sum = ListBuffer[Double]()
  var li = List[(String,Int)]()
  var n =0
  var  total_bill = 0.0
  var discount=0.0
  var ans=0.0
  var usr_category=0


   def Add_Cart(): Unit = {

     println("How many items you wanna buy-: ")
     n = readInt()

     for (i <- 0 until n) {
       println("Choose any Item from Catalogue -: ")
       iname += readLine()

       println("Enter the Quantity of Item -: ")
       item_quan += readInt()

      for (j <- 0 to 9) {
        if (iname(i).equalsIgnoreCase(item(j).name) && (item(j).id/100)== usr_category){
          itemname += ("Item Name-: " + iname(i) + ", " + "Price-: " + item(j).unitprice + " ,Quantity -: ").toString()
           }
     }

     }

     li = (itemname, item_quan).zipped.toList             // print items in cart
     println("Products added in Cart are -: ")
     li.foreach(println)
 }
  def Cart_discount: Unit ={
    // Cart_discount function
    var l= li.length
    for(i <- 0 until l) {
      for(j <- 0 to 9) {
        if (iname(i)==(item(j).name)) {
          var total = (item(j).unitprice).split(" ")

          if (HM_Grocery.no == HM_Grocery.hm.get((item(j).id/100)).get) {

            HM_Grocery.no match {
              case 1 => discount = discount+ (total(0).toDouble) * item_quan(i) * 0.60
              case 2 => discount = discount+ (total(0).toDouble) * item_quan(i) * 0.40
              case _ => discount = 0.0
            }

          }

        }
      }
    }

  }


  def Check_Cart: Unit ={
  println("Do you want to Checkout y/n")                 // Checkout_Cart function
  val ans = readLine()
  if(ans=="y"){
    println("Your Items in Cart Are -:")
    li.foreach(println)
     var l= li.length

    for(i <- 0 until l){
      for (j <- 0 to 9) {
        if (iname(i)==(item(j).name)) {
          var total = (item(j).unitprice).split(" ")
          sum += (total(0).toDouble) * item_quan(i)
           total_bill = total_bill+ sum(i)
        }
      }
    }
    println(total_bill)
    println("The total Amount to be paid is-: "+(total_bill-discount))
  }
}


  def Order_Summary: Unit ={                             // order_summary function
    println("Your Order Summery is ")
    println("Items Purchased are -")
    li.foreach(println)

    println("The total Amount to be paid is-: "+ (total_bill-discount))

  }


  def update_toCart: Unit = {                                 // Update_Cart function

    breakable  {
      for (i <- 0 until n) {
        println("Do you wanna update your Cart Delete/Update Quantity/No")
        val usr = readLine()
        if (usr == "Update Quantity") {
          println("Enter the product name to be Updated from Cart")
          val update_name = readLine()
          for (i <- 0 until n) {
            if (update_name == iname(i)) {

              println("Enter the Quantity")
              val new_item_quan = readInt()
              item_quan(i) = new_item_quan
            }
          }


        }
        else if (usr == "Delete") {
          println("Enter the name of item to be deleted")
          var del = readLine()
          for (i <- 0 until n) {
            if (del == iname(i)) {
              itemname.remove(i)
              item_quan.remove(i)
            }
          }
        }

        else {

          break

        }
      }
      }
      println("Updated Cart is -:")
      li = (itemname, item_quan).zipped.toList
      li.foreach(println)
    }


               // Story 6&7

    // display the Catalogue items


  val xml_file = XML.loadFile("/home/dashmeet/IdeaProjects/scala practice/src/HM_Grocery_Store/HM_Data.xml")
  var item =  (xml_file \\ "item").map(showItems).toList
  var cat_item =  (xml_file \\ "category").map(Show_Cat).toList
  println("                        Welcome to H&M Grocery Store                      ")


  println("Categories are-:")
  cat_item.foreach(println)
  println(" Choose Any Category -:")
   usr_category = readInt()
  var i =0
  for(i <- 0 to 9){
    if(((item(i).id)/100) == usr_category){

        println(item(i))
    }

  }

}


     object HM_Grocery extends App{

       var no = 0
     var hm=Map[Int,Int]()
     val password = "1234"
      println("Login As a Admin Or User")
      var a = readLine()
      if(a=="Admin") {
        println("Enter the Password")
        var p = readLine()
        if (p == password) {
          println("Welcome to Admin part")

          println("Discounts on Different Categories -:")
                  println("1-: 60% discount on Beauty & Hygiene")
                  println("2-: 40% discount on Fruits & Vegetable")
                       println("Choose the Discount or print 0")
                     no = readInt()
                    hm = Map(1-> 1,2 -> 2, 3 -> 3, 4 -> 4,5 ->5)
          println("Do u wanna continue As User")
          var p= readLine()
          if(p=="y") {
            var obj = new HM_Grocery
            obj.Add_Cart()
            obj.update_toCart
            obj.Cart_discount
            obj.Check_Cart
            obj.Order_Summary
          }
          else{
            println("Byeee!!!!!!!")
          }
              }

          else
          {
            println("Wrong Password")
          }

        }
        else if (a == "User") {
          var obj1 = new HM_Grocery
          obj1.Add_Cart()
          obj1.Cart_discount
          obj1.Check_Cart
          obj1.Order_Summary
        }



}