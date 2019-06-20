import scala.collection.mutable._
import scala.util.control.Breaks._
import scala.io.Source._

val PLUS = 1
val MINUS = 2
val DIV = 3
val TIMES = 4
val SQRT = 5
val INT = 6
val DOUBLE = 7
val ARROW = 8
val VAR = 9
val FUNC = 10
val LPAREN = 11
val RPAREN = 12
val NOTEQUAL = 13
val WHILE = 14
val BOOL = 15
val UNIT = 16
val EQUAL = 17
val COLONEQUAL = 18

var finalTypeEnviron = HashMap.empty[Int,Array[String]]
var parsed2 = HashMap.empty[String,Array[String]]
var parsed3 = ArrayBuffer[String]()
var finalExpressions = HashMap.empty[String,Array[String]]
var head = ""
var finalExpArr = ArrayBuffer[Array[String]]()
var finalTypArr = ArrayBuffer[Array[String]]()

val types = HashMap.empty[Int,String]
types += (INT -> "int")
types += (DOUBLE -> "double")

var typeEnvironPassed = true

def typeFile(filename:String): Unit ={

  val source = fromFile(filename)
  val map = HashMap.empty[Int,String]
  var parsed = HashMap.empty[Int,Array[String]]
  val arr = source.mkString.split("\\r\\n")
  val buffer = ArrayBuffer[String]()
  val symbols = ArrayBuffer[String]()

  for ( i <- 0 to arr.length-1){
    map += (i -> arr(i))
  }

  for ( i <- 0 to map.size-1) {
    val wordArr1 = map(i).mkString.split(" ")
    for ( j <- 0 to wordArr1.length-1) {
      val wordArr2 = wordArr1(j).replaceAll("\\(", "(\n")
      val wordArr3 = wordArr2.replaceAll("\\)","\n)")
      val wordArr4 = wordArr3.replaceAll(",","")
      val wordArr5 = wordArr4.split("\n")
      for ( k <- 0 to wordArr5.length-1) {
        buffer += wordArr5(k)
      }
    }
  }

  for ( i <- 0 to buffer.length-1) {
    symbols += getType(buffer(i))
  }

  var idx = 0
  var str = ""

  for ( i <- 0 to symbols.length-1) {
     str = str + symbols(i).toString + " "
    if(i > 0 && symbols(i-1) == ARROW.toString){
      parsed += (idx -> str.split(" "))
      idx = idx + 1
      str = ""
   }
  }
  finalTypeEnviron = parsed
}

def expressionFile(filename:String): Unit ={

  val source = fromFile(filename)
  val map = HashMap.empty[Int,String]
  var parsed = HashMap.empty[String,Array[String]]
  val arr = source.mkString.split("\\r\\n")
  val buffer = ArrayBuffer[String]()

  for ( i <- 0 to arr.length-1){
    map += (i -> arr(i))
  }

  for ( i <- 0 to map.size-1) {
    val wordArr1 = map(i).mkString.split(" ")
    for ( j <- 0 to wordArr1.length-1) {
      val wordArr2 = wordArr1(j).replaceAll("\\(", "(\n")
      val wordArr3 = wordArr2.replaceAll("\\)","\n)")
      val wordArr4 = wordArr3.replaceAll(",","")
      val wordArr5 = wordArr4.split("\n")
      for ( k <- 0 to wordArr5.length-1) {
        buffer += wordArr5(k)
      }
    }
  }

  breakable {
      for (i <- 0 to buffer.length - 1) {
        if (buffer(i).contains("->")) {
          head = buffer(i+1)
          break
        }
      }
  }


  var str = ""

  for ( i <- 0 to buffer.length-1) {
    str = str + buffer(i).toString + " "
    if(i > 0 && buffer(i-1) == "->"){
      parsed += (buffer(i) -> str.split(" "))
      str = ""
    }
  }
  parsed2 = parsed
}

def getType(x:String): String = {

  x match {
    case "+" => PLUS.toString
    case "-" => MINUS.toString
    case "/" => DIV.toString
    case "*" => TIMES.toString
    case "->" => ARROW.toString
    case "(" => LPAREN.toString
    case ")" => RPAREN.toString
    case "double" => DOUBLE.toString
    case "int" => INT.toString
    case "double&" => DOUBLE.toString
    case "int&" => INT.toString
    case "&double" => DOUBLE.toString
    case "&int" => INT.toString
    case "sqrt" => SQRT.toString
    case _ => x.toString
  }
}

def convertIntermediates(someMap:HashMap[String,Array[String]],index:Int): String ={
  val str = "$"
  val num = index
  val pos = str + num
  if(!someMap.keySet.contains(pos)){
    return "key not found"
  }
  for(i <- someMap(pos)){
    if(i.contains("$")){
      val idx = someMap(pos).indexOf(i)
      val tmp = someMap(pos).mkString(" ")
      var tmp2 = tmp.substring(tmp.indexOf(LPAREN.toString)+2,tmp.indexOf(RPAREN.toString))
      tmp2 = tmp2.trim
      if(tmp2.contains("$") && tmp2.length == 2) {
        tmp2 = convertIntermediates(someMap, tmp2.replace("$", "").toInt)
        val arr = someMap(pos)
        arr(idx) = tmp2
        someMap.update(pos, arr)
        for(i <- someMap(pos)){
          if(i.contains("$")){
            val idx = someMap(pos).indexOf(i)
            tmp2 = someMap(pos)(idx)
            tmp2 = convertIntermediates(someMap, tmp2.replace("$", "").toInt)
          }
        }
        return tmp2
      }
      else if(tmp2.contains("$") && tmp2.length > 2){
        val tmp3 = tmp2.substring(0,2)
        val tmp4 = tmp2.substring(3,5)
        tmp2 = tmp3
        tmp2 = convertIntermediates(someMap, tmp2.replace("$", "").toInt)
        var arr = someMap(pos)
        arr(idx) = tmp2
        someMap.update(pos, arr)

        tmp2 = tmp4
        tmp2 = convertIntermediates(someMap, tmp2.replace("$", "").toInt)
        arr = someMap(pos)
        arr(idx+1) = tmp2
        someMap.update(pos, arr)

        for(i <- someMap(pos)){
          if(i.contains("$")){
            val idx = someMap(pos).indexOf(i)
            tmp2 = someMap(pos)(idx)
            tmp2 = convertIntermediates(someMap, tmp2.replace("$", "").toInt)
          }
        }
        return tmp2
      }
      else if(!tmp2.contains("$") && tmp2.length == 1){
        var arr = someMap(pos)
        arr(idx) = tmp2
        someMap.update(pos, arr)
        return tmp2
      }
      else if(!tmp2.contains("$") && tmp2.length > 1){
        val tmp3 = tmp2.substring(0,1)
        val tmp4 = tmp2.substring(2,3)
        if(tmp3 == tmp4){
          tmp2 = tmp3
        }
        val arr = someMap(pos)
        arr(idx) = tmp2
        someMap.update(pos, arr)
        return tmp2
      }
    }

  }
  return pos
}

def invalidType(map:HashMap[Int,Array[String]]): Unit ={
  breakable {
    for (i <- 0 to map.size - 1) {
      for (j <- map(i)) {
        val tmp = map(i).mkString(" ")
        var tmp2 = tmp.substring(tmp.indexOf(LPAREN.toString) + 2, tmp.indexOf(RPAREN.toString))
        tmp2 = tmp2.trim
        if (tmp2.length > 1) {
          val tmp3 = tmp2.substring(0, 1)
          val tmp4 = tmp2.substring(2, 3)
          val tmp5 = tmp.substring(tmp.indexOf(ARROW.toString) + 2)
          if (tmp3 == tmp4 && tmp5 != tmp3) {
            println("there is no operation that takes " + types(tmp3.toInt) + " and " + types(tmp5.toInt))
            typeEnvironPassed = false
            break
          }
        }
        else if (tmp2.length == 1) {
          val tmp3 = tmp2.substring(0, 1)
          val tmp4 = tmp.substring(tmp.indexOf(ARROW.toString) + 2)
          if (tmp4 != tmp3) {
            println("there is no operation that takes " + types(tmp3.toInt) + " and " + types(tmp4.toInt))
            typeEnvironPassed = false
            break
          }
        }
      }
    }
  }
}





 // program run

// run for valid case
typeFile("type.txt")

// run for case with errors
//typeFile("badtype.txt")

// check for ambiguity

breakable {
  for (i <- finalTypeEnviron) {
    for (j <- finalTypeEnviron) {
      if (j != i && i._2.sameElements(j._2)) {
        println("type environment is ambiguous")
        typeEnvironPassed = false
        break
      }
    }
  }
}

// check for invalid operations

invalidType(finalTypeEnviron)

if(typeEnvironPassed) {
  expressionFile("expressions.txt")

  var num = 0
  var pos = "$" + num
  var str = parsed2(pos)

  // conversion of expressions into symbols

  for (i <- 0 to parsed2.size - 1) {
    for (j <- 0 to str.length - 1) {
      parsed3 += getType(str(j))
    }
    num = num + 1
    pos = "$" + num
    if (!parsed2.keySet.contains(pos)) {
      num = num + 1
      pos = "$" + num
    }
    if (parsed2.keySet.contains(pos)) {
      str = parsed2(pos)
    }
  }

  // conversion of symbols into hashmap

  var idx = 0
  var str2 = ""
  var posstr = "$" + idx

  for (i <- 0 to parsed3.length - 1) {
    str2 = str2 + parsed3(i).toString + " "
    if (i > 0 && parsed3(i - 1) == ARROW.toString) {
      if (!parsed2.keySet.contains(posstr)) {
        idx = idx + 1
        posstr = "$" + idx
      }
      if (parsed2.keySet.contains(posstr)) {
        finalExpressions += (posstr -> str2.split(" "))
        idx = idx + 1
        posstr = "$" + idx
        str2 = ""
      }
    }
  }

  // get the head of the expression tree, convert all variables to their types

  head = head.replace("$", "").trim
  val head2 = head.toInt
  convertIntermediates(finalExpressions, head2)

  // final check

  for (i <- finalExpressions) {
    finalExpArr += i._2
  }

  for (i <- finalTypeEnviron) {
    finalTypArr += i._2
  }

  var count = 0

  breakable {
    for (i <- finalTypArr) {
      for (k <- 0 to finalExpArr.length - 1) {
        if (finalExpArr(k).sameElements(i)) {
          count += 1
        }
      }
    }
  }

  if (count == finalExpArr.length) {
    println("The file is type correct.")
  } else {
    println("Error, rule not found.")
  }
}



