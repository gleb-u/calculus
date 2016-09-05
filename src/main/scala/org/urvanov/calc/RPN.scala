package org.urvanov.calc

import scala.collection.mutable.{ListBuffer, Stack}
import scala.util.Try
import org.json4s._

/**
  * Class to perform calculations via Reverse Polish Notation
  * Supported operations: - + / * ( ), unary minus
  * Public methods to perform calculation: performCalculationJObject, performCalculation
  * Created by Gleb Urvanov
  */
object RPN {
  
  /**
    * Facade method to perform expression calculation
    * @param expression string with expression. Wrong characters will be ignored
    * @return JObject with results of calculations
    */
  def performCalculationJObject(expression: String): JObject = {
	val calcResult = performCalculation(expression)
	calcResult._1 match {
		case true => new JObject(List(
						"error" -> JBool(false), 
						"result" -> JDouble(calcResult._2)))
		case false => new JObject(List(
						"error" -> JBool(true), 
						"message" -> JString("Bad expression")))
	}
  }

  /**
    * Facade method to perform expression calculation
    * @param expression string with expression. Wrong characters will be ignored
    * @return tuple (is success, result of calculation)
    */
  def performCalculation(expression: String): (Boolean, Double) = {
    val list = splitString(expression)
    var isSuccess = true
    var res = 0.0
    try {
      res = calculateRPN(buildRPN(list))
    } catch {
      case e:Exception => isSuccess = false
    }
    (isSuccess,res)
  }

  /**
    * Splits string into token list
    * valid characters are digits, dot and *-+/()
    * unary minus is supported too
    * other characters will be ignored
    * @param input string to split
    * @return expression in form of list of valid tokens
    */
  private def splitString (input: String): List[String] = {
    var result = new ListBuffer[String]()
    var buffer= ""
    val digit= "0123456789."
    val symbol= "+*/()"

    def addAndFlushBuffer() = {
      if (buffer.nonEmpty) result+= buffer
      buffer = ""
    }

    //Character ~ used as unary minus
    def addMinus() = {
      if (result.isEmpty || (((symbol contains result.last) || result.last.equals("-")) && !result.last.equals(")")))
		result+= "~" else
        result += "-"
    }

    for (c <- input.toCharArray) {
      if (digit contains c) buffer+= c else {
        addAndFlushBuffer()
        if (c == '-') addMinus() else
          if (symbol contains c) result+= c.toString
      }
    }
    addAndFlushBuffer()

    result.toList

  }

  /**
    * Builds Reverse Polish Notation from given token list
    * Correct equation as a result is not guaranteed
    * @param tokens expression in infix form
    * @return expression in RPN or empty list in case of bad expression given
    */
  private def buildRPN(tokens: List[String]): List[String]= {
    val rightassoc = 0
    val leftassoc = 1
    //Map of operators with association and priority. Seen this hint on github
    val operators = Map("~"->(4,rightassoc),"/"->(3,leftassoc),"*"->(3,leftassoc),"+"->(2,leftassoc),"-"->(2,leftassoc),
      "("->(1,leftassoc))
    val leftbracket = "("
    val rightbracket = ")"
    val stack = new Stack[String]
    var output = new ListBuffer[String]

    def isNumber(exp:String) = Try{exp.toDouble}.isSuccess

    def isRightAssoc(operator: String) = operators(operator)._2 == rightassoc

    def addToOutputFromStack(): ListBuffer[String] = output += stack.pop

    def handleOperator(token: String) = {
      while (stack.nonEmpty && (operators(stack.top)._1 >= operators(token)._1)) addToOutputFromStack()
      stack.push(token)
    }

    //At first my realisation of Shunting Yard looked like this
    //After reading about map functional combinator made a decision to rewrite it
    /*for (token <- tokens) {
      if (isNumber(token)) output+= token else
      if (token.equals(leftbracket)) stack.push(token) else
      if (token.equals(rightbracket)) {
        while (stack.nonEmpty && (!stack.top.equals(leftbracket))) addToOutputFromStack()
        if (stack.isEmpty) return List("")//BAD EXPRESSION
        stack.pop
      } else
      if (isRightAssoc(token)) stack.push(token) else
      if (!isRightAssoc(token)) handleOperator(token) else {
        return List("")
        //BAD EXPRESSION
      }
    }*/

    //Shunting Yard
    tokens map {
      case token:String if isNumber(token) => output+= token
      case token:String if token.equals(leftbracket) => stack.push(token)
      case token:String if token.equals(rightbracket) => {
        while (stack.nonEmpty && (!stack.top.equals(leftbracket))) addToOutputFromStack()
        if (stack.isEmpty) return List("")//Bad expression
        stack.pop
      }
      case token:String if isRightAssoc(token) => stack.push(token)
      case token:String if !isRightAssoc(token) && (!token.equals(leftbracket)) => handleOperator(token)
      case _ => return List("")//Bad expression
    }

    while (stack.nonEmpty) addToOutputFromStack()

    output.toList
  }

  /**
    * Calculates result of equation given in Reverse Polish Notation
    * @param tokens expression in RPN
    * @return result of calculations
    * throws exceptions in case of bad expression
    */
  private def calculateRPN(tokens: List[String]): Double= {
    val stack= List[Double]()

    //It is interesting feature have never met before
    def foldingFunction(values: List[Double], token: String): List[Double] = {
      values match {
        case x::rest if token.equals("~") => -x:: rest
        case x::y::rest => token match {
          case "-" => y - x :: rest //well, that was hard. rest.::(y-x) is much more clear for Scala beginner
          case "/" => y / x :: rest
          case "+" => y + x :: rest
          case "*" => y * x :: rest
          case token:String => token.toDouble :: values
        }
        case List() => token.toDouble :: values
        case List(_) => token.toDouble :: values
      }
    }

    val result = tokens.foldLeft(stack)(foldingFunction)
    result.length match {
      case 1 => result.head
      case _ => throw new IllegalArgumentException("Bad expression")
    }

  }


}
