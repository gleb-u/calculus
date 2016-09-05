package org.urvanov.calc

import org.scalatra._
import org.json4s._
import org.json4s.native.JsonMethods._

/**
  * Servlet to perform calculations upon BASE64-decoded UTF-8 string
  * Endpoint GET /calculus?query=[expression]
  * Supported operations: - + / * ( ), unary minus
  * Returns JSON response
  * Created by Gleb Urvanov
  */
class CalcServlet extends CalcStack {

  def decodeString(in: String): String = new String(java.util.Base64.getUrlDecoder.decode(in), "UTF-8")
  
  get("/calculus") {
	val expression = decodeString(params("query"))
	val jobject = RPN.performCalculationJObject(expression)
	compact(render(jobject))
  }

}
