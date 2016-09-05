package org.urvanov.calc

import org.scalatest.FunSuite

class RPNSuite extends FunSuite {
	
	test("2 * (23/(3*3))- 23 * (2*3) must be calculated correctly") {
		val res = RPN.performCalculation("2 * (23/(3*3))- 23 * (2*3)")
		assert(res._1 && res._2 == (2.0 * (23.0/(3.0*3.0))- 23.0 * (2.0*3.0)))
	}
	
	test("1 + 1 must be equal 2") {
		val res = RPN.performCalculation("1 + 1")
		assert(res._1 && res._2 == 2)
	}
	
	test("1 + (1) must be equal 2") {
		val res = RPN.performCalculation("1 + (1)")
		assert(res._1 && res._2 == 2)
	}
	
	test("1 + (-1) must be equal 0") {
		val res = RPN.performCalculation("1 + (-1)")
		assert(res._1 && res._2 == 0)
	}
	
	test("-1 + -1 must be equal -2") {
		val res = RPN.performCalculation("-1 + -1")
		assert(res._1 && res._2 == -2)
	}
	
	test ("(1+3) * (4-2) must be equal 8") {
		val res = RPN.performCalculation("(1+3) * (4-2)")
		assert(res._1 && res._2 == 8)
	}
	
	test ("(1+3)xxx * (yyy4-2)z  z / 3 must be qual (8/3)") {
		val res = RPN.performCalculation("(1+3)xxx * (yyy4-2)z  z / 3")
		assert(res._1 && res._2 == (8.0 / 3.0))
	}
	
	test ("20 must be qual 20") {
		val res = RPN.performCalculation("20")
		assert(res._1 && res._2 == 20)
	}
	
	test ("1 + * 2 must give error") {
		assert(!RPN.performCalculation("1 + * 2")._1)
	}
	
	test ("1 + (2)) must give error") {
		assert(!RPN.performCalculation("1 + (2))")._1)
	}
	
	test ("+ must give error") {
		assert(!RPN.performCalculation("+")._1)
	}
	
	test ("2 2 must give error") {
		assert(!RPN.performCalculation("2 2")._1)
	}

}