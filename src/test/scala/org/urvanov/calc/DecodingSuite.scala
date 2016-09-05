package org.urvanov.calc

import org.scalatest.FunSuite

class DecodingSuite extends FunSuite {
	
	test("MiAqICgyMy8oMyozKSktIDIzICogKDIqMyk= decoded must be equal to 2 * (23/(3*3))- 23 * (2*3)") {
		val servlet = new CalcServlet()
		assert(servlet.decodeString("MiAqICgyMy8oMyozKSktIDIzICogKDIqMyk=") equals "2 * (23/(3*3))- 23 * (2*3)")
	}
}