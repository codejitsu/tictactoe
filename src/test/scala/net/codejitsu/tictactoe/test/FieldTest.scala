package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._

import net.codejitsu.tictactoe.Field

class FieldTest {
	@Test def initialFieldIsEmpty() {
		val field = Field()
		
		assertTrue(field.isEmpty)
	}
}