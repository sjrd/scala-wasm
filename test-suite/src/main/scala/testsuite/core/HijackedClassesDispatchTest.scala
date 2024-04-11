package testsuite.core

import testsuite.Assert.ok

object HijackedClassesDispatchTest {
  def main(): Unit = {
    val obj = new Test()
    val otherObj = new Test()
    val obj2 = new Test2()
    val otherObj2 = new Test2()
    ok(
      testToString(true, "true") &&
        testToString(54321, "54321") &&
        testToString(obj, "Test class") &&
        testToStringStartsWith(obj2, "testsuite.core.HijackedClassesDispatchTest$Test2@") &&
        testToString('A', "A") &&
        testToStringStartsWith(new Array[Int](1), "[I@") &&
        testToStringStartsWith(new Array[String](1), "[Ljava.lang.String;@") &&
        testToStringStartsWith(new Array[Array[AnyRef]](1), "[[Ljava.lang.Object;@") &&
        testHashCode(true, 1231) &&
        testHashCode(54321, 54321) &&
        testHashCode("foo", 101574) &&
        testHashCode(obj, 123) &&
        testHashCode(obj2, 1) && // first object for which we ask idHashCode
        testHashCode(obj2, 1) && // it is also 1 the second time we ask
        testHashCode('A', 65) &&
        testIntValue(Int.box(5), 5) &&
        testIntValue(Long.box(6L), 6) &&
        testIntValue(Double.box(7.5), 7) &&
        testIntValue(new CustomNumber(), 789) &&
        testLength("foo", 3) &&
        testLength(new CustomCharSeq(), 54) &&
        testCharAt("foobar", 3, 'b') &&
        testCharAt(new CustomCharSeq(), 3, 'A') &&
        testEquals(true, 1, false) &&
        testEquals(1.0, 1, true) &&
        testEquals("foo", "foo", true) &&
        testEquals("foo", "bar", false) &&
        testEquals(obj, obj2, false) &&
        testEquals(obj, otherObj, true) &&
        testEquals(obj2, otherObj2, false) &&
        testNotifyAll(true) &&
        testNotifyAll(obj)
    )
  }

  def testToString(x: Any, expected: String): Boolean =
    x.toString() == expected

  def testToStringStartsWith(x: Any, expectedPrefix: String): Boolean =
    x.toString().startsWith(expectedPrefix)

  def testHashCode(x: Any, expected: Int): Boolean =
    x.hashCode() == expected

  def testIntValue(x: Number, expected: Int): Boolean =
    x.intValue() == expected

  def testLength(x: CharSequence, expected: Int): Boolean =
    x.length() == expected

  def testCharAt(x: CharSequence, i: Int, expected: Char): Boolean =
    x.charAt(i) == expected

  def testEquals(x: Any, y: Any, expected: Boolean): Boolean =
    x.asInstanceOf[AnyRef].equals(y) == expected

  def testNotifyAll(x: Any): Boolean = {
    // This is just to test that the call validates and does not trap
    x.asInstanceOf[AnyRef].notifyAll()
    true
  }

  class Test {
    override def toString(): String = "Test class"

    override def hashCode(): Int = 123

    override def equals(that: Any): Boolean =
      that.isInstanceOf[Test]
  }

  class Test2

  class CustomNumber() extends Number {
    def value(): Int = 789
    def intValue(): Int = value()
    def longValue(): Long = 789L
    def floatValue(): Float = 789.0f
    def doubleValue(): Double = 789.0
  }

  class CustomCharSeq extends CharSequence {
    def length(): Int = 54
    override def toString(): String = "CustomCharSeq"
    def charAt(index: Int): Char = 'A'
    def subSequence(start: Int, end: Int): CharSequence = this
  }
}
