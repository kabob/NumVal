/*
 * Copyright (c) 2013, Robert W Kohlenberger
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * The views and conclusions contained in the software and documentation are those
 * of the authors and should not be interpreted as representing official policies, 
 * either expressed or implied, of the FreeBSD Project.
 */

/**
 * <!-- File TestNumVal.scala -->
 * @author Robert W Kohlenberger
 */

package org.oxland.numval

import java.math.MathContext

import scala.Enumeration

import org.scalatest.Spec
import org.scalatest.Ignore

/**
 * <!-- TestNumVal --> Unit test the behaviors of trait NumVal.
 *
 * <b>Adding Test Cases</b>
 *
 * If <code>trait NumVal</code> is modified to support additional numeric types or operators,
 * this class will also need modification to verify the new behavior.  Here is a recipe for expanding test coverage.
 *
 * To add new test data for existing types and operators (easy), include new test values in method
 * <code>valueList</code> for the appropriate value type(s).
 *
 * To add tests for new operators on existing types (moderate difficulty), define a new test method and its
 * corresponding (unary or binary) test function.  For example, the '+' operator has this test method:
 *
 *       <code>def testPlus = binaryOp(addFunc)</code>
 *
 * Test method <code>testPlus</code> passes its binary test function <code>addFunc</code> to method binaryOp,
 * which applies all permutations of the test data in <code>valueList</code> to that function.  To test a new
 * binary operator you can follow the <code>testPlus</code>/<code>addFunc</code> example.  To test a new unary
 * operator, you can follow a unary test example, such as <code>testAbv</code>/<code>abvFunc</code>.
 *
 * To add tests for a new numeric type (e.g. <code>Complex</code>) is tedious, due to the combinatorial explosion
 * of operators, types and values.  First, the new type must be added to object <code>NumTypes</code>, and new test
 * values added to method <code>valueList</code>.  Then, each existing test function must be supplemented with cases
 * that test the new type.  Binary test functions must include cases in both directions, e.g.
 *
 *     <code>case (u:Complex, v:Boolean)</code> ...
 *     <code>case (u:Complex, v:Byte)</code> ...
 *     ...
 *     <code>case (u:Boolean, v:Complex)</code> ...
 *     ...
 *     <code>case (u:Byte, v:Complex)</code> ...
 *     ...
 *
 * Perhaps ScalaTest provides a means to factor out this combinatorial explosion, or else Reflection or Scala's
 * upcoming macros could reduce this code.  At least the existing test code has the virtue of being readable and
 * real-world code, if exhaustively repetitive code.
 *
 * Fortunately, <i>using</i> <code>NumVal</code> avoids the very combinatorial explosion needed to <i>test</i> it!
 */
class TestNumVal extends Spec {
  //  var caseCount = 0

  /**
   * <!-- NumType --> Enumeration of numeric types
   */
  protected object NumType extends Enumeration { // TODO: use same NumType object as NumVal?
    type NumType = Value
    val unit, bool, byte, char, short, int, long, float, double, bigInt, bigDec, unknown = Value
  }
  import NumType._

  /**
   * <!-- valueList --> Create an array of test values for a given numeric type.
   * @param t The numeric type.
   * @return The Array of test values for type t.
   */
  protected def valueList(t: NumType): Array[NumVal] =
    t match {
      case `bool` => Array(true, false)
      case `byte` => Array(Byte.MinValue, -2.byteValue, -1.byteValue, 0.byteValue, 1.byteValue, 2.byteValue, Byte.MaxValue)
      case `char` => Array(Char.MinValue, 1.toChar, 2.toChar, Char.MaxValue)
      case `short` => Array(Short.MinValue, -2.toShort, -1.toShort, 0.toShort, 1.toShort, 2.toShort, Short.MaxValue)
      case `int` => Array(Int.MinValue, -2, -1, 0, 1, 2, Int.MaxValue)
      case `long` => Array(Long.MinValue, -2l, -1l, 0l, 1l, 2l, Long.MaxValue)
      case `float` => Array(Float.NegativeInfinity, Float.MinValue, -3.6f, -3.5f, -3.4f, -2f, -1f, -0.3f, -Float.MinPositiveValue, 0f,
        Float.MinPositiveValue, 0.3f, 1f, 2f, 3.4f, 3.5f, 3.6f, Float.MaxValue, Float.PositiveInfinity)
      case `double` => Array(Double.NegativeInfinity, Double.MinValue, -0.3, -3.6, -3.5, -3.4, -2.0, -1.0, -0.3, -Double.MinPositiveValue, 0.0,
        Double.MinPositiveValue, 0.3, 1.0, 2.0, 3.4, 3.5, 3.6, Double.MaxValue, Double.PositiveInfinity)
      case `bigInt` => Array(BigInt(Long.MinValue), BigInt(-2), BigInt(-1), BigInt(0), BigInt(1), BigInt(2), BigInt(Long.MaxValue))
      case `bigDec` => Array(BigDecimal(Double.MinValue), BigDecimal("-3.6"), BigDecimal("-3.5"), BigDecimal("-3.4"),
        BigDecimal("-2"), BigDecimal("-1"), BigDecimal(-Double.MinPositiveValue), BigDecimal("0"),
        BigDecimal(Double.MinPositiveValue), BigDecimal("1"), BigDecimal("2"), BigDecimal("3.4"),
        BigDecimal("3.5"), BigDecimal("3.6"), BigDecimal(Double.MaxValue))
      case _ => Array()
    }

  // Verify that each array is the expected type:
  //  NumType.values.foreach {
  //    (t: NumType) => {
  //    val array = valueList(t)
  //    println(t.toString +": " + array.getClass.getComponentType)
  //    }
  //  }

  /**
   * <!-- unaryOp --> Apply each valueList value to a unary operation.  Each f invocation with a unique operand constitutes a test case.
   * Employs the Visitor pattern.
   * @param f The function applied.
   */
  protected def unaryOp(f: (Any) => Unit) {
    for (t <- NumType.values) {
      val vals = valueList(t)
      for (x <- vals) f(x) // apply each value x to the function
    }
  }

  /**
   * <!-- binaryOp --> Apply pairs of valueList values to a binary operation.  Each f invocation with a unique x,y pair constitutes a test case.
   * Employs the Visitor pattern.
   * @param f The function applied to each value permutation.
   */
  protected def binaryOp(f: (Any, Any) => Unit) {
    for ( // for each permutation of 2 numeric types (100)
      t <- NumType.values;
      u <- NumType.values
    ) {
      val arrayT = valueList(t)
      val arrayU = valueList(u)
      for ( // for each permutation of 2 typed (array member) values (~100)
        x <- arrayT;
        y <- arrayU
      ) f(x, y) // test the function (one of 16 binary ops)
    }
  }

  /**
   * <!-- toTypeOf --> Convert a NumVal to a given numeric type.
   * @param n The input NumVal.
   * @param typeVal Value of the desired numeric type.
   * @return The converted result.
   */
  protected def toTypeOf(n: NumVal, typeVal: Any) =
    typeVal match {
      case t: Byte => n.toByte
      case t: Char => n.toInt.toChar
      case t: Short => n.toShort
      case t: Int => n.toInt
      case t: Long => n.toLong
      case t: Float => n.toFloat
      case t: Double => n.toDouble
      case t: BigInt => n.toBigInt
      case t: BigDecimal => n.toBigDecimal
    }

  /**
   * <!-- className --> Convenience method to obtain the unqualified class name.
   * @param x The object for which to obtain the class name.
   * @return the class name string.
   */
  protected def className(x: Any) = { // the concrete class name
    val sa = x.getClass.getName.split('.')
    sa(sa.length - 1)
  }

  /**
   * <!-- toTypeString --> Create a string of the form <type>(<value>).
   * @param x The object for which to obtain the type and value.
   * @return the type and value string.
   */
  protected def toTypeString(x: Any) =
    className(x) + "(" +
      (x match { case t: Char => t.toInt.toString case _ => x.toString }) + ")"

  /**
   * <!-- bool2Int --> Convert a Boolean value to Int.
   * @param b The Boolean input value.
   * @return The Int result.
   */
  protected def bool2Int(b: Boolean) = if (b) 1 else 0

  /**
   * <!-- float2Double --> Convert a Float value to Double.
   * @param b The Boolean input value.
   * @return The Int result.
   */
  protected def float2Double(f: Float) = {
    if (f.isInfinity) f.toDouble // can't convert to BigDec
    else BigDecimal(f.toString).toDouble // preserves accuracy of low order digits
  }

  /**
   * <!-- expectStr --> A better behaved "expect" method for Strings
   * @param expected The expected string
   * @param actual The actual string
   */
  protected def expectStr(expected: String, clue: String)(actual: String) {
    if (expected != actual) {
      val preface = if (clue.isEmpty) clue else clue + ".  "
      fail(preface + "Expected \"" + expected + "\", but got \"" + actual + "\"")
    }
  }

  /**
   * <!-- safeNumVal --> Convert a Float, Double or BigDecimal to NumVal.
   * @param a The input floating point value.
   * @return The NumVal value.
   */
  protected def safeNumVal(a: Any): NumVal =
    a match {
      case t: Int => NumVal(t)
      case t: Long => NumVal(t)
      case t: Float => NumVal(t)
      case t: Double => NumVal(t)
      case t: BigInt => NumVal(t)
      case t: BigDecimal => NumVal(t)
      case _ => NumVal(Double.NaN)
    }

  /**
   * <!-- safeSubInt --> Subtract 2 Ints, widening if needed to Long to avoid overflow.
   * @param x The value from which to subtract.
   * @param y The value to subtract.
   * @return The difference.
   */
  def safeSubInt(x: Int, y: Int): AnyVal = {
    val d = x - y;
    if ((Int.MinValue & (x ^ y)) != 0 && (Int.MinValue & (x ^ d)) != 0) x.longValue - y else d
  }

  /**
   * <!-- safeSubLong --> Subtract 2 Longs, widening if needed to BigInt to avoid overflow.
   * @param x The value from which to subtract.
   * @param y The value to subtract.
   * @return The difference.
   */
  def safeSubLong(x: Long, y: Long): Any =
    {
      val d = x - y;
      if ((Long.MinValue & (x ^ y)) != 0 && (Long.MinValue & (x ^ d)) != 0) BigInt(x) - y else d
    }

  /**
   * <!-- subtractInKind --> Convert and subtract a NumVal from a numeric value.
   * @param m The minuend.
   * @param n The NumVal subtrahend.
   * @return The difference.  Subtracting equal infinities gives a zero result.
   */
  protected def subtractInKind(m: Any, n: NumVal): NumVal =
    m match {
      case t: Byte => t - n.toByte
      case t: Char => t - n.toInt.toChar
      case t: Short => t - n.toShort
      case t: Int => safeNumVal(safeSubInt(t, n.toInt))
      case t: Long => safeNumVal(safeSubLong(t, n.toLong))
      case t: Float => if (!t.isInfinity) t - n.toFloat else if (t == n.toFloat) 0.0f else t
      case t: Double => if (!t.isInfinity) t - n.toDouble else if (t == n.toDouble) 0.0 else t
      case t: BigInt => t - n.toBigInt
      case t: BigDecimal => t - n.toBigDecimal
      case _ => Double.NaN
    }

  //*** Begin unary test functions

  /**
   * <!-- toByteFunc --> Function to test NumVal toByte method.
   * @param x The value to test toByte.
   */
  protected def toByteFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toByte.
     * @param b The Scala Byte value.
     * @param a The NumVal for which to find the Byte value.
     */
    def vfy(b: Byte, a: NumVal) {
      //      caseCount += 1
      val asByte = a.toByte
      if (b != asByte) {
        val clue = "test value: " + toTypeString(x)
        expectResult(b, clue) { asByte }
      }
    }

    // Method toByteFunc body
    (x) match {
      case (u: Boolean) =>
        val b = if (u) 1.byteValue else 0.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: Byte) =>
        val b = u; val a = NumVal(u); vfy(b, a)
      case (u: Char) =>
        val b = u.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: Short) =>
        val b = u.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: Int) =>
        val b = u.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: Long) =>
        val b = u.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: Float) =>
        val b = u.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: Double) =>
        val b = u.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: BigInt) =>
        val b = u.byteValue; val a = NumVal(u); vfy(b, a)
      case (u: BigDecimal) => val b = u.byteValue; val a = NumVal(u); vfy(b, a)
    }
  }

  /**
   * <!-- toShortFunc --> Function to test NumVal toShort method.
   * @param x The value to test toShort.
   */
  protected def toShortFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toShort.
     * @param s The Scala Short value.
     * @param a The NumVal for which to find the Short value.
     */
    def vfy(s: Short, a: NumVal) {
      //      caseCount += 1
      val asShort = a.toShort
      if (s != asShort) {
        val clue = "test value: " + toTypeString(x)
        expectResult(s, clue) { asShort }
      }
    }

    // Method toShortFunc body
    (x) match {
      case (u: Boolean) =>
        val s = if (u) 1.shortValue else 0.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: Byte) =>
        val s = u.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: Char) =>
        val s = u.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: Short) =>
        val s = u; val a = NumVal(u); vfy(s, a)
      case (u: Int) =>
        val s = u.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: Long) =>
        val s = u.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: Float) =>
        val s = u.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: Double) =>
        val s = u.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: BigInt) =>
        val s = u.shortValue; val a = NumVal(u); vfy(s, a)
      case (u: BigDecimal) => val s = u.shortValue; val a = NumVal(u); vfy(s, a)
    }
  }

  /**
   * <!-- toIntFunc --> Function to test NumVal toInt method.
   * @param x The value to test toInt.
   */
  protected def toIntFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toInt.
     * @param i The Scala Int value.
     * @param a The NumVal for which to find the Int value.
     */
    def vfy(i: Int, a: NumVal) {
      //      caseCount += 1
      val asInt = a.toInt
      if (i != asInt) {
        val clue = "test value: " + toTypeString(x)
        expectResult(i, clue) { asInt }
      }
    }

    // Method toShortFunc body
    (x) match {
      case (u: Boolean) =>
        val i = bool2Int(u); val a = NumVal(u); vfy(i, a)
      case (u: Byte) =>
        val i = u.intValue; val a = NumVal(u); vfy(i, a)
      case (u: Char) =>
        val i = u.intValue; val a = NumVal(u); vfy(i, a)
      case (u: Short) =>
        val i = u.intValue; val a = NumVal(u); vfy(i, a)
      case (u: Int) =>
        val i = u; val a = NumVal(u); vfy(i, a)
      case (u: Long) =>
        val i = u.intValue; val a = NumVal(u); vfy(i, a)
      case (u: Float) =>
        val i = u.intValue; val a = NumVal(u); vfy(i, a)
      case (u: Double) =>
        val i = u.intValue; val a = NumVal(u); vfy(i, a)
      case (u: BigInt) =>
        val i = u.intValue; val a = NumVal(u); vfy(i, a)
      case (u: BigDecimal) => val i = u.intValue; val a = NumVal(u); vfy(i, a)
    }
  }

  /**
   * <!-- toLongFunc --> Function to test NumVal toLong method.
   * @param x The value to test toLong.
   */
  protected def toLongFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toLong.
     * @param l The Scala Long value.
     * @param a The NumVal for which to find the Long value.
     */
    def vfy(l: Long, a: NumVal) {
      //      caseCount += 1
      val asLong = a.toLong
      if (l != asLong) {
        val clue = "test value: " + toTypeString(x)
        expectResult(l, clue) { asLong }
      }
    }

    // Method toLongFunc body
    (x) match {
      case (u: Boolean) =>
        val l = if (u) 1l else 0l; val a = NumVal(u); vfy(l, a)
      case (u: Byte) =>
        val l = u.longValue; val a = NumVal(u); vfy(l, a)
      case (u: Char) =>
        val l = u.longValue; val a = NumVal(u); vfy(l, a)
      case (u: Short) =>
        val l = u.longValue; val a = NumVal(u); vfy(l, a)
      case (u: Int) =>
        val l = u.longValue; val a = NumVal(u); vfy(l, a)
      case (u: Long) =>
        val l = u; val a = NumVal(u); vfy(l, a)
      case (u: Float) =>
        val l = u.longValue; val a = NumVal(u); vfy(l, a)
      case (u: Double) =>
        val l = u.longValue; val a = NumVal(u); vfy(l, a)
      case (u: BigInt) =>
        val l = u.longValue; val a = NumVal(u); vfy(l, a)
      case (u: BigDecimal) => val l = u.longValue; val a = NumVal(u); vfy(l, a)
    }
  }

  /**
   * <!-- toFloatFunc --> Function to test NumVal toFloat method.
   * @param x The value to test toFloat.
   */
  protected def toFloatFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toFloat.
     * @param f The Scala Float value.
     * @param a The NumVal for which to find the Float value.
     */
    def vfy(f: Float, a: NumVal) {
      //      caseCount += 1
      val asFloat = a.toFloat
      if (f != asFloat) {
        val clue = "test value: " + toTypeString(x)
        expectResult(f, clue) { asFloat }
      }
    }

    // Method toFloatFunc body
    (x) match {
      case (u: Boolean) =>
        val f = if (u) 1f else 0f; val a = NumVal(u); vfy(f, a)
      case (u: Byte) =>
        val f = u.floatValue; val a = NumVal(u); vfy(f, a)
      case (u: Char) =>
        val f = u.floatValue; val a = NumVal(u); vfy(f, a)
      case (u: Short) =>
        val f = u.floatValue; val a = NumVal(u); vfy(f, a)
      case (u: Int) =>
        val f = u.floatValue; val a = NumVal(u); vfy(f, a)
      case (u: Long) =>
        val f = u.floatValue; val a = NumVal(u); vfy(f, a)
      case (u: Float) =>
        val f = u; val a = NumVal(u); vfy(f, a)
      case (u: Double) =>
        val f = u.floatValue; val a = NumVal(u); vfy(f, a)
      case (u: BigInt) =>
        val f = u.floatValue; val a = NumVal(u); vfy(f, a)
      case (u: BigDecimal) => val f = u.floatValue; val a = NumVal(u); vfy(f, a)
    }
  }

  /**
   * <!-- toDoubleFunc --> Function to test NumVal toDouble method.
   * @param x The value to test toDouble.
   */
  protected def toDoubleFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toDouble.
     * @param d The Scala Double value.
     * @param a The NumVal for which to find the Double value.
     */
    def vfy(d: Double, a: NumVal) {
      //      caseCount += 1
      val asDouble = a.toDouble
      if (d != asDouble) {
        val clue = "test value: " + toTypeString(x)
        expectResult(d, clue) { asDouble }
      }
    }

    // Method toDoubleFunc body
    (x) match {
      case (u: Boolean) =>
        val d = if (u) 1.0 else 0.0; val a = NumVal(u); vfy(d, a)
      case (u: Byte) =>
        val d = u.doubleValue; val a = NumVal(u); vfy(d, a)
      case (u: Char) =>
        val d = u.doubleValue; val a = NumVal(u); vfy(d, a)
      case (u: Short) =>
        val d = u.doubleValue; val a = NumVal(u); vfy(d, a)
      case (u: Int) =>
        val d = u.doubleValue; val a = NumVal(u); vfy(d, a)
      case (u: Long) =>
        val d = u.doubleValue; val a = NumVal(u); vfy(d, a)
      case (u: Float) =>
        val d = if (u.isInfinity) u.toDouble else BigDecimal(u.toString).toDouble; val a = NumVal(u); vfy(d, a)
      case (u: Double) =>
        val d = u; val a = NumVal(u); vfy(d, a)
      case (u: BigInt) =>
        val d = u.doubleValue; val a = NumVal(u); vfy(d, a)
      case (u: BigDecimal) => val d = u.doubleValue; val a = NumVal(u); vfy(d, a)
    }
  }

  /**
   * <!-- toBigIntFunc --> Function to test NumVal toBigInt method.
   * @param x The value to test toBigInt.
   */
  protected def toBigIntFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toBigInt.
     * @param bi The Scala BigInt value.
     * @param a The NumVal for which to find the BigInt value.
     */
    def vfy(bi: BigInt, a: NumVal) {
      //      caseCount += 1
      val asBigInt = a.toBigInt
      if (bi != asBigInt) {
        val clue = "test value: " + toTypeString(x)
        expectResult(bi, clue) { asBigInt }
      }
    }

    // Method toBigIntFunc body
    (x) match {
      case (u: Boolean) =>
        val bi = if (u) BigInt(1) else BigInt(0); val a = NumVal(u); vfy(bi, a)
      case (u: Byte) =>
        val bi = BigInt(u); val a = NumVal(u); vfy(bi, a)
      case (u: Char) =>
        val bi = BigInt(u); val a = NumVal(u); vfy(bi, a)
      case (u: Short) =>
        val bi = BigInt(u); val a = NumVal(u); vfy(bi, a)
      case (u: Int) =>
        val bi = BigInt(u); val a = NumVal(u); vfy(bi, a)
      case (u: Long) =>
        val bi = BigInt(u); val a = NumVal(u); vfy(bi, a)
      case (u: Float) =>
        val bi = if (!u.isInfinity) BigDecimal(u.toString).toBigInt else if (u.isPosInfinity) BigInt(Long.MaxValue) else BigInt(Long.MinValue); val a = NumVal(u); vfy(bi, a)
      case (u: Double) =>
        val bi = if (!u.isInfinity) BigDecimal(u.toString).toBigInt else if (u.isPosInfinity) BigInt(Long.MaxValue) else BigInt(Long.MinValue); val a = NumVal(u); vfy(bi, a)
      case (u: BigInt) =>
        val bi = u; val a = NumVal(u); vfy(bi, a)
      case (u: BigDecimal) => val bi = u.toBigInt; val a = NumVal(u); vfy(bi, a)
    }
  }

  /**
   * <!-- toBigDecimalFunc --> Function to test NumVal toBigDecimal method.
   * @param x The value to test toBigDecimal.
   */
  protected def toBigDecimalFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toBigDecimal.
     * @param bd The Scala BigDecimal value.
     * @param a The NumVal for which to find the BigDecimal value.
     */
    def vfy(bd: BigDecimal, a: NumVal) {
      //      caseCount += 1
      val asBigDecimal = a.toBigDecimal
      if (bd != asBigDecimal) {
        val clue = "test value: " + toTypeString(x)
        expectResult(bd, clue) { asBigDecimal }
      }
    }

    // Method toBigDecimalFunc body
    (x) match {
      case (u: Boolean) =>
        val bd = if (u) BigDecimal(1) else BigDecimal(0); val a = NumVal(u); vfy(bd, a)
      case (u: Byte) =>
        val bd = BigDecimal(u); val a = NumVal(u); vfy(bd, a)
      case (u: Char) =>
        val bd = BigDecimal(u); val a = NumVal(u); vfy(bd, a)
      case (u: Short) =>
        val bd = BigDecimal(u); val a = NumVal(u); vfy(bd, a)
      case (u: Int) =>
        val bd = BigDecimal(u); val a = NumVal(u); vfy(bd, a)
      case (u: Long) =>
        val bd = BigDecimal(u); val a = NumVal(u); vfy(bd, a)
      case (u: Float) =>
        val bd: BigDecimal = if (!u.isInfinity) BigDecimal(u.toString) else if (u.isNegInfinity) Double.MinValue else Double.MaxValue; val a = NumVal(u); vfy(bd, a)
      case (u: Double) =>
        val bd: BigDecimal = if (!u.isInfinity) BigDecimal(u) else if (u.isNegInfinity) Double.MinValue else Double.MaxValue; val a = NumVal(u); vfy(bd, a)
      case (u: BigInt) =>
        val bd = BigDecimal(u); val a = NumVal(u); vfy(bd, a)
      case (u: BigDecimal) => val bd = u; val a = NumVal(u); vfy(bd, a)
    }
  }

  /**
   * <!-- toNativeFunc --> Function to test NumVal toNative method.
   * @param x The value to test toNative.
   */
  protected def toNativeFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toNative.
     * @param original The Scala Native value.
     * @param a The NumVal for which to find the Native value.
     */
    def vfy(a: NumVal) {
      //      caseCount += 1
      val asNative = a.toNative
      val clue = "Original number not returned from NumVal."
      expectStr(toTypeString(x), clue)(toTypeString(a.toNative))
    }

    // Method toNativeFunc body
    (x) match {
      case (u: Boolean) =>
        val a = NumVal(u); vfy(a)
      case (u: Byte) =>
        val a = NumVal(u); vfy(a)
      case (u: Char) =>
        val a = NumVal(u); vfy(a)
      case (u: Short) =>
        val a = NumVal(u); vfy(a)
      case (u: Int) =>
        val a = NumVal(u); vfy(a)
      case (u: Long) =>
        val a = NumVal(u); vfy(a)
      case (u: Float) =>
        val a = NumVal(u); vfy(a)
      case (u: Double) =>
        val a = NumVal(u); vfy(a)
      case (u: BigInt) =>
        val a = NumVal(u); vfy(a)
      case (u: BigDecimal) => val a = NumVal(u); vfy(a)
    }
  }

  /**
   * <!-- toTypeStringFunc --> Function to test NumVal toTypeString method.
   * @param x The value to test toTypeString.
   */
  protected def toTypeStringFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toTypeString.
     * @param s The Scala numeric type-value string.
     * @param a The NumVal for which to find the type-value string.
     */
    def vfy(s: String, a: NumVal) {
      val eStr = s.replace("(", "Val(")
      val aStr = a.toTypeString
      expectStr(eStr, "") { aStr }
    }

    // Method toTypeStringFunc body
    (x) match {
      case (u: Boolean) =>
        val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
      case (u: Byte) =>
        val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
      case (u: Char) =>
        val s = toTypeString(u).replace("Character", "Char"); val a = NumVal(u); vfy(s, a)
      case (u: Short) =>
        val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
      case (u: Int) =>
        val s = toTypeString(u).replace("Integer", "Int"); val a = NumVal(u); vfy(s, a)
      case (u: Long) =>
        val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
      case (u: Float) =>
        val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
      case (u: Double) =>
        val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
      case (u: BigInt) =>
        val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
      case (u: BigDecimal) => val s = toTypeString(u); val a = NumVal(u); vfy(s, a)
    }
  }

  /**
   * <!-- toStringFunc --> Function to test NumVal toString method.
   * @param x The value to test toString.
   */
  protected def toStringFunc(x: Any) {

    /**
     * <!-- vfy --> Verify toString.
     * @param a The NumVal for which to find the value string.
     */
    def vfy(a: NumVal) {
      //      caseCount += 1
      val s = x match { case t: Char => t.toInt.toString case _ => x.toString }
      val aStr = a.toString
      if (s != aStr) {
        val clue = "test value: " + toTypeString(x)
        expectStr(s, clue) { aStr }
      }
    }

    // Method toStringFunc body
    (x) match {
      case (u: Boolean) =>
        val a = NumVal(u); vfy(a)
      case (u: Byte) =>
        val a = NumVal(u); vfy(a)
      case (u: Char) =>
        val a = NumVal(u); vfy(a)
      case (u: Short) =>
        val a = NumVal(u); vfy(a)
      case (u: Int) =>
        val a = NumVal(u); vfy(a)
      case (u: Long) =>
        val a = NumVal(u); vfy(a)
      case (u: Float) =>
        val a = NumVal(u); vfy(a)
      case (u: Double) =>
        val a = NumVal(u); vfy(a)
      case (u: BigInt) =>
        val a = NumVal(u); vfy(a)
      case (u: BigDecimal) => val a = NumVal(u); vfy(a)
    }
  }

  /**
   * <!-- hashCodeFunc --> Function to test NumVal hashCode method.
   * @param x The value to test hashCode.
   */
  protected def hashCodeFunc(x: Any) {

    /**
     * <!-- vfy --> Verify hashCode.
     * @param a The NumVal for which to find the hash code.
     */
    def vfy(a: NumVal) {
      //      caseCount += 1
      val expected = x.hashCode
      val actual = a.hashCode
      if (expected != actual) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method hashCodeFunc body
    (x) match {
      case (u: Boolean) =>
        val a = NumVal(u); vfy(a)
      case (u: Byte) =>
        val a = NumVal(u); vfy(a)
      case (u: Char) =>
        val a = NumVal(u); vfy(a)
      case (u: Short) =>
        val a = NumVal(u); vfy(a)
      case (u: Int) =>
        val a = NumVal(u); vfy(a)
      case (u: Long) =>
        val a = NumVal(u); vfy(a)
      case (u: Float) =>
        val a = NumVal(u); vfy(a)
      case (u: Double) =>
        val a = NumVal(u); vfy(a)
      case (u: BigInt) =>
        val a = NumVal(u); vfy(a)
      case (u: BigDecimal) => val a = NumVal(u); vfy(a)
    }
  }

  /**
   * <!-- izNaNFunc --> Function to test NumVal isNaN method.
   * @param x The value to test isNaN.
   */
  protected def izNaNFunc(x: Any) {

    /**
     * <!-- vfy --> Verify isNaN.
     * @param expected Whether NaN is expected.
     * @param a The NumVal for which to find isNaN.
     */
    def vfy(expected: Boolean, a: NumVal) {
      //      caseCount += 1
      val actual = a.isNaN
      if (expected != actual) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method izNaNFunc body
    (x) match {
      case (u: Boolean) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Byte) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Char) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Short) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Int) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Long) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Float) =>
        val expect = u.isNaN; val a = NumVal(u); vfy(expect, a)
      case (u: Double) =>
        val expect = u.isNaN; val a = NumVal(u); vfy(expect, a)
      case (u: BigInt) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: BigDecimal) => val expect = false; val a = NumVal(u); vfy(expect, a)
    }
  }

  /**
   * <!-- izInfinityFunc --> Function to test NumVal isInfinity method.
   * @param x The value to test isInfinity.
   */
  protected def izInfinityFunc(x: Any) {

    /**
     * <!-- vfy --> Verify isInfinity.
     * @param expected Whether positive or negative Infinity is expected.
     * @param a The NumVal for which to find isInfinity.
     */
    def vfy(expected: Boolean, a: NumVal) {
      //      caseCount += 1
      val actual = a.isInfinity
      if (expected != actual) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method izInfinityFunc body
    (x) match {
      case (u: Boolean) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Byte) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Char) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Short) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Int) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Long) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Float) =>
        val expect = u.isInfinity; val a = NumVal(u); vfy(expect, a)
      case (u: Double) =>
        val expect = u.isInfinity; val a = NumVal(u); vfy(expect, a)
      case (u: BigInt) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: BigDecimal) => val expect = false; val a = NumVal(u); vfy(expect, a)
    }
  }

  /**
   * <!-- izPosInfinityFunc --> Function to test NumVal isPosInfinity method.
   * @param x The value to test isPosInfinity.
   */
  protected def izPosInfinityFunc(x: Any) {

    /**
     * <!-- vfy --> Verify isPosInfinity.
     * @param expected Whether positive Infinity is expected.
     * @param a The NumVal for which to find isPosInfinity.
     */
    def vfy(expected: Boolean, a: NumVal) {
      //      caseCount += 1
      val actual = a.isPosInfinity
      if (expected != actual) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method izPosInfinityFunc body
    (x) match {
      case (u: Boolean) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Byte) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Char) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Short) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Int) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Long) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Float) =>
        val expect = u.isPosInfinity; val a = NumVal(u); vfy(expect, a)
      case (u: Double) =>
        val expect = u.isPosInfinity; val a = NumVal(u); vfy(expect, a)
      case (u: BigInt) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: BigDecimal) => val expect = false; val a = NumVal(u); vfy(expect, a)
    }
  }

  /**
   * <!-- izNegInfinityFunc --> Function to test NumVal isNegInfinity method.
   * @param x The value to test isNegInfinity.
   */
  protected def izNegInfinityFunc(x: Any) {

    /**
     * <!-- vfy --> Verify isNegInfinity.
     * @param expected Whether negative Infinity is expected.
     * @param a The NumVal for which to find isNegInfinity.
     */
    def vfy(expected: Boolean, a: NumVal) {
      //      caseCount += 1
      val actual = a.isNegInfinity
      if (expected != actual) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method izNegInfinityFunc body
    (x) match {
      case (u: Boolean) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Byte) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Char) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Short) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Int) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Long) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: Float) =>
        val expect = u.isNegInfinity; val a = NumVal(u); vfy(expect, a)
      case (u: Double) =>
        val expect = u.isNegInfinity; val a = NumVal(u); vfy(expect, a)
      case (u: BigInt) =>
        val expect = false; val a = NumVal(u); vfy(expect, a)
      case (u: BigDecimal) => val expect = false; val a = NumVal(u); vfy(expect, a)
    }
  }

  /**
   * <!-- intPartFunc --> Function to test NumVal intPart method.
   * @param x The value to test intPart.
   */
  protected def intPartFunc(x: Any) {

    /**
     * <!-- vfy --> Verify intPart.
     * @param expected The integer part of x.
     * @param a The NumVal for which to find the int part.
     */
    def vfy(expected: BigInt, a: NumVal) {
      //      caseCount += 1
      val actual = a.intPart.toBigInt
      if (expected != actual) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method intPartFunc body
    (x) match {
      case (u: Boolean) =>
        val expect = if (u) BigInt(1) else BigInt(0); val a = NumVal(u); vfy(expect, a)
      case (u: Byte) =>
        val expect = BigInt(u); val a = NumVal(u); vfy(expect, a)
      case (u: Char) =>
        val expect = BigInt(u); val a = NumVal(u); vfy(expect, a)
      case (u: Short) =>
        val expect = BigInt(u); val a = NumVal(u); vfy(expect, a)
      case (u: Int) =>
        val expect = BigInt(u); val a = NumVal(u); vfy(expect, a)
      case (u: Long) =>
        val expect = BigInt(u); val a = NumVal(u); vfy(expect, a)
      case (u: Float) =>
        val expect = if (!u.isInfinity) BigDecimal(x.toString).toBigInt else if (u.isNegInfinity) BigInt(Long.MinValue) else BigInt(Long.MaxValue); val a = NumVal(u); vfy(expect, a)
      case (u: Double) =>
        val expect = if (!u.isInfinity) BigDecimal(x.toString).toBigInt else if (u.isNegInfinity) BigInt(Long.MinValue) else BigInt(Long.MaxValue); val a = NumVal(u); vfy(expect, a)
      case (u: BigInt) =>
        val expect = u; val a = NumVal(u); vfy(expect, a)
      case (u: BigDecimal) => val expect = u.toBigInt; val a = NumVal(u); vfy(expect, a)
    }
  }

  /**
   * <!-- fracPartFunc --> Function to test NumVal fracPart method.
   * @param x The value to test fracPart.
   */
  protected def fracPartFunc(x: Any) {

    /**
     * <!-- vfy --> Verify fracPart.
     * @param expected The fractional part of x.
     * @param a The NumVal for which to find the frac part.
     */
    def vfy(expected: BigDecimal, a: NumVal) {
      //      caseCount += 1
      val actual = a.fracPart.toBigDecimal
      if (expected != actual) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method fracPartFunc body
    (x) match {
      case (u: Boolean) =>
        val expect = BigDecimal(0); val a = NumVal(u); vfy(expect, a)
      case (u: Byte) =>
        val expect = BigDecimal(0); val a = NumVal(u); vfy(expect, a)
      case (u: Char) =>
        val expect = BigDecimal(0); val a = NumVal(u); vfy(expect, a)
      case (u: Short) =>
        val expect = BigDecimal(0); val a = NumVal(u); vfy(expect, a)
      case (u: Int) =>
        val expect = BigDecimal(0); val a = NumVal(u); vfy(expect, a)
      case (u: Long) =>
        val expect = BigDecimal(0); val a = NumVal(u); vfy(expect, a)
      case (u: Float) =>
        val expect = if (u.isPosInfinity || u.isNegInfinity) BigDecimal(0) else { val bd = BigDecimal(u.toString); (bd - BigDecimal(bd.toBigInt)) }; val a = NumVal(u); vfy(expect, a)
      case (u: Double) =>
        val expect = if (u.isPosInfinity || u.isNegInfinity) BigDecimal(0) else { val bd = BigDecimal(u); (bd - BigDecimal(bd.toBigInt)) }; val a = NumVal(u); vfy(expect, a)
      case (u: BigInt) =>
        val expect = BigDecimal(0); val a = NumVal(u); vfy(expect, a)
      case (u: BigDecimal) => val expect = u - BigDecimal(u.toBigInt); val a = NumVal(u); vfy(expect, a)
    }
  }

  /**
   * <!-- abvFunc --> Function to test NumVal abv (absolute value).
   * @param x The value to test abv.
   */
  protected def abvFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the absolute value.
     * @param expected The absolute value of the Scala numeric type.
     * @param a The NumVal for which to find the absolute value.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val actual = a.abv
      if (actual != expected) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method abvFunc body
    (x) match {
      case (u: Boolean) =>
        val abs = bool2Int(u).abs; val a = NumVal(u); vfy(abs, a)
      case (u: Byte) =>
        val abs = u.shortValue.abs; val a = NumVal(u); vfy(abs, a)
      case (u: Char) =>
        val abs = u.abs; val a = NumVal(u); vfy(abs, a)
      case (u: Short) =>
        val abs = u.intValue.abs; val a = NumVal(u); vfy(abs, a)
      case (u: Int) =>
        val abs = u.longValue.abs; val a = NumVal(u); vfy(abs, a)
      case (u: Long) =>
        val abs = BigInt(u).abs; val a = NumVal(u); vfy(abs, a)
      case (u: Float) =>
        val abs = u.abs; val a = NumVal(u); vfy(abs, a)
      case (u: Double) =>
        val abs = u.abs; val a = NumVal(u); vfy(abs, a)
      case (u: BigInt) =>
        val abs = u.abs; val a = NumVal(u); vfy(abs, a)
      case (u: BigDecimal) => val abs = u.abs; val a = NumVal(u); vfy(abs, a)
    }
  }

  /**
   * <!-- signumFunc --> Function to test NumVal signum (the sign as [-1,0,1]).
   * @param x The value to test signum.
   */
  protected def signumFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the sign value.
     * @param expected The sign value of the Scala numeric type.
     * @param a The NumVal for which to find the sign value.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val actual = a.sign
      if (actual != expected) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method signumFunc body
    (x) match {
      case (u: Boolean) =>
        val sign = bool2Int(u).signum; val a = NumVal(u); vfy(sign, a)
      case (u: Byte) =>
        val sign = u.shortValue.signum; val a = NumVal(u); vfy(sign, a)
      case (u: Char) =>
        val sign = u.signum; val a = NumVal(u); vfy(sign, a)
      case (u: Short) =>
        val sign = u.intValue.signum; val a = NumVal(u); vfy(sign, a)
      case (u: Int) =>
        val sign = u.longValue.signum; val a = NumVal(u); vfy(sign, a)
      case (u: Long) =>
        val sign = BigInt(u).signum; val a = NumVal(u); vfy(sign, a)
      case (u: Float) =>
        val sign = u.signum; val a = NumVal(u); vfy(sign, a)
      case (u: Double) =>
        val sign = u.signum; val a = NumVal(u); vfy(sign, a)
      case (u: BigInt) =>
        val sign = u.signum; val a = NumVal(u); vfy(sign, a)
      case (u: BigDecimal) => val sign = u.signum; val a = NumVal(u); vfy(sign, a)
    }
  }

  /**
   * <!-- roundFunc --> Function to test NumVal round.
   * @param x The value to test round.
   */
  protected def roundFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the round value.
     * @param expected The round value of the Scala numeric type.
     * @param a The NumVal for which to find the round value.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val actual = a.round
      if (actual != expected) {
        val clue = "test value: " + toTypeString(x) + ", expected x.round: " + toTypeString(expected) + ", but got a.round: " + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method roundFunc body
    (x) match {
      case (u: Boolean) =>
        val rnd = bool2Int(u); val a = NumVal(u); vfy(rnd, a)
      case (u: Byte) =>
        val rnd = u; val a = NumVal(u); vfy(rnd, a)
      case (u: Char) =>
        val rnd = u; val a = NumVal(u); vfy(rnd, a)
      case (u: Short) =>
        val rnd = u; val a = NumVal(u); vfy(rnd, a)
      case (u: Int) =>
        val rnd = u; val a = NumVal(u); vfy(rnd, a)
      case (u: Long) =>
        val rnd = u; val a = NumVal(u); vfy(rnd, a)
      case (u: Float) =>
        val rnd = if (!u.isInfinity) BigDecimal(u.toString).setScale(0, BigDecimal.RoundingMode.HALF_EVEN) else u; val a = NumVal(u); vfy(rnd, a)
      case (u: Double) =>
        val rnd = if (!u.isInfinity) BigDecimal(u).setScale(0, BigDecimal.RoundingMode.HALF_EVEN) else u; val a = NumVal(u); vfy(rnd, a)
      case (u: BigInt) =>
        val rnd = u; val a = NumVal(u); vfy(rnd, a)
      case (u: BigDecimal) =>
        val rnd = u.setScale(0, BigDecimal.RoundingMode.HALF_EVEN); val a = NumVal(u); vfy(rnd, a)
    }
  }

  /**
   * <!-- log2Func --> Function to test NumVal log2 (log base 2).
   * @param x The value to test log2.
   */
  protected def log2Func(x: Any) {

    /**
     * <!-- vfy --> Verify the log base 2.
     * @param expected The log2 of the Scala numeric type.
     * @param a The NumVal for which to find the log2.
     */
    def vfy(expected: Double, isNaN: Boolean, a: NumVal) {
      //      caseCount += 1
      val actual = a.log2
      val ulp = math.ulp(expected)
      val notEq =
        if (actual.isNaN || isNaN) actual.isNaN != isNaN
        else if (a >= Long.MaxValue) math.round(expected).toInt != actual.round.toInt    // inaccurate scala.math.log for huge x
        else if (a <= Double.MinPositiveValue) false    // inaccurate scala.math.log for tiny x
        else (actual > expected + ulp || actual < expected - ulp)
      if (notEq) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    val ln2 = scala.math.log(2)
    def refLb(d:Double) = scala.math.log(d) / ln2

    // Method log2Func body
    (x) match {
      case (u: Boolean) =>
        val log = refLb(bool2Int(u)); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Byte) =>
        val log = refLb(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Char) =>
        val log = refLb(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Short) =>
        val log = refLb(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Int) =>
        val log = refLb(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Long) =>
        val log = refLb(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Float) =>
        val log = refLb(float2Double(u)); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Double) =>
        val log = refLb(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: BigInt) =>
        val log = refLb(u.toDouble); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: BigDecimal) => val log = refLb(u.toDouble); val a = NumVal(u); vfy(log, log.isNaN, a)
    }
  }

  /**
   * <!-- log10Func --> Function to test NumVal log10 (log base 10).
   * @param x The value to test log10.
   */
  protected def log10Func(x: Any) {

    /**
     * <!-- vfy --> Verify the log base 10.
     * @param expected The log10 of the Scala numeric type.
     * @param a The NumVal for which to find the log10.
     */
    def vfy(expected: Double, isNaN: Boolean, a: NumVal) {
      //      caseCount += 1
      val actual = a.log10
      val ulp = math.ulp(expected)
      val notEq =
        if (actual.isNaN || isNaN) actual.isNaN != isNaN
        else if (a > 0 && a <= Double.MinPositiveValue) math.round(expected).toInt != actual.round.toInt    // inaccurate scala.math.log for tiny x
        else (actual > expected + ulp || actual < expected - ulp)
      if (notEq) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method log10Func body
    (x) match {
      case (u: Boolean) =>
        val log = scala.math.log10(bool2Int(u)); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Byte) =>
        val log = scala.math.log10(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Char) =>
        val log = scala.math.log10(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Short) =>
        val log = scala.math.log10(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Int) =>
        val log = scala.math.log10(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Long) =>
        val log = scala.math.log10(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Float) =>
        val log = scala.math.log10(float2Double(u)); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: Double) =>
        val log = scala.math.log10(u); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: BigInt) =>
        val log = scala.math.log10(u.toDouble); val a = NumVal(u); vfy(log, log.isNaN, a)
      case (u: BigDecimal) => val log = scala.math.log10(u.toDouble); val a = NumVal(u); vfy(log, log.isNaN, a)
    }
  }

  /**
   * <!-- lnFunc --> Function to test NumVal ln (natural log).
   * @param x The value to test ln.
   */
  protected def lnFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the log base 10.
     * @param expected The ln of the Scala numeric type.
     * @param isNaN Whether the expected value is NaN.
     * @param a The NumVal for which to find the ln.
     */
    def vfy(expected: Double, isNaN: Boolean, a: NumVal) {
      //      caseCount += 1
      val actual = a.ln
      val ulp = math.ulp(expected)
      val notEq =
        if (actual.isNaN || isNaN) actual.isNaN != isNaN
        else if (a > 0 && a <= Double.MinPositiveValue) math.round(expected).toInt != actual.round.toInt    // inaccurate scala.math.log for tiny x
        else (actual > expected + ulp || actual < expected - ulp)
      if (notEq) {
        val clue = "test value: " + toTypeString(x)
        expectResult(expected, clue) { actual }
      }
    }

    // Method lnFunc body
    (x) match {
      case (u: Boolean) =>
        val ln = scala.math.log(bool2Int(u)); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: Byte) =>
        val ln = scala.math.log(u); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: Char) =>
        val ln = scala.math.log(u); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: Short) =>
        val ln = scala.math.log(u); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: Int) =>
        val ln = scala.math.log(u); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: Long) =>
        val ln = scala.math.log(u); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: Float) =>
        val ln = scala.math.log(float2Double(u)); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: Double) =>
        val ln = scala.math.log(u); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: BigInt) =>
        val ln = scala.math.log(u.toDouble); val a = NumVal(u); vfy(ln, ln.isNaN, a)
      case (u: BigDecimal) => val ln = scala.math.log(u.toDouble); val a = NumVal(u); vfy(ln, ln.isNaN, a)
    }
  }

  /**
   * <!-- expFunc --> Function to test NumVal exp (exponent of e).
   * @param x The value to test exp.
   */
  protected def expFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The exp of the Scala numeric type.
     * @param a The NumVal for which to find the exp.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(expected)
      val actual = a.exp
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method expFunc body
    (x) match {
      case (u: Boolean) =>
        val exp = scala.math.exp(bool2Int(u)); val a = NumVal(u); vfy(exp, a)
      case (u: Byte) =>
        val exp = scala.math.exp(u); val a = NumVal(u); vfy(exp, a)
      case (u: Char) =>
        val exp = scala.math.exp(u); val a = NumVal(u); vfy(exp, a)
      case (u: Short) =>
        val exp = scala.math.exp(u); val a = NumVal(u); vfy(exp, a)
      case (u: Int) =>
        val exp = scala.math.exp(u); val a = NumVal(u); vfy(exp, a)
      case (u: Long) =>
        val exp = scala.math.exp(u); val a = NumVal(u); vfy(exp, a)
      case (u: Float) =>
        val exp = scala.math.exp(float2Double(u)); val a = NumVal(u); vfy(exp, a)
      case (u: Double) =>
        val exp = scala.math.exp(u); val a = NumVal(u); vfy(exp, a)
      case (u: BigInt) =>
        val exp = scala.math.exp(u.toDouble); val a = NumVal(u); vfy(exp, a)
      case (u: BigDecimal) =>
        val exp = scala.math.exp(u.toDouble); val a = NumVal(u); vfy(exp, a)
    }
  }

  val bigPi:BigDecimal = BigDecimal("3." +
      "14159265358979323846264338327950288419716939937510" +
      "58209749445923078164062862089986280348253421170679" +
      "82148086513282306647093844609550582231725359408128" +
      "48111745028410270193852110555964462294895493038196" +
      "44288109756659334461284756482337867831652712019091" +
      "45648566923460348610454326648213393607260249141273" +
      "72458700660631558817488152092096282925409171536436" +
      "78925903600113305305488204665213841469519415116094" +
      "33057270365759591953092186117381932611793105118548" +
      "07446237996274956735188575272489122793818301194912" +
      "98336733624406566430860213949463952247371907021798" +
      "60943702770539217176293176752384674818467669405132")

  /**
   * <!-- normalizeRadians --> Reduce a BigInt angle to within the range -pi .. +pi.
   * @param x The BigInt angle to normalize, in radians.
   * @return The Double normalized result in radians.
   */
  private def normalizeRadians(x:BigInt):Double = {
    val bigX = BigDecimal(x, bigPi.mc)
    val doublePi = bigPi * 2
    val normalize = bigX > bigPi || bigX < -bigPi
    val result = if (normalize) {
      val mult = bigX / doublePi
      val rem = (mult - BigDecimal(mult.toBigInt)) * doublePi
      if (rem > bigPi) rem - doublePi else if (rem < -bigPi) rem + doublePi else rem
    } else bigX
    result.toDouble
  }

  /**
   * <!-- sinFunc --> Function to test NumVal sine.
   * @param x The value to test sine.
   */
  protected def sinFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The sine of the Scala numeric type.
     * @param a The NumVal for which to find the sine.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = 1.0e-14    // by inspection; error in java.math sin
      val actual = a.sin
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method sinFunc body
    (x) match {
      case (u: Boolean) =>
        val sin = scala.math.sin(bool2Int(u)); val a = NumVal(u); vfy(sin, a)
      case (u: Byte) =>
        val sin = scala.math.sin(u); val a = NumVal(u); vfy(sin, a)
      case (u: Char) =>
        val sin = scala.math.sin(u); val a = NumVal(u); vfy(sin, a)
      case (u: Short) =>
        val sin = scala.math.sin(u); val a = NumVal(u); vfy(sin, a)
      case (u: Int) =>
        val sin = scala.math.sin(u); val a = NumVal(u); vfy(sin, a)
      case (u: Long) =>
        val sin = scala.math.sin(u); val a = NumVal(u); vfy(sin, a)
      case (u: Float) =>
        val sin = scala.math.sin(float2Double(u)); val a = NumVal(u); vfy(sin, a)
      case (u: Double) =>
        val sin = scala.math.sin(u); val a = NumVal(u); vfy(sin, a)
      case (u: BigInt) =>
        val sin = scala.math.sin(normalizeRadians(u)); val a = NumVal(u); vfy(sin, a)
      case (u: BigDecimal) =>
        if (u < Double.MaxValue && u > Double.MinValue) {    // periodic functions on huge numbers are nonsensical
          val sin = scala.math.sin(u.toDouble); val a = NumVal(u); vfy(sin, a)
        }
    }
  }

  /**
   * <!-- cosFunc --> Function to test NumVal cosine.
   * @param x The value to test cosine.
   */
  protected def cosFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The cosine of the Scala numeric type.
     * @param a The NumVal for which to find the cosine.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = 1.0e-14    // by inspection; error in java.math cos
      val actual = a.cos
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method cosFunc body
    (x) match {
      case (u: Boolean) =>
        val cos = scala.math.cos(bool2Int(u)); val a = NumVal(u); vfy(cos, a)
      case (u: Byte) =>
        val cos = scala.math.cos(u); val a = NumVal(u); vfy(cos, a)
      case (u: Char) =>
        val cos = scala.math.cos(u); val a = NumVal(u); vfy(cos, a)
      case (u: Short) =>
        val cos = scala.math.cos(u); val a = NumVal(u); vfy(cos, a)
      case (u: Int) =>
        val cos = scala.math.cos(u); val a = NumVal(u); vfy(cos, a)
      case (u: Long) =>
        val cos = scala.math.cos(u); val a = NumVal(u); vfy(cos, a)
      case (u: Float) =>
        val cos = scala.math.cos(float2Double(u)); val a = NumVal(u); vfy(cos, a)
      case (u: Double) =>
        val cos = scala.math.cos(u); val a = NumVal(u); vfy(cos, a)
      case (u: BigInt) =>
        val cos = scala.math.cos(normalizeRadians(u)); val a = NumVal(u); vfy(cos, a)
      case (u: BigDecimal) =>
        if (u < Double.MaxValue && u > Double.MinValue) {    // periodic functions on huge numbers are nonsensical
          val cos = scala.math.cos(u.toDouble); val a = NumVal(u); vfy(cos, a)
        }
    }
  }

  /**
   * <!-- tanFunc --> Function to test NumVal tangent.
   * @param x The value to test tangent.
   */
  protected def tanFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The tangent of the Scala numeric type.
     * @param a The NumVal for which to find the tangent.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = 1.0e-11    // by inspection; error in java.math tan
      val actual = a.tan
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method tanFunc body
    (x) match {
      case (u: Boolean) =>
        val tan = scala.math.tan(bool2Int(u)); val a = NumVal(u); vfy(tan, a)
      case (u: Byte) =>
        val tan = scala.math.tan(u); val a = NumVal(u); vfy(tan, a)
      case (u: Char) =>
        val tan = scala.math.tan(u); val a = NumVal(u); vfy(tan, a)
      case (u: Short) =>
        val tan = scala.math.tan(u); val a = NumVal(u); vfy(tan, a)
      case (u: Int) =>
        val tan = scala.math.tan(u); val a = NumVal(u); vfy(tan, a)
      case (u: Long) =>
        val tan = scala.math.tan(u); val a = NumVal(u); vfy(tan, a)
      case (u: Float) =>
        val tan = scala.math.tan(float2Double(u)); val a = NumVal(u); vfy(tan, a)
      case (u: Double) =>
        val tan = scala.math.tan(u); val a = NumVal(u); vfy(tan, a)
      case (u: BigInt) =>
        val tan = scala.math.tan(normalizeRadians(u)); val a = NumVal(u); vfy(tan, a)
      case (u: BigDecimal) =>
        if (u < Double.MaxValue && u > Double.MinValue) {    // periodic functions on huge numbers are nonsensical
          val tan = scala.math.tan(u.toDouble); val a = NumVal(u); vfy(tan, a)
        }
    }
  }

  /**
   * <!-- asinFunc --> Function to test NumVal arc sine.
   * @param x The value to test arc sine.
   */
  protected def asinFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The arc sine of the Scala numeric type.
     * @param a The NumVal for which to find the arc sine.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(expected)
      val actual = a.asin
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method asinFunc body
    (x) match {
      case (u: Boolean) =>
        val asin = scala.math.asin(bool2Int(u)); val a = NumVal(u); vfy(asin, a)
      case (u: Byte) =>
        val asin = scala.math.asin(u); val a = NumVal(u); vfy(asin, a)
      case (u: Char) =>
        val asin = scala.math.asin(u); val a = NumVal(u); vfy(asin, a)
      case (u: Short) =>
        val asin = scala.math.asin(u); val a = NumVal(u); vfy(asin, a)
      case (u: Int) =>
        val asin = scala.math.asin(u); val a = NumVal(u); vfy(asin, a)
      case (u: Long) =>
        val asin = scala.math.asin(u); val a = NumVal(u); vfy(asin, a)
      case (u: Float) =>
        val asin = scala.math.asin(float2Double(u)); val a = NumVal(u); vfy(asin, a)
      case (u: Double) =>
        val asin = scala.math.asin(u); val a = NumVal(u); vfy(asin, a)
      case (u: BigInt) =>
        val asin = scala.math.asin(u.toDouble); val a = NumVal(u); vfy(asin, a)
      case (u: BigDecimal) =>
        val asin = scala.math.asin(u.toDouble); val a = NumVal(u); vfy(asin, a)
    }
  }

  /**
   * <!-- acosFunc --> Function to test NumVal arc cosine.
   * @param x The value to test arc cosine.
   */
  protected def acosFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The arc cosine of the Scala numeric type.
     * @param a The NumVal for which to find the arc cosine.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(expected)
      val actual = a.acos
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expectedErr:" + expectedErr + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method acosFunc body
    (x) match {
      case (u: Boolean) =>
        val acos = scala.math.acos(bool2Int(u)); val a = NumVal(u); vfy(acos, a)
      case (u: Byte) =>
        val acos = scala.math.acos(u); val a = NumVal(u); vfy(acos, a)
      case (u: Char) =>
        val acos = scala.math.acos(u); val a = NumVal(u); vfy(acos, a)
      case (u: Short) =>
        val acos = scala.math.acos(u); val a = NumVal(u); vfy(acos, a)
      case (u: Int) =>
        val acos = scala.math.acos(u); val a = NumVal(u); vfy(acos, a)
      case (u: Long) =>
        val acos = scala.math.acos(u); val a = NumVal(u); vfy(acos, a)
      case (u: Float) =>
        val acos = scala.math.acos(float2Double(u)); val a = NumVal(u); vfy(acos, a)
      case (u: Double) =>
        val acos = scala.math.acos(u); val a = NumVal(u); vfy(acos, a)
      case (u: BigInt) =>
        val acos = scala.math.acos(u.toDouble); val a = NumVal(u); vfy(acos, a)
      case (u: BigDecimal) =>
        val acos = scala.math.acos(u.toDouble); val a = NumVal(u); vfy(acos, a)
    }
  }

  /**
   * <!-- atanFunc --> Function to test NumVal arc tangent.
   * @param x The value to test arc tangent.
   */
  protected def atanFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The arc tangent of the Scala numeric type.
     * @param a The NumVal for which to find the arc tangent.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(expected)
      val actual = a.atan
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method atanFunc body
    (x) match {
      case (u: Boolean) =>
        val atan = scala.math.atan(bool2Int(u)); val a = NumVal(u); vfy(atan, a)
      case (u: Byte) =>
        val atan = scala.math.atan(u); val a = NumVal(u); vfy(atan, a)
      case (u: Char) =>
        val atan = scala.math.atan(u); val a = NumVal(u); vfy(atan, a)
      case (u: Short) =>
        val atan = scala.math.atan(u); val a = NumVal(u); vfy(atan, a)
      case (u: Int) =>
        val atan = scala.math.atan(u); val a = NumVal(u); vfy(atan, a)
      case (u: Long) =>
        val atan = scala.math.atan(u); val a = NumVal(u); vfy(atan, a)
      case (u: Float) =>
        val atan = scala.math.atan(float2Double(u)); val a = NumVal(u); vfy(atan, a)
      case (u: Double) =>
        val atan = scala.math.atan(u); val a = NumVal(u); vfy(atan, a)
      case (u: BigInt) =>
        val atan = scala.math.atan(u.toDouble); val a = NumVal(u); vfy(atan, a)
      case (u: BigDecimal) =>
        val atan = scala.math.atan(u.toDouble); val a = NumVal(u); vfy(atan, a)
    }
  }

  /**
   * <!-- ulpFunc --> Function to test NumVal ulp (Unit in the Last Place).
   * @param x The value to test ulp.
   */
  protected def ulpFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The ulp of the Scala numeric type.
     * @param a The NumVal for which to find the ulp.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(expected)
      val actual = a.ulp
      val neq = actual.isNaN != isNaN(expected)
      if (neq || subtractInKind(expected, actual).abv > expectedErr) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method ulpFunc body
    (x) match {
      case (u: Boolean) =>
        val ulp = 1; val a = NumVal(u); vfy(ulp, a)
      case (u: Byte) =>
        val ulp = 1; val a = NumVal(u); vfy(ulp, a)
      case (u: Char) =>
        val ulp = 1; val a = NumVal(u); vfy(ulp, a)
      case (u: Short) =>
        val ulp = 1; val a = NumVal(u); vfy(ulp, a)
      case (u: Int) =>
        val ulp = 1; val a = NumVal(u); vfy(ulp, a)
      case (u: Long) =>
        val ulp = 1; val a = NumVal(u); vfy(ulp, a)
      case (u: Float) =>
        val ulp = scala.math.ulp(u); val a = NumVal(u); vfy(ulp, a)
      case (u: Double) =>
        val ulp = scala.math.ulp(u); val a = NumVal(u); vfy(ulp, a)
      case (u: BigInt) =>
        val ulp = 1; val a = NumVal(u); vfy(ulp, a)
      case (u: BigDecimal) => val ulp = u.ulp; val a = NumVal(u); vfy(ulp, a)
    }
  }

  /**
   * <!-- unaryPlusFunc --> Function to test NumVal unary + (no-op).
   * @param x The value to test unary +.
   */
  protected def unaryPlusFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The unaryPlus of the Scala numeric type.
     * @param a The NumVal for which to find the unaryPlus.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val actual = +a
      if (a != expected) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method unaryPlusFunc body
    (x) match {
      case (u: Boolean) =>
        val plus = bool2Int(u); val a = NumVal(u); vfy(plus, a)
      case (u: Byte) =>
        val plus = +u; val a = NumVal(u); vfy(plus, a)
      case (u: Char) =>
        val plus = +u; val a = NumVal(u); vfy(plus, a)
      case (u: Short) =>
        val plus = +u; val a = NumVal(u); vfy(plus, a)
      case (u: Int) =>
        val plus = +u; val a = NumVal(u); vfy(plus, a)
      case (u: Long) =>
        val plus = +u; val a = NumVal(u); vfy(plus, a)
      case (u: Float) =>
        val plus = +u; val a = NumVal(u); vfy(plus, a)
      case (u: Double) =>
        val plus = +u; val a = NumVal(u); vfy(plus, a)
      case (u: BigInt) =>
        val plus = u; val a = NumVal(u); vfy(plus, a)
      case (u: BigDecimal) => val plus = u; val a = NumVal(u); vfy(plus, a)
    }
  }

  /**
   * <!-- unaryMinusFunc --> Function to test NumVal unary -.
   * @param x The value to test unary -.
   */
  protected def unaryMinusFunc(x: Any) {

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The unaryMinus of the Scala numeric type.
     * @param a The NumVal for which to find the unaryMinus.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val actual = -a
      if (a != expected) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method unaryMinusFunc body
    (x) match {
      case (u: Boolean) =>
        val minus = if (u) -1 else 0; val a = NumVal(u); vfy(minus, a)
      case (u: Byte) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: Char) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: Short) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: Int) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: Long) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: Float) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: Double) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: BigInt) =>
        val minus = -u; val a = NumVal(u); vfy(minus, a)
      case (u: BigDecimal) => val minus = -u; val a = NumVal(u); vfy(minus, a)
    }
  }

  /**
   * <!-- unaryNotFunc --> Function to test NumVal unary ~.
   * @param x The value to test unary ~.
   */
  protected def unaryNotFunc(x: Any) {

    /**
     * <!-- notIntPart --> Find the bit-wise NOT of the integer part of a floating point number.
     * @param u The input floating point number.
     * @return The bit-wise NOT result.
     */
    def notIntPart(u: Any) = {
      u match {
        case v: Float =>
          if (!v.isInfinity) ~(BigDecimal(v.toString).toBigInt)
          else if (v.isPosInfinity) BigInt(~Long.MaxValue) else BigInt(~Long.MinValue)
        case v: Double =>
          if (!v.isInfinity) ~(BigDecimal(v).toBigInt)
          else if (v.isPosInfinity) BigInt(~Long.MaxValue) else BigInt(~Long.MinValue)
        case v: BigDecimal => ~(v.toBigInt)
      }
    }

    /**
     * <!-- vfy --> Verify the natural log.
     * @param expected The unaryNot of the Scala numeric type.
     * @param a The NumVal for which to find the unary_~.
     */
    def vfy(expected: Any, a: NumVal) {
      //      caseCount += 1
      val actual = ~a
      if (a != expected) {
        val clue = "test value: " + toTypeString(x) + ", expected:" + toTypeString(expected) + ", actual:" + actual.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method unaryNotFunc body
    (x) match {
      case (u: Boolean) =>
        val not = if (u) 0 else 1; val a = NumVal(u); vfy(not, a)
      case (u: Byte) =>
        val not = ~u; val a = NumVal(u); vfy(not, a)
      case (u: Char) =>
        val not = ~u; val a = NumVal(u); vfy(not, a)
      case (u: Short) =>
        val not = ~u; val a = NumVal(u); vfy(not, a)
      case (u: Int) =>
        val not = ~u; val a = NumVal(u); vfy(not, a)
      case (u: Long) =>
        val not = ~u; val a = NumVal(u); vfy(not, a)
      case (u: Float) =>
        val not = notIntPart(u); val a = NumVal(u); vfy(not, a)
      case (u: Double) =>
        val not = notIntPart(u); val a = NumVal(u); vfy(not, a)
      case (u: BigInt) =>
        val not = ~u; val a = NumVal(u); vfy(not, a)
      case (u: BigDecimal) => val not = notIntPart(u); val a = NumVal(u); vfy(not, a)
    }
  }

  //*** Begin binary test functions

  // Useful constants
  val n0 = NumVal(0)
  val np1 = NumVal(1)
  val nm1 = NumVal(-1)

  /**
   * <!-- safeSub --> Subtract two floating point numbers, but preserve Float or Double infinity.
   * @param a The first value.
   * @param b The second value.
   * @return The sum as Float, Double or BigDecimal.  Subtracting opposite-signed infinities gives a zero Float or Double result.
   */
  protected def safeSub(a: Any, b: Any): Any =
    (a, b) match {
      case (u: Float, v: Float) => if (!u.isInfinity) u - v else if (v.isInfinity && u == v) 0.0f else u
      case (u: Float, v: Double) => if (!u.isInfinity) u - v else if (v.isInfinity && u == v) 0.0 else u.toDouble
      case (u: Float, v: BigDecimal) => if (!u.isInfinity) BigDecimal(u.toString) - v else u
      case (u: Double, v: Float) => if (!u.isInfinity) u - v else if (v.isInfinity && u == v) 0.0 else u
      case (u: Double, v: Double) => if (!u.isInfinity) u - v else if (v.isInfinity && u == v) 0.0 else u
      case (u: Double, v: BigDecimal) => if (!u.isInfinity) u - v else u
      case (u: BigDecimal, v: Float) => if (!v.isInfinity) u - v else -v
      case (u: BigDecimal, v: Double) => if (!v.isInfinity) u - v else -v
      case (u: BigDecimal, v: BigDecimal) => u - v
      case _ => Double.NaN
    }

  /**
   * <!-- xerr --> Give the expected error for a floating point value.
   * @param a The input value.
   * @return The expected error.
   */
  protected def xerr(a: Any): NumVal = // TODO: add "expectedError" to NumVal?
    a match {
      case f: Float =>
        val err = math.ulp(f); if (err.isInfinity) 0 else err
      case d: Double =>
        val err = math.ulp(d); if (err.isInfinity) 0 else err
      case bd: BigDecimal => bd.ulp
      case fv: FloatVal =>
        val err = fv.ulp; if (err.isInfinity) 0 else err
      case dv: DoubleVal =>
        val err = dv.ulp; if (err.isInfinity) 0 else err
      case bv: BigDecimalVal => bv.ulp
      case _ => 0
    }

  /**
   * <!-- isNaN --> Tell whether a value is Not a Number.
   * @param n The value to check.
   * @return Whether NaN.
   */
  protected def isNaN(n: Any) =
    n match {
      case t: Float => t.isNaN
      case t: Double => t.isNaN
      case _ => false
    }

  /**
   * <!-- minFunc --> Function to test NumVal "min" method.
   * @param x, y The values to compare.
   */
  protected def minFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify "min".
     * @param expected The expected minimum.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a min b
      if (actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method minFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val min = bool2Int(u) min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: Byte) =>
        val min = bool2Int(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: Char) =>
        val min = bool2Int(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: Short) =>
        val min = bool2Int(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: Int) =>
        val min = bool2Int(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: Long) =>
        val min = bool2Int(u).longValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: Float) =>
        val min = bool2Int(u).floatValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: Double) =>
        val min = bool2Int(u).doubleValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: BigInt) =>
        val min = BigInt(bool2Int(u)) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val min = BigDecimal(bool2Int(u)) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: Byte, v: Boolean) =>
        val min = u.intValue min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: Byte) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: Char) =>
        val min = u.intValue min v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: Short) =>
        val min = u.shortValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: Int) =>
        val min = u.intValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: Long) =>
        val min = u.longValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: Float) =>
        val min = u.floatValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: Double) =>
        val min = u.doubleValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: BigInt) =>
        val min = BigInt(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Byte, v: BigDecimal) =>
        val min = BigDecimal(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: Char, v: Boolean) =>
        val min = u.intValue min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: Byte) =>
        val min = u.intValue min v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: Char) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: Short) =>
        val min = u.intValue min v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: Int) =>
        val min = u.intValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: Long) =>
        val min = u.longValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: Float) =>
        val min = u.floatValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: Double) =>
        val min = u.doubleValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: BigInt) =>
        val min = BigInt(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Char, v: BigDecimal) =>
        val min = BigDecimal(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: Short, v: Boolean) =>
        val min = u.intValue min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: Byte) =>
        val min = u.intValue min v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: Char) =>
        val min = u.intValue min v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: Short) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: Int) =>
        val min = u.intValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: Long) =>
        val min = u.longValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: Float) =>
        val min = u.floatValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: Double) =>
        val min = u.doubleValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: BigInt) =>
        val min = BigInt(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Short, v: BigDecimal) =>
        val min = BigDecimal(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: Int, v: Boolean) =>
        val min = u min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: Byte) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: Char) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: Short) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: Int) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: Long) =>
        val min = u.longValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: Float) =>
        val min: AnyVal = if (u <= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: Double) =>
        val min = u.doubleValue min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: BigInt) =>
        val min = BigInt(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Int, v: BigDecimal) =>
        val min = BigDecimal(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: Long, v: Boolean) =>
        val min = u min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: Byte) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: Char) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: Short) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: Int) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: Long) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: Float) =>
        val min: AnyVal = if (u <= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: Double) =>
        val min: AnyVal = if (u <= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: BigInt) =>
        val min = BigInt(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Long, v: BigDecimal) =>
        val min = BigDecimal(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: Float, v: Boolean) =>
        val min = u min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: Byte) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: Char) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: Short) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: Int) =>
        val min: AnyVal = if (u <= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: Long) =>
        val min: AnyVal = if (u <= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: Float) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: Double) =>
        val min = float2Double(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: BigInt) =>
        val min = if (!u.isInfinity) BigDecimal(u.toString) min BigDecimal(v) else if (u.isNegInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Float, v: BigDecimal) =>
        val min = if (!u.isInfinity) BigDecimal(u.toString) min v else if (u.isNegInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: Double, v: Boolean) =>
        val min = u min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: Byte) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: Char) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: Short) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: Int) =>
        val min: AnyVal = if (u <= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: Long) =>
        val min: AnyVal = if (u <= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: Float) =>
        val min = u min float2Double(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: Double) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: BigInt) =>
        val min = if (!u.isInfinity) BigDecimal(u) min BigDecimal(v) else if (u.isNegInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: Double, v: BigDecimal) =>
        val min = if (!u.isInfinity) BigDecimal(u) min v else if (u.isNegInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: BigInt, v: Boolean) =>
        val min = u min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: Byte) =>
        val min = u min BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: Char) =>
        val min = u min BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: Short) =>
        val min = u min BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: Int) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: Long) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: Float) =>
        val min = if (!v.isInfinity) BigDecimal(u) min BigDecimal(v.toString) else if (v.isNegInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: Double) =>
        val min = if (!v.isInfinity) BigDecimal(u) min BigDecimal(v) else if (v.isNegInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: BigInt) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val min = BigDecimal(u) min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val min = u min bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: Byte) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: Char) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: Short) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: Int) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: Long) =>
        val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: Float) =>
        val min = if (!v.isInfinity) u min BigDecimal(v.toString) else if (v.isNegInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: Double) =>
        val min = if (!v.isInfinity) u min BigDecimal(v) else if (v.isNegInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val min = u min BigDecimal(v); val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
      case (u: BigDecimal, v: BigDecimal) => val min = u min v; val a = NumVal(u); val b = NumVal(v); vfy(min, a, b)
    }
  }

  /**
   * <!-- maxFunc --> Function to test NumVal "max" method.
   * @param x, y The values to compare.
   */
  protected def maxFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify "max".
     * @param expected The expected maximum.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a max b
      if (actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method maxFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val max = bool2Int(u) max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: Byte) =>
        val max = bool2Int(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: Char) =>
        val max = bool2Int(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: Short) =>
        val max = bool2Int(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: Int) =>
        val max = bool2Int(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: Long) =>
        val max = bool2Int(u).longValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: Float) =>
        val max = bool2Int(u).floatValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: Double) =>
        val max = bool2Int(u).doubleValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: BigInt) =>
        val max = BigInt(bool2Int(u)) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val max = BigDecimal(bool2Int(u)) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: Byte, v: Boolean) =>
        val max = u.intValue max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: Byte) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: Char) =>
        val max = u.intValue max v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: Short) =>
        val max = u.shortValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: Int) =>
        val max = u.intValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: Long) =>
        val max = u.longValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: Float) =>
        val max = u.floatValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: Double) =>
        val max = u.doubleValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: BigInt) =>
        val max = BigInt(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Byte, v: BigDecimal) =>
        val max = BigDecimal(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: Char, v: Boolean) =>
        val max = u.intValue max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: Byte) =>
        val max = u.intValue max v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: Char) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: Short) =>
        val max = u.intValue max v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: Int) =>
        val max = u.intValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: Long) =>
        val max = u.longValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: Float) =>
        val max = u.floatValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: Double) =>
        val max = u.doubleValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: BigInt) =>
        val max = BigInt(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Char, v: BigDecimal) =>
        val max = BigDecimal(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: Short, v: Boolean) =>
        val max = u.intValue max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: Byte) =>
        val max = u.intValue max v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: Char) =>
        val max = u.intValue max v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: Short) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: Int) =>
        val max = u.intValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: Long) =>
        val max = u.longValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: Float) =>
        val max = u.floatValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: Double) =>
        val max = u.doubleValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: BigInt) =>
        val max = BigInt(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Short, v: BigDecimal) =>
        val max = BigDecimal(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: Int, v: Boolean) =>
        val max = u max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: Byte) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: Char) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: Short) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: Int) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: Long) =>
        val max = u.longValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: Float) =>
        val max: AnyVal = if (u >= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: Double) =>
        val max = u.doubleValue max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: BigInt) =>
        val max = BigInt(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Int, v: BigDecimal) =>
        val max = BigDecimal(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: Long, v: Boolean) =>
        val max = u max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: Byte) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: Char) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: Short) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: Int) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: Long) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: Float) =>
        val max: AnyVal = if (u >= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: Double) =>
        val max: AnyVal = if (u >= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: BigInt) =>
        val max = BigInt(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Long, v: BigDecimal) =>
        val max = BigDecimal(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: Float, v: Boolean) =>
        val max = u max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: Byte) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: Char) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: Short) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: Int) =>
        val max: AnyVal = if (u >= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: Long) =>
        val max: AnyVal = if (u >= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: Float) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: Double) =>
        val max = float2Double(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: BigInt) =>
        val max = if (!u.isInfinity) BigDecimal(u.toString) max BigDecimal(v) else if (u.isPosInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Float, v: BigDecimal) =>
        val max = if (!u.isInfinity) BigDecimal(u.toString) max v else if (u.isPosInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: Double, v: Boolean) =>
        val max = u max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: Byte) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: Char) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: Short) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: Int) =>
        val max: AnyVal = if (u >= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: Long) =>
        val max: AnyVal = if (u >= v) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: Float) =>
        val max = u max float2Double(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: Double) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: BigInt) =>
        val max = if (!u.isInfinity) BigDecimal(u) max BigDecimal(v) else if (u.isPosInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: Double, v: BigDecimal) =>
        val max = if (!u.isInfinity) BigDecimal(u) max v else if (u.isPosInfinity) u else v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: BigInt, v: Boolean) =>
        val max = u max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: Byte) =>
        val max = u max BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: Char) =>
        val max = u max BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: Short) =>
        val max = u max BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: Int) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: Long) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: Float) =>
        val max = if (!v.isInfinity) BigDecimal(u) max BigDecimal(v.toString) else if (v.isPosInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: Double) =>
        val max = if (!v.isInfinity) BigDecimal(u) max BigDecimal(v) else if (v.isPosInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: BigInt) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val max = BigDecimal(u) max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val max = u max bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: Byte) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: Char) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: Short) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: Int) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: Long) =>
        val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: Float) =>
        val max = if (!v.isInfinity) u max BigDecimal(v.toString) else if (v.isPosInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: Double) =>
        val max = if (!v.isInfinity) u max BigDecimal(v) else if (v.isPosInfinity) v else u; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val max = u max BigDecimal(v); val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
      case (u: BigDecimal, v: BigDecimal) => val max = u max v; val a = NumVal(u); val b = NumVal(v); vfy(max, a, b)
    }
  }

  // Singleton BigInt zero and BigInt one
  object bi0 extends BigInt(java.math.BigInteger.ZERO)
  object bi1 extends BigInt(java.math.BigInteger.ONE)

  /**
   * <!-- bool2BigInt --> Convert a Boolean value to BigInt.
   * @param b The Boolean value to convert.
   * @return The BigInt result.
   */
  protected def bool2BigInt(b: Boolean) = if (b) bi1 else bi0

  /**
   * <!-- limint --> Limit an integer value to Short range.
   * @param i The Int value to limit.
   * @return The limited Int result.
   */
  protected def limint(i: Int): Int = if (i < Short.MinValue) Short.MinValue else if (i > Short.MaxValue) Short.MaxValue else i

  /**
   * <!-- shiftLeftFunc --> Function to test NumVal binary shift left.
   * @param x The value to shift
   * @param y The bits to shift.
   */
  protected def shiftLeftFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify shift left.
     * @param expected The expected shifted Scala numeric value.
     * @param a The NumVal to shift.
     * @param b The NumVal bits to shift.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a << b
      if (actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method shiftLeftFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val s = bool2BigInt(u) << limint(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Byte) =>
        val s = bool2BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Char) =>
        val s = bool2BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Short) =>
        val s = bool2BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Int) =>
        val s = bool2BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Long) =>
        val s = bool2BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Float) =>
        val s = bool2BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Double) =>
        val s = bool2BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: BigInt) =>
        val s = bool2BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val s = bool2BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Byte, v: Boolean) =>
        val s = BigInt(u) << bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Byte) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Char) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Short) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Int) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Long) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Float) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Double) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: BigInt) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: BigDecimal) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Char, v: Boolean) =>
        val s = BigInt(u) << bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Byte) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Char) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Short) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Int) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Long) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Float) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Double) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: BigInt) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: BigDecimal) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Short, v: Boolean) =>
        val s = BigInt(u) << bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Byte) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Char) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Short) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Int) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Long) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Float) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Double) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: BigInt) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: BigDecimal) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Int, v: Boolean) =>
        val s = BigInt(u) << bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Byte) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Char) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Short) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Int) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Long) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Float) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Double) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: BigInt) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: BigDecimal) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Long, v: Boolean) =>
        val s = BigInt(u) << bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Byte) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Char) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Short) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Int) =>
        val s = BigInt(u) << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Long) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Float) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Double) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: BigInt) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: BigDecimal) =>
        val s = BigInt(u) << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Float, v: Boolean) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Byte) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Char) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Short) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Int) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Long) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Float) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Double) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: BigInt) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: BigDecimal) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Double, v: Boolean) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Byte) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Char) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Short) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Int) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Long) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Float) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Double) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: BigInt) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: BigDecimal) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt << limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: BigInt, v: Boolean) =>
        val s = u << bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Byte) =>
        val s = u << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Char) =>
        val s = u << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Short) =>
        val s = u << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Int) =>
        val s = u << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Long) =>
        val s = u << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Float) =>
        val s = u << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Double) =>
        val s = u << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: BigInt) =>
        val s = u << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val s = u << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val s = u.toBigInt << bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Byte) =>
        val s = u.toBigInt << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Char) =>
        val s = u.toBigInt << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Short) =>
        val s = u.toBigInt << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Int) =>
        val s = u.toBigInt << limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Long) =>
        val s = u.toBigInt << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Float) =>
        val s = u.toBigInt << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Double) =>
        val s = u.toBigInt << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val s = u.toBigInt << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: BigDecimal) => val s = u.toBigInt << limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
    }
  }

  /**
   * <!-- shiftRightUnsignedFunc --> Function to test NumVal binary shift right unsigned.
   * @param x The value to shift
   * @param y The bits to shift.
   */
  protected def shiftRightUnsignedFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify shift right unsigned.
     * @param expected The expected shifted Scala numeric value.
     * @param a The NumVal to shift.
     * @param b The NumVal bits to shift.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a >>> b
      if (actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method shiftRightUnsignedFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val s = bool2Int(u).longValue >>> limint(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Byte) =>
        val s = bool2Int(u).longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Char) =>
        val s = bool2Int(u).longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Short) =>
        val s = bool2Int(u).longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Int) =>
        val s = bool2Int(u).longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Long) =>
        val s = bool2Int(u).longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Float) =>
        val s = bool2Int(u).longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Double) =>
        val s = bool2Int(u).longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: BigInt) =>
        val s = bool2Int(u).longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val s = bool2Int(u).longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Byte, v: Boolean) =>
        val s = u.longValue >>> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Byte) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Char) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Short) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Int) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Long) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Float) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Double) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: BigInt) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: BigDecimal) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Char, v: Boolean) =>
        val s = u.longValue >>> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Byte) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Char) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Short) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Int) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Long) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Float) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Double) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: BigInt) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: BigDecimal) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Short, v: Boolean) =>
        val s = u.longValue >>> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Byte) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Char) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Short) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Int) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Long) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Float) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Double) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: BigInt) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: BigDecimal) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Int, v: Boolean) =>
        val s = u.longValue >>> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Byte) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Char) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Short) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Int) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Long) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Float) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Double) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: BigInt) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: BigDecimal) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Long, v: Boolean) =>
        val s = u >>> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Byte) =>
        val s = u >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Char) =>
        val s = u >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Short) =>
        val s = u >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Int) =>
        val s = u >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Long) =>
        val s = u >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Float) =>
        val s = u >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Double) =>
        val s = u >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: BigInt) =>
        val s = u >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: BigDecimal) =>
        val s = u >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Float, v: Boolean) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Byte) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Char) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Short) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Int) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Long) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Float) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Double) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: BigInt) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: BigDecimal) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Double, v: Boolean) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Byte) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Char) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Short) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Int) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Long) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Float) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Double) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: BigInt) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: BigDecimal) =>
        val s: AnyVal = if (u.isInfinity) u else (u.longValue >>> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: BigInt, v: Boolean) =>
        val s = u.longValue >>> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Byte) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Char) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Short) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Int) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Long) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Float) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Double) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: BigInt) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val s = u.longValue >>> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Byte) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Char) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Short) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Int) =>
        val s = u.longValue >>> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Long) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Float) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Double) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: BigDecimal) => val s = u.longValue >>> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
    }
  }

  /**
   * <!-- shiftRightFunc --> Function to test NumVal binary shift right.
   * @param x The value to shift
   * @param y The bits to shift.
   */
  protected def shiftRightFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify shift right.
     * @param expected The expected shifted Scala numeric value.
     * @param a The NumVal to shift.
     * @param b The NumVal bits to shift.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a >> b
      if (actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method shiftRightFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val s = bool2BigInt(u) >> limint(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Byte) =>
        val s = bool2BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Char) =>
        val s = bool2BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Short) =>
        val s = bool2BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Int) =>
        val s = bool2BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Long) =>
        val s = bool2BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Float) =>
        val s = bool2BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: Double) =>
        val s = bool2BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: BigInt) =>
        val s = bool2BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val s = bool2BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Byte, v: Boolean) =>
        val s = BigInt(u) >> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Byte) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Char) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Short) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Int) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Long) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Float) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: Double) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: BigInt) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Byte, v: BigDecimal) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Char, v: Boolean) =>
        val s = BigInt(u) >> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Byte) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Char) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Short) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Int) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Long) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Float) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: Double) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: BigInt) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Char, v: BigDecimal) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Short, v: Boolean) =>
        val s = BigInt(u) >> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Byte) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Char) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Short) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Int) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Long) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Float) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: Double) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: BigInt) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Short, v: BigDecimal) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Int, v: Boolean) =>
        val s = BigInt(u) >> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Byte) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Char) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Short) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Int) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Long) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Float) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: Double) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: BigInt) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Int, v: BigDecimal) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Long, v: Boolean) =>
        val s = BigInt(u) >> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Byte) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Char) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Short) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Int) =>
        val s = BigInt(u) >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Long) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Float) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: Double) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: BigInt) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Long, v: BigDecimal) =>
        val s = BigInt(u) >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Float, v: Boolean) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Byte) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Char) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Short) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Int) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Long) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Float) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: Double) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: BigInt) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Float, v: BigDecimal) =>
        val s = if (u.isInfinity) u else (BigDecimal(u.toString).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: Double, v: Boolean) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Byte) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Char) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Short) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Int) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> limint(v)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Long) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Float) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: Double) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: BigInt) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: Double, v: BigDecimal) =>
        val s = if (u.isInfinity) u else (BigDecimal(u).toBigInt >> limint(v.intValue)); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: BigInt, v: Boolean) =>
        val s = u >> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Byte) =>
        val s = u >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Char) =>
        val s = u >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Short) =>
        val s = u >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Int) =>
        val s = u >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Long) =>
        val s = u >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Float) =>
        val s = u >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: Double) =>
        val s = u >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: BigInt) =>
        val s = u >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val s = u >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val s = u.toBigInt >> bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Byte) =>
        val s = u.toBigInt >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Char) =>
        val s = u.toBigInt >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Short) =>
        val s = u.toBigInt >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Int) =>
        val s = u.toBigInt >> limint(v); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Long) =>
        val s = u.toBigInt >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Float) =>
        val s = u.toBigInt >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: Double) =>
        val s = u.toBigInt >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val s = u.toBigInt >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
      case (u: BigDecimal, v: BigDecimal) => val s = u.toBigInt >> limint(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(s, a, b)
    }
  }

  /**
   * <!-- orFunc --> Function to test NumVal binary OR.
   * @param x, y The values to OR together.
   */
  protected def orFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify bit-wise OR.
     * @param expected The expected OR of Scala numeric values.
     * @param a, b The NumVals to OR.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a | b
      if (!(actual.isNaN && isNaN(expected)) && actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method orFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val or = if (u || v) 1 else 0; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: Byte) =>
        val or = bool2Int(u).byteValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: Char) =>
        val or = bool2Int(u).toChar | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: Short) =>
        val or = bool2Int(u).shortValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: Int) =>
        val or = bool2Int(u) | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: Long) =>
        val or = bool2Int(u).longValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else BigInt(bool2Int(u)) | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else BigInt(bool2Int(u)) | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: BigInt) =>
        val or = BigInt(bool2Int(u)) | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val or = bool2BigInt(u) | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: Byte, v: Boolean) =>
        val or = u | bool2Int(v).byteValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: Byte) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: Char) =>
        val or = u.intValue | v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: Short) =>
        val or = u.shortValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: Int) =>
        val or = u.intValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: Long) =>
        val or = u.longValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else BigInt(u) | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else BigInt(u) | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: BigInt) =>
        val or = BigInt(u) | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Byte, v: BigDecimal) =>
        val or = BigInt(u) | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: Char, v: Boolean) =>
        val or = u | bool2Int(v).toChar; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: Byte) =>
        val or = u.intValue | v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: Char) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: Short) =>
        val or = u.intValue | v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: Int) =>
        val or = u.intValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: Long) =>
        val or = u.longValue | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else BigInt(u) | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else BigInt(u) | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: BigInt) =>
        val or = BigInt(u) | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Char, v: BigDecimal) =>
        val or = BigInt(u) | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: Short, v: Boolean) =>
        val or = u | bool2Int(v).shortValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: Byte) =>
        val or = u | v.shortValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: Char) =>
        val or = u.intValue | v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: Short) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: Int) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: Long) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else BigInt(u) | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else BigInt(u) | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: BigInt) =>
        val or = BigInt(u) | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Short, v: BigDecimal) =>
        val or = BigInt(u) | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: Int, v: Boolean) =>
        val or = u | bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: Byte) =>
        val or = u | v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: Char) =>
        val or = u.intValue | v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: Short) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: Int) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: Long) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else BigInt(u) | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else BigInt(u) | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: BigInt) =>
        val or = BigInt(u) | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Int, v: BigDecimal) =>
        val or = BigInt(u) | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: Long, v: Boolean) =>
        val or = u | bool2Int(v).longValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: Byte) =>
        val or = u | v.longValue; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: Char) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: Short) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: Int) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: Long) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else BigInt(u) | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else BigInt(u) | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: BigInt) =>
        val or = BigInt(u) | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Long, v: BigDecimal) =>
        val or = BigInt(u) | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: Float, v: Boolean) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | BigInt(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: Byte) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: Char) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: Short) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: Int) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: Long) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: Float) =>
        val or: Any = if (u.isInfinity || v.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: Double) =>
        val or: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: BigInt) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Float, v: BigDecimal) =>
        val or: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: Double, v: Boolean) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: Byte) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: Char) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: Short) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: Int) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: Long) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: Float) =>
        val or: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: Double) =>
        val or: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: BigInt) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: Double, v: BigDecimal) =>
        val or: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: BigInt, v: Boolean) =>
        val or = u | bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: Byte) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: Char) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: Short) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: Int) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: Long) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else u | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else u | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: BigInt) =>
        val or = u | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val or = u | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val or = u.toBigInt | bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: Byte) =>
        val or = u.toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: Char) =>
        val or = u.toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: Short) =>
        val or = u.toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: Int) =>
        val or = u.toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: Long) =>
        val or = u.toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: Float) =>
        val or: Any = if (v.isInfinity) Float.NaN else u.toBigInt | BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: Double) =>
        val or: Any = if (v.isInfinity) Double.NaN else u.toBigInt | BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val or = u.toBigInt | v; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
      case (u: BigDecimal, v: BigDecimal) => val or = u.toBigInt | v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(or, a, b)
    }
  }

  /**
   * <!-- andFunc --> Function to test NumVal binary AND.
   * @param x, y The values to AND together.
   */
  protected def andFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify bit-wise AND.
     * @param expected The expected AND of Scala numeric values.
     * @param a, b The NumVals to AND.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a & b
      if (!(actual.isNaN && isNaN(expected)) && actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method andFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val and = if (u && v) 1 else 0; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: Byte) =>
        val and = bool2Int(u).byteValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: Char) =>
        val and = bool2Int(u).toChar & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: Short) =>
        val and = bool2Int(u).shortValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: Int) =>
        val and = bool2Int(u) & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: Long) =>
        val and = bool2Int(u).longValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else BigInt(bool2Int(u)) & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else BigInt(bool2Int(u)) & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: BigInt) =>
        val and = BigInt(bool2Int(u)) & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val and = bool2BigInt(u) & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: Byte, v: Boolean) =>
        val and = u & bool2Int(v).byteValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: Byte) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: Char) =>
        val and = u.intValue & v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: Short) =>
        val and = u.shortValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: Int) =>
        val and = u.intValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: Long) =>
        val and = u.longValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else BigInt(u) & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else BigInt(u) & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: BigInt) =>
        val and = BigInt(u) & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Byte, v: BigDecimal) =>
        val and = BigInt(u) & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: Char, v: Boolean) =>
        val and = u & bool2Int(v).toChar; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: Byte) =>
        val and = u.intValue & v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: Char) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: Short) =>
        val and = u.intValue & v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: Int) =>
        val and = u.intValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: Long) =>
        val and = u.longValue & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else BigInt(u) & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else BigInt(u) & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: BigInt) =>
        val and = BigInt(u) & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Char, v: BigDecimal) =>
        val and = BigInt(u) & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: Short, v: Boolean) =>
        val and = u & bool2Int(v).shortValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: Byte) =>
        val and = u & v.shortValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: Char) =>
        val and = u.intValue & v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: Short) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: Int) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: Long) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else BigInt(u) & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else BigInt(u) & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: BigInt) =>
        val and = BigInt(u) & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Short, v: BigDecimal) =>
        val and = BigInt(u) & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: Int, v: Boolean) =>
        val and = u & bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: Byte) =>
        val and = u & v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: Char) =>
        val and = u.intValue & v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: Short) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: Int) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: Long) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else BigInt(u) & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else BigInt(u) & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: BigInt) =>
        val and = BigInt(u) & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Int, v: BigDecimal) =>
        val and = BigInt(u) & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: Long, v: Boolean) =>
        val and = u & bool2Int(v).longValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: Byte) =>
        val and = u & v.longValue; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: Char) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: Short) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: Int) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: Long) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else BigInt(u) & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else BigInt(u) & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: BigInt) =>
        val and = BigInt(u) & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Long, v: BigDecimal) =>
        val and = BigInt(u) & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: Float, v: Boolean) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & BigInt(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: Byte) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: Char) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: Short) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: Int) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: Long) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: Float) =>
        val and: Any = if (u.isInfinity || v.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: Double) =>
        val and: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: BigInt) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Float, v: BigDecimal) =>
        val and: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: Double, v: Boolean) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: Byte) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: Char) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: Short) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: Int) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: Long) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: Float) =>
        val and: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: Double) =>
        val and: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: BigInt) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: Double, v: BigDecimal) =>
        val and: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: BigInt, v: Boolean) =>
        val and = u & bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: Byte) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: Char) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: Short) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: Int) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: Long) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else u & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else u & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: BigInt) =>
        val and = u & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val and = u & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val and = u.toBigInt & bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: Byte) =>
        val and = u.toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: Char) =>
        val and = u.toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: Short) =>
        val and = u.toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: Int) =>
        val and = u.toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: Long) =>
        val and = u.toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: Float) =>
        val and: Any = if (v.isInfinity) Float.NaN else u.toBigInt & BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: Double) =>
        val and: Any = if (v.isInfinity) Double.NaN else u.toBigInt & BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val and = u.toBigInt & v; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
      case (u: BigDecimal, v: BigDecimal) => val and = u.toBigInt & v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(and, a, b)
    }
  }

  /**
   * <!-- xorFunc --> Function to test NumVal binary XOR.
   * @param x, y The values to XOR together.
   */
  protected def xorFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify bit-wise XOR.
     * @param expected The expected XOR of Scala numeric values.
     * @param a, b The NumVals to XOR.
     */
    def vfy(expected: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a ^ b
      if (!(actual.isNaN && isNaN(expected)) && actual != expected) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected=" + toTypeString(expected) + ", actual=" + toTypeString(actual)
        expectResult(expected, clue) { actual }
      }
    }

    // Method xorFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val xor = if ((u || v) && !(u && v)) 1 else 0; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: Byte) =>
        val xor = bool2Int(u).byteValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: Char) =>
        val xor = bool2Int(u).toChar ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: Short) =>
        val xor = bool2Int(u).shortValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: Int) =>
        val xor = bool2Int(u) ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: Long) =>
        val xor = bool2Int(u).longValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else BigInt(bool2Int(u)) ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else BigInt(bool2Int(u)) ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: BigInt) =>
        val xor = BigInt(bool2Int(u)) ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val xor = bool2BigInt(u) ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: Byte, v: Boolean) =>
        val xor = u ^ bool2Int(v).byteValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: Byte) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: Char) =>
        val xor = u.intValue ^ v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: Short) =>
        val xor = u.shortValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: Int) =>
        val xor = u.intValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: Long) =>
        val xor = u.longValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else BigInt(u) ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else BigInt(u) ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: BigInt) =>
        val xor = BigInt(u) ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Byte, v: BigDecimal) =>
        val xor = BigInt(u) ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: Char, v: Boolean) =>
        val xor = u ^ bool2Int(v).toChar; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: Byte) =>
        val xor = u.intValue ^ v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: Char) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: Short) =>
        val xor = u.intValue ^ v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: Int) =>
        val xor = u.intValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: Long) =>
        val xor = u.longValue ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else BigInt(u) ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else BigInt(u) ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: BigInt) =>
        val xor = BigInt(u) ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Char, v: BigDecimal) =>
        val xor = BigInt(u) ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: Short, v: Boolean) =>
        val xor = u ^ bool2Int(v).shortValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: Byte) =>
        val xor = u ^ v.shortValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: Char) =>
        val xor = u.intValue ^ v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: Short) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: Int) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: Long) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else BigInt(u) ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else BigInt(u) ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: BigInt) =>
        val xor = BigInt(u) ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Short, v: BigDecimal) =>
        val xor = BigInt(u) ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: Int, v: Boolean) =>
        val xor = u ^ bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: Byte) =>
        val xor = u ^ v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: Char) =>
        val xor = u.intValue ^ v.intValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: Short) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: Int) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: Long) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else BigInt(u) ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else BigInt(u) ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: BigInt) =>
        val xor = BigInt(u) ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Int, v: BigDecimal) =>
        val xor = BigInt(u) ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: Long, v: Boolean) =>
        val xor = u ^ bool2Int(v).longValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: Byte) =>
        val xor = u ^ v.longValue; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: Char) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: Short) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: Int) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: Long) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else BigInt(u) ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else BigInt(u) ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: BigInt) =>
        val xor = BigInt(u) ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Long, v: BigDecimal) =>
        val xor = BigInt(u) ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: Float, v: Boolean) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ BigInt(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: Byte) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: Char) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: Short) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: Int) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: Long) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: Float) =>
        val xor: Any = if (u.isInfinity || v.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: Double) =>
        val xor: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: BigInt) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Float, v: BigDecimal) =>
        val xor: Any = if (u.isInfinity) Float.NaN else BigDecimal(u.toString).toBigInt ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: Double, v: Boolean) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: Byte) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: Char) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: Short) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: Int) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: Long) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: Float) =>
        val xor: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: Double) =>
        val xor: Any = if (u.isInfinity || v.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: BigInt) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: Double, v: BigDecimal) =>
        val xor: Any = if (u.isInfinity) Double.NaN else BigDecimal(u.toString).toBigInt ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: BigInt, v: Boolean) =>
        val xor = u ^ bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: Byte) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: Char) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: Short) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: Int) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: Long) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else u ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else u ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: BigInt) =>
        val xor = u ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val xor = u ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val xor = u.toBigInt ^ bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: Byte) =>
        val xor = u.toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: Char) =>
        val xor = u.toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: Short) =>
        val xor = u.toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: Int) =>
        val xor = u.toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: Long) =>
        val xor = u.toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: Float) =>
        val xor: Any = if (v.isInfinity) Float.NaN else u.toBigInt ^ BigDecimal(v.toString).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: Double) =>
        val xor: Any = if (v.isInfinity) Double.NaN else u.toBigInt ^ BigDecimal(v).toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val xor = u.toBigInt ^ v; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
      case (u: BigDecimal, v: BigDecimal) => val xor = u.toBigInt ^ v.toBigInt; val a = NumVal(u); val b = NumVal(v); vfy(xor, a, b)
    }
  }

  /**
   * <!-- equalsFunc --> Function to test NumVal equals method.
   * @param x, y The values to compare.
   */
  protected def equalsFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify equality.
     * @param expected The expected equality.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(expected: Boolean, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a.equals(b)
      if (expected != actual) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected x == y: " + expected + ", but got a.equals(b): " + actual
        expectResult(expected, clue) { actual }
      }
    }

    // Method equalsFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: Byte) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: Char) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: Short) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: Int) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: Long) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: Float) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: Double) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: BigInt) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val eq = bool2Int(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: Byte, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: Float) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: BigInt) =>
        val eq = BigInt(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Byte, v: BigDecimal) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: Char, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: Float) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: BigInt) =>
        val eq = BigInt(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Char, v: BigDecimal) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: Short, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: Float) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: BigInt) =>
        val eq = BigInt(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Short, v: BigDecimal) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: Int, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: Float) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: BigInt) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Int, v: BigDecimal) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: Long, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: Float) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: BigInt) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Long, v: BigDecimal) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: Float, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: Float) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: Double) =>
        val eq = float2Double(u) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: BigInt) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Float, v: BigDecimal) =>
        val eq = (if (u.isInfinity) false else BigDecimal(u.toString)) == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: Double, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: Float) =>
        val eq = u == float2Double(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: BigInt) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: Double, v: BigDecimal) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: BigInt, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: Byte) =>
        val eq = u == BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: Char) =>
        val eq = u == BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: Short) =>
        val eq = u == BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: Float) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: BigInt) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val eq = u == bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: Byte) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: Char) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: Short) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: Int) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: Long) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: Float) =>
        val eq = u == (if (v.isInfinity) false else BigDecimal(v.toString)); val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: Double) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
      case (u: BigDecimal, v: BigDecimal) => val eq = u == v; val a = NumVal(u); val b = NumVal(v); vfy(eq, a, b)
    }
  }

  /**
   * <!-- compareFunc --> Function to test NumVal compare method.
   * @param x, y The values to compare.
   */
  protected def compareFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify compare.
     * @param comp The comparison x-y result.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(comp: Int, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val expected = if (comp < 0) -1 else if (comp > 0) 1 else 0
      val actual = a.compare(b)
      if (expected != actual) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString
        expectResult(expected, clue) { actual }
      }
    }

    // Method compareFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val c = if (u == v) 0 else if (u) 1 else -1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: Byte) =>
        val c = bool2Int(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: Char) =>
        val c = bool2Int(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: Short) =>
        val c = bool2Int(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: Int) =>
        val c = bool2Int(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: Long) =>
        val c = bool2Int(u).longValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: Float) =>
        val c = bool2Int(u).floatValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: Double) =>
        val c = bool2Int(u).doubleValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: BigInt) =>
        val c = BigInt(bool2Int(u)).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val c = BigDecimal(bool2Int(u)).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: Byte, v: Boolean) =>
        val c = u.intValue.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: Char) =>
        val c = u.intValue.compare(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: Short) =>
        val c = u.shortValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: Int) =>
        val c = u.intValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: Long) =>
        val c = u.longValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: Float) =>
        val c = u.floatValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: Double) =>
        val c = u.doubleValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: BigInt) =>
        val c = BigInt(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Byte, v: BigDecimal) =>
        val c = BigDecimal(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: Char, v: Boolean) =>
        val c = u.intValue.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: Byte) =>
        val c = u.intValue.compare(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: Char) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: Short) =>
        val c = u.intValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: Int) =>
        val c = u.intValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: Long) =>
        val c = u.longValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: Float) =>
        val c = u.floatValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: Double) =>
        val c = u.doubleValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: BigInt) =>
        val c = BigInt(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Char, v: BigDecimal) =>
        val c = BigDecimal(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: Short, v: Boolean) =>
        val c = u.intValue.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: Char) =>
        val c = u.intValue.compare(v.intValue); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: Short) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: Int) =>
        val c = u.intValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: Long) =>
        val c = u.longValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: Float) =>
        val c = u.floatValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: Double) =>
        val c = u.doubleValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: BigInt) =>
        val c = BigInt(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Short, v: BigDecimal) =>
        val c = BigDecimal(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: Int, v: Boolean) =>
        val c = u.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: Char) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: Short) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: Int) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: Long) =>
        val c = u.longValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: Float) =>
        val c = u.floatValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: Double) =>
        val c = u.doubleValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: BigInt) =>
        val c = BigInt(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Int, v: BigDecimal) =>
        val c = BigDecimal(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: Long, v: Boolean) =>
        val c = u.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: Char) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: Short) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: Int) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: Long) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: Float) =>
        val c = u.doubleValue.compare(v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: Double) =>
        val c = u.doubleValue.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: BigInt) =>
        val c = BigInt(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Long, v: BigDecimal) =>
        val c = BigDecimal(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: Float, v: Boolean) =>
        val c = u.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: Char) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: Short) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: Int) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: Long) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: Float) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: Double) =>
        val c = float2Double(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: BigInt) =>
        val c = if (!u.isInfinity) BigDecimal(u).compare(BigDecimal(v)) else if (u.isPosInfinity) 1 else -1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Float, v: BigDecimal) =>
        val c = if (!u.isInfinity) BigDecimal(u.toString).compare(v) else if (u.isPosInfinity) 1 else -1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: Double, v: Boolean) =>
        val c = u.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: Char) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: Short) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: Int) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: Long) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: Float) =>
        val c = u.compare(float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: Double) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: BigInt) =>
        val c = if (!u.isInfinity) BigDecimal(u).compare(BigDecimal(v)) else if (u.isPosInfinity) 1 else -1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: Double, v: BigDecimal) =>
        val c = if (!u.isInfinity) BigDecimal(u).compare(v) else if (u.isPosInfinity) 1 else -1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: BigInt, v: Boolean) =>
        val c = u.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: Char) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: Short) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: Int) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: Long) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: Float) =>
        val c = if (!v.isInfinity) BigDecimal(u).compare(BigDecimal(v)) else if (v.isPosInfinity) -1 else 1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: Double) =>
        val c = if (!v.isInfinity) BigDecimal(u).compare(BigDecimal(v)) else if (v.isPosInfinity) -1 else 1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: BigInt) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val c = BigDecimal(u).compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val c = u.compare(bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: Byte) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: Char) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: Short) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: Int) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: Long) =>
        val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: Float) =>
        val c = if (!v.isInfinity) u.compare(float2Double(v)) else if (v.isPosInfinity) -1 else 1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: Double) =>
        val c = if (!v.isInfinity) u.compare(v) else if (v.isPosInfinity) -1 else 1; val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val c = u.compare(BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
      case (u: BigDecimal, v: BigDecimal) => val c = u.compare(v); val a = NumVal(u); val b = NumVal(v); vfy(c, a, b)
    }
  }

  /**
   * <!-- ltFunc --> Function to test NumVal "<" method.
   * @param x, y The values to compare.
   */
  protected def ltFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify "<".
     * @param expected The expected conditional.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(expected: Boolean, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a < b
      if (expected != actual) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected x < y =" + expected + ", but got a < b =" + actual
        expectResult(expected, clue) { actual }
      }
    }

    // Method ltFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val lt = if (u == v) false else v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: Byte) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: Char) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: Short) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: Int) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: Long) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: Float) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: Double) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: BigInt) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val lt = bool2Int(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: Byte, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: Float) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: Double) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: BigInt) =>
        val lt = BigInt(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Byte, v: BigDecimal) =>
        val lt = BigDecimal(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: Char, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: Float) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: Double) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: BigInt) =>
        val lt = BigInt(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Char, v: BigDecimal) =>
        val lt = BigDecimal(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: Short, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: Float) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: Double) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: BigInt) =>
        val lt = BigInt(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Short, v: BigDecimal) =>
        val lt = BigDecimal(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: Int, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: Float) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: Double) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: BigInt) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Int, v: BigDecimal) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: Long, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: Float) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: Double) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: BigInt) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Long, v: BigDecimal) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: Float, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: Float) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: Double) =>
        val lt = float2Double(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: BigInt) =>
        val lt = if (!u.isInfinity) BigDecimal(u) < BigDecimal(v) else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Float, v: BigDecimal) =>
        val lt = if (!u.isInfinity) BigDecimal(u.toString) < v else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: Double, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: Float) =>
        val lt = u < float2Double(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: Double) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: BigInt) =>
        val lt = if (!u.isInfinity) BigDecimal(u) < BigDecimal(v) else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: Double, v: BigDecimal) =>
        val lt = if (!u.isInfinity) u < v else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: BigInt, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: Byte) =>
        val lt = u < BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: Char) =>
        val lt = u < BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: Short) =>
        val lt = u < BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: Float) =>
        val lt = if (!v.isInfinity) BigDecimal(u) < BigDecimal(v) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: Double) =>
        val lt = if (!v.isInfinity) BigDecimal(u) < BigDecimal(v) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: BigInt) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val lt = BigDecimal(u) < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val lt = u < bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: Byte) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: Char) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: Short) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: Int) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: Long) =>
        val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: Float) =>
        val lt = if (!v.isInfinity) u < BigDecimal(v.toString) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: Double) =>
        val lt = if (!v.isInfinity) u < BigDecimal(v) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val lt = u < BigDecimal(v); val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
      case (u: BigDecimal, v: BigDecimal) => val lt = u < v; val a = NumVal(u); val b = NumVal(v); vfy(lt, a, b)
    }
  }

  /**
   * <!-- leFunc --> Function to test NumVal "<=" method.
   * @param x, y The values to compare.
   */
  protected def leFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify "<=".
     * @param expected The expected conditional.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(expected: Boolean, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a <= b
      if (expected != actual) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected x <= y =" + expected + ", but got a <= b =" + actual
        expectResult(expected, clue) { actual }
      }
    }

    // Method leFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val le = if (u == v) true else v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: Byte) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: Char) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: Short) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: Int) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: Long) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: Float) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: Double) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: BigInt) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val le = bool2Int(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: Byte, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: Float) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: Double) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: BigInt) =>
        val le = BigInt(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Byte, v: BigDecimal) =>
        val le = BigDecimal(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: Char, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: Float) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: Double) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: BigInt) =>
        val le = BigInt(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Char, v: BigDecimal) =>
        val le = BigDecimal(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: Short, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: Float) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: Double) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: BigInt) =>
        val le = BigInt(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Short, v: BigDecimal) =>
        val le = BigDecimal(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: Int, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: Float) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: Double) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: BigInt) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Int, v: BigDecimal) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: Long, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: Float) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: Double) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: BigInt) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Long, v: BigDecimal) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: Float, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: Float) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: Double) =>
        val le = float2Double(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: BigInt) =>
        val le = if (!u.isInfinity) BigDecimal(u) <= BigDecimal(v) else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Float, v: BigDecimal) =>
        val le = if (!u.isInfinity) BigDecimal(u.toString) <= v else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: Double, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: Float) =>
        val le = u <= float2Double(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: Double) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: BigInt) =>
        val le = if (!u.isInfinity) BigDecimal(u) <= BigDecimal(v) else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: Double, v: BigDecimal) =>
        val le = if (!u.isInfinity) u <= v else u.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: BigInt, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: Byte) =>
        val le = u <= BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: Char) =>
        val le = u <= BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: Short) =>
        val le = u <= BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: Float) =>
        val le = if (!v.isInfinity) BigDecimal(u) <= BigDecimal(v) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: Double) =>
        val le = if (!v.isInfinity) BigDecimal(u) <= BigDecimal(v) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: BigInt) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val le = BigDecimal(u) <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val le = u <= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: Byte) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: Char) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: Short) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: Int) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: Long) =>
        val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: Float) =>
        val le = if (!v.isInfinity) u <= BigDecimal(v.toString) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: Double) =>
        val le = if (!v.isInfinity) u <= BigDecimal(v) else v.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val le = u <= BigDecimal(v); val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
      case (u: BigDecimal, v: BigDecimal) => val le = u <= v; val a = NumVal(u); val b = NumVal(v); vfy(le, a, b)
    }
  }

  /**
   * <!-- gtFunc --> Function to test NumVal ">" method.
   * @param x, y The values to compare.
   */
  protected def gtFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify ">".
     * @param expected The expected conditional.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(expected: Boolean, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a > b
      if (expected != actual) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected x > y =" + expected + ", but got a > b =" + actual
        expectResult(expected, clue) { actual }
      }
    }

    // Method gtFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val gt = if (u == v) false else u; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: Byte) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: Char) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: Short) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: Int) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: Long) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: Float) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: Double) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: BigInt) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val gt = bool2Int(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: Byte, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: Float) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: Double) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: BigInt) =>
        val gt = BigInt(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Byte, v: BigDecimal) =>
        val gt = BigDecimal(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: Char, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: Float) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: Double) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: BigInt) =>
        val gt = BigInt(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Char, v: BigDecimal) =>
        val gt = BigDecimal(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: Short, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: Float) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: Double) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: BigInt) =>
        val gt = BigInt(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Short, v: BigDecimal) =>
        val gt = BigDecimal(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: Int, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: Float) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: Double) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: BigInt) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Int, v: BigDecimal) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: Long, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: Float) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: Double) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: BigInt) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Long, v: BigDecimal) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: Float, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: Float) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: Double) =>
        val gt = float2Double(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: BigInt) =>
        val gt = if (!u.isInfinity) BigDecimal(u) > BigDecimal(v) else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Float, v: BigDecimal) =>
        val gt = if (!u.isInfinity) BigDecimal(u.toString) > v else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: Double, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: Float) =>
        val gt = u > float2Double(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: Double) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: BigInt) =>
        val gt = if (!u.isInfinity) BigDecimal(u) > BigDecimal(v) else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: Double, v: BigDecimal) =>
        val gt = if (!u.isInfinity) u > v else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: BigInt, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: Byte) =>
        val gt = u > BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: Char) =>
        val gt = u > BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: Short) =>
        val gt = u > BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: Float) =>
        val gt = if (!v.isInfinity) BigDecimal(u) > BigDecimal(v) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: Double) =>
        val gt = if (!v.isInfinity) BigDecimal(u) > BigDecimal(v) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: BigInt) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val gt = BigDecimal(u) > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val gt = u > bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: Byte) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: Char) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: Short) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: Int) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: Long) =>
        val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: Float) =>
        val gt = if (!v.isInfinity) u > BigDecimal(v.toString) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: Double) =>
        val gt = if (!v.isInfinity) u > BigDecimal(v) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val gt = u > BigDecimal(v); val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
      case (u: BigDecimal, v: BigDecimal) => val gt = u > v; val a = NumVal(u); val b = NumVal(v); vfy(gt, a, b)
    }
  }

  /**
   * <!-- geFunc --> Function to test NumVal ">=" method.
   * @param x, y The values to compare.
   */
  protected def geFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify ">=".
     * @param expected The expected conditional.
     * @param a The first NumVal.
     * @param b The second NumVal.
     */
    def vfy(expected: Boolean, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val actual = a >= b
      if (expected != actual) {
        val clue = "x: " + toTypeString(x) + ", y: " + toTypeString(y) + ", a: " + a.toTypeString + ", b: " + b.toTypeString +
          ", expected x >= y =" + expected + ", but got a >= b =" + actual
        expectResult(expected, clue) { actual }
      }
    }

    // Method geFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val ge = if (u == v) true else u; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: Byte) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: Char) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: Short) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: Int) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: Long) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: Float) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: Double) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: BigInt) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val ge = bool2Int(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: Byte, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: Float) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: Double) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: BigInt) =>
        val ge = BigInt(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Byte, v: BigDecimal) =>
        val ge = BigDecimal(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: Char, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: Float) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: Double) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: BigInt) =>
        val ge = BigInt(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Char, v: BigDecimal) =>
        val ge = BigDecimal(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: Short, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: Float) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: Double) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: BigInt) =>
        val ge = BigInt(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Short, v: BigDecimal) =>
        val ge = BigDecimal(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: Int, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: Float) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: Double) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: BigInt) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Int, v: BigDecimal) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: Long, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: Float) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: Double) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: BigInt) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Long, v: BigDecimal) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: Float, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: Float) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: Double) =>
        val ge = float2Double(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: BigInt) =>
        val ge = if (!u.isInfinity) BigDecimal(u) >= BigDecimal(v) else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Float, v: BigDecimal) =>
        val ge = if (!u.isInfinity) BigDecimal(u.toString) >= v else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: Double, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: Float) =>
        val ge = u >= float2Double(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: Double) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: BigInt) =>
        val ge = if (!u.isInfinity) BigDecimal(u) >= BigDecimal(v) else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: Double, v: BigDecimal) =>
        val ge = if (!u.isInfinity) u >= v else u.isPosInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: BigInt, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: Byte) =>
        val ge = u >= BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: Char) =>
        val ge = u >= BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: Short) =>
        val ge = u >= BigInt(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: Float) =>
        val ge = if (!v.isInfinity) BigDecimal(u) >= BigDecimal(v) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: Double) =>
        val ge = if (!v.isInfinity) BigDecimal(u) >= BigDecimal(v) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: BigInt) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val ge = BigDecimal(u) >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val ge = u >= bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: Byte) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: Char) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: Short) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: Int) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: Long) =>
        val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: Float) =>
        val ge = if (!v.isInfinity) u >= BigDecimal(v.toString) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: Double) =>
        val ge = if (!v.isInfinity) u >= BigDecimal(v) else v.isNegInfinity; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val ge = u >= BigDecimal(v); val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
      case (u: BigDecimal, v: BigDecimal) => val ge = u >= v; val a = NumVal(u); val b = NumVal(v); vfy(ge, a, b)
    }
  }

  /**
   * <!-- addFunc --> Function to test NumVal binary addition.
   * @param x, y The values to add.
   */
  protected def addFunc(x: Any, y: Any) {

    /**
     * <!-- safeAddInt --> Add 2 Ints, widening if needed to Long to avoid overflow.
     * @param x, y The values to add.
     * @return The sum.
     */
    def safeAddInt(x: Int, y: Int): AnyVal = {
      val s = x + y;
      if ((Int.MinValue & (x ^ y)) == 0 && (Int.MinValue & (x ^ s)) != 0) x.longValue + y else s
    }

    /**
     * <!-- safeAddLong --> Add 2 Longs, widening if needed to BigInt to avoid overflow.
     * @param x, y The values to add.
     * @return The sum.
     */
    def safeAddLong(x: Long, y: Long): Any =
      {
        val s = x + y;
        if ((Long.MinValue & (x ^ y)) == 0 && (Long.MinValue & (x ^ s)) != 0) BigInt(x) + y else s
      }

    /**
     * <!-- safeAdd --> Add two floating point numbers, but preserve Float or Double infinity.
     * @param a The first value.
     * @param b The second value.
     * @return The sum as Float, Double or BigDecimal.  Adding opposite-signed infinities gives a zero Float or Double result.
     */
    def safeAdd(a: Any, b: Any): Any =
      (a, b) match {
        case (u: Float, v: Float) => if (!u.isInfinity) u + v else if (v.isInfinity && u != v) 0.0f else u
        case (u: Float, v: Double) => if (!u.isInfinity) u + v else if (v.isInfinity && u != v) 0.0 else u.toDouble
        case (u: Float, v: BigDecimal) => if (!u.isInfinity) v + u else u
        case (u: Double, v: Float) => if (!u.isInfinity) u + v else if (v.isInfinity && u != v) 0.0 else u
        case (u: Double, v: Double) => if (!u.isInfinity) u + v else if (v.isInfinity && u != v) 0.0 else u
        case (u: Double, v: BigDecimal) => if (!u.isInfinity) v + u else u
        case (u: BigDecimal, v: Float) => if (!v.isInfinity) u + v else v
        case (u: BigDecimal, v: Double) => if (!v.isInfinity) u + v else v
        case (u: BigDecimal, v: BigDecimal) => u + v
        case _ => Double.NaN
      }

    /**
     * <!-- ident --> Given sum "s" of a+b, verify a+b == NumVal(a) + NumVal(b)
     * @param s The sum a+b as a Scala numeric type.
     * @param a The first addend.
     * @param b The second addend.
     */
    def ident(s: Any, a: NumVal, b: NumVal) {
      val expectedErr = xerr(s) + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aPlusB = a + b
      val neq = aPlusB.isNaN != isNaN(s)
      if (neq || subtractInKind(s, aPlusB).abv > expectedErr) {
        val actual = toTypeOf(aPlusB, s)
        val clue = "identity failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(s) + ", got: " + toTypeString(aPlusB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aPlusB: " + aPlusB.toTypeString
        expectResult(s, clue) { actual }
      }
    }

    /**
     * <!-- expectedDiffPair --> Given sum "sn" of a+b, return the pair(xa = sn-b, xb = sn-a).
     * @param sn The sum NumVal.
     * @param a The first addend.
     * @param b The second addend.
     * @return The "expected" a and b values by subtraction.
     */
    def expectedDiffPair(sn: NumVal, a: NumVal, b: NumVal): (NumVal, NumVal) = {
      val err = xerr(sn)
      var xa = a
      var xb = b
      if (sn.isInfinity) {
        if (b == sn) xa = (if (sn.x.isInstanceOf[Float]) NumVal(0.0f) else NumVal(0.0))
        else xa = sn
        if (a == sn) xb = (if (sn.x.isInstanceOf[Float]) NumVal(0.0f) else NumVal(0.0))
        else xb = sn
      } else {
        if (b.isInfinity) xa = -b
        else if (err > b.abv) xa = sn
        else if (sn == b && err > a.abv) xa = 0
        if (a.isInfinity) xb = -a
        else if (err > a.abv) xb = sn
        else if (sn == a && err > b.abv) xb = 0
      }
      (xa, xb)
    }

    /**
     * <!-- inverse --> Given sum "sn" of a+b, verify b == sn-a and a == sn-b.
     * @param sn The sum NumVal.
     * @param a The first addend.
     * @param b The second addend.
     */
    def inverse(sn: NumVal, a: NumVal, b: NumVal) {
      val expectedErr = xerr(sn) + xerr(a).max(xerr(b)) // expected error is the sum error + max of a or b error
      val (expectedA, expectedB) = expectedDiffPair(sn, a, b)
      val resultB = sn - a
      val resultA = sn - b
      if ((expectedA - resultA).abv > expectedErr) {
        val clueA = "inverse failed (a = sn-b). x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(expectedA) + ", got: " + toTypeString(resultA) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", sn: " + sn.toTypeString
        expectResult(expectedA, clueA) { resultA }
      }
      if ((expectedB - resultB).abv > expectedErr) {
        val clueB = "inverse failed (b = sn-a). x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + expectedB + ", got: " + toTypeString(resultB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", sn: " + sn.toTypeString
        expectResult(expectedB, clueB) { resultB }
      }
    }

    /**
     * <!-- commute --> Verify that a+b == b+a.
     * @param a The first addend.
     * @param b The second addend.
     */
    def commute(a: NumVal, b: NumVal) {
      val first = a + b
      val second = b + a
      if (first != second) {
        val clue = "commutative failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString +
          ", a+b: " + first.toTypeString + ", b+a: " + second.toTypeString
        assert(first === second, clue)
      }
    }

    /**
     * <!-- assoc --> Verify that (a+b)+c == a+(b+c).
     * @param a The first addend.
     * @param b The second addend.
     * @param c A third value to test associativity.
     */
    def assoc(a: NumVal, b: NumVal, c: NumVal) {
      val maxErr = xerr(a) max xerr(b) max xerr(c)
      val maxVal = a.abv max b.abv max c.abv
      var expectedErr = maxErr * maxVal * 2 // expected error is the sum of a, b and c error
      val first = (a + b) + c
      val second = a + (b + c)
      if ((first - second).abv > expectedErr) {
        val clue = "associative failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          " a:" + a.toTypeString + ", b:" + b.toTypeString + " c:" + c.toTypeString +
          ", (a+b)+c: " + first.toTypeString + ", a+(b+c): " + second.toTypeString
        assert(first === second, clue)
      }
    }

    /**
     * <!-- vfy --> Verify the properties of addition.
     * @param s The sum a+b as a Scala numeric type.
     * @param sn The sum NumVal.
     * @param a The first addend.
     * @param b The second addend.
     * @param c A third value to test associativity.
     */
    def vfy(s: Any, sn: NumVal, a: NumVal, b: NumVal, c: NumVal) {
      //    caseCount += 1
      ident(s, a, b)
      inverse(sn, a, b)
      commute(a, b)
      assoc(a, b, c)
    }

    // Method addFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val s = bool2Int(u) + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: Byte) =>
        val s = bool2Int(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: Char) =>
        val s = bool2Int(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: Short) =>
        val s = bool2Int(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: Int) =>
        val s = safeAddInt(bool2Int(u), v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: Long) =>
        val s = safeAddLong(bool2Int(u), v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: Float) =>
        val s = bool2Int(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: Double) =>
        val s = bool2Int(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: BigInt) =>
        val s = BigInt(bool2Int(u)) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Boolean, v: BigDecimal) =>
        val s = BigDecimal(bool2Int(u)) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: Byte, v: Boolean) =>
        val s = u + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: Byte) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: Char) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: Short) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: Int) =>
        val s = safeAddInt(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: Long) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: Float) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: Double) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: BigInt) =>
        val s = BigInt(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Byte, v: BigDecimal) =>
        val s = BigDecimal(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: Char, v: Boolean) =>
        val s = u + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: Byte) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: Char) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: Short) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: Int) =>
        val s = safeAddInt(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: Long) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: Float) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: Double) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: BigInt) =>
        val s = BigInt(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Char, v: BigDecimal) =>
        val s = BigDecimal(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: Short, v: Boolean) =>
        val s = u + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: Byte) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: Char) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: Short) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: Int) =>
        val s = safeAddInt(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: Long) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: Float) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: Double) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: BigInt) =>
        val s = BigInt(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Short, v: BigDecimal) =>
        val s = BigDecimal(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: Int, v: Boolean) =>
        val s = safeAddInt(u, bool2Int(v)); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: Byte) =>
        val s = safeAddInt(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: Char) =>
        val s = safeAddInt(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: Short) =>
        val s = safeAddInt(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: Int) =>
        val s = safeAddInt(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: Long) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: Float) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: Double) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: BigInt) =>
        val s = BigInt(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Int, v: BigDecimal) =>
        val s = BigDecimal(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: Long, v: Boolean) =>
        val s = safeAddLong(u, bool2Int(v)); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: Byte) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: Char) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: Short) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: Int) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: Long) =>
        val s = safeAddLong(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: Float) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: Double) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: BigInt) =>
        val s = BigInt(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Long, v: BigDecimal) =>
        val s = BigDecimal(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: Float, v: Boolean) =>
        val s = u + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: Byte) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: Char) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: Short) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: Int) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: Long) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: Float) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: Double) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: BigInt) =>
        val s = safeAdd(u, BigDecimal(v)); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Float, v: BigDecimal) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: Double, v: Boolean) =>
        val s = u + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: Byte) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: Char) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: Short) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: Int) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: Long) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: Float) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: Double) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: BigInt) =>
        val s = safeAdd(u, BigDecimal(v)); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: Double, v: BigDecimal) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: BigInt, v: Boolean) =>
        val s = u + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: Byte) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: Char) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: Short) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: Int) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: Long) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: Float) =>
        val s = safeAdd(BigDecimal(u), v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: Double) =>
        val s = safeAdd(BigDecimal(u), v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: BigInt) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigInt, v: BigDecimal) =>
        val s = BigDecimal(u) + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)

      case (u: BigDecimal, v: Boolean) =>
        val s = u + bool2Int(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: Byte) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: Char) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: Short) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: Int) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: Long) =>
        val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: Float) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: Double) =>
        val s = safeAdd(u, v); val sn = safeNumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: BigInt) =>
        val s = u + BigDecimal(v); val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
      case (u: BigDecimal, v: BigDecimal) => val s = u + v; val sn = NumVal(s); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(s, sn, a, b, c)
    }
  }

  /**
   * <!-- subFunc --> Function to test NumVal binary subtraction.
   * @param x, y The values to subtract.
   */
  protected def subFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify subtraction.
     * @param d The difference a-b as a Scala numeric type.
     * @param a The minuend.
     * @param b The subtrahend.
     */
    def vfy(d: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(d) + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aMinusB = a - b
      val neq = aMinusB.isNaN != isNaN(d)
      if (neq || subtractInKind(d, aMinusB).abv > expectedErr) {
        val actual = toTypeOf(aMinusB, d)
        val clue = "x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(d) + ", got: " + toTypeString(aMinusB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aMinusB: " + aMinusB.toTypeString
        expectResult(d, clue) { actual }
      }
    }

    // Method subFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val d = bool2Int(u) - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: Byte) =>
        val d = bool2Int(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: Char) =>
        val d = bool2Int(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: Short) =>
        val d = bool2Int(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: Int) =>
        val d = safeSubInt(bool2Int(u), v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: Long) =>
        val d = safeSubLong(bool2Int(u), v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: Float) =>
        val d = bool2Int(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: Double) =>
        val d = bool2Int(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: BigInt) =>
        val d = BigInt(bool2Int(u)) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val d = BigDecimal(bool2Int(u)) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: Byte, v: Boolean) =>
        val d = u - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: Byte) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: Char) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: Short) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: Int) =>
        val d = safeSubInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: Long) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: Float) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: Double) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: BigInt) =>
        val d = BigInt(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Byte, v: BigDecimal) =>
        val d = BigDecimal(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: Char, v: Boolean) =>
        val d = u - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: Byte) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: Char) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: Short) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: Int) =>
        val d = safeSubInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: Long) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: Float) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: Double) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: BigInt) =>
        val d = BigInt(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Char, v: BigDecimal) =>
        val d = BigDecimal(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: Short, v: Boolean) =>
        val d = u - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: Byte) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: Char) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: Short) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: Int) =>
        val d = safeSubInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: Long) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: Float) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: Double) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: BigInt) =>
        val d = BigInt(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Short, v: BigDecimal) =>
        val d = BigDecimal(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: Int, v: Boolean) =>
        val d = safeSubInt(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: Byte) =>
        val d = safeSubInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: Char) =>
        val d = safeSubInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: Short) =>
        val d = safeSubInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: Int) =>
        val d = safeSubInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: Long) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: Float) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: Double) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: BigInt) =>
        val d = BigInt(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Int, v: BigDecimal) =>
        val d = BigDecimal(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: Long, v: Boolean) =>
        val d = safeSubLong(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: Byte) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: Char) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: Short) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: Int) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: Long) =>
        val d = safeSubLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: Float) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: Double) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: BigInt) =>
        val d = BigInt(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Long, v: BigDecimal) =>
        val d = BigDecimal(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: Float, v: Boolean) =>
        val d = u - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: Byte) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: Char) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: Short) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: Int) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: Long) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: Float) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: Double) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: BigInt) =>
        val d = safeSub(u, BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Float, v: BigDecimal) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: Double, v: Boolean) =>
        val d = u - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: Byte) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: Char) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: Short) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: Int) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: Long) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: Float) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: Double) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: BigInt) =>
        val d = safeSub(u, BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: Double, v: BigDecimal) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: BigInt, v: Boolean) =>
        val d = u - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: Byte) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: Char) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: Short) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: Int) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: Long) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: Float) =>
        val d = safeSub(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: Double) =>
        val d = safeSub(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: BigInt) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val d = BigDecimal(u) - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val d = u - bool2Int(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: Byte) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: Char) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: Short) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: Int) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: Long) =>
        val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: Float) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: Double) =>
        val d = safeSub(u, v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val d = u - BigDecimal(v); val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
      case (u: BigDecimal, v: BigDecimal) => val d = u - v; val a = NumVal(u); val b = NumVal(v); vfy(d, a, b)
    }
  }

  /**
   * <!-- multFunc --> Function to test NumVal binary multiplication.
   * @param x, y The values to multiply.
   */
  protected def multFunc(x: Any, y: Any) {

    // TODO: try removing xerr calls (set expectedErr = 0) to see if needed for multiply.

    /**
     * <!-- magInt --> Find the magnitude of an integer.
     * @param x The integer value.
     * @return The magnitude.
     */
    def magInt(x: Int) = 31 - Integer.numberOfLeadingZeros(if (x < 0) -x else x)

    /**
     * <!-- safeMultInt --> Multiply 2 Ints, widening if needed to Long to avoid overflow.
     * @param x, y The values to multiply.
     * @return The product.
     */
    def safeMultInt(x: Int, y: Int): AnyVal = if (magInt(x) + magInt(y) < 30) x * y else x.toLong * y

    /**
     * <!-- magLong --> Find the magnitude of a long integer.
     * @param x The long integer value.
     * @return The magnitude.
     */
    def magLong(x: Long) = 63 - java.lang.Long.numberOfLeadingZeros(if (x < 0) -x else x)

    /**
     * <!-- safeMultLong --> Multiply 2 Longs, widening if needed to BigInt to avoid overflow.
     * @param x, y The values to multiply.
     * @return The product.
     */
    def safeMultLong(x: Long, y: Long): Any = if (magLong(x) + magLong(y) < 62) x * y else BigInt(x) * y

    /**
     * <!-- safeMultFloat --> Multiply a long and float, widening if needed to BigDecimal to avoid overflow.
     * @param x, y The values to multiply.
     * @return The product.
     */
    def safeMultFloat(l: Long, f: Float): Any =
      {
        if (f.isInfinity)
          if (l == 0) Float.NaN
          else if (l < 0) -f else f
        else
          BigDecimal(f.toString) * l
      }

    /**
     * <!-- safeMultDouble --> Multiply a long and double, widening if needed to BigDecimal to avoid overflow.
     * @param x, y The values to multiply.
     * @return The product.
     */
    def safeMultDouble(l: Long, d: Double): Any =
      {
        if (d.isInfinity)
          if (l == 0) Double.NaN
          else if (l < 0) -d else d
        else
          BigDecimal(d) * l
      }

    /**
     * <!-- safeMult --> Multiply two floating point numbers (Float, Double or BigDecimal), but preserve Float or Double infinity.
     * @param a The first value.
     * @param b The second value.
     * @return The product as Float, Double or BigDecimal.
     */
    def safeMult(a: Any, b: Any): Any =
      (a, b) match {
        case (u: Float, v: Float) => if (u.isInfinity) if (v == 0) Float.NaN else if (v < 0) -u else u else if (v.isInfinity) if (u == 0) Float.NaN else if (u < 0) -v else v else BigDecimal(u.toString) * BigDecimal(v.toString)
        case (u: Float, v: Double) => if (u.isInfinity) if (v == 0) Double.NaN else if (v < 0) -u else u else if (v.isInfinity) if (u == 0) Double.NaN else if (u < 0) -v else v else BigDecimal(u.toString) * v
        case (u: Float, v: BigInt) => if (u.isInfinity) if (v == 0) Float.NaN else if (v < 0) -u else u else BigDecimal(u.toString) * BigDecimal(v)
        case (u: Float, v: BigDecimal) => if (u.isInfinity) if (v == 0) Float.NaN else if (v < 0) -u else u else BigDecimal(u.toString) * v
        case (u: Double, v: Float) => if (u.isInfinity) if (v == 0) Double.NaN else if (v < 0) -u else u else if (v.isInfinity) if (u == 0) Double.NaN else if (u < 0) -v else v else BigDecimal(v.toString) * u
        case (u: Double, v: Double) => if (u.isInfinity) if (v == 0) Double.NaN else if (v < 0) -u else u else if (v.isInfinity) if (u == 0) Double.NaN else if (u < 0) -v else v else BigDecimal(u) * v
        case (u: Double, v: BigInt) => if (u.isInfinity) if (v == 0) Double.NaN else if (v < 0) -u else u else BigDecimal(v) * u
        case (u: Double, v: BigDecimal) => if (u.isInfinity) if (v == 0) Double.NaN else if (v < 0) -u else u else v * u
        case (u: BigInt, v: Float) => if (v.isInfinity) if (u == 0) Float.NaN else if (u < 0) -v else v else BigDecimal(v.toString) * BigDecimal(u)
        case (u: BigInt, v: Double) => if (v.isInfinity) if (u == 0) Double.NaN else if (u < 0) -v else v else BigDecimal(u) * BigDecimal(v)
        case (u: BigInt, v: BigInt) => u * v
        case (u: BigInt, v: BigDecimal) => BigDecimal(u) * v
        case (u: BigDecimal, v: Float) => if (v.isInfinity) if (u == 0) Float.NaN else if (u < 0) -v else v else BigDecimal(v.toString) * u
        case (u: BigDecimal, v: Double) => if (v.isInfinity) if (u == 0) Double.NaN else if (u < 0) -v else v else u * v
        case (u: BigDecimal, v: BigInt) => u * BigDecimal(v)
        case (u: BigDecimal, v: BigDecimal) => u * v
        case _ => Double.NaN
      }

    /**
     * <!-- ident --> Given product "p" of a*b, verify a*b == NumVal(a) * NumVal(b)
     * @param p The product a*b as a Scala numeric type.
     * @param a The first factor.
     * @param b The second factor.
     */
    def ident(p: Any, a: NumVal, b: NumVal) {
      val expectedErr = xerr(p) + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aTimesB = a * b
      val neq = aTimesB.isNaN != isNaN(p)
      if (neq || subtractInKind(p, aTimesB).abv > expectedErr) {
        val actual = toTypeOf(aTimesB, p)
        val clue = "identity failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(p) + ", got: " + toTypeString(aTimesB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aTimesB: " + aTimesB.toTypeString
        expectResult(p, clue) { actual }
      }
    }

    /**
     * <!-- expectedQuotientPair --> Given product "pn" of a*b, return the pair(xa = pn/b, xb = pn/a).
     * @param pn The product NumVal.
     * @param a The first factor.
     * @param b The second factor.
     * @return The "expected" a and b values by division.
     */
    def expectedQuotientPair(pn: NumVal, a: NumVal, b: NumVal): (NumVal, NumVal) = {
      var xa = a
      var xb = b
      if (pn == 0) {
        xa = n0
        xb = n0
      } else if (pn.isInfinity) {
        xa =
          if (b.isInfinity)
            if (pn == b) np1
            else nm1
          else pn * b
        xb =
          if (a.isInfinity)
            if (pn == a) np1
            else nm1
          else pn * a
      } else {
        if (b.isInfinity) xa = n0
        if (a.isInfinity) xb = n0
      }
      (xa, xb)
    }

    /**
     * <!-- inverse --> Given product "pn" of a*b, verify b == pn/a and a == pn/b.
     * @param pn The product NumVal.
     * @param a The first factor.
     * @param b The second factor.
     */
    def inverse(pn: NumVal, a: NumVal, b: NumVal) {
      val expectedErr = (xerr(pn) + xerr(a).max(xerr(b))) * 2 // expected error is the product error + max of a or b error
      val (expectedA, expectedB) = expectedQuotientPair(pn, a, b)
      val resultB = pn / a
      val resultA = pn / b
      if ((expectedA - resultA).abv > expectedErr) {
        val clueA = "inverse failed (a = pn/b). x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(expectedA) + ", got: " + toTypeString(resultA) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", pn: " + pn.toTypeString
        expectResult(expectedA, clueA) { resultA }
      }
      if ((expectedB - resultB).abv > expectedErr) {
        val clueB = "inverse failed (b = pn/a). x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + expectedB + ", got: " + toTypeString(resultB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", pn: " + pn.toTypeString
        expectResult(expectedB, clueB) { resultB }
      }
    }

    /**
     * <!-- commute --> Verify that a*b == b*a.
     * @param a The first factor.
     * @param b The second factor.
     */
    def commute(a: NumVal, b: NumVal) {
      val first = a * b
      val second = b * a
      if (!(first.isNaN && second.isNaN) && first != second) {
        val clue = "commutative failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString +
          ", a*b: " + first.toTypeString + ", b*a: " + second.toTypeString
        assert(first === second, clue)
      }
    }

    /**
     * <!-- assoc --> Verify that (a*b)*c == a*(b*c).
     * @param a The first factor.
     * @param b The second factor.
     * @param c A third value to test associativity.
     */
    def assoc(a: NumVal, b: NumVal, c: NumVal) {
      val maxErr = xerr(a) max xerr(b) max xerr(c)
      val maxVal = a.abv max b.abv max c.abv
      val expectedErr = maxErr * maxVal * maxVal
      val first = (a * b) * c
      val second = a * (b * c)
      if (!(first.isNaN && second.isNaN) && (first - second).abv > expectedErr) {
        val clue = "associative failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", c:" + c.toTypeString +
          ", (a*b): " + (a * b).toTypeString + ", (b*c): " + (b * c).toTypeString +
          ", (a*b)*c: " + first.toTypeString + ", a*(b*c): " + second.toTypeString
        assert(first === second, clue)
      }
    }

    /**
     * <!-- distrib --> Verify that a*(b+c) == a*b + a*c.
     * @param a The first factor.
     * @param b The second factor.
     * @param c A third value to test distrubutivity.
     */
    def distrib(a: NumVal, b: NumVal, c: NumVal) {
      val first = a * (b + c)
      val second = a * b + a * c

      var expectedErr = if (first.isNaN || first.isInfinity) NumVal(0) else
        (xerr(b + c) * first).abv max ((xerr(a) + xerr(b) + xerr(c)) * first).abv * 2
      if ((first - second).abv > expectedErr) {
        val clue = "distributive failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          " a:" + a.toTypeString + ", b:" + b.toTypeString + " c:" + c.toTypeString +
          ", a*(b+c): " + first.toTypeString + ", a*b + a*c: " + second.toTypeString
        assert(first === second, clue)
      }
    }

    /**
     * <!-- vfy --> Verify the properties of multiplication.
     * @param p The product a*b as a Scala numeric type.
     * @param pn The product NumVal.
     * @param a The first factor.
     * @param b The second factor.
     * @param c A third value to test associativity and distrubutivity.
     */
    def vfy(p: Any, pn: NumVal, a: NumVal, b: NumVal, c: NumVal) {
      ident(p, a, b)
      if (!pn.isNaN)
        inverse(pn, a, b)
      commute(a, b)
      assoc(a, b, c)
      if (!a.isInfinity)
        distrib(a, b, c)
    }

    // Method multFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val p = bool2Int(u) * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: Byte) =>
        val p = bool2Int(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: Char) =>
        val p = bool2Int(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: Short) =>
        val p = bool2Int(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: Int) =>
        val p = safeMultInt(bool2Int(u), v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: Long) =>
        val p = safeMultLong(bool2Int(u), v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: Float) =>
        val p = bool2Int(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: Double) =>
        val p = bool2Int(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: BigInt) =>
        val p = BigInt(bool2Int(u)) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Boolean, v: BigDecimal) =>
        val p = BigDecimal(bool2Int(u)) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: Byte, v: Boolean) =>
        val p = u * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: Byte) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: Char) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: Short) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: Int) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: Long) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: Float) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: Double) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: BigInt) =>
        val p = BigInt(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Byte, v: BigDecimal) =>
        val p = BigDecimal(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: Char, v: Boolean) =>
        val p = u * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: Byte) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: Char) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: Short) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: Int) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: Long) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: Float) =>
        val p = safeMultFloat(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: Double) =>
        val p = safeMultDouble(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: BigInt) =>
        val p = BigInt(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Char, v: BigDecimal) =>
        val p = BigDecimal(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: Short, v: Boolean) =>
        val p = u * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: Byte) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: Char) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: Short) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: Int) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: Long) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: Float) =>
        val p = safeMultFloat(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: Double) =>
        val p = safeMultDouble(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: BigInt) =>
        val p = BigInt(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Short, v: BigDecimal) =>
        val p = BigDecimal(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: Int, v: Boolean) =>
        val p = safeMultInt(u, bool2Int(v)); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: Byte) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: Char) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: Short) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: Int) =>
        val p = safeMultInt(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: Long) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: Float) =>
        val p = safeMultFloat(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: Double) =>
        val p = safeMultDouble(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: BigInt) =>
        val p = BigInt(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Int, v: BigDecimal) =>
        val p = BigDecimal(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: Long, v: Boolean) =>
        val p = safeMultLong(u, bool2Int(v)); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: Byte) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: Char) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: Short) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: Int) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: Long) =>
        val p = safeMultLong(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: Float) =>
        val p = safeMultFloat(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: Double) =>
        val p = safeMultDouble(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: BigInt) =>
        val p = BigInt(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Long, v: BigDecimal) =>
        val p = BigDecimal(u) * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: Float, v: Boolean) =>
        val p = u * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: Byte) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: Char) =>
        val p = safeMultFloat(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: Short) =>
        val p = safeMultFloat(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: Int) =>
        val p = safeMultFloat(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: Long) =>
        val p = safeMultFloat(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: Float) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: Double) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: BigInt) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Float, v: BigDecimal) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: Double, v: Boolean) =>
        val p = u * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: Byte) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: Char) =>
        val p = safeMultDouble(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: Short) =>
        val p = safeMultDouble(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: Int) =>
        val p = safeMultDouble(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: Long) =>
        val p = safeMultDouble(v, u); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: Float) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: Double) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: BigInt) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: Double, v: BigDecimal) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: BigInt, v: Boolean) =>
        val p = u * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: Byte) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: Char) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: Short) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: Int) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: Long) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: Float) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: Double) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: BigInt) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigInt, v: BigDecimal) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)

      case (u: BigDecimal, v: Boolean) =>
        val p = u * bool2Int(v); val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: Byte) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: Char) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: Short) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: Int) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: Long) =>
        val p = u * v; val pn = NumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: Float) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: Double) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: BigInt) =>
        val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
      case (u: BigDecimal, v: BigDecimal) => val p = safeMult(u, v); val pn = safeNumVal(p); val a = NumVal(u); val b = NumVal(v); val c = a + 1; vfy(p, pn, a, b, c)
    }
  }

  /**
   * <!-- divideFunc --> Function to test NumVal binary division.
   * @param x The dividend.
   * @param y The divisor.
   */
  protected def divideFunc(x: Any, y: Any) {

    /**
     * <!-- safeDivideInt --> Divide 2 Ints, yielding 0 if the dividend is 0 and infinity if divisor is 0.
     * @param x The dividend.
     * @param y The divisor.
     * @return The quotient.
     */
    def safeDivideInt(x: Int, y: Int): AnyVal =
      if (y == 0)
        if (x == 0) Double.NaN
        else if (x < 0) Double.NegativeInfinity
        else Double.PositiveInfinity
      else x.doubleValue / y

    /**
     * <!-- safeDivideLong --> Divide 2 Longs, yielding 0 if the dividend is 0 and infinity if divisor is 0.
     * @param x The dividend.
     * @param y The divisor
     * @return The quotient.
     */
    def safeDivideLong(x: Long, y: Long): Any =
      if (y == 0)
        if (x == 0) Double.NaN
        else if (x < 0) Double.NegativeInfinity
        else Double.PositiveInfinity
      else x.doubleValue / y

    /**
     * <!-- divideFloat --> Divide 2 Floats.
     * @param x The dividend.
     * @param y The divisor
     * @return The quotient.
     */
    def divideFloat(x: Float, y: Float): Any = {
      val d = java.lang.Math.getExponent(x) - java.lang.Math.getExponent(y)
      if (d < -125) (BigDecimal(x.toString) / BigDecimal(y.toString)).doubleValue // promote to avoid underflow
      else {
        val ad = if (d < 0) -d else d
        if (ad > 19) (BigDecimal(x.toString) / BigDecimal(y.toString)).doubleValue // promote to preserve precision
        else x / y
      }
    }

    /**
     * <!-- divideDouble --> Divide 2 Floats.
     * @param x The dividend.
     * @param y The divisor
     * @return The quotient.
     */
    def divideDouble(x: Double, y: Double): Any = {
      val d = java.lang.Math.getExponent(x) - java.lang.Math.getExponent(y)
      if (d < -1021) (BigDecimal(x.toString) / BigDecimal(y.toString)).doubleValue // promote to avoid underflow
      else {
        val ad = if (d < 0) -d else d
        if (ad > 48) (BigDecimal(x.toString) / BigDecimal(y.toString)).doubleValue // promote to preserve precision
        else x / y
      }
    }

    /**
     * <!-- safeDivide --> Divide two floating point numbers (Float, Double or BigDecimal), but preserve Float or Double infinity.
     * @param a The dividend.
     * @param b The divisor.
     * @return The quotient as Float, Double or BigDecimal.
     */
    def safeDivide(a: Any, b: Any): Any =
      (a, b) match {
        case (u: Float, v: Float) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v.isInfinity) 0.0f
          else if (v == 0.0)
            if (u == 0) Float.NaN
            else if (u < 0.0) Float.NegativeInfinity
            else Float.PositiveInfinity
          else divideFloat(u, v)
        case (u: Float, v: Double) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v.isInfinity) 0.0
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else divideDouble(BigDecimal(u.toString).toDouble, v)  // preserve accuracy of low order digits
        case (u: Float, v: BigInt) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v == 0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u.toString) / BigDecimal(v)
        case (u: Float, v: BigDecimal) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u.toString) / v

        case (u: Double, v: Float) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v.isInfinity)  0.0
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else divideDouble(u, BigDecimal(v.toString).toDouble)  // preserve accuracy of low order digits
        case (u: Double, v: Double) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v.isInfinity) 0.0
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else divideDouble(u, v)
        case (u: Double, v: BigInt) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u) / BigDecimal(v)
        case (u: Double, v: BigDecimal) =>
          if (u.isInfinity)
            if (v == 0) u
            else if (u == v) 1
            else if (u == -v) -1
            else if (v < 0) -u
            else u
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u) / v

        case (u: BigInt, v: Float) =>
          if (v.isInfinity) 0
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u) / BigDecimal(v.toString)
        case (u: BigInt, v: Double) =>
          if (v.isInfinity) 0
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u) / v
        case (u: BigInt, v: BigInt) =>
          if (v == 0)
            if (u == 0) Double.NaN
            else if (u < 0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u) / BigDecimal(v)
        case (u: BigInt, v: BigDecimal) =>
          if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else BigDecimal(u) / v

        case (u: BigDecimal, v: Float) =>
          if (v.isInfinity) 0
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else u / BigDecimal(v.toString)
        case (u: BigDecimal, v: Double) =>
          if (v.isInfinity) 0
          else if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else u / v
        case (u: BigDecimal, v: BigInt) =>
          if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else u / BigDecimal(v)
        case (u: BigDecimal, v: BigDecimal) =>
          if (v == 0.0)
            if (u == 0) Double.NaN
            else if (u < 0.0) Double.NegativeInfinity
            else Double.PositiveInfinity
          else u / v

        case _ => Double.NaN
      }

    /**
     * <!-- vfy --> Verify division.
     * @param q The quotient a/b as a Scala numeric type.
     * @param a The dividend.
     * @param b The divisor.
     */
    def vfy(q: Any, a: NumVal, b: NumVal) {
      val expectedErr = xerr(q) + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aDivByB = a / b
      val neq = aDivByB.isNaN != isNaN(q)
      if (neq || subtractInKind(q, aDivByB).abv > expectedErr) {
        val actual = toTypeOf(aDivByB, q)
        val clue = "identity failed. x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(q) + ", got: " + toTypeString(aDivByB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aDivByB: " + aDivByB.toTypeString
        expectResult(q, clue) { actual }
      }
    }

    // Method divideFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val q = safeDivideInt(bool2Int(u), bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Byte) =>
        val q = if (v != 0) bool2Int(u).floatValue / v else if (!u) Float.NaN else Float.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Char) =>
        val q = safeDivideInt(bool2Int(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Short) =>
        val q = if (v != 0) bool2Int(u).floatValue / v else if (!u) Float.NaN else Float.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Int) =>
        val q = safeDivideInt(bool2Int(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Long) =>
        val q = safeDivideLong(bool2Int(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Float) =>
        val q = safeDivide(bool2Int(u).toFloat, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Double) =>
        val q = safeDivide(bool2Int(u).toDouble, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: BigInt) =>
        val q = safeDivide(BigInt(bool2Int(u)), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val q = safeDivide(BigDecimal(bool2Int(u)), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Byte, v: Boolean) =>
        val q = safeDivideInt(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Byte) =>
        val q = if (v != 0) u.floatValue / v else if (u == 0) Float.NaN else if (u < 0) Float.NegativeInfinity else Float.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Char) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Short) =>
        val q = if (v != 0) u.floatValue / v else if (u == 0) Float.NaN else if (u < 0) Float.NegativeInfinity else Float.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Int) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Long) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Float) =>
        val q = safeDivide(u.toFloat, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Double) =>
        val q = safeDivide(u.toDouble, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: BigInt) =>
        val q = safeDivide(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: BigDecimal) =>
        val q = safeDivide(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Char, v: Boolean) =>
        val q = safeDivideInt(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Byte) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Char) =>
        val q = if (v != 0) u.floatValue / v else if (u == 0) Float.NaN else Float.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Short) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Int) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Long) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Float) =>
        val q = safeDivide(u.toFloat, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Double) =>
        val q = safeDivide(u.toDouble, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: BigInt) =>
        val q = safeDivide(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: BigDecimal) =>
        val q = safeDivide(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Short, v: Boolean) =>
        val q = safeDivideInt(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Byte) =>
        val q = if (v != 0) u.floatValue / v else if (u == 0) Float.NaN else if (u < 0) Float.NegativeInfinity else Float.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Char) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Short) =>
        val q = if (v != 0) u.floatValue / v else if (u == 0) Float.NaN else if (u < 0) Float.NegativeInfinity else Float.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Int) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Long) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Float) =>
        val q = safeDivide(u.toFloat, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Double) =>
        val q = safeDivide(u.toDouble, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: BigInt) =>
        val q = safeDivide(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: BigDecimal) =>
        val q = safeDivide(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Int, v: Boolean) =>
        val q = safeDivideInt(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Byte) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Char) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Short) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Int) =>
        val q = safeDivideInt(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Long) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Float) =>
        val q = safeDivide(u.toDouble, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Double) =>
        val q = safeDivide(u.toDouble, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: BigInt) =>
        val q = safeDivide(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: BigDecimal) =>
        val q = safeDivide(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Long, v: Boolean) =>
        val q = safeDivideLong(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Byte) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Char) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Short) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Int) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Long) =>
        val q = safeDivideLong(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Float) =>
        val q = safeDivide(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Double) =>
        val q = safeDivide(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: BigInt) =>
        val q = safeDivide(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: BigDecimal) =>
        val q = safeDivide(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Float, v: Boolean) =>
        val q = safeDivide(u, bool2Int(v).toFloat); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Byte) =>
        val q = safeDivide(u, v.toFloat); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Char) =>
        val q = safeDivide(u, v.toFloat); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Short) =>
        val q = safeDivide(u, v.toFloat); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Int) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Long) =>
        val q = safeDivide(u, v.toFloat); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Float) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Double) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: BigInt) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: BigDecimal) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Double, v: Boolean) =>
        val q = safeDivide(u, bool2Int(v).toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Byte) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Char) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Short) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Int) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Long) =>
        val q = safeDivide(u, BigInt(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Float) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Double) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: BigInt) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: BigDecimal) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: BigInt, v: Boolean) =>
        val q = safeDivide(u, bool2Int(v).toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Byte) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Char) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Short) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Int) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Long) =>
        val q = safeDivide(u, BigInt(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Float) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Double) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: BigInt) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val q = safeDivide(u, bool2Int(v).toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Byte) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Char) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Short) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Int) =>
        val q = safeDivide(u, v.toDouble); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Long) =>
        val q = safeDivide(u, BigInt(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Float) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Double) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: BigDecimal) => val q = safeDivide(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
    }
  }

  /**
   * <!-- divFunc --> Function to test NumVal integer "div".
   * @param x The dividend.
   * @param y The divisor.
   */
  protected def divFunc(x: Any, y: Any) {

    /**
     * <!-- safeDivDouble --> Div two Doubles as BigInts.
     * @param a The dividend.
     * @param b The divisor.
     * @return The quotient as BigInt (or Double infinity).
     */
    def safeDivDouble(a: Double, b: Double) = {
      if (b.isInfinity)
        if (a.isInfinity)
          if (a == b) 1 // inf / inf or -inf / -inf
          else -1 // -inf / inf or inf / -inf
        else 0 // x / inf
      else if (a.isInfinity) Double.NaN // inf / y
      else {
        val ba = BigDecimal(a).toBigInt
        val bb = BigDecimal(b).toBigInt
        if (bb == 0) Double.NaN // x / 0
        else ba / bb
      }
    }

    /**
     * <!-- safeDiv --> Divide two numbers (Double, BigInt or BigDecimal).
     * @param a The dividend.
     * @param b The divisor.
     * @return The quotient as BigInt (or Double NaN).
     */
    def safeDiv(a: Any, b: Any) =
      (a, b) match {
        case (u: Double, v: BigInt) => {
          if (u.isInfinity) Double.NaN // inf / y
          else {
            val bu = BigDecimal(u).toBigInt
            if (v == 0) Double.NaN // x / 0
            else bu / v
          }
        }
        case (u: Double, v: BigDecimal) => {
          if (u.isInfinity) Double.NaN // inf / y
          else {
            val bu = BigDecimal(u).toBigInt
            val bv = v.toBigInt
            if (bv == 0) Double.NaN // x / 0
            else bu / bv
          }
        }

        case (u: BigInt, v: Double) => {
          if (v.isInfinity) 0 // x / inf
          else {
            val bv = BigDecimal(v).toBigInt
            if (bv == 0) Double.NaN // x / 0
            else u / bv
          }
        }
        case (u: BigInt, v: BigInt) => if (v == 0) Double.NaN else u / v
        case (u: BigInt, v: BigDecimal) => {
          val bv = v.toBigInt
          if (bv == 0) Double.NaN // x / 0
          else u / bv
        }

        case (u: BigDecimal, v: Double) => {
          if (v.isInfinity) 0 // x / inf
          else {
            val bu = u.toBigInt
            val bv = BigDecimal(v).toBigInt
            if (bv == 0) Double.NaN // x / 0
            else bu / bv
          }
        }
        case (u: BigDecimal, v: BigInt) => {
          val bu = u.toBigInt
          if (v == 0) Double.NaN // x / 0
          else bu / v
        }
        case (u: BigDecimal, v: BigDecimal) => {
          val bu = u.toBigInt
          val bv = v.toBigInt
          if (bv == 0) Double.NaN // x / 0
          else bu / bv
        }

        case _ => Double.NaN
      }

    /**
     * <!-- vfy --> Verify integer "div".
     * @param q The quotient a div b as a Scala integer type.
     * @param a The dividend.
     * @param b The divisor.
     */
    def vfy(q: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(q) + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aDivB = a div b
      val neq = aDivB.isNaN != isNaN(q)
      if (neq || subtractInKind(q, aDivB).abv > expectedErr) {
        val actual = toTypeOf(aDivB, q)
        val clue = "x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(q) + ", got: " + toTypeString(aDivB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aDivB: " + aDivB.toTypeString
        expectResult(q, clue) { actual }
      }
    }

    // Method divFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val q = { val ui = bool2Int(u); val vi = bool2Int(v); if (vi == 0) Float.NaN else ui / vi }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Byte) =>
        val q = { val ui = bool2Int(u); if (v == 0) Float.NaN else ui / v }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Char) =>
        val q = { val ui = bool2Int(u); if (v == 0) Float.NaN else ui / v }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Short) =>
        val q = { val ui = bool2Int(u); if (v == 0) Float.NaN else ui / v }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Int) =>
        val q = { val ui = bool2Int(u); if (v == 0) Double.NaN else ui / v }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Long) =>
        val q = { val ui = bool2Int(u); if (v == 0) Double.NaN else ui / v }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Float) =>
        val q = safeDivDouble(bool2Int(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: Double) =>
        val q = safeDivDouble(bool2Int(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: BigInt) =>
        val q = safeDiv(bool2Int(u).doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val q = safeDiv(bool2Int(u).doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Byte, v: Boolean) =>
        val q = { val vi = bool2Int(v); if (vi == 0) Float.NaN else u / vi }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Byte) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Char) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Short) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Int) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Long) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Float) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: Double) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: BigInt) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Byte, v: BigDecimal) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Char, v: Boolean) =>
        val q = { val vi = bool2Int(v); if (vi == 0) Float.NaN else u / vi }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Byte) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Char) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Short) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Int) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Long) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Float) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: Double) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: BigInt) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Char, v: BigDecimal) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Short, v: Boolean) =>
        val q = { val vi = bool2Int(v); if (vi == 0) Float.NaN else u / vi }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Byte) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Char) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Short) =>
        val q = if (v == 0) Float.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Int) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Long) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Float) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: Double) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: BigInt) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Short, v: BigDecimal) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Int, v: Boolean) =>
        val q = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u / vi }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Byte) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Char) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Short) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Int) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Long) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Float) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: Double) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: BigInt) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Int, v: BigDecimal) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Long, v: Boolean) =>
        val q = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u / vi }; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Byte) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Char) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Short) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Int) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Long) =>
        val q = if (v == 0) Double.NaN else u / v; val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Float) =>
        val q = safeDiv(BigInt(u), float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: Double) =>
        val q = safeDiv(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: BigInt) =>
        val q = safeDiv(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Long, v: BigDecimal) =>
        val q = safeDiv(BigInt(u), v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Float, v: Boolean) =>
        val q = safeDivDouble(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Byte) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Char) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Short) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Int) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Long) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Float) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: Double) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: BigInt) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Float, v: BigDecimal) =>
        val q = safeDiv(u.doubleValue, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: Double, v: Boolean) =>
        val q = safeDivDouble(u, bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Byte) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Char) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Short) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Int) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Long) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Float) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: Double) =>
        val q = safeDivDouble(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: BigInt) =>
        val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: Double, v: BigDecimal) =>
        val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: BigInt, v: Boolean) =>
        val q = safeDiv(u, bool2Int(v).doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Byte) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Char) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Short) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Int) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Long) =>
        val q = safeDiv(u, BigInt(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Float) =>
        val q = safeDiv(u, float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: Double) =>
        val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: BigInt) =>
        val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val q = safeDiv(u, bool2Int(v).doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Byte) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Char) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Short) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Int) =>
        val q = safeDiv(u, v.doubleValue); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Long) =>
        val q = safeDiv(u, BigInt(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Float) =>
        val q = safeDiv(u, float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: Double) =>
        val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
      case (u: BigDecimal, v: BigDecimal) => val q = safeDiv(u, v); val a = NumVal(u); val b = NumVal(v); vfy(q, a, b)
    }
  }

  /**
   * <!-- modFunc --> Function to test NumVal modulo operation.
   * @param x The dividend.
   * @param y The divisor.
   */
  protected def modFunc(x: Any, y: Any) {

    /**
     * <!-- safeModBigDecimal --> Safely perform BigDecimal modulo BigDecimal.
     * @param a The dividend.
     * @param b The divisor.
     * @return The modulo result, or NaN if the divisor is near zero.
     */
    def safeModBigDecimal(a: BigDecimal, b: BigDecimal) = {
      if (b.abs <= Float.MinPositiveValue) Double.NaN
      else if (a.abs >= Float.MaxValue) a.doubleValue % b.doubleValue // BigDecimal limitation!
      else a % b
    }

    /**
     * <!-- vfy --> Verify the modulo operation.
     * @param m The modulo a%b as a Scala integer type.
     * @param a The dividend.
     * @param b The divisor.
     */
    def vfy(m: Any, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(m) + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aModB = a % b
      val neq = aModB.isNaN != isNaN(m)
      if (neq || subtractInKind(m, aModB).abv > expectedErr) {
        val actual = toTypeOf(aModB, m)
        val clue = "x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(m) + ", got: " + toTypeString(aModB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aModB: " + aModB.toTypeString
        expectResult(m, clue) { actual }
      }
    }

    // Method modFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Float.NaN else bool2Int(u) % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: Byte) =>
        val m = if (v == 0) Float.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: Char) =>
        val m = if (v == 0) Float.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: Short) =>
        val m = if (v == 0) Float.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: Int) =>
        val m = if (v == 0) Double.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: Long) =>
        val m = if (v == 0) Double.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: Float) =>
        val m = if (v == 0.0f) Double.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: Double) =>
        val m = if (v == 0.0) Double.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: BigInt) =>
        val m = if (v == 0) Double.NaN else bool2Int(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val m = if (v.abs <= Double.MinPositiveValue) Double.NaN else BigDecimal(bool2Int(u)) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: Byte, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Float.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: Byte) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: Char) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: Short) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: Int) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: Long) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: Float) =>
        val m = if (v == 0.0f) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: Double) =>
        val m = if (v == 0.0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: BigInt) =>
        val m = if (v == 0) Double.NaN else BigInt(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Byte, v: BigDecimal) =>
        val m = if (v.abs <= Double.MinPositiveValue) Double.NaN else BigDecimal(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: Char, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Float.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: Byte) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: Char) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: Short) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: Int) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: Long) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: Float) =>
        val m = if (v == 0.0f) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: Double) =>
        val m = if (v == 0.0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: BigInt) =>
        val m = if (v == 0) Double.NaN else BigInt(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Char, v: BigDecimal) =>
        val m = if (v.abs <= Double.MinPositiveValue) Double.NaN else BigDecimal(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: Short, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Float.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: Byte) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: Char) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: Short) =>
        val m = if (v == 0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: Int) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: Long) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: Float) =>
        val m = if (v == 0.0f) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: Double) =>
        val m = if (v == 0.0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: BigInt) =>
        val m = if (v == 0) Double.NaN else BigInt(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Short, v: BigDecimal) =>
        val m = if (v.abs <= Double.MinPositiveValue) Double.NaN else BigDecimal(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: Int, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: Byte) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: Char) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: Short) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: Int) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: Long) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: Float) =>
        val m = if (v == 0.0) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: Double) =>
        val m = if (v == 0.0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: BigInt) =>
        val m = if (v == 0) Double.NaN else BigInt(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Int, v: BigDecimal) =>
        val m = if (v.abs <= Double.MinPositiveValue) Double.NaN else BigDecimal(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: Long, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: Byte) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: Char) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: Short) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: Int) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: Long) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: Float) =>
        val m = if (v.isInfinity) u else safeModBigDecimal(BigDecimal(u), BigDecimal(v.toString)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: Double) =>
        val m = if (v.isInfinity) u else safeModBigDecimal(BigDecimal(u), BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: BigInt) =>
        val m = if (v == 0) Double.NaN else BigInt(u) % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Long, v: BigDecimal) =>
        val m = safeModBigDecimal(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: Float, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: Byte) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: Char) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: Short) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: Int) =>
        val m = if (u.isInfinity) Float.NaN else safeModBigDecimal(BigDecimal(u.toString), BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: Long) =>
        val m = if (u.isInfinity) Float.NaN else safeModBigDecimal(BigDecimal(u.toString), BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: Float) =>
        val m = if (v == 0.0f) Float.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: Double) =>
        val m = if (u.isInfinity || v == 0.0) Double.NaN else BigDecimal(u.toString).doubleValue % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: BigInt) =>
        val m = if (u.isInfinity) Float.NaN else safeModBigDecimal(BigDecimal(u.toString), BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Float, v: BigDecimal) =>
        val m = if (u.isInfinity) Float.NaN else safeModBigDecimal(BigDecimal(u.toString), v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: Double, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: Byte) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: Char) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: Short) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: Int) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: Long) =>
        val m = if (u.isInfinity) Double.NaN else safeModBigDecimal(BigDecimal(u), BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: Float) =>
        val m = if (u.isInfinity || v == 0.0f) Double.NaN else if (v.isInfinity) u else u % BigDecimal(v.toString).doubleValue; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: Double) =>
        val m = if (v == 0.0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: BigInt) =>
        val m = if (u.isInfinity || v == 0) Double.NaN else safeModBigDecimal(BigDecimal(u), BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: Double, v: BigDecimal) =>
        val m = if (u.isInfinity) Double.NaN else safeModBigDecimal(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: BigInt, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: Byte) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: Char) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: Short) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: Int) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: Long) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: Float) =>
        val m = if (v.isInfinity) u else safeModBigDecimal(BigDecimal(u), BigDecimal(v.toString)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: Double) =>
        val m = if (v.isInfinity) u else safeModBigDecimal(BigDecimal(u), BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: BigInt) =>
        val m = if (v == 0) Double.NaN else u % v; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val m = safeModBigDecimal(BigDecimal(u), v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val m = { val vi = bool2Int(v); if (vi == 0) Double.NaN else u % vi }; val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: Byte) =>
        val m = safeModBigDecimal(u, v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: Char) =>
        val m = safeModBigDecimal(u, v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: Short) =>
        val m = safeModBigDecimal(u, v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: Int) =>
        val m = safeModBigDecimal(u, v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: Long) =>
        val m = safeModBigDecimal(u, v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: Float) =>
        val m = if (v.isInfinity) u else safeModBigDecimal(u, BigDecimal(v.toString)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: Double) =>
        val m = if (v.isInfinity) u else safeModBigDecimal(u, v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val m = safeModBigDecimal(u, BigDecimal(v)); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
      case (u: BigDecimal, v: BigDecimal) => val m = safeModBigDecimal(u, v); val a = NumVal(u); val b = NumVal(v); vfy(m, a, b)
    }
  }

  /**
   * <!-- logFunc --> Function to test NumVal log (arbitrary base) operation.
   * @param x The value (power) for which to find the log.
   * @param y The base.
   */
  protected def logFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify the log (arbitrary base) operation.
     * @param expected The expected test result.
     * @param a The value (power) for which to find the log.
     * @param b The base.
     */
    def vfy(expected: Double, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val expectedErr = xerr(expected) + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aLogB = a.log(b)
      val notEq =
        if (expected.isNaN || aLogB.isNaN) expected.isNaN != aLogB.isNaN
        else if (a > 0 && a <= Double.MinPositiveValue) math.round(expected).toInt != aLogB.round.toInt    // inaccurate scala.math.log for tiny x
        else if (b > 0 && b <= Double.MinPositiveValue) math.round(expected).toInt != aLogB.round.toInt    // inaccurate scala.math.log for tiny base
        else subtractInKind(expected, aLogB).abv > expectedErr
      if (notEq) {
        val actual = toTypeOf(aLogB, expected)
        val clue = "x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(expected) + ", got: " + toTypeString(aLogB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aLogB: " + aLogB.toTypeString
        expectResult(expected, clue) { actual }
      }
      // TODO: Test log(a*b) === log(a) + log(b)
      // TODO: Test log(BigDecimal(a,mc600) === BigDecimal(log(BigDecimal(a,mc700)),mc600)
    }

    def refLg(value:Double, base:Double):Double =
      if (value < 0.0) Double.NaN
      else if (value == 0.0) Double.NegativeInfinity
      else if (value == 1.0) 0.0
      else if (value == base) 1.0
      else if (base <= 0.0 || base == 1.0 || base.isInfinity) Double.NaN
      else if (value.isInfinity) Double.PositiveInfinity
      else scala.math.log(value) / scala.math.log(base)

    def bigToDouble(big:Any):Double =
      big match {
        case bi:BigInt => bi.toDouble
        case bd:BigDecimal => bd.toDouble
        case _ => Double.NaN
      }

    // Method logFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: Byte) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: Char) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: Short) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: Int) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: Long) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: Float) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: Double) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: BigInt) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val lg = if (u) 0.0 else Double.NegativeInfinity; val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: Byte, v: Boolean) =>
        val lg = refLg(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: Byte) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: Char) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: Short) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: Int) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: Long) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: Float) =>
        val lg = refLg(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: Double) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: BigInt) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Byte, v: BigDecimal) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: Char, v: Boolean) =>
        val lg = refLg(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: Byte) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: Char) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: Short) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: Int) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: Long) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: Float) =>
        val lg = refLg(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: Double) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: BigInt) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Char, v: BigDecimal) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: Short, v: Boolean) =>
        val lg = refLg(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: Byte) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: Char) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: Short) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: Int) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: Long) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: Float) =>
        val lg = refLg(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: Double) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: BigInt) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Short, v: BigDecimal) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: Int, v: Boolean) =>
        val lg = refLg(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: Byte) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: Char) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: Short) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: Int) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: Long) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: Float) =>
        val lg = refLg(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: Double) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: BigInt) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Int, v: BigDecimal) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: Long, v: Boolean) =>
        val lg = refLg(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: Byte) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: Char) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: Short) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: Int) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: Long) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: Float) =>
        val lg = refLg(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: Double) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: BigInt) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Long, v: BigDecimal) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: Float, v: Boolean) =>
        val lg = refLg(float2Double(u),bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: Byte) =>
        val lg = refLg(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: Char) =>
        val lg = refLg(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: Short) =>
        val lg = refLg(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: Int) =>
        val lg = refLg(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: Long) =>
        val lg = refLg(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: Float) =>
        val lg = refLg(float2Double(u),float2Double(v)); val a = NumVal(u);  val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: Double) =>
        val lg = refLg(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: BigInt) =>
        val lg = refLg(float2Double(u),bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Float, v: BigDecimal) =>
        val lg = refLg(float2Double(u),bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: Double, v: Boolean) =>
        val lg = refLg(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: Byte) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: Char) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: Short) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: Int) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: Long) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: Float) =>
        val lg = refLg(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: Double) =>
        val lg = refLg(u,v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: BigInt) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: Double, v: BigDecimal) =>
        val lg = refLg(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: BigInt, v: Boolean) =>
        val lg = refLg(bigToDouble(u),bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: Byte) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: Char) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: Short) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: Int) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: Long) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: Float) =>
        val lg = refLg(bigToDouble(u),float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: Double) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: BigInt) =>
        val lg = refLg(bigToDouble(u),bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val lg = refLg(bigToDouble(u), bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val lg = refLg(bigToDouble(u),bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: Byte) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: Char) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: Short) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: Int) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: Long) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: Float) =>
        val lg = refLg(bigToDouble(u),float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: Double) =>
        val lg = refLg(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val lg = refLg(bigToDouble(u), bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
      case (u: BigDecimal, v: BigDecimal) =>
        val lg = refLg(bigToDouble(u), bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(lg, a, b)
    }
  }

  /**
   * <!-- powFunc --> Function to test NumVal pow (arbitrary base) operation.
   * @param x The base value to raise to the given exponent.
   * @param y The exponent.
   */
  protected def powFunc(x: Any, y: Any) {

    /**
     * <!-- vfy --> Verify the pow (arbitrary base) operation.
     * @param xpect The expected test result.
     * @param a The base value to raise to the given exponent.
     * @param b The exponent.
     */
    def vfy(xpect: Double, a: NumVal, b: NumVal) {
      //      caseCount += 1
      val errInExpected = xerr(xpect) * 65    // TODO: Replace! instead of comparing expectedErr with expected - aPowB, compare xerr with aPowB / expected - 1
      val expectedErr = errInExpected + xerr(a).max(xerr(b)) // expected error is the max of sum, a or b
      val aPowB = a.pow(b)
      val expected =
        if (a.isNegInfinity && b.rep.id == NumType.bigDec.id) {
          if (b == 0) 1
          else if (b.fracPart != 0) Double.NaN 
          else if (b < 0) 0
          else if ((b.intPart & 1) == 1) Double.NegativeInfinity
          else Double.PositiveInfinity
        } else xpect
      val notEq =
        if (expected.isNaN || aPowB.isNaN) expected.isNaN != aPowB.isNaN
        else if (a.rep.id == NumType.bigDec.id && a > 0 && a <= Double.MinPositiveValue)    // TODO: Delete after replacing comparison of expectedErr to expected - aPowB, with comparison of xerr with aPowB / expected - 1
          expectedErr < aPowB / xpect - 1    // inaccurate scala.math.log for tiny x
        else subtractInKind(expected, aPowB).abv > expectedErr    // TODO: Replace! instead of comparing expectedErr with expected - aPowB, compare xerr with aPowB / expected - 1
      if (notEq) {
        val actual = toTypeOf(aPowB, expected)
        val clue = "x:" + toTypeString(x) + ", y:" + toTypeString(y) + ", expectedErr:" + expectedErr +
          ", expected: " + toTypeString(expected) + ", got: " + toTypeString(aPowB) +
          ", a:" + a.toTypeString + ", b:" + b.toTypeString + ", aPowB: " + aPowB.toTypeString
        expectResult(expected, clue) { actual }
      }
      // TODO: Test pow(a+b) === pow(a) + pow(b)
      // TODO: Test pow(BigDecimal(a,mc600) === BigDecimal(pow(BigDecimal(a,mc700)),mc600)
    }

    /**
     * <!-- refPow --> Compute a reference value for the power, using Java/Scala math lib and sanity checks.
     * @param value The base value to raise to the given exponent.
     * @param expon The exponent.
     * @return The expected power result.
     */
    def refPow(value:Double, expon:Double):Double =
      if (expon == 0.0) 1.0
      else if (value < 0.0) {
        if (expon.isInfinity)
          if (value > -1.0)
            if (expon > 0) 0.0    // pos inf exponent of fractional value yields 0
            else Double.NaN        // infinite result, but sign is indeterminate
          else if (value == -1) Double.NaN  // is result +1 or -1? Depends whether infinity is even or odd!
          else
            if (expon < 0) 0.0    // neg (reciprocal) inf exponent of value < -1 yields 0
            else Double.NaN        // infinite result, but sign is indeterminate
        else {
          val intExp = expon.toLong
          if (expon != intExp) Double.NaN    // fract exponent of neg number; no support (yet) for imaginary numbers
          else if (value == -1.0)
            if ((intExp & 1) == 1) -1.0      // odd exponent preserves sign
            else 1.0                        // even exponent gives absolute value
          else if (value.isNegInfinity)
            if (intExp < 0) 0.0
            else if ((intExp & 1) == 1) Double.NegativeInfinity      // odd exponent preserves sign
            else Double.PositiveInfinity    // even exponent gives absolute value
          else if (intExp > 999999999L)
            if (value > -1.0) 0.0
            else if ((intExp & 1) == 1) Double.NegativeInfinity    // odd exponent preserves sign
            else Double.PositiveInfinity    // even exponent gives absolute value
          else if (intExp < -999999999L)    // 1 / (value ^ bigInt)
            if (value < -1.0) 0.0            // reciprocal of a large number
            else if ((intExp & 1) == 1) Double.NegativeInfinity    // odd exponent preserves sign
            else Double.PositiveInfinity    // even exponent gives absolute value
          else {    // neg, non-inf value != -1, and integral, non-zero exponent within [-999999999L .. 999999999L]
            val (negExp, absExp) = if (intExp < 0) (true, -intExp) else (false, intExp)
            var result = BigDecimal(value).pow(absExp.toInt)
            if (negExp && result == 0.0)
              if ((intExp & 1) == 1) Double.NegativeInfinity      // odd exponent preserves sign
              else Double.PositiveInfinity    // even exponent gives absolute value
            else {
              if (negExp)    // negative exponent so take reciprocal
                result = BigDecimal("1.0",result.mc) / result
              if (result > Double.MaxValue) Double.PositiveInfinity
              else if (result < Double.MinValue) Double.NegativeInfinity
              else if (result < Double.MinPositiveValue && result > -Double.MinPositiveValue) 0.0
              else result.toDouble
            }
          }
        }
      }
      else if (value == 0.0)
        if (expon < 0.0) Double.PositiveInfinity
        else 0.0
      else if (value == 1.0) 1.0
      else {    // positive value != 1 and non-zero exponent
        if (expon.isNegInfinity)
          if (value > 1.0) 0.0
          else Double.PositiveInfinity
        else if (expon.isPosInfinity)
          if (value > 1.0) Double.PositiveInfinity
          else 0.0
        else if (value.isPosInfinity)
          if (expon < 0.0) 0.0
          else Double.PositiveInfinity
        else scala.math.pow(value, expon)
      }

    def bigToDouble(big:Any):Double =
      big match {
        case bi:BigInt => bi.toDouble
        case bd:BigDecimal => bd.toDouble
        case _ => Double.NaN
      }

    // Method powFunc body
    (x, y) match {
      case (u: Boolean, v: Boolean) =>
        val pow = if (!v) 1.0 else bool2Int(u); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: Byte) =>
        val pow = if (v == 0) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: Char) =>
        val pow = if (v == 0) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: Short) =>
        val pow = if (v == 0) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: Int) =>
        val pow = if (v == 0) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: Long) =>
        val pow = if (v == 0L) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: Float) =>
        val pow = if (v == 0.0f) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: Double) =>
        val pow = if (v == 0.0) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: BigInt) =>
        val pow = if (v == 0) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Boolean, v: BigDecimal) =>
        val pow = if (v == 0) 1.0 else if (u) 1.0 else if (v > 0) 0.0 else Double.PositiveInfinity; val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: Byte, v: Boolean) =>
        val pow = refPow(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: Byte) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: Char) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: Short) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: Int) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: Long) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: Float) =>
        val pow = refPow(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: Double) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: BigInt) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Byte, v: BigDecimal) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: Char, v: Boolean) =>
        val pow = refPow(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: Byte) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: Char) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: Short) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: Int) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: Long) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: Float) =>
        val pow = refPow(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: Double) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: BigInt) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Char, v: BigDecimal) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: Short, v: Boolean) =>
        val pow = refPow(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: Byte) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: Char) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: Short) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: Int) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: Long) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: Float) =>
        val pow = refPow(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: Double) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: BigInt) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Short, v: BigDecimal) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: Int, v: Boolean) =>
        val pow = refPow(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: Byte) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: Char) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: Short) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: Int) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: Long) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: Float) =>
        val pow = refPow(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: Double) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: BigInt) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Int, v: BigDecimal) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: Long, v: Boolean) =>
        val pow = refPow(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: Byte) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: Char) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: Short) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: Int) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: Long) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: Float) =>
        val pow = refPow(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: Double) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: BigInt) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Long, v: BigDecimal) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: Float, v: Boolean) =>
        val pow = refPow(float2Double(u),bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: Byte) =>
        val pow = refPow(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: Char) =>
        val pow = refPow(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: Short) =>
        val pow = refPow(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: Int) =>
        val pow = refPow(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: Long) =>
        val pow = refPow(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: Float) =>
        val pow = refPow(float2Double(u),float2Double(v)); val a = NumVal(u);  val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: Double) =>
        val pow = refPow(float2Double(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: BigInt) =>
        val pow = refPow(float2Double(u),bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Float, v: BigDecimal) =>
        val pow = refPow(float2Double(u),bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: Double, v: Boolean) =>
        val pow = refPow(u,bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: Byte) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: Char) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: Short) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: Int) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: Long) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: Float) =>
        val pow = refPow(u,float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: Double) =>
        val pow = refPow(u,v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: BigInt) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: Double, v: BigDecimal) =>
        val pow = refPow(u,bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: BigInt, v: Boolean) =>
        val pow = refPow(bigToDouble(u),bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: Byte) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: Char) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: Short) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: Int) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: Long) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: Float) =>
        val pow = refPow(bigToDouble(u),float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: Double) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: BigInt) =>
        val pow = refPow(bigToDouble(u),bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigInt, v: BigDecimal) =>
        val pow = refPow(bigToDouble(u), bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)

      case (u: BigDecimal, v: Boolean) =>
        val pow = refPow(bigToDouble(u),bool2Int(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: Byte) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: Char) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: Short) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: Int) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: Long) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: Float) =>
        val pow = refPow(bigToDouble(u),float2Double(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: Double) =>
        val pow = refPow(bigToDouble(u),v); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: BigInt) =>
        val pow = refPow(bigToDouble(u), bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
      case (u: BigDecimal, v: BigDecimal) =>
        val pow = refPow(bigToDouble(u), bigToDouble(v)); val a = NumVal(u); val b = NumVal(v); vfy(pow, a, b)
    }
  }

  object `Function unit tests` {

    object `Unary tests` {

      /**
       * <!-- test toByte --> Test of the NumVal "toByte" method.
       */
      protected def `test toByte` = unaryOp(toByteFunc)

      /**
       * <!-- test toShort --> Test of the NumVal "toShort" method.
       */
      protected def `test toShort` = unaryOp(toShortFunc)

      /**
       * <!-- test toInt --> Test of the NumVal "toInt" method.
       */
      protected def `test toInt` = unaryOp(toIntFunc)

      /**
       * <!-- test toLong --> Test of the NumVal "toLong" method.
       */
      protected def `test toLong` = unaryOp(toLongFunc)

      /**
       * <!-- test toFloat --> Test of the NumVal "toFloat" method.
       */
      protected def `test toFloat` = unaryOp(toFloatFunc)

      /**
       * <!-- test toDouble --> Test of the NumVal "toDouble" method.
       */
      protected def `test toDouble` = unaryOp(toDoubleFunc)

      /**
       * <!-- test toBigInt --> Test of the NumVal "toBigInt" method.
       */
      protected def `test toBigInt` = unaryOp(toBigIntFunc)

      /**
       * <!-- test toBigDecimal --> Test of the NumVal "toBigDecimal" method.
       */
      protected def `test toBigDecimal` = unaryOp(toBigDecimalFunc)

      /**
       * <!-- test toNative --> Test of the NumVal "toNative" method.
       */
      protected def `test toNative` = unaryOp(toNativeFunc)

      /**
       * <!-- test toTypeString --> Test the NumVal "toTypeString" method
       */
      protected def `test toTypeString` = unaryOp(toTypeStringFunc)

      /**
       * <!-- test toString --> Test the NumVal "toString" method
       */
      protected def `test toString` = unaryOp(toStringFunc)

      /**
       * <!-- test hashCode --> Test the NumVal "hashCode" method
       */
      protected def `test hashCode` = unaryOp(hashCodeFunc)

      /**
       * <!-- test isNaN --> Test the NumVal "isNaN" method
       */
      protected def `test isNaN` = unaryOp(izNaNFunc)

      /**
       * <!-- test isInfinity --> Test the NumVal "isInfinity" method
       */
      protected def `test isInfinity` = unaryOp(izInfinityFunc)

      /**
       * <!-- test isPosInfinity --> Test the NumVal "isPosInfinity" method
       */
      protected def `test isPosInfinity` = unaryOp(izPosInfinityFunc)

      /**
       * <!-- test isNegInfinity --> Test the NumVal "isNegInfinity" method
       */
      protected def `test isNegInfinity` = unaryOp(izNegInfinityFunc)

      /**
       * <!-- test intPart --> Test the NumVal "intPart" method
       */
      protected def `test intPart` = unaryOp(intPartFunc)

      /**
       * <!-- test fracPart --> Test the NumVal "fracPart" method
       */
      protected def `test fracPart` = unaryOp(fracPartFunc)

      /**
       * <!-- test abv --> Test of the NumVal "abv" method.
       */
      protected def `test abv` = unaryOp(abvFunc)

      /**
       * <!-- test signum --> Test of the NumVal "signum" method.
       */
      protected def `test signum` = unaryOp(signumFunc)

      /**
       * <!-- test round --> Test of the NumVal "round" method.
       */
      protected def `test round` = unaryOp(roundFunc)

      /**
       * <!-- test log2 --> Test of the NumVal "log2" method.
       */
      protected def `test log2` = unaryOp(log2Func)

      /**
       * <!-- test log10 --> Test of the NumVal "log10" method.
       */
      protected def `test log10` = unaryOp(log10Func)

      /**
       * <!-- test ln --> Test of the NumVal "ln" method.
       */
      protected def `test ln` = unaryOp(lnFunc)

      /**
       * <!-- test exp --> Test of the NumVal "exp" method.
       */
      protected def `test exp` = unaryOp(expFunc)

      /**
       * <!-- test sin --> Test of the NumVal "sin" method.
       */
      protected def `test sin` = unaryOp(sinFunc)

      /**
       * <!-- test cos --> Test of the NumVal "cos" method.
       */
      protected def `test cos` = unaryOp(cosFunc)

      /**
       * <!-- test tan --> Test of the NumVal "tan" method.
       */
      protected def `test tan` = unaryOp(tanFunc)

      /**
       * <!-- test asin --> Test of the NumVal "asin" method.
       */
      protected def `test asin` = unaryOp(asinFunc)

      /**
       * <!-- test acos --> Test of the NumVal "acos" method.
       */
      protected def `test acos` = unaryOp(acosFunc)

      /**
       * <!-- test atan --> Test of the NumVal "atan" method.
       */
      protected def `test atan` = unaryOp(atanFunc)

      /**
       * <!-- test ulp --> Test of the NumVal "ulp" method.
       */
      protected def `test ulp` = unaryOp(ulpFunc)

      /**
       * <!-- test unaryPlus --> Test of the NumVal "unaryPlus" method.
       */
      protected def `test unaryPlus` = unaryOp(unaryPlusFunc)

      /**
       * <!-- test unaryMinus --> Test of the NumVal "unaryMinus" method.
       */
      protected def `test unaryMinus` = unaryOp(unaryMinusFunc)

      /**
       * <!-- test unaryNot --> Test of the NumVal "unaryNot" method.
       */
      protected def `test unaryNot` = unaryOp(unaryNotFunc)

    }

    object `Binary tests` {

      /**
       * <!-- test min --> Test of the NumVal "min" method.
       */
      protected def `test min` = binaryOp(minFunc)

      /**
       * <!-- test max --> Test of the NumVal "max" method.
       */
      protected def `test max` = binaryOp(maxFunc)

      /**
       * <!-- test shiftLeft --> Test of the NumVal "<<" method.
       */
      //@Ignore
      protected def `test shiftLeft` = binaryOp(shiftLeftFunc)

      /**
       * <!-- test shiftRightUnsigned --> Test of the NumVal ">>>" method.
       */
      protected def `test shiftRightUnsigned` = binaryOp(shiftRightUnsignedFunc)

      /**
       * <!-- test shiftRight --> Test of the NumVal ">>" method.
       */
      //@Ignore
      protected def `test shiftRight` = binaryOp(shiftRightFunc)

      /**
       * <!-- test or --> Test of the NumVal "|" method.
       */
      protected def `test or` = binaryOp(orFunc)

      /**
       * <!-- test and --> Test of the NumVal "|" method.
       */
      protected def `test and` = binaryOp(andFunc)

      /**
       * <!-- test xor --> Test of the NumVal "|" method.
       */
      protected def `test xor` = binaryOp(xorFunc)

      /**
       * <!-- test equals --> Test of the NumVal "equals" method.
       */
      protected def `test equals` = binaryOp(equalsFunc)

      /**
       * <!-- test compare --> Test of the NumVal "compare" method.
       */
      protected def `test compare` = binaryOp(compareFunc)

      /**
       * <!-- test lt --> Test of the NumVal "<" method.
       */
      protected def `test lt` = binaryOp(ltFunc)

      /**
       * <!-- test le --> Test of the NumVal "<=" method.
       */
      protected def `test le` = binaryOp(leFunc)

      /**
       * <!-- test gt --> Test of the NumVal ">" method.
       */
      protected def `test gt` = binaryOp(gtFunc)

      /**
       * <!-- test ge --> Test of the NumVal ">=" method.
       */
      protected def `test ge` = binaryOp(geFunc)

      /**
       * <!-- test plus --> Test of the NumVal '+' method.
       */
      //@Ignore
      protected def `test plus` = binaryOp(addFunc)

      /**
       * <!-- test minus --> Test of the NumVal '-' method.
       */
      protected def `test minus` = binaryOp(subFunc)

      /**
       * <!-- test times --> Test of the NumVal '*' method.
       */
      //@Ignore
      protected def `test times` = binaryOp(multFunc)

      /**
       * <!-- test divide --> Test of the NumVal '/' method.
       */
      protected def `test divide` = binaryOp(divideFunc)

      /**
       * <!-- test div --> Test of the NumVal 'div' method.
       */
      protected def `test div` = binaryOp(divFunc)

      /**
       * <!-- test mod --> Test of the NumVal '%' method.
       */
      protected def `test mod` = binaryOp(modFunc)

      /**
       * <!-- test log --> Test of the NumVal 'log' method.
       */
      protected def `test log` = binaryOp(logFunc)

      /**
       * <!-- test pow --> Test of the NumVal 'pow' method.
       */
      protected def `test pow` = binaryOp(powFunc)

    }
  }
}
