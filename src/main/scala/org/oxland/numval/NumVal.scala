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
 * <!-- File NumVal.scala -->
 * @author Robert W Kohlenberger
 */

package org.oxland.numval

/**
 * <!-- NumType --> Enumeration of types, ordered by dynamic range
 * Ordering types by dynamic range roughly corresponds with ordering by precision.
 * An exception is that a 32-bit "float" has greater range than a 64-bit "long".
 * (Implementation Note: This object must be declared OUTSIDE NumVal, or else "equals" between different NumVal NumTypes fails!)
 */
private[numval] object NumType extends Enumeration { // add any new types before "unknown"
  val unit, bool, byte, char, short, int, long, float, double, bigInt, bigDec, unknown = Value
}

/**
 * <!-- ErrorPolicy --> Enumeration of error policies
 */
private[numval] object ErrorPolicy extends Enumeration {
  val silent, callback, exception, log = Value
}

/**
 * <!-- NumVal --> Represents an abstract number value.
 * Enables numeric operations on numbers without knowing their explicit types.
 *
 * <b>Use Case</b>
 *
 * Suppose you have an equation that you wish to generalize over any numeric types a and b.  You might try this:
 * <pre>  <code>def equation(a:AnyVal, b:AnyVal) = a * a + b</code></pre>
 * 
 * Unfortunately, it won't compile because the superclass of primitive number types, AnyVal, lacks + and <code>*</code>
 * operators. The NumVal trait enables you to write the equivalent code for the numeric AnyVal types plus BigInt and BigDecimal:
 * <pre>  <code>def equation(a:NumVal, b:NumVal) = a * a + b</code></pre>
 * 
 * The values passed for a and b can now be any combination of: Boolean, Byte, Char, Short, Int, Long, Float, Double,
 * BigInt or BigDecimal.  Object NumVal provides implicit conversions from any of these types to NumVal.
 * In mixed type operations, precision is maintained by promoting operands prior to performing the operation.
 * Operations on BigDecimals are performed at the operands' precision. A Boolean operand is treated as an integer
 * value (false => 0, true => 1).
 *
 * <b>Public interface</b>
 *
 * These operations work across all 10 supported numeric types:
 *
 * <p>
 * <pre><i>  Unary Operations</i></pre>
 * <table border=1 cellspacing=0 cellpadding=6>
 * <col width=100>
 * <col width=100>
 * <col width=100>
 * <col width=100>
 * 
 * <tr><td>toByte</td> <td>toString</td> <td>+ (unary plus)</td> <td>exp</td></tr>
 * <tr><td>toShort</td> <td>toTypeString</td> <td>- (unary minus)</td> <td>ln</td></tr>
 * <tr><td>toInt</td> <td> </td> <td>~ (unary not)</td> <td>log10</td></tr>
 * <tr><td>toLong</td> <td>isNaN</td> <td> </td> <td>log2</td></tr>
 * <tr><td>toFloat</td> <td>isInfinity</td> <td>hashCode</td> <td> </td></tr>
 * <tr><td>toDouble</td> <td>isNegInfinity</td> <td>intPart</td> <td>sin</td></tr>
 * <tr><td>toBigInt</td> <td>isPosInfinity</td> <td>fracPart</td> <td>cos</td></tr>
 * <tr><td>toBigDecimal</td> <td> </td> <td>sign (signum)</td> <td>tan</td></tr>
 * <tr><td>toNative</td> <td> </td> <td>abv (abs)</td> <td>asin</td></tr>
 * <tr><td> </td> <td> </td> <td>round</td> <td>acos</td></tr>
 * <tr><td> </td> <td> </td> <td>ulp</td> <td>atan</td></tr>
 * 
 * </table>
 * <p><p>
 * <pre><i>  Binary Operations</i></pre>
 * <table border=1 cellspacing=0 cellpadding=6>
 * <col width=40>
 * <col width=40>
 * <col width=40>
 * <tr><td>==</td> <td>+</td> <td>compare</td></tr>
 * <tr><td>&gt=</td> <td>-</td> <td>div</td></tr>
 * <tr><td>&gt</td> <td>*</td> <td>log</td></tr>
 * <tr><td>&lt=</td> <td>/</td> <td>pow</td></tr>
 * <tr><td>&lt</td> <td>%</td> <td>max</td></tr>
 * <tr><td> </td> <td>&</td> <td>min</td></tr>
 * <tr><td> </td> <td>|</td> <td> </td></tr>
 * <tr><td> </td> <td>^</td> <td> </td></tr>
 * <tr><td> </td> <td>&lt&lt</td> <td> </td></tr>
 * <tr><td> </td> <td>&gt&gt</td> <td> </td></tr>
 * <tr><td> </td> <td>&gt&gt&gt</td> <td> </td></tr>
 * </table>
 * <p>
 *
 * <b>Usage</b>
 * 
 * There are 2 ways to include NumVal in your projects.  One is to add the source files Transcendental.scala
 * and NumVal.scala to your source code and recompile.  The other way is to place the NumVal.jar file in your
 * project's class path.  In Eclipse, open Project > Properties > Java Build Path > Libraries tab,
 * then click the Add External JARs... button to find and select the NumVal.jar file.
 * 
 * To explicitly declare NumVal types (as for example in the Use Case method "equation"), add this line to your code:
 * <pre>  <code>import org.oxland.math._</code></pre>
 * To implicitly use NumVal conversions and operations (e.g. to call <code>equation(BigDecimal(6.4),true + 0.04)</code>),
 * add this line:
 * <pre>  <code>import org.oxland.math.NumVal._</code></pre>
 * 
 * These imports are restricted to the code blocks (in curly braces) in which they occur. For safety and clarity,
 * it is recommended to limit the scope of implicits to where they are needed.
 * 
 * For more examples and differences from standard Scala numerics, see the README file that accompanies this code.
 *
 * <b>Code Design</b>
 *
 * Trait NumVal provides common math operations across all supported number types.  These operations are implemented
 * by 10 NumVal subtypes (case classes), one for the specific number type that it wraps. NumVal defines a convenience
 * method, normalizePair, used by binary operations to promote operand types in order to preserve mathematical
 * correctness (e.g. infinities) and precision.
 *
 * The companion object NumVal supplies <code>apply</code> methods to convert from each supported number type to its NumVal
 * wrapper. These are invoked by the implicit conversion methods of object NumVal. Also included is an implicit object,
 * NumValNumeric, that implements Numeric with type parameter NumVal. NumValNumeric is supplied to methods that declare an
 * implicit Numeric parameter, such as List.sum.
 * 
 * The NumVal trait is declared as <code>sealed</code> to restrict subclasses to this file, so that all implicit conversions
 * are collocated. A separate file, Transcendental, implements arbitrary precision math functions used by the NumVal subtypes.
 *
 * The NumVal types do not use Scala 2.10 Value Classes due to limitations on their use.
 * NumVal does not implement Numeric, since a type mismatch error occurs when invoking Byte method abs (redefined in Numeric).
 * Two NumVal method names had to be changed to avoid collision with Scala's implicits: abv (abs) and sign (signum).
 *
 * <b>Author's note</b>
 *
 * This code attempts to follow recommended Scala practices, but there is certainly room for improvement!
 * I'm happy for suggestions or comments on this code.  Please post feedback to the "Suggestions" file.
 *
 * Thanks!
 */
sealed trait NumVal extends Ordered[NumVal] {

  private[numval] val x: Any // the underlying numeric value

  /**
   * <!-- compare --> Implement the Ordered trait
   * @param that The value to compare with this
   * @return the result [-1,0,1] depending if this is less, equal or greater than that
   */
  def compare(that: NumVal): Int = {
    if (this < that) -1
    else if (this > that) 1
    else 0
  }

  // Type conversions

  /**
   * <!-- toByte --> Convert this NumVal to a Byte
   */
  def toByte: Byte

  /**
   * <!-- toShort --> Convert this NumVal to a Short
   */
  def toShort: Short

  /**
   * <!-- toInt --> Convert this NumVal to an Int
   */
  def toInt: Int

  /**
   * <!-- toLong --> Convert this NumVal to a Long
   */
  def toLong: Long

  /**
   * <!-- toFloat --> Convert this NumVal to a Float
   */
  def toFloat: Float

  /**
   * <!-- toDouble --> Convert this NumVal to a Double
   */
  def toDouble: Double

  /**
   * <!-- toByte --> Convert this NumVal to a BigInt
   */
  def toBigInt: BigInt

  /**
   * <!-- toBigDecimal --> Convert this NumVal to a BigDecimal
   */
  def toBigDecimal: BigDecimal

  /**
   * <!-- toNative --> Obtain the internal value representation.
   * @return The native value.
   */
  def toNative: Any = x

// Implement Numeric  --> Warning: If NumVal implements Numeric a type mismatch error occurs when calling Byte method abs (redefined in Numeric)
//  def fromInt(x:Int):NumVal = NumVal(x)
//  def minus(x:NumVal, y:NumVal):NumVal = x - y
//  def negate(x:NumVal):NumVal = -x
//  def plus(x:NumVal, y:NumVal):NumVal = x + y
//  def times(x:NumVal, y:NumVal):NumVal = x * y
//  def toDouble(x:NumVal):Double = x.toDouble
//  def toFloat(x:NumVal):Float = x.toFloat
//  def toInt(x:NumVal):Int = x.toInt
//  def toLong(x:NumVal):Long = x.toLong
//  def compare(x:NumVal, y:NumVal): Int = x.compare(y)

  protected[numval] val rep = { // initialize this representation by numeric type
    import NumType._
    x match {
      case t: Boolean => bool // 1 bit
      case t: Int => int // signed 32 bit
      case t: Float => float // 32 bit real
      case t: Double => double // 64 bit real
      case t: Byte => byte // 8 bit
      case t: Char => char // unsigned 16 bit
      case t: Short => short // signed 16 bit
      case t: Long => long // signed 64 bit
      case t: BigInt => bigInt // arbitrary size
      case t: BigDecimal => bigDec // arbitrary size real
      case t: Unit => unit // nothing
      case _ => unknown // catch-all
    }
  }

  /**
   * <!-- convert --> Transform another NumVal type to this type
   * @param from The NumVal to convert
   * @return the converted result
   */
  protected def convert(from: NumVal): NumVal = {
    import NumType._
    rep match {
      case `bool` => NumVal(if ((from.toByte & 1) == 1) true else false)
      case `int` => NumVal(from.toInt)
      case `float` => NumVal(from.toFloat)
      case `double` => NumVal(from.toDouble)
      case `byte` => NumVal(from.toByte)
      case `char` => NumVal(from.toInt.toChar)
      case `short` => NumVal(from.toShort)
      case `long` => NumVal(from.toLong)
      case `bigInt` => NumVal(from.toBigInt)
      case `bigDec` => NumVal(from.toBigDecimal)
      case `unit` => Unit.asInstanceOf[NumVal]
    }
  }

  private def testLt(nv: NumVal, d: Double) =
    nv match {
      case b: BigIntVal => b.x.toDouble < d
      case b: BigDecimalVal => b.x.toDouble < d
      case _ => false
    }

  private def testGt(nv: NumVal, d: Double) =
    nv match {
      case b: BigIntVal => b.x.toDouble > d
      case b: BigDecimalVal => b.x.toDouble > d
      case _ => false
    }

  /**
   * <!-- convertBig --> Transform another NumVal type to this type
   * @param from The NumVal to convert
   * @return the converted result
   */
  protected def convertBig(from: NumVal): NumVal = {
    import NumType._
    rep match {
      case `bool` => NumVal(if ((from.toByte & 1) == 1) true else false)
      case `int` => NumVal(from.toInt)
      case `float` => if (testGt(from, Float.MaxValue)) NumVal(Float.MaxValue) else if (testLt(from, Float.MinValue)) NumVal(Float.MinValue) else NumVal(from.toFloat)
      case `double` => if (testGt(from, Double.MaxValue)) NumVal(Double.MaxValue) else if (testLt(from, Double.MinValue)) NumVal(Double.MinValue) else NumVal(from.toDouble)
      case `byte` => NumVal(from.toByte)
      case `char` => NumVal(from.toInt.toChar)
      case `short` => NumVal(from.toShort)
      case `long` => NumVal(from.toLong)
      case `bigInt` => NumVal(from.toBigInt)
      case `bigDec` => NumVal(from.toBigDecimal)
      case `unit` => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- normalizeOrderedPair --> Create a pair of NumVals of the same sub-type by extending precision
   * @param u, v Two NumVals, with type u below (before) type v, by NumType enumeration
   * @return the normalized pair.
   */
  protected def normalizeOrderedPair(u: NumVal, v: NumVal): (NumVal, NumVal) = {
    val uRep = u.rep.id
    val vRep = v.rep.id
    import NumType._
    if (vRep < int.id && (uRep == char.id || vRep == char.id))
      (NumVal(u.toInt), NumVal(v.toInt)) // both are integral types, one is char; promote both to int
    else if (vRep < float.id) (v.convert(u), v) // non-fractional promotion of u type to v type
    else if (vRep < bigInt.id) // v is float or double
      if (uRep < int.id) (v.convert(u), v) // promote u (short or below) to v (float or double) type
      else if (uRep == long.id && !v.isInfinity) (NumVal(u.toBigDecimal), NumVal(v.toBigDecimal)) // promote u (long) and v (float or double) to BigDec type
      else if (vRep == double.id) (NumVal(u.toDouble), v) // promote u (int or float) to v (double) type
      else (NumVal(u.toDouble), NumVal(v.toDouble)) // promote both to double type
    else if (vRep == bigInt.id && uRep < float.id) (NumVal(u.toBigInt), v) // promote u (long or below) to v (BigInt) type
    else if (vRep == bigDec.id) // vRep is bigDec, uRep is float, double or bigInt
      if (u.isInfinity || u.isNaN) (u, u.convertBig(v)) // preserve (infinite or nan) u, convert v to u type
      else (NumVal(u.toBigDecimal), v) // promote u (BigInt or below) to v (BigDec) type
    else // vRep is bigInt, uRep is float or double
      if (u.isInfinity || u.isNaN) (u, u.convertBig(v)) // preserve (infinite or nan) u, convert v to u type          
      else (NumVal(u.toBigDecimal), NumVal(v.toBigDecimal)) // promote both to BigDec type
  }

  /**
   * <!-- normalizePair --> Create a pair of NumVals of the same sub-type, extending precision if necessary
   * @param a, b Two NumVals of possibly differing sub-type
   * @return the normalized pair.
   */
  protected def normalizePair(a: NumVal, b: NumVal) = {
    val aRep = a.rep
    val bRep = b.rep
    if (aRep == bRep) (a, b)
    // TODO: is this really needed (try commenting out):
    else if (aRep == NumType.unit || bRep == NumType.unit) (Unit.asInstanceOf[NumVal], Unit.asInstanceOf[NumVal])
    else if (aRep.id < bRep.id) // a needs higher precision
      normalizeOrderedPair(a, b)
    else { // b needs higher precision
      val (nb, na) = normalizeOrderedPair(b, a)
      (na, nb) // reverse to the original order
    }
  }

  // TODO: Change each op result of NaN into a method call ("errResult"?), with behavior governed by enum ErrorPolicy:
  //        "silent" - just return NaN
  //        "callback" - notify an Observer and return NaN
  //        "log" - log a message to a log file and return NaN
  //        "exception" - throw an exception
  //  var policies:Set[ErrorPolicy]
  //  def errResult[T](result:T, msg:String):T = { ...; result }
  //  def errResultEx[T](result:T, ex:Throwable):T = { ...; result }
  // Add static methods to object NumVal:
  //  "setErrPolicy(p:ErrorPolicy)", "setErrPolicies(Traversable[ErrorPolicy])", "setErrObserver(o:Observer)", "setLogFile(f:File)"
  // TODO: Also change the FloatVal and DoubleVal operations toBigInt and toBigDecimal to call errResult if x is Float.NaN or Double.NaN.
  //  Attempts to return those values as BigInt or BigDecimal are guaranteed to throw "NumberFormatException".

  // TODO: Add static methods to object NumVal:
  //  "getBigPrecision():Int, setBigPrecision(p:Int)" - creates a MathContext to use for all new BigDecimals

  // TODO: Implement a journaling feature that tracks a NumVal's operations and operands.
  //        Works with error policies to report operation history for debugging.
  //        Add static method SetJournalingDepth to govern the size of the wrap-around journaling history buffer.
  // TODO: Document the journaling feature ("Wash your hands! You don't know where that number has been.")

  // TODO: JavaDoc all public (unary and binary) ops!

  // Unary ops

  /**
   * <!-- isNaN --> Tell whether this NumVal is NaN (not a number).
   */
  def isNaN: Boolean = false

  /**
   * <!-- isInfinity --> Tell whether this NumVal is (positive or negative) infinity.
   */
  def isInfinity: Boolean = false

  /**
   * <!-- isPosInfinity --> Tell whether this NumVal is positive infinity.
   */
  def isPosInfinity: Boolean = false

  /**
   * <!-- isNegInfinity --> Tell whether this NumVal is negative infinity.
   */
  def isNegInfinity: Boolean = false

  /**
   * <!-- intPart --> Find the signed integer part of this NumVal by truncating towards zero.
   */
  def intPart: NumVal

  /**
   * <!-- fracPart --> Find the signed fractional part of this NumVal by subtracting the integer part.
   */
  def fracPart: NumVal

  /**
   * <!-- NumVal --> Find the absolute value of this NumVal.
   */
  def abv: NumVal

  /**
   * <!-- sign --> Find the sign of this NumVal, one of [-1,0,1].
   */
  def sign: Int

  /**
   * <!-- round --> Round this NumVal.
   */
  def round: NumVal

  /**
   * <!-- log2 --> Find the log base 2 of this NumVal.
   */
  def log2: NumVal

  /**
   * <!-- log10 --> Find the log base 10 of this NumVal.
   */
  def log10: NumVal

  /**
   * <!-- ln --> Find the natual log of this NumVal.
   */
  def ln: NumVal

  /**
   * <!-- exp --> Find e raised to the power of this NumVal.
   */
  def exp: NumVal

  /**
   * <!-- sin --> Find the sine of this (radians) NumVal.
   */
  def sin: NumVal

  /**
   * <!-- cos --> Find the cosine of this (radians) NumVal.
   */
  def cos: NumVal

  /**
   * <!-- tan --> Find the tangent of this (radians) NumVal.
   */
  def tan: NumVal

  /**
   * <!-- asin --> Find the arc sine in radians of this NumVal.
   */
  def asin: NumVal

  /**
   * <!-- acos --> Find the arc cosine in radians of this NumVal.
   */
  def acos: NumVal

  /**
   * <!-- atan --> Find the arc tangent in radians of this NumVal.
   */
  def atan: NumVal

  /**
   * <!-- ulp --> Find the unit in the last place (smallest possible value change) of this NumVal.
   */
  def ulp: NumVal

  // TODO: define a mutable NumVal subtrait (Bridge pattern?), (NumVar? Counter?) that implements the pre- and post- increment and decrement operators
  //    def ++ : NumVal            // post-increment
  //    def -- : NumVal            // post-decrement
  //    def unary_++ : NumVal          // pre-increment
  //    def unary_-- : NumVal          // pre-decrement

  /**
   * <!-- unary_+ --> Unary plus (no-op).
   */
  def unary_+ : NumVal // no-op

  /**
   * <!-- unary_- --> Unary minus.
   */
  def unary_- : NumVal // negation

  /**
   * <!-- unary_~ --> Unary bitwise NOT (one's complement).
   */
  def unary_~ : NumVal // bitwise complement

  // Comparison ops

  /**
   * <!-- &lt --> Tell whether this is less than that.
   * @param that The other NumVal to compare.
   * @return The comparison result.
   */
  override def <(that: NumVal): Boolean = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.x < n.x
      case (m: IntVal, n: IntVal) => m.x < n.x
      case (m: FloatVal, n: FloatVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x < n.x
        else if (this.isInfinity) m.isNegInfinity else n.isPosInfinity
      }
      case (m: DoubleVal, n: DoubleVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x < n.x
        else if (this.isInfinity) m.isNegInfinity else n.isPosInfinity
      }
      case (m: ByteVal, n: ByteVal) => m.x < n.x
      case (m: CharVal, n: CharVal) => m.x < n.x
      case (m: ShortVal, n: ShortVal) => m.x < n.x
      case (m: LongVal, n: LongVal) => m.x < n.x
      case (m: BigIntVal, n: BigIntVal) => m.x < n.x
      case (m: BigDecimalVal, n: BigDecimalVal) => m.x < n.x
      case _ => Unit.asInstanceOf[Boolean]
    }
  }

  /**
   * <!-- &lt= --> Tell whether this is less or equal to than that.
   * @param that The other NumVal to compare.
   * @return The comparison result.
   */
  override def <=(that: NumVal): Boolean = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.x <= n.x
      case (m: IntVal, n: IntVal) => m.x <= n.x
      case (m: FloatVal, n: FloatVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x <= n.x
        else if (this.isInfinity) m.isNegInfinity else n.isPosInfinity
      }
      case (m: DoubleVal, n: DoubleVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x <= n.x
        else if (this.isInfinity) m.isNegInfinity else n.isPosInfinity
      }
      case (m: ByteVal, n: ByteVal) => m.x <= n.x
      case (m: CharVal, n: CharVal) => m.x <= n.x
      case (m: ShortVal, n: ShortVal) => m.x <= n.x
      case (m: LongVal, n: LongVal) => m.x <= n.x
      case (m: BigIntVal, n: BigIntVal) => m.x <= n.x
      case (m: BigDecimalVal, n: BigDecimalVal) => m.x <= n.x
      case _ => Unit.asInstanceOf[Boolean]
    }
  }

  /**
   * <!-- &gt --> Tell whether this is greater than that.
   * @param that The other NumVal to compare.
   * @return The comparison result.
   */
  override def >(that: NumVal): Boolean = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.x > n.x
      case (m: IntVal, n: IntVal) => m.x > n.x
      case (m: FloatVal, n: FloatVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x > n.x
        else if (this.isInfinity) m.isPosInfinity else n.isNegInfinity
      }
      case (m: DoubleVal, n: DoubleVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x > n.x
        else if (this.isInfinity) m.isPosInfinity else n.isNegInfinity
      }
      case (m: ByteVal, n: ByteVal) => m.x > n.x
      case (m: CharVal, n: CharVal) => m.x > n.x
      case (m: ShortVal, n: ShortVal) => m.x > n.x
      case (m: LongVal, n: LongVal) => m.x > n.x
      case (m: BigIntVal, n: BigIntVal) => m.x > n.x
      case (m: BigDecimalVal, n: BigDecimalVal) => m.x > n.x
      case _ => Unit.asInstanceOf[Boolean]
    }
  }

  /**
   * <!-- &gt= --> Tell whether this is greater or equal to that.
   * @param that The other NumVal to compare.
   * @return The comparison result.
   */
  override def >=(that: NumVal): Boolean = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.x >= n.x
      case (m: IntVal, n: IntVal) => m.x >= n.x
      case (m: FloatVal, n: FloatVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x >= n.x
        else if (this.isInfinity) m.isPosInfinity else n.isNegInfinity
      }
      case (m: DoubleVal, n: DoubleVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity)) m.x >= n.x
        else if (this.isInfinity) m.isPosInfinity else n.isNegInfinity
      }
      case (m: ByteVal, n: ByteVal) => m.x >= n.x
      case (m: CharVal, n: CharVal) => m.x >= n.x
      case (m: ShortVal, n: ShortVal) => m.x >= n.x
      case (m: LongVal, n: LongVal) => m.x >= n.x
      case (m: BigIntVal, n: BigIntVal) => m.x >= n.x
      case (m: BigDecimalVal, n: BigDecimalVal) => m.x >= n.x
      case _ => Unit.asInstanceOf[Boolean]
    }
  }

  // Binary ops

  /**
   * <!-- min --> Find the numerically smaller value.
   * @param that The other NumVal to compare.
   * @return The lesser value.
   */
  def min(that: NumVal): NumVal = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => if (m <= n) this else that
      case (m: IntVal, n: IntVal) => if (m <= n) this else that
      case (m: FloatVal, n: FloatVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity))
          if (m.x <= n.x) this else that
        else if (this.isInfinity)
          if (this.isNegInfinity) this else that
        else if (that.isPosInfinity) this else that
      }
      case (m: DoubleVal, n: DoubleVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity))
          if (m.x <= n.x) this else that
        else if (this.isInfinity)
          if (this.isNegInfinity) this else that
        else if (that.isPosInfinity) this else that
      }
      case (m: ByteVal, n: ByteVal) => if (m <= n) this else that
      case (m: CharVal, n: CharVal) => if (m <= n) this else that
      case (m: ShortVal, n: ShortVal) => if (m <= n) this else that
      case (m: LongVal, n: LongVal) => if (m <= n) this else that
      case (m: BigIntVal, n: BigIntVal) => if (m <= n) this else that
      case (m: BigDecimalVal, n: BigDecimalVal) => if (m <= n) this else that
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- max --> Find the numerically larger value.
   * @param that The other NumVal to compare.
   * @return The greater value.
   */
  def max(that: NumVal): NumVal = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => if (m >= n) this else that
      case (m: IntVal, n: IntVal) => if (m >= n) this else that
      case (m: FloatVal, n: FloatVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity))
          if (m.x >= n.x) this else that
        else if (this.isInfinity)
          if (this.isPosInfinity) this else that
        else if (that.isNegInfinity) this else that
      }
      case (m: DoubleVal, n: DoubleVal) => {
        if (!(m.isInfinity || n.isInfinity) || (this.isInfinity && that.isInfinity))
          if (m.x >= n.x) this else that
        else if (this.isInfinity)
          if (this.isPosInfinity) this else that
        else if (that.isNegInfinity) this else that
      }
      case (m: ByteVal, n: ByteVal) => if (m >= n) this else that
      case (m: CharVal, n: CharVal) => if (m >= n) this else that
      case (m: ShortVal, n: ShortVal) => if (m >= n) this else that
      case (m: LongVal, n: LongVal) => if (m >= n) this else that
      case (m: BigIntVal, n: BigIntVal) => if (m >= n) this else that
      case (m: BigDecimalVal, n: BigDecimalVal) => if (m >= n) this else that
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  protected def impl_<<(shift: Int): NumVal

  /**
   * <!-- &lt&lt --> Bit shift left the signed integer part.
   * @param shift The number of bit positions to shift.
   * @return The shifted value.
   */
  def <<(shift: NumVal): NumVal = {
    import NumVal.limitInt
    (shift) match {
      case (s: BooleanVal) => impl_<<(s.toInt)
      case (s: IntVal) => impl_<<(limitInt(s.toInt))
      case (s: FloatVal) => impl_<<(limitInt(s.toInt))
      case (s: DoubleVal) => impl_<<(limitInt(s.toInt))
      case (s: ByteVal) => impl_<<(s.toInt)
      case (s: CharVal) => impl_<<(limitInt(s.toInt))
      case (s: ShortVal) => impl_<<(s.toInt)
      case (s: LongVal) => impl_<<(limitInt(s.toInt))
      case (s: BigIntVal) => impl_<<(limitInt(s.toInt))
      case (s: BigDecimalVal) => impl_<<(limitInt(s.toInt))
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  protected def impl_>>>(shift: Int): NumVal

  /**
   * <!-- &gt&gt&gt --> Unsigned bit shift right the signed integer part.
   * @param shift The number of bit positions to shift.
   * @return The shifted value (high bits filled with zeros).
   */
  def >>>(shift: NumVal): NumVal = {
    import NumVal.limitInt
    (shift) match {
      case (s: BooleanVal) => impl_>>>(s.toInt)
      case (s: IntVal) => impl_>>>(limitInt(s.toInt))
      case (s: FloatVal) => impl_>>>(limitInt(s.toInt))
      case (s: DoubleVal) => impl_>>>(limitInt(s.toInt))
      case (s: ByteVal) => impl_>>>(s.toInt)
      case (s: CharVal) => impl_>>>(limitInt(s.toInt))
      case (s: ShortVal) => impl_>>>(s.toInt)
      case (s: LongVal) => impl_>>>(limitInt(s.toInt))
      case (s: BigIntVal) => impl_>>>(limitInt(s.toInt))
      case (s: BigDecimalVal) => impl_>>>(limitInt(s.toInt))
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  protected def impl_>>(shift: Int): NumVal

  /**
   * <!-- &gt&gt --> Signed bit shift right the signed integer part.
   * @param shift The number of bit positions to shift.
   * @return The shifted value (high bits filled with the sign bit).
   */
  def >>(shift: NumVal): NumVal = {
    import NumVal.limitInt
    (shift) match {
      case (s: BooleanVal) => impl_>>(s.toInt)
      case (s: IntVal) => impl_>>(limitInt(s.toInt))
      case (s: FloatVal) => impl_>>(limitInt(s.toInt))
      case (s: DoubleVal) => impl_>>(limitInt(s.toInt))
      case (s: ByteVal) => impl_>>(s.toInt)
      case (s: CharVal) => impl_>>(limitInt(s.toInt))
      case (s: ShortVal) => impl_>>(s.toInt)
      case (s: LongVal) => impl_>>(limitInt(s.toInt))
      case (s: BigIntVal) => impl_>>(limitInt(s.toInt))
      case (s: BigDecimalVal) => impl_>>(limitInt(s.toInt))
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- | --> Bitwise OR the signed integer part.
   * @param that The number to OR with this.
   * @return The value of this ORed with that.
   */
  def |(that: NumVal): NumVal = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.impl_|(n)
      case (m: IntVal, n: IntVal) => m.impl_|(n)
      case (m: FloatVal, n: FloatVal) => m.impl_|(n)
      case (m: DoubleVal, n: DoubleVal) => m.impl_|(n)
      case (m: ByteVal, n: ByteVal) => m.impl_|(n)
      case (m: CharVal, n: CharVal) => m.impl_|(n)
      case (m: ShortVal, n: ShortVal) => m.impl_|(n)
      case (m: LongVal, n: LongVal) => m.impl_|(n)
      case (m: BigIntVal, n: BigIntVal) => m.impl_|(n)
      case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_|(n)
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- && --> Bitwise AND the signed integer part.
   * @param that The number to AND with this.
   * @return The value of this ANDed with that.
   */
  def &(that: NumVal): NumVal = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.impl_&(n)
      case (m: IntVal, n: IntVal) => m.impl_&(n)
      case (m: FloatVal, n: FloatVal) => m.impl_&(n)
      case (m: DoubleVal, n: DoubleVal) => m.impl_&(n)
      case (m: ByteVal, n: ByteVal) => m.impl_&(n)
      case (m: CharVal, n: CharVal) => m.impl_&(n)
      case (m: ShortVal, n: ShortVal) => m.impl_&(n)
      case (m: LongVal, n: LongVal) => m.impl_&(n)
      case (m: BigIntVal, n: BigIntVal) => m.impl_&(n)
      case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_&(n)
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- ^ --> Bitwise XOR the signed integer part.
   * @param that The number to XOR with this.
   * @return The value of this XORed with that.
   */
  def ^(that: NumVal): NumVal = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.impl_^(n)
      case (m: IntVal, n: IntVal) => m.impl_^(n)
      case (m: FloatVal, n: FloatVal) => m.impl_^(n)
      case (m: DoubleVal, n: DoubleVal) => m.impl_^(n)
      case (m: ByteVal, n: ByteVal) => m.impl_^(n)
      case (m: CharVal, n: CharVal) => m.impl_^(n)
      case (m: ShortVal, n: ShortVal) => m.impl_^(n)
      case (m: LongVal, n: LongVal) => m.impl_^(n)
      case (m: BigIntVal, n: BigIntVal) => m.impl_^(n)
      case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_^(n)
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- + --> Add.
   * @param that The number to add with this.
   * @return The value of this added to that.
   */
  def +(that: NumVal): NumVal = {
    if (that.isNaN) that
    else if (that.isInfinity) that
    else {
      val (na, nb) = normalizePair(this, that)
      (na, nb) match {
        case (m: BooleanVal, n: BooleanVal) => m.impl_+(n)
        case (m: IntVal, n: IntVal) => m.impl_+(n)
        case (m: FloatVal, n: FloatVal) => m.impl_+(n)
        case (m: DoubleVal, n: DoubleVal) => m.impl_+(n)
        case (m: ByteVal, n: ByteVal) => m.impl_+(n)
        case (m: CharVal, n: CharVal) => m.impl_+(n)
        case (m: ShortVal, n: ShortVal) => m.impl_+(n)
        case (m: LongVal, n: LongVal) => m.impl_+(n)
        case (m: BigIntVal, n: BigIntVal) => m.impl_+(n)
        case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_+(n)
        case _ => Unit.asInstanceOf[NumVal]
      }
    }
  }

  /**
   * <!-- - --> Subtract.
   * @param that The number to subtract from this.
   * @return The value of this minus that.
   */
  def -(that: NumVal): NumVal = {
    if (that.isNaN) that
    else if (that.isInfinity) -that
    else {
      val (na, nb) = normalizePair(this, that)
      (na, nb) match {
        case (m: BooleanVal, n: BooleanVal) => m.impl_-(n)
        case (m: IntVal, n: IntVal) => m.impl_-(n)
        case (m: FloatVal, n: FloatVal) => m.impl_-(n)
        case (m: DoubleVal, n: DoubleVal) => m.impl_-(n)
        case (m: ByteVal, n: ByteVal) => m.impl_-(n)
        case (m: CharVal, n: CharVal) => m.impl_-(n)
        case (m: ShortVal, n: ShortVal) => m.impl_-(n)
        case (m: LongVal, n: LongVal) => m.impl_-(n)
        case (m: BigIntVal, n: BigIntVal) => m.impl_-(n)
        case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_-(n)
        case _ => Unit.asInstanceOf[NumVal]
      }
    }
  }

  /**
   * <!-- * --> Multiply.
   * @param that The number to multiply with this.
   * @return The value of this times that.
   */
  def *(that: NumVal): NumVal = {
    if (that.isNaN) that
    else if (that.isInfinity)
      if (this == 0) NumVal.Dnan
      else if (this.sign < 0) -that else that
    else {
      val (na, nb) = normalizePair(this, that)
      (na, nb) match {
        case (m: BooleanVal, n: BooleanVal) => m.impl_*(n)
        case (m: IntVal, n: IntVal) => m.impl_*(n)
        case (m: FloatVal, n: FloatVal) => m.impl_*(n)
        case (m: DoubleVal, n: DoubleVal) => m.impl_*(n)
        case (m: ByteVal, n: ByteVal) => m.impl_*(n)
        case (m: CharVal, n: CharVal) => m.impl_*(n)
        case (m: ShortVal, n: ShortVal) => m.impl_*(n)
        case (m: LongVal, n: LongVal) => m.impl_*(n)
        case (m: BigIntVal, n: BigIntVal) => m.impl_*(n)
        case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_*(n)
        case _ => Unit.asInstanceOf[NumVal]
      }
    }
  }

  /**
   * <!-- / --> Divide.
   * @param that The divisor.
   * @return The value of this divided by that.
   */
  def /(that: NumVal): NumVal = {
    if (that.isNaN) that
    else if (that.isInfinity) NumVal.I0
    else if (that == 0) {
      if (this == 0) NumVal.Dnan
      else {
        val sign = this.sign
        if (sign == 0) this
        else if (sign < 0) NumVal.NegInf else NumVal.PosInf
      }
    } else {
      val (na, nb) = normalizePair(this, that)
      (na, nb) match {
        case (m: BooleanVal, n: BooleanVal) => m.impl_/(n)
        case (m: IntVal, n: IntVal) => m.impl_/(n)
        case (m: FloatVal, n: FloatVal) => m.impl_/(n)
        case (m: DoubleVal, n: DoubleVal) => m.impl_/(n)
        case (m: ByteVal, n: ByteVal) => m.impl_/(n)
        case (m: CharVal, n: CharVal) => m.impl_/(n)
        case (m: ShortVal, n: ShortVal) => m.impl_/(n)
        case (m: LongVal, n: LongVal) => m.impl_/(n)
        case (m: BigIntVal, n: BigIntVal) => m.impl_/(n)
        case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_/(n)
        case _ => Unit.asInstanceOf[NumVal]
      }
    }
  }

  /**
   * <!-- div --> Integer division.
   * @param that The divisor.
   * @return The integer value of this.intPart divided by that.intPart.
   */
  def div(that: NumVal): NumVal = {
    if (that == 0) NumVal.Dnan
    else {
      val (na, nb) = normalizePair(this, that)
      (na, nb) match {
        case (m: BooleanVal, n: BooleanVal) => m.impl_div(n)
        case (m: IntVal, n: IntVal) => m.impl_div(n)
        case (m: FloatVal, n: FloatVal) => m.impl_div(n)
        case (m: DoubleVal, n: DoubleVal) => m.impl_div(n)
        case (m: ByteVal, n: ByteVal) => m.impl_div(n)
        case (m: CharVal, n: CharVal) => m.impl_div(n)
        case (m: ShortVal, n: ShortVal) => m.impl_div(n)
        case (m: LongVal, n: LongVal) => m.impl_div(n)
        case (m: BigIntVal, n: BigIntVal) => m.impl_div(n)
        case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_div(n)
        case _ => Unit.asInstanceOf[NumVal]
      }
    }
  }

  /**
   * <!-- % --> Modulo.
   * @param that The divisor.
   * @return The value of this modulo that.
   */
  def %(that: NumVal): NumVal = {
    if (that == 0) NumVal.Dnan
    else {
      val (na, nb) = normalizePair(this, that)
      (na, nb) match {
        case (m: BooleanVal, n: BooleanVal) => m.impl_%(n)
        case (m: IntVal, n: IntVal) => m.impl_%(n)
        case (m: FloatVal, n: FloatVal) => m.impl_%(n)
        case (m: DoubleVal, n: DoubleVal) => m.impl_%(n)
        case (m: ByteVal, n: ByteVal) => m.impl_%(n)
        case (m: CharVal, n: CharVal) => m.impl_%(n)
        case (m: ShortVal, n: ShortVal) => m.impl_%(n)
        case (m: LongVal, n: LongVal) => m.impl_%(n)
        case (m: BigIntVal, n: BigIntVal) => m.impl_%(n)
        case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_%(n)
        case _ => Unit.asInstanceOf[NumVal]
      }
    }
  }

  /**
   * <!-- pow --> Power.
   * @param that The exponent.
   * @return The value of this raised to the power that.
   */
  def pow(that: NumVal): NumVal = {
    val (na, nb) = normalizePair(this, that)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.impl_pow(n)
      case (m: IntVal, n: IntVal) => m.impl_pow(n)
      case (m: FloatVal, n: FloatVal) => m.impl_pow(n)
      case (m: DoubleVal, n: DoubleVal) => m.impl_pow(n)
      case (m: ByteVal, n: ByteVal) => m.impl_pow(n)
      case (m: CharVal, n: CharVal) => m.impl_pow(n)
      case (m: ShortVal, n: ShortVal) => m.impl_pow(n)
      case (m: LongVal, n: LongVal) => m.impl_pow(n)
      case (m: BigIntVal, n: BigIntVal) => m.impl_pow(n)
      case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_pow(n)
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- log --> Logarithm.
   * @param base The base.
   * @return The log base that of this.
   */
  def log(base: NumVal): NumVal = {
    val (na, nb) = normalizePair(this, base)
    (na, nb) match {
      case (m: BooleanVal, n: BooleanVal) => m.impl_log(n)
      case (m: IntVal, n: IntVal) => m.impl_log(n)
      case (m: FloatVal, n: FloatVal) => m.impl_log(n)
      case (m: DoubleVal, n: DoubleVal) => m.impl_log(n)
      case (m: ByteVal, n: ByteVal) => m.impl_log(n)
      case (m: CharVal, n: CharVal) => m.impl_log(n)
      case (m: ShortVal, n: ShortVal) => m.impl_log(n)
      case (m: LongVal, n: LongVal) => m.impl_log(n)
      case (m: BigIntVal, n: BigIntVal) => m.impl_log(n)
      case (m: BigDecimalVal, n: BigDecimalVal) => m.impl_log(n)
      case _ => Unit.asInstanceOf[NumVal]
    }
  }

  /**
   * <!-- hashCode --> Hash code.
   */
  override def hashCode = x.hashCode

  /**
   * <!-- canEqual --> Determine whether another object can be compared with this.  NumVal subclasses should override for additional state.
   * @param other The value to compare for equality.
   * @return Whether a comparison is valid.
   */
  def canEqual(other: Any) = other.isInstanceOf[NumVal]

  /**
   * <!-- equals --> Compare this value with another for equality.
   * @param other The value to compare for equality.
   * @return Whether other equals this.
   */
  override def equals(other: Any) = other match {
    case that: NumVal => {
      if (that canEqual this) {
        if (!this.isInfinity && !that.isInfinity) {
          val (me, you) = normalizePair(this, that)
          me.x == you.x
        } else if (!this.isInfinity || !that.isInfinity) false
        else (this.sign == that.sign)
      } else false
    }
    case that: Byte => equals(NumVal(that))
    case that: Char => equals(NumVal(that))
    case that: Short => equals(NumVal(that))
    case that: Int => equals(NumVal(that))
    case that: Long => equals(NumVal(that))
    case that: Float => equals(NumVal(that))
    case that: Double => equals(NumVal(that))
    case that: BigInt => equals(NumVal(that))
    case that: BigDecimal => equals(NumVal(that))
    case _ => false
  }

  /**
   *  <!-- className --> Obtain the immediate class name of this NumVal.
   * @return The class name of this instance.
   */
  private val className = { // the concrete class name
    val sa = this.getClass.getName.split('.')
    sa(sa.length - 1)
  }

  /**
   * <!-- toTypeString --> Create a string of the form <type>(<value>).
   * @return The type and value string.
   */
  def toTypeString =
    className + "(" +
      (x match { case t: Char => t.toInt.toString case _ => x.toString }) + ")"

  /**
   *  <!-- toString --> Give the numeric value as a String.
   * @return The string value.
   */
  override def toString() = x match { case t: Char => t.toInt.toString case _ => x.toString }

} // end trait NumVal

/**
 * <!-- NumVal --> Companion object with factory methods and implicit conversions.  For more information, see the trait documentation.
 */
object NumVal {

  private[numval] val MaxIntBitLength = BigInt(Int.MaxValue).bitLength // Int precision
  private[numval] val MaxLongBitLength = BigInt(Long.MaxValue).bitLength // Long precision
  private[numval] val RoundFloat = java.math.MathContext.DECIMAL32
  private[numval] val RoundDouble = java.math.MathContext.DECIMAL64

  // Since BooleanVal constant (val) fields are initialized using I0 and I1,
  // these must be initialized BEFORE the BooleanVal constructor is called,
  // otherwise null references result!!
  private[numval] val I0 = new IntVal(0) // singleton int zero
  private[numval] val I1 = new IntVal(1) // singleton int one
  private[numval] val PosInf = NumVal(Double.PositiveInfinity)
  private[numval] val NegInf = NumVal(Double.NegativeInfinity)


  // BooleanVal initializations must come AFTER initializing I0 and I1, otherwise null references result!!
  private[numval] lazy val T = new BooleanVal(true) // singleton true
  private[numval] lazy val F = new BooleanVal(false) // singleton false
  private[numval] lazy val Fnan = new FloatVal(Float.NaN) // singleton Float NaN
  private[numval] lazy val Dnan = new DoubleVal(Double.NaN) // singleton Double NaN

  private[numval] lazy val Bi0 = new BigInt(java.math.BigInteger.ZERO) // singleton BigInt zero
  //    private[numval] val Bi1 = new BigInt(java.math.BigInteger.ONE)      // singleton BigInt one
  private[numval] lazy val Bd0 = new BigDecimal(java.math.BigDecimal.ZERO) // singleton BigDecimal zero
  private[numval] lazy val Bd1 = new BigDecimal(java.math.BigDecimal.ONE) // singleton BigDecimal one

  /**
   * <!-- apply --> Factory method to wrap Boolean.
   */
  def apply(x: Boolean): NumVal = if (x) T else F // factory methods

  /**
   * <!-- apply --> Factory method to wrap Byte.
   */
  def apply(x: Byte): NumVal = ByteVal(x)

  /**
   * <!-- apply --> Factory method to wrap Char.
   */
  def apply(x: Char): NumVal = CharVal(x)

  /**
   * <!-- apply --> Factory method to wrap Short.
   */
  def apply(x: Short): NumVal = ShortVal(x)

  /**
   * <!-- apply --> Factory method to wrap Int.
   */
  def apply(x: Int): NumVal = IntVal(x)

  /**
   * <!-- apply --> Factory method to wrap Long.
   */
  def apply(x: Long): NumVal = LongVal(x)

  /**
   * <!-- apply --> Factory method to wrap Float.
   */
  def apply(x: Float): NumVal = FloatVal(x)

  /**
   * <!-- apply --> Factory method to wrap Double.
   */
  def apply(x: Double): NumVal = DoubleVal(x)

  /**
   * <!-- apply --> Factory method to wrap BigInt.
   */
  def apply(x: BigInt): NumVal = BigIntVal(x)

  /**
   * <!-- apply --> Factory method to wrap BigDecimal.
   */
  def apply(x: BigDecimal): NumVal = BigDecimalVal(x)

  import scala.language.implicitConversions

  implicit def boolean2val(x: Boolean) = NumVal(x) // implicit conversions
  implicit def byte2Val(x: Byte) = NumVal(x)
  implicit def char2val(x: Char) = NumVal(x)
  implicit def short2val(x: Short) = NumVal(x)
  implicit def int2val(x: Int) = NumVal(x)
  implicit def long2val(x: Long) = NumVal(x)
  implicit def float2val(x: Float) = NumVal(x)
  implicit def double2val(x: Double) = NumVal(x)
  implicit def bigInt2val(x: BigInt) = NumVal(x)
  implicit def bigDecimal2val(x: BigDecimal) = NumVal(x)

  /**
   * <!-- implicit object NumValNumeric --> Supplies an implicit Numeric parameter for functions such as List.sum
   */
  implicit object NumValNumeric extends Numeric[NumVal] {
    val num = NumVal
    def fromInt(x: Int):NumVal = NumVal(x)
    def minus(x: NumVal, y: NumVal):NumVal = x - y
    def negate(x: NumVal):NumVal = -x
    def plus(x: NumVal, y: NumVal):NumVal = x + y
    def times(x: NumVal, y: NumVal):NumVal = x * y
    def toDouble(x: NumVal):Double = x.toDouble
    def toFloat(x: NumVal):Float = x.toFloat
    def toInt(x: NumVal):Int = x.toInt
    def toLong(x: NumVal):Long = x.toLong
    def compare(x:NumVal,y:NumVal): Int = x.compare(y)
  }

  /**
   * <!-- smallestAccurateInt --> Find the smallest integer representation of a BigDecimal that preserves its precision.
   * @param bi The input BigDecimal value.
   * @return The smallest accurate integer result.
   */
  private[numval] def smallestAccurateInt(bi: BigInt): NumVal = {
    val bitLength = bi.bitLength // prefer smallest accurate representation
    if (bitLength > MaxLongBitLength) bi else if (bi.bitLength > MaxIntBitLength) bi.toLong else bi.toInt
  }

  /**
   * <!-- limitInt --> Limit an Int value to Short range
   * @param i The input Int value.
   * @return The limited Int result.
   */
  private[numval] def limitInt(i: Int): Int = if (i < Short.MinValue) Short.MinValue else if (i > Short.MaxValue) Short.MaxValue else i

  /**
   * <!-- magInt --> The base 2 magnitude of an integer
   * @param x The input Int value.
   * @return The magnitude.
   */
  private[numval] def magInt(x: Int) = 31 - Integer.numberOfLeadingZeros(if (x < 0) -x else x)

  /**
   * <!-- rndDouble --> Accurately round a double value.
   * @param x The input value.
   * @return The rounded Double result.
   */
  private[numval] def rndDouble(x: Double) = BigDecimal(x).round(RoundDouble).doubleValue

  /**
   * <!-- safeLog --> Give the Double log of x to a given base, with sanity checks
   * @param x The value (power) for which to obtain the log.
   * @param base The log base.
   * @return The Double log result
    */
  private[numval] def safeLog(x: Double, base: Double): Double = {
    (x,base) match {    // perform sanity checks for special values of x and base
      case (a, b) if a < 0.0 => Double.NaN
      case (0.0, b) => Double.NegativeInfinity
      case (1.0, b) => 0.0
      case (a, b) if a == b => 1.0
      case (a, b) if b <= 0.0 => Double.NaN
      case (a, 1.0) => Double.NaN
      case (a, Double.PositiveInfinity) => Double.NaN
      case (Double.PositiveInfinity, b) => Double.PositiveInfinity
      case (a, b) => math.log(a) / math.log(b)
    }
  }

  /**
   * <!-- safePow --> Give the Double-precision power of x raised to a given exponent, with sanity checks.
   * @param x The value (base) to raise to the given exponent.
   * @param expon The exponent.
   * @return The Double power result
   */
  private[numval] def safePow(x: Double, expon: Double): Double = {
    if (expon == 0.0) 1.0
    else if (expon == 1.0) x
    else if (x < 0.0) {
      if (expon.isInfinite)
        if (x > -1.0)
          if (expon > 0) 0.0    // positive infinite power of a fraction yields zero
          else Double.NaN        // can't find sign of neg x raised to inf expon (even or odd?)
        else if (x == -1) Double.NaN  // can't find sign of -1 raised to inf expon (even or odd?)
        else
          if (expon < 0) 0.0    // negative (reciprocal) infinite power of x < -1 yields zero
          else Double.NaN        // can't find sign of neg x raised to inf expon (even or odd?)
      else {
        val intExp = expon.toLong
        if (expon != intExp) Double.NaN    // fract exponent of neg number; no support (yet) for imaginary numbers
        else {    // x is < 0, expon is a non-zero and non-infinite integer
          val oddExp = (intExp & 1) == 1
          (x,intExp) match {
            case (-1, b) => if (oddExp) -1.0 else 1.0
            case (Double.NegativeInfinity, b) =>
              if (b < 0.0) 0.0
              else if (oddExp) Double.NegativeInfinity    // odd exponent preserves sign
              else Double.PositiveInfinity
            case (a, b) => math.pow(x,expon)
          }
        }
      }
    } else    // x is positive or 0
      (x,expon) match {
        case (0.0, b) => if (b > 0.0) 0.0 else Double.PositiveInfinity
        case (1.0, b) => 1.0
        case (a, Double.NegativeInfinity) => if (a > 1.0) 0.0 else Double.PositiveInfinity
        case (a, Double.PositiveInfinity) => if (a > 1.0) Double.PositiveInfinity else 0.0
        case (Double.PositiveInfinity, b) => if (b > 0.0) Double.PositiveInfinity else 0.0
        case (a, b) => math.pow(x,expon)
      }
  }

  /**
   * <!-- bigIntPow --> Raise a BigInt base to a given BigInt exponent.
   * @param value The base value to raise to the exponent.
   * @param exp The exponent.
   * @return The rounded BigInt result.
   */
  private[numval] def bigIntPow(value: BigInt, expon: BigInt):NumVal = {
    if (expon == 0) I1
    else if (expon == 1.0) value
    else {
      val (negExp, absExp) = if (expon < 0) (true, -expon) else (false, expon)
      val (negVal, absVal) = if (value < 0) (true, -value) else (false, value)
      if (negVal) {
        val oddExp = ((expon & 1) == 1)
        if (value == -1) if (oddExp) -I1 else I1
        else {
          val power = Transcendental.bigIntPow(BigDecimal(absVal), absExp)
          (power,negExp,oddExp) match {
            case (bd:BigDecimalVal,true,false) => Bd1(bd.x.mc)/bd
            case (bd:BigDecimalVal,true,true) => -Bd1(bd.x.mc)/bd    // restore neg sign if odd exponent
            case (bd:BigDecimalVal,false,false) => bd
            case (bd:BigDecimalVal,false,true) => -bd    // restore neg sign if odd exponent
            case (PosInf,true,z) => I0
            case (PosInf,false,false) => power
            case (PosInf,false,true) => -power        // restore neg sign if odd exponent
            case (x,y,z) => x
          }
        }
      } else if (value == 0)
        if (negExp) PosInf else I0
      else if (value == 1) I1
      else {    // pos value
        val power = Transcendental.bigIntPow(BigDecimal(absVal), absExp)
        (power,negExp) match {
          case (bd:BigDecimalVal,true) => Bd1(bd.x.mc) / bd
          case (bd:BigDecimalVal,false) => bd
          case (PosInf,true) => I0
          case (PosInf,false) => PosInf
          case (x,y) => x
        }
      }
    }
  }

}    // NumVal companion object

private[numval] case class BooleanVal(x: Boolean) extends NumVal {
  val toByte: Byte = if (x) 1 else 0
  val toShort: Short = if (x) 1 else 0
  val toInt: Int = if (x) 1 else 0
  val toLong: Long = if (x) 1 else 0
  val toFloat: Float = if (x) 1 else 0
  val toDouble: Double = if (x) 1 else 0
  val toBigInt: BigInt = if (x) 1 else 0
  val toBigDecimal: BigDecimal = if (x) 1 else 0

  import NumVal.{ I0, I1 }
  val intPart: NumVal = if (x) I1 else I0
  val fracPart: NumVal = I0
  val abv: NumVal = intPart
  val sign: Int = if (x) 1 else 0
  val round: NumVal = intPart
  val log2: NumVal = if (x) I0 else Double.NegativeInfinity
  val log10: NumVal = if (x) I0 else Double.NegativeInfinity
  val ln: NumVal = if (x) I0 else Double.NegativeInfinity
  val exp: NumVal = if (x) math.E else 1
  def sin: NumVal = if (x) math.sin(1) else I0
  def cos: NumVal = if (x) math.cos(1) else I1
  def tan: NumVal = if (x) math.tan(1) else I0
  def asin: NumVal = if (x) math.asin(1) else I0
  def acos: NumVal = if (x) I0 else math.acos(0)
  def atan: NumVal = if (x) math.atan(1) else I0
  val ulp: NumVal = I1
  val unary_+ : NumVal = this
  val unary_- : NumVal = if (x) -1 else I0
  val unary_~ : NumVal = if (x) I0 else I1

  import NumVal.smallestAccurateInt

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(BigInt(if (x) 1 else 0) << shift)
  def impl_>>>(shift: Int): NumVal = (if (x) 1l else 0l) >>> shift
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(BigInt(if (x) 1 else 0) >> shift)
  def impl_|(that: BooleanVal): NumVal = x || that.x
  def impl_&(that: BooleanVal): NumVal = x && that.x
  def impl_^(that: BooleanVal): NumVal = (x || that.x) && !(x && that.x)
  def impl_+(that: BooleanVal): NumVal = if (x && that.x) 2 else (x || that.x)
  def impl_-(that: BooleanVal): NumVal = if (x) !that.x else if (that.x) -1 else NumVal.F
  def impl_*(that: BooleanVal): NumVal = x && that.x // this times that
  def impl_/(that: BooleanVal): NumVal = if (that.x) x else if (x) Double.PositiveInfinity else NumVal.F
  def impl_div(that: BooleanVal): BooleanVal = this
  def impl_%(that: BooleanVal): NumVal = if (that.x) NumVal.F else Double.NaN
  def impl_pow(expon: BooleanVal): NumVal = if (!x && expon.x) NumVal.F else NumVal.T
  def impl_log(base: BooleanVal): NumVal = if (x) NumVal.F else Double.NegativeInfinity
}

private[numval] case class ByteVal(x: Byte) extends NumVal {
  def toByte: Byte = x
  def toShort: Short = x
  def toInt: Int = x
  def toLong: Long = x
  def toFloat: Float = x
  def toDouble: Double = x
  def toBigInt: BigInt = x
  def toBigDecimal: BigDecimal = x

  def intPart: NumVal = this
  def fracPart: NumVal = NumVal.I0
  def abv: NumVal = if (x == Byte.MinValue) x.shortValue.abs else if (x < 0) x.abs else this // if underflow, promote to Short
  def sign: Int = if (x == 0) 0 else if (x < 0) -1 else 1
  def round: NumVal = this
  def log2: NumVal = NumVal.safeLog(x,2)
  def log10: NumVal = math.log10(x)
  def ln: NumVal = math.log(x)
  def exp: NumVal = math.exp(x)
  def sin: NumVal = math.sin(x)
  def cos: NumVal = math.cos(x)
  def tan: NumVal = math.tan(x)
  def asin: NumVal = math.asin(x)
  def acos: NumVal = math.acos(x)
  def atan: NumVal = math.atan(x)
  def ulp: NumVal = NumVal.I1
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~x

  import NumVal.{ safePow, smallestAccurateInt }

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(BigInt(x) << shift)
  def impl_>>>(shift: Int): NumVal = x.longValue >>> shift
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(BigInt(x) >> shift)
  def impl_|(that: ByteVal): NumVal = x | that.x
  def impl_&(that: ByteVal): NumVal = x & that.x
  def impl_^(that: ByteVal): NumVal = x ^ that.x
  def impl_+(that: ByteVal): NumVal = x + that.x
  def impl_-(that: ByteVal): NumVal = x - that.x
  def impl_*(that: ByteVal): NumVal = x * that.x
  def impl_/(that: ByteVal): NumVal = x.floatValue / that.x
  def impl_div(that: ByteVal): NumVal = (x / that.x).shortValue
  def impl_%(that: ByteVal): NumVal = if (that.x == 0) Double.NaN else (x % that.x).byteValue
  def impl_pow(expon: ByteVal): NumVal = safePow(x, expon.x)
  def impl_log(base: ByteVal): NumVal = NumVal.safeLog(x,base.x)
}

private[numval] case class CharVal(x: Char) extends NumVal {
	import runtime.RichChar
  def toByte: Byte = { val ch:RichChar = x; ch.byteValue }
  def toShort: Short = { val ch:RichChar = x; ch.shortValue }
  def toInt: Int = x
  def toLong: Long = x
  def toFloat: Float = x
  def toDouble: Double = x
  def toBigInt: BigInt = x
  def toBigDecimal: BigDecimal = x

  def intPart: NumVal = this
  def fracPart: NumVal = NumVal.I0
  def abv: NumVal = x.toInt.abs
  def sign: Int = { val i = x.toInt; if (i == 0) 0 else if (i < 0) -1 else 1 }
  def round: NumVal = this
  def log2: NumVal = NumVal.safeLog(x,2)
  def log10: NumVal = math.log10(x)
  def ln: NumVal = math.log(x)
  def exp: NumVal = math.exp(x)
  def sin: NumVal = math.sin(x)
  def cos: NumVal = math.cos(x)
  def tan: NumVal = math.tan(x)
  def asin: NumVal = math.asin(x)
  def acos: NumVal = math.acos(x)
  def atan: NumVal = math.atan(x)
  def ulp: NumVal = NumVal.I1
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~x

  import NumVal.{ magInt, safePow, smallestAccurateInt }

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(BigInt(x) << shift)
  def impl_>>>(shift: Int): NumVal = { val ch:RichChar = x; ch.longValue >>> shift }
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(BigInt(x) >> shift)
  def impl_|(that: CharVal): NumVal = x | that.x
  def impl_&(that: CharVal): NumVal = x & that.x
  def impl_^(that: CharVal): NumVal = x ^ that.x
  def impl_+(that: CharVal): NumVal = x + that.x
  def impl_-(that: CharVal): NumVal = x - that.x
  def impl_*(that: CharVal): NumVal = if (magInt(x) + magInt(that.x) < 30) x * that.x else x.toLong * that.x
  def impl_/(that: CharVal): NumVal = { val ch:RichChar = x; ch.floatValue / that.x }
  def impl_div(that: CharVal): NumVal = (x / that.x).toChar
  def impl_%(that: CharVal): NumVal = if (that.x == 0) Double.NaN else (x % that.x).toChar
  def impl_pow(expon: CharVal): NumVal = safePow(x, expon.x)
  def impl_log(base: CharVal): NumVal = NumVal.safeLog(x,base.x)
}

private[numval] case class ShortVal(x: Short) extends NumVal {
  def toByte: Byte = x.byteValue
  def toShort: Short = x
  def toInt: Int = x
  def toLong: Long = x
  def toFloat: Float = x
  def toDouble: Double = x
  def toBigInt: BigInt = x
  def toBigDecimal: BigDecimal = x

  def intPart: NumVal = this
  def fracPart: NumVal = NumVal.I0
  def abv: NumVal = if (x == Short.MinValue) x.intValue.abs else if (x < 0) x.abs else this // if underflow, promote to Int
  def sign: Int = if (x == 0) 0 else if (x < 0) -1 else 1
  def round: NumVal = this
  def log2: NumVal = NumVal.safeLog(x,2)
  def log10: NumVal = math.log10(x)
  def ln: NumVal = math.log(x)
  def exp: NumVal = math.exp(x)
  def sin: NumVal = math.sin(x)
  def cos: NumVal = math.cos(x)
  def tan: NumVal = math.tan(x)
  def asin: NumVal = math.asin(x)
  def acos: NumVal = math.acos(x)
  def atan: NumVal = math.atan(x)
  def ulp: NumVal = NumVal.I1
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~x

  import NumVal.{ safePow, smallestAccurateInt }

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(BigInt(x) << shift)
  def impl_>>>(shift: Int): NumVal = x.longValue >>> shift
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(BigInt(x) >> shift)
  def impl_|(that: ShortVal): NumVal = x | that.x
  def impl_&(that: ShortVal): NumVal = x & that.x
  def impl_^(that: ShortVal): NumVal = x ^ that.x
  def impl_+(that: ShortVal): NumVal = x + that.x
  def impl_-(that: ShortVal): NumVal = x - that.x
  def impl_*(that: ShortVal): NumVal = x * that.x
  def impl_/(that: ShortVal): NumVal = x.floatValue / that.x
  def impl_div(that: ShortVal): NumVal = x / that.x
  def impl_%(that: ShortVal): NumVal = if (that.x == 0) Double.NaN else (x % that.x).shortValue
  def impl_pow(expon: ShortVal): NumVal = safePow(x, expon.x)
  def impl_log(base: ShortVal): NumVal = NumVal.safeLog(x,base.x)
}

private[numval] case class IntVal(x: Int) extends NumVal {
  def toByte: Byte = x.byteValue
  def toShort: Short = x.shortValue
  def toInt: Int = x
  def toLong: Long = x
  def toFloat: Float = x
  def toDouble: Double = x
  def toBigInt: BigInt = x
  def toBigDecimal: BigDecimal = x

  def intPart: NumVal = this
  def fracPart: NumVal = NumVal.I0
  def abv: NumVal = if (x == Int.MinValue) x.longValue.abs else if (x < 0) x.abs else this // if underflow, promote to Long
  def sign: Int = if (x == 0) 0 else if (x < 0) -1 else 1
  def round: NumVal = this
  def log2: NumVal = NumVal.safeLog(x,2)
  def log10: NumVal = math.log10(x)
  def ln: NumVal = math.log(x)
  def exp: NumVal = math.exp(x)
  def sin: NumVal = math.sin(x)
  def cos: NumVal = math.cos(x)
  def tan: NumVal = math.tan(x)
  def asin: NumVal = math.asin(x)
  def acos: NumVal = math.acos(x)
  def atan: NumVal = math.atan(x)
  def ulp: NumVal = NumVal.I1
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~x

  import NumVal.{ magInt, safePow, smallestAccurateInt }

  override def %(that: NumVal): NumVal =
    if (that.rep == NumType.float) {
      val u = this.x
      val v = that.toFloat
      val res = u % v
      res
//      this.x % that.toFloat // preserve precision of that
    }
    else super.%(that)

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(BigInt(x) << shift)
  def impl_>>>(shift: Int): NumVal = x.longValue >>> shift
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(BigInt(x) >> shift)
  def impl_|(that: IntVal): NumVal = x | that.x
  def impl_&(that: IntVal): NumVal = x & that.x
  def impl_^(that: IntVal): NumVal = x ^ that.x
  def impl_+(that: IntVal): NumVal = {
    val s = x + that.x;
    if ((Int.MinValue & (x ^ that.x)) == 0 && (Int.MinValue & (x ^ s)) != 0)
      x.longValue + that.x
    else s
  }
  def impl_-(that: IntVal): NumVal = {
    val d = x - that.x;
    if ((Int.MinValue & (x ^ that.x)) != 0 && (Int.MinValue & (x ^ d)) != 0)
      x.longValue - that.x
    else d
  }
  def impl_*(that: IntVal): NumVal = if (magInt(x) + magInt(that.x) < 30) x * that.x else x.toLong * that.x
  def impl_/(that: IntVal): NumVal = x.doubleValue / that.x
  def impl_div(that: IntVal): NumVal = x / that.x
  def impl_%(that: IntVal): NumVal = if (that.x == 0) Double.NaN else x % that.x
  def impl_pow(expon: IntVal): NumVal = safePow(x, expon.x)
  def impl_log(base: IntVal): NumVal = NumVal.safeLog(x,base.x)
}

private[numval] case class LongVal(x: Long) extends NumVal {
  def toByte: Byte = x.byteValue
  def toShort: Short = x.shortValue
  def toInt: Int = x.intValue
  def toLong: Long = x
  def toFloat: Float = x
  def toDouble: Double = x
  def toBigInt: BigInt = x
  def toBigDecimal: BigDecimal = x

  def intPart: NumVal = this
  def fracPart: NumVal = NumVal.I0
  def abv: NumVal = if (x == Long.MinValue) BigInt(x).abs else if (x < 0L) x.abs else this // if underflow, promote to BigInt
  def sign: Int = if (x == 0) 0 else if (x < 0) -1 else 1
  def round: NumVal = this
  def log2: NumVal = NumVal.safeLog(x,2)
  def log10: NumVal = math.log10(x)
  def ln: NumVal = math.log(x)
  def exp: NumVal = math.exp(x)
  def sin: NumVal = math.sin(x)
  def cos: NumVal = math.cos(x)
  def tan: NumVal = math.tan(x)
  def asin: NumVal = math.asin(x)
  def acos: NumVal = math.acos(x)
  def atan: NumVal = math.atan(x)
  def ulp: NumVal = 1L
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~x

  import NumVal.{ bigIntPow, safePow, smallestAccurateInt }

  private def magLong(x: Long) = 63 - java.lang.Long.numberOfLeadingZeros(if (x < 0) -x else x)

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(BigInt(x) << shift)
  def impl_>>>(shift: Int): NumVal = x >>> shift
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(BigInt(x) >> shift)
  def impl_|(that: LongVal): NumVal = x | that.x
  def impl_&(that: LongVal): NumVal = x & that.x
  def impl_^(that: LongVal): NumVal = x ^ that.x
  def impl_+(that: LongVal): NumVal = {
    val s = x + that.x;
    if ((Long.MinValue & (x ^ that.x)) == 0 && (Long.MinValue & (x ^ s)) != 0)
      BigInt(x) + that.x
    else s
  }
  def impl_-(that: LongVal): NumVal = {
    val d = x - that.x;
    if ((Long.MinValue & (x ^ that.x)) != 0 && (Long.MinValue & (x ^ d)) != 0)
      BigInt(x) - that.x
    else d
  }
  def impl_*(that: LongVal): NumVal = if (magLong(x) + magLong(that.x) < 62) x * that.x else BigInt(x) * that.x
  def impl_/(that: LongVal): NumVal = x.doubleValue / that.x
  def impl_div(that: LongVal): NumVal = x / that.x
  def impl_%(that: LongVal): NumVal = if (that.x == 0) Double.NaN else x % that.x
  def impl_pow(expon: LongVal): NumVal = bigIntPow(BigInt(x), BigInt(expon.x))
  def impl_log(base: LongVal): NumVal = NumVal.safeLog(x,base.x)
}

private[numval] case class FloatVal(x: Float) extends NumVal {
  import NumVal.{ Fnan, RoundDouble, safePow, smallestAccurateInt }

  def toByte: Byte = x.byteValue
  def toShort: Short = x.shortValue
  def toInt: Int = x.intValue
  def toLong: Long = x.longValue
  def toFloat: Float = x

  // TODO: Replace all BigDecimal(whatever.toString) .toDouble or .doubleValue with rndDouble(whatever,7)

  def toDouble: Double =
    if (x.isInfinity || x.isNaN) x.toDouble // can't convert to BigDec
    else BigDecimal(x.toString).toDouble // preserves accuracy of low order digits
  def toBigInt: BigInt = {
    if (!x.isInfinity) BigDecimal(x.toString).toBigInt
    else if (x.isPosInfinity) BigInt(Long.MaxValue) else BigInt(Long.MinValue)
  }
  def toBigDecimal: BigDecimal = if (!x.isInfinity) BigDecimal(x.toString) else if (x.isNegInfinity) Double.MinValue else Double.MaxValue

  override def isNaN: Boolean = x.isNaN
  override def isInfinity: Boolean = x.isInfinity
  override def isPosInfinity: Boolean = x.isPosInfinity
  override def isNegInfinity: Boolean = x.isNegInfinity

  def intPart: NumVal =
    if (!x.isInfinity) smallestAccurateInt(BigDecimal(x.toString).toBigInt)
    else if (x.isPosInfinity) BigInt(Long.MaxValue) else BigInt(Long.MinValue)
  def fracPart: NumVal = if (x.isInfinity) NumVal.I0 else { val bd = BigDecimal(x.toString); (bd - BigDecimal(bd.toBigInt)).floatValue }
  def abv: NumVal = if (x < 0.0f) x.abs else this
  def sign: Int = if (x == 0.0f) 0 else if (x < 0) -1 else 1

  def round: NumVal =
    if (!x.isInfinity)
      smallestAccurateInt(BigDecimal(x.toString).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt)
    else this

  def log2: NumVal = NumVal.safeLog(toDouble,2)
  def log10: NumVal = math.log10(toDouble)
  def ln: NumVal = math.log(toDouble)
  def exp: NumVal = math.exp(toDouble)
  def sin: NumVal = math.sin(toDouble)
  def cos: NumVal = math.cos(toDouble)
  def tan: NumVal = math.tan(toDouble)
  def asin: NumVal = math.asin(toDouble)
  def acos: NumVal = math.acos(toDouble)
  def atan: NumVal = math.atan(toDouble)
  def ulp: NumVal = math.ulp(x)
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~intPart

  def impl_<<(shift: Int): NumVal = if (x.isInfinity) this else smallestAccurateInt(BigDecimal(x.toString).toBigInt << shift)
  def impl_>>>(shift: Int): NumVal = if (x.isInfinity) this else (x.longValue >>> shift)
  def impl_>>(shift: Int): NumVal = if (x.isInfinity) this else smallestAccurateInt(BigDecimal(x.toString).toBigInt >> shift)
  def impl_|(that: FloatVal): NumVal =
    if (x.isInfinity || that.x.isInfinity) Fnan
    else smallestAccurateInt(BigDecimal(x.toString).toBigInt | BigDecimal(that.x.toString).toBigInt)
  def impl_&(that: FloatVal): NumVal =
    if (x.isInfinity || that.x.isInfinity) Fnan
    else smallestAccurateInt(BigDecimal(x.toString).toBigInt & BigDecimal(that.x.toString).toBigInt)
  def impl_^(that: FloatVal): NumVal =
    if (x.isInfinity || that.x.isInfinity) Fnan
    else smallestAccurateInt(BigDecimal(x.toString).toBigInt ^ BigDecimal(that.x.toString).toBigInt)

  // Override + - * / operators to handle NaN and infinity
  override def +(that: NumVal): NumVal =
    if (this.x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (x == that.x) this else 0.0f
      else this
    else super.+(that)
  override def -(that: NumVal): NumVal =
    if (this.x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (x == that.x) 0.0f else this
      else this
    else super.-(that)
  override def *(that: NumVal): NumVal =
    if (x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (that.isNegInfinity) -x else this
      else if (that == 0) Float.NaN else if (that < 0) -x else this
    else super.*(that)
  override def /(that: NumVal): NumVal =
    if (x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (x == that.x) 1.0f else -1.0f
      else if (that < 0) -x else this
    else super./(that)
  override def log(base: NumVal): NumVal =
    if (x.isPosInfinity && base.rep == NumType.bigDec)
      if (base <= NumVal.Bd0 || base == NumVal.Bd1) Float.NaN
      else Float.PositiveInfinity
    else super.log(base)
  override def pow(expon: NumVal): NumVal =
    if (expon == 0) NumVal.I1
    else if (x.isNegInfinity && expon.rep.id >= NumType.bigInt.id)
      if (expon.fracPart != 0) Float.NaN
      else if (expon < 0) NumVal.I0
      else if ((expon & 1) == 1) x else Float.PositiveInfinity
    else if (x.isPosInfinity && expon.rep.id == NumType.bigDec.id)
      if (expon < 0) NumVal.I0 else Float.PositiveInfinity
    else super.pow(expon)

  def impl_+(that: FloatVal): NumVal = {
    val res = x + that.x
    if (!res.isInfinity) res else x.doubleValue + that.x.doubleValue
  }
  def impl_-(that: FloatVal): NumVal = {
    val res = x - that.x
    if (!res.isInfinity) res else x.doubleValue - that.x.doubleValue
  }

  def impl_*(that: FloatVal): NumVal = {
    val s = java.lang.Math.getExponent(x)
    val t = java.lang.Math.getExponent(that.x)
    val scale = s + t
    val as = if (scale < 0) -scale else scale
    if (as > 126) (BigDecimal(x.toString) * BigDecimal(that.x.toString)).round(RoundDouble).doubleValue // promote to avoid overflow
    else {
      val d = s - t
      val ad = if (d < 0) -d else d
      if (ad > 11) (BigDecimal(x.toString) * BigDecimal(that.x.toString)).round(RoundDouble).doubleValue // promote to preserve precision
      else x * that.x
    }
  }

  def impl_/(that: FloatVal): NumVal = {
    val d = java.lang.Math.getExponent(x) - java.lang.Math.getExponent(that.x)
    if (d < -125) (BigDecimal(x.toString) / BigDecimal(that.x.toString)).doubleValue // promote to avoid underflow
    else {
      val ad = if (d < 0) -d else d
      if (ad > 19) (BigDecimal(x.toString) / BigDecimal(that.x.toString)).doubleValue // promote to preserve precision
      else x / that.x
    }
  }

  def impl_div(that: FloatVal): NumVal = {
    if (that.x.isInfinity)
      if (x.isInfinity)
        if (x == that.x) 1
        else -1
      else 0
    else if (x.isInfinity) Double.NaN
    else {
      val thisX = BigDecimal(x.toString).toBigInt
      val thatX = BigDecimal(that.x.toString).toBigInt
      if (thatX == 0) DoubleVal(Double.NaN)
      else smallestAccurateInt(thisX / thatX)
    }
  }
  def impl_%(that: FloatVal): NumVal = if (that.x == 0.0f) Double.NaN else x % that.x
  def impl_pow(expon: FloatVal): NumVal = safePow(toDouble, expon.toDouble)
  def impl_log(base: FloatVal): NumVal = NumVal.safeLog(toDouble,base.toDouble)
}

private[numval] case class DoubleVal(x: Double) extends NumVal {
  import NumVal.{ Dnan, rndDouble, safePow, smallestAccurateInt }

  def toByte: Byte = x.byteValue
  def toShort: Short = x.shortValue
  def toInt: Int = x.intValue
  def toLong: Long = x.longValue
  def toFloat: Float = x.floatValue
  def toDouble: Double = x
  def toBigInt: BigInt = {
    if (!x.isInfinity) BigDecimal(x).toBigInt
    else if (x.isPosInfinity) BigInt(Long.MaxValue) else BigInt(Long.MinValue)
  }
  def toBigDecimal: BigDecimal = if (!x.isInfinity) x else if (x.isNegInfinity) Double.MinValue else Double.MaxValue

  override def isNaN: Boolean = x.isNaN
  override def isInfinity: Boolean = x.isInfinity
  override def isPosInfinity: Boolean = x.isPosInfinity
  override def isNegInfinity: Boolean = x.isNegInfinity

  def intPart: NumVal =
    if (!x.isInfinity) smallestAccurateInt(BigDecimal(x).toBigInt)
    else if (x.isPosInfinity) BigInt(Long.MaxValue) else BigInt(Long.MinValue)
  def fracPart: NumVal = if (x.isInfinity) NumVal.I0 else { val bd = BigDecimal(x); (bd - BigDecimal(bd.toBigInt)).doubleValue }
  def abv: NumVal = if (x < 0.0) x.abs else this
  def sign: Int = if (x == 0.0) 0 else if (x < 0) -1 else 1

  def round: NumVal =
    if (!x.isInfinity)
      smallestAccurateInt(BigDecimal(x).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt)
    else this

  def log2: NumVal = NumVal.safeLog(x,2)
  def log10: NumVal = math.log10(x)
  def ln: NumVal = math.log(x)
  def exp: NumVal = math.exp(x)
  def sin: NumVal = math.sin(x)
  def cos: NumVal = math.cos(x)
  def tan: NumVal = math.tan(x)
  def asin: NumVal = math.asin(x)
  def acos: NumVal = math.acos(x)
  def atan: NumVal = math.atan(x)
  def ulp: NumVal = math.ulp(x)
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~intPart

  def impl_<<(shift: Int): NumVal = if (x.isInfinity) this else smallestAccurateInt(BigDecimal(x).toBigInt << shift)
  def impl_>>>(shift: Int): NumVal = if (x.isInfinity) this else (x.longValue >>> shift)
  def impl_>>(shift: Int): NumVal = if (x.isInfinity) this else smallestAccurateInt(BigDecimal(x).toBigInt >> shift)
  def impl_|(that: DoubleVal): NumVal =
    if (x.isInfinity || that.x.isInfinity) Dnan
    else smallestAccurateInt(BigDecimal(x).toBigInt | BigDecimal(that.x).toBigInt)
  def impl_&(that: DoubleVal): NumVal =
    if (x.isInfinity || that.x.isInfinity) Dnan
    else smallestAccurateInt(BigDecimal(x).toBigInt & BigDecimal(that.x).toBigInt)
  def impl_^(that: DoubleVal): NumVal =
    if (x.isInfinity || that.x.isInfinity) Dnan
    else smallestAccurateInt(BigDecimal(x).toBigInt ^ BigDecimal(that.x).toBigInt)

  // Override + - * / operators to handle NaN and infinity
  override def +(that: NumVal): NumVal =
    if (this.x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (x == that.x) this else 0.0
      else this
    else super.+(that)
  override def -(that: NumVal): NumVal =
    if (this.x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (x == that.x) 0.0 else this
      else this
    else super.-(that)
  override def *(that: NumVal): NumVal =
    if (x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (that.isNegInfinity) -x else this
      else if (that == 0) Double.NaN else if (that < 0) -x else this
    else super.*(that)
  override def /(that: NumVal): NumVal =
    if (x.isNaN) this
    else if (x.isInfinity)
      if (that.isNaN) that
      else if (that.isInfinity)
        if (x == that.x) 1.0 else -1.0
      else if (that < 0) -x else this
    else super./(that)
  override def log(base: NumVal): NumVal =
    if (x.isPosInfinity && base.rep == NumType.bigDec)
      if (base <= NumVal.Bd0 || base == NumVal.Bd1) Double.NaN
      else Double.PositiveInfinity
    else super.log(base)
  override def pow(expon: NumVal): NumVal =
    if (expon == 0) NumVal.I1
    else if (x.isNegInfinity && expon.rep.id >= NumType.bigInt.id)
      if (expon.fracPart != 0) Double.NaN
      else if (expon < 0) NumVal.I0
      else if ((expon & 1) == 1) x else Double.PositiveInfinity
    else if (x.isPosInfinity && expon.rep.id == NumType.bigDec.id)
      if (expon < 0) NumVal.I0 else Double.PositiveInfinity
    else super.pow(expon)

  def impl_+(that: DoubleVal): NumVal = {
    val res = x + that.x
    if (!res.isInfinity) res else BigDecimal(x) + that.x
  }
  def impl_-(that: DoubleVal): NumVal = {
    val res = x - that.x
    if (!res.isInfinity) res else BigDecimal(x) - that.x
  }

  def impl_*(that: DoubleVal): NumVal = {
    val s = java.lang.Math.getExponent(x)
    val t = java.lang.Math.getExponent(that.x)
    val scale = s + t
    val as = if (scale < 0) -scale else scale
    if (as > 1022) BigDecimal(x) * that.x // promote to avoid overflow
    else {
      val d = s - t
      val ad = if (d < 0) -d else d
      if (ad > 26) BigDecimal(x) * that.x // promote to preserve precision
      /* Note: the following test, whether to rndDouble, was needed to squash this failed unit test case:
 * - testTimes *** FAILED ***
 *   associative failed. x:Boolean(true), y:Double(-4.9E-324),
 *   expectedErr:4.8020E-647,
 *   a:BooleanVal(true), b:DoubleVal(-4.9E-324), c:IntVal(2),
 *   (a*b): BigDecimalVal(-4.90E-324), (b*c): DoubleVal(-1.0E-323),
 *   (a*b)*c: BigDecimalVal(-9.80E-324), a*(b*c): BigDecimalVal(-1.00E-323)
 *   
 *   -9.80E-324 did not equal -1.00E-323 (TestNumVal.scala:3709)
 * 
 * TODO: Revisit this to see if there is a smarter way than rounding everything above var "as" value 253.
 * I.e. Can the problem be avoided before the * operator calls normalizePair (or inside normalizePair)? 
 */
      else if (as < 254) x * that.x
      else rndDouble(x * that.x)
    }
  }

  def impl_/(that: DoubleVal): NumVal = {
    val d = java.lang.Math.getExponent(x) - java.lang.Math.getExponent(that.x)
    if (d < -1021) BigDecimal(x) / that.x // promote to avoid underflow
    else {
      val ad = if (d < 0) -d else d
      if (ad > 48) BigDecimal(x) / that.x // promote to preserve precision
      else x / that.x
    }
  }

  def impl_div(that: DoubleVal): NumVal = {
    if (that.x.isInfinity)
      if (x.isInfinity)
        if (x == that.x) 1
        else -1
      else 0
    else if (x.isInfinity) Double.NaN
    else {
      val thisX = BigDecimal(x).toBigInt
      val thatX = BigDecimal(that.x).toBigInt
      if (thatX == 0) DoubleVal(Double.NaN)
      else smallestAccurateInt(thisX / thatX)
    }
  }
  def impl_%(that: DoubleVal): NumVal = if (that.x == 0.0f) Double.NaN else x % that.x
  def impl_pow(expon: DoubleVal): NumVal = safePow(x, expon.x)
  def impl_log(base: DoubleVal): NumVal = NumVal.safeLog(x,base.x)
}

private[numval] case class BigIntVal(x: BigInt) extends NumVal {
  import NumVal.{ bigIntPow, smallestAccurateInt }

  // TODO: Implement a generation counter to track the number of operations since smallestAccurateInt was called.
  // TODO: Document the generational demotion strategy ("Wash your hands! You don't know where that number has been.")

  def toByte: Byte = x.byteValue
  def toShort: Short = x.shortValue
  def toInt: Int = x.intValue
  def toLong: Long = x.longValue
  def toFloat: Float = x.floatValue
  def toDouble: Double = x.doubleValue
  def toBigInt: BigInt = x
  def toBigDecimal: BigDecimal = BigDecimal(x)

  def intPart: NumVal = this
  def fracPart: NumVal = NumVal.I0
  def abv: NumVal = if (x < NumVal.Bi0) x.abs else this
  def sign: Int = x.signum
  def round: NumVal = this
  def log2: NumVal = Transcendental.log2(BigDecimal(x))
  def log10: NumVal = Transcendental.log10(BigDecimal(x))
  def ln: NumVal = Transcendental.ln(BigDecimal(x))
  def exp: NumVal = Transcendental.exp(BigDecimal(x))
  def sin: NumVal = Transcendental.sin(BigDecimal(x))
  def cos: NumVal = Transcendental.cos(BigDecimal(x))
  def tan: NumVal = Transcendental.tan(BigDecimal(x))
  def asin: NumVal = Transcendental.asin(BigDecimal(x))
  def acos: NumVal = Transcendental.acos(BigDecimal(x))
  def atan: NumVal = Transcendental.atan(BigDecimal(x))
  def ulp: NumVal = NumVal.I1
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~x

  override def %(that: NumVal): NumVal =
    if (that.isInfinity) this // preserve precision of this (don't demote to that type)
    else super.%(that)

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(x << shift)
  def impl_>>>(shift: Int): NumVal = x.longValue >>> shift
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(x >> shift)
  def impl_|(that: BigIntVal): NumVal = x | that.x
  def impl_&(that: BigIntVal): NumVal = x & that.x
  def impl_^(that: BigIntVal): NumVal = x ^ that.x
  def impl_+(that: BigIntVal): NumVal = x + that.x
  def impl_-(that: BigIntVal): NumVal = x - that.x
  def impl_*(that: BigIntVal): NumVal = x * that.x
  def impl_/(that: BigIntVal): NumVal =
    if (x == 0) NumVal.I0
    else if (that.x == 0)
      if (x > 0) Double.PositiveInfinity
      else Double.NegativeInfinity
    else BigDecimal(x) / BigDecimal(that.x)
  def impl_div(that: BigIntVal): NumVal = x / that.x
  def impl_%(that: BigIntVal): NumVal = x % that.x
  def impl_pow(expon: BigIntVal): NumVal = bigIntPow(x,expon.x)
  def impl_log(base: BigIntVal): NumVal = Transcendental.log(BigDecimal(x), BigDecimal(base.x))
}

private[numval] case class BigDecimalVal(x: BigDecimal) extends NumVal {
  import NumVal.{ Bd0, Bd1, I0, I1, smallestAccurateInt }

  // TODO: Implement a generation counter to track the number of operations since smallestAccurateFloat was called.
  // TODO: Document the generational demotion strategy ("Wash your hands! You don't know where that number has been.")

  def toByte: Byte = x.byteValue
  def toShort: Short = x.shortValue
  def toInt: Int = x.intValue
  def toLong: Long = x.longValue
  def toFloat: Float = x.floatValue
  def toDouble: Double = x.doubleValue
  def toBigInt: BigInt = xint.toBigInt
  def toBigDecimal: BigDecimal = x

  private[numval] def xfrac = x.remainder(Bd1(x.mc))  // avoid infinite recursion; avoid calling public defs
  private[numval] def xint = x - x.remainder(Bd1(x.mc))

  def intPart: NumVal = smallestAccurateInt(xint.toBigInt)
  def fracPart: NumVal = xfrac
  def abv: NumVal = if (x < 0) x.abs else this
  def sign: Int = x.signum

  def round: NumVal =  // x.round(x.mc) << doesn't work for tiny x
  {    // round half up
    val (negx, absx) = if (x < 0) (true, -x) else (false, x)
    val frac = absx.remainder(Bd1(x.mc))
    val result = if (frac < BigDecimal("0.5")) absx - frac else absx - frac + 1
    if (negx) -result else result
  }

  def log2: NumVal = Transcendental.log2(x)
  def log10: NumVal = Transcendental.log10(x)
  def ln: NumVal = Transcendental.ln(x)
  def exp: NumVal = Transcendental.exp(x)
  def sin: NumVal = Transcendental.sin(x)
  def cos: NumVal = Transcendental.cos(x)
  def tan: NumVal = Transcendental.tan(x)
  def asin: NumVal = Transcendental.asin(x)
  def acos: NumVal = Transcendental.acos(x)
  def atan: NumVal = Transcendental.atan(x)
  def ulp: NumVal = x.ulp
  def unary_+ : NumVal = this
  def unary_- : NumVal = -x
  def unary_~ : NumVal = ~intPart

  override def %(that: NumVal): NumVal =
    if (that.isInfinity) this // preserve precision of this (don't demote to that type)
    else super.%(that)
  override def log(base: NumVal): NumVal =
    if (x < Bd0) Double.NaN    // avoid rounding this to zero when base is Float or Double infinity
    else if (base.isInfinity && x != Bd0 && x != Bd1) Double.NaN
    else super.log(base)
  override def pow(expon: NumVal): NumVal =
    if (expon.isInfinity)
      if (x < 0)
        if (x > -1.0)
          if (expon > 0) I0    // positive infinite power of a fraction yields zero
          else Double.NaN      // can't find sign of neg x raised to inf b (even or odd?)
        else if (x == -1) Double.NaN  // can't find sign of -1 raised to inf b (even or odd?)
        else
          if (expon < 0) I0    // negative (reciprocal) infinite power of x < -1 yields zero
          else Double.NaN      // can't find sign of neg x raised to inf b (even or odd?)
      else
        if (x < 1)
          if (expon > 0) I0
          else Double.PositiveInfinity
        else if (x == 1) I1
        else
          if (expon < 0) I0
          else Double.PositiveInfinity
    else super.pow(expon)

  def impl_<<(shift: Int): NumVal = smallestAccurateInt(xint.toBigInt << shift)
  def impl_>>>(shift: Int): NumVal = x.longValue >>> shift
  def impl_>>(shift: Int): NumVal = smallestAccurateInt(xint.toBigInt >> shift)
  def impl_|(that: BigDecimalVal): NumVal = xint.toBigInt | (that.xint).toBigInt
  def impl_&(that: BigDecimalVal): NumVal = (x - x.remainder(Bd1(x.mc))).toBigInt & (that.x - that.x.remainder(Bd1(x.mc))).toBigInt
  def impl_^(that: BigDecimalVal): NumVal = xint.toBigInt ^ that.xint.toBigInt
  def impl_+(that: BigDecimalVal): NumVal = x + that.x
  def impl_-(that: BigDecimalVal): NumVal = x - that.x
  def impl_*(that: BigDecimalVal): NumVal = x * that.x
  def impl_/(that: BigDecimalVal): NumVal = {
    if (x == 0) NumVal.I0
    else if (that.x == 0)
      if (x > 0) Double.PositiveInfinity
      else Double.NegativeInfinity
    else x / that.x
  }
  def impl_div(that: BigDecimalVal): NumVal = {
    val thisX = (xint).toBigInt
    val thatX = (that.xint).toBigInt
    if (thatX == 0) DoubleVal(Double.NaN)
    else smallestAccurateInt((xint).toBigInt / (that.xint).toBigInt)
  }
  def impl_%(that: BigDecimalVal): NumVal =
    if (that.x.abs <= Float.MinPositiveValue) Double.NaN // BigDecimal.% throws "ArithmeticException: Division impossible" !
    else if (x.abs >= Float.MaxValue) x.doubleValue % that.x.doubleValue // BigDecimal.% throws "ArithmeticException: Division impossible" !
    else x % that.x
  def impl_pow(expon: BigDecimalVal): NumVal = Transcendental.pow(x, expon.x)
  def impl_log(base: BigDecimalVal): NumVal = Transcendental.log(x, base.x)
}

class DebugNv0 {}

object DebugNv0 {
  def main(args: Array[String]) {
    println("in DebugNv0")
	}
}
