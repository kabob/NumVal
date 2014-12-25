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
 * <!-- File Transcendental.scala -->
 * @author Robert W Kohlenberger
 */

package org.oxland.math

import scala.annotation.tailrec

/*  
 *  Constants:
 *    e Find the base of natural logs
 *    pi Find the constant pi
 *  
 *  Functions:
 *    exp Find e^x
 *    ln Find the natural log
 *    log2 Find log base 2
 *    log10 Find log base 10
 *    log  Find the log to an arbitrary base
 *    pow Find x^y
 *    sin Find the sine
 *    cos Find the cosine
 *    tan Find the tangent
 *    asin Find the arc sine
 *    acos Find the arc cosine
 *    atan Find the arc tangent
 *  
 *  TODO:
 *    toDegrees
 *    toRadians
 *    root (generalize private sqrt)
 *    atan2
 *    sinh
 *    cosh
 *    tanh
 *    hypot
 */

/**
 *  <!-- Transcendental --> High-performance implementations of arbitrary precision constants and transcendental functions.
 */
object Transcendental {
  private val sqrt2 = math.sqrt(2.0)

  private case class CfIteration(n:Int, value: BigDecimal, num0: BigDecimal, den0: BigDecimal, num1: BigDecimal, den1: BigDecimal)

  /**
   * <!-- cfRecurrence --> The generalized recurrence method for solving a continued fraction.
   *     Continued fractions have the form: b0 + a1 / (b1 + a2 / (b2 + a3 / (b3 + ...)))
   * @see http://en.wikipedia.org/wiki/Fundamental_recurrence_formulas
   * @param iter The initial iteration state.
   * @param an Function to calculate the nth a term
   * @param bn Function to calculate the nth b term
   * @param mc The math context; specifies the calculation precision
   */
  private def cfRecurrence(
    iter: CfIteration,
    an: Int => BigDecimal,
    bn: Int => BigDecimal,
    mc: java.math.MathContext
  ) = {
    val bd1 = BigDecimal("1", mc)
    val tolerance = BigDecimal(bd1.bigDecimal.movePointLeft(mc.getPrecision - 1))

    @tailrec
    def recurse(n:Int, value: BigDecimal, num0: BigDecimal, den0: BigDecimal, num1: BigDecimal, den1: BigDecimal): CfIteration = {
      val a2 = an(n)
      val b2 = bn(n)
      val num2 = b2 * num1 + a2 * num0
      val den2 = b2 * den1 + a2 * den0
      val ratio = num2 / den2
      val diff = if (ratio > value) ratio - value else value - ratio
      if (diff < tolerance)
        CfIteration(n+1, ratio, num1, den1, num2, den2)
      else
        recurse(n+1, ratio, num1, den1, num2, den2)
    }

    recurse(iter.n, iter.value, iter.num0, iter.den0, iter.num1, iter.den1)
  }

  /**
   * <!-- cfFunction --> Solve a continued fraction function.
   * @param b0 The zeroth b term.
   * @param a1 The first a term (numerator)
   * @param b1 The first b term
   * @param an Function to calculate the nth a term
   * @param bn Function to calculate the nth b term
   * @param mc The math context; specifies the calculation precision
   */
  private def cfFunction (
    b0: BigDecimal,
    a1: BigDecimal,
    b1: BigDecimal,
    an: Int => BigDecimal,
    bn: Int => BigDecimal,
    mc: java.math.MathContext
  ): CfIteration = {
    val bd1 = BigDecimal("1", mc)
    val num1 = b1 * b0 + a1
    val initValue = num1 / b1
    val iter = CfIteration(2, initValue, b0, bd1, num1, b1)
    cfRecurrence(iter,an,bn,mc)
  }

  private object LazyE {
    private val decimalPad = 1    // extend precision for internal calculation by this many digits
    private var singleton: CfIteration = {
      val precision = 501 + decimalPad
      val mc = new java.math.MathContext(precision)
      new CfIteration(    // initialize the continued fraction iteration
        114,
        BigDecimal("2." +
          "7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274" +
          "2746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901" +
          "1573834187930702154089149934884167509244761460668082264800168477411853742345442437107539077744992069" +
          "5517027618386062613313845830007520449338265602976067371132007093287091274437470472306969772093101416" +
          "9283681902551510865746377211125238978442505695369677078544996996794686445490598793163688923009879312" +
          "77", mc
        ),
        BigDecimal(
          "8996080104763437127832682924101965241480205688792704175476186801110574173685534906252778563296424758" +
          "1433465159644190796785304256476443934301304105462162316284398498774402005370859567205497585957640257" +
          "476233771462116.681693597912067442733818648008658803037682399035467074892999757525279078823610134918" +
          "4536957181990146636962890625",mc),
        BigDecimal(
          "3309472921673903546245679293445317194054275142111425314067269358694639257548460197607888085671660134" +
          "4683056069682999876030888595123754559764889533825244940056935095714808015387995689705218493363372668" +
          "293991912538847.136821685067594131302760435272335959616343150982770465458039621532411542897955669673" +
          "1378906406462192535400390625",mc),
        BigDecimal(
          "2024128108812211052236594549934904531645563180053453005636204354260202522505593509919596248239330319" +
          "8205387340319384555281014551502643462936171906188784902878054841517269888405718312115392606360983454" +
          "049799050911621085.991408408821041027520982699720835779353878383215307859747112775380696028312832446" +
          "26092709950171411037445068359375",mc),
        BigDecimal(
          "7446351175292445980523426213606289044007995211895354318010394613385067507135409791081674590961193140" +
          "5461385875697325490860512602501048969689240376374857055086979459646288467199397762144236649014479347" +
          "97460611664053642.1802791880871178497403737992525597398062984763925793645117921107082139143412691240" +
          "0870423880405724048614501953125",mc)
      )
    }

    def value() = singleton.value

    def value(mc:java.math.MathContext):BigDecimal = {
      val requiredPrecision = mc.getPrecision + decimalPad
      if (requiredPrecision > singleton.value.mc.getPrecision) {    // calculate extended precision e
        val preciseMc = new java.math.MathContext(requiredPrecision)
        val bd0 = BigDecimal("0",preciseMc)
        val bdQtr = BigDecimal("0.25",preciseMc)
        val an = (n:Int) => bdQtr
        val bn = (n:Int) => bd0 + (2 * n - 1)
        singleton = cfRecurrence(singleton, an, bn, preciseMc)
      }
      singleton.value(mc)
    }

    def value(precision:Int):BigDecimal = {
      value(new java.math.MathContext(precision))
    }

  }      // LazyE

  /**
   * <!-- e --> The base of natural logs.
   * @param mc The MathContext with the desired precision.
   * @return The value of e.
   */
  def e(mc:java.math.MathContext) = LazyE.value(mc)

  /**
   * <!-- e --> The base of natural logs.
   * @param precision The desired precision (number of digits).
   * @return The value of e.
   */
  def e(precision:Int) = LazyE.value(precision)

  private object LazyPi {

    val decimalPad = 1

    case class Iteration(value:BigDecimal, q:BigInt, r:BigInt, t:BigInt, i:Int)

    private var singleton = sPIgot(501 + decimalPad, Iteration(0, 1,180,60,2))

    def value() = singleton.value

    /**
     * <!-- sPIgot --> Compute approximate pi using a "decimal digit spigot" algorithm.
     * @see http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf
     * @param newDigits The number of new digits (iterations) to calculate.
     * @param iter The last calculated iteration.
     * @return A new iteration with the new digits calculated.
     */
    private def sPIgot(newDigits:Int, iter:Iteration):Iteration = {

      /*
       * <!-- g --> Solve the next n digits of pi.  Each recursion yields 1 more decimal digit.
       * @param q The current numerator factor.
       * @param r The current numerator term.
       * @param t The current denominator factor.
       * @param i The current iteration number.
       * @param n The recursion termination counter.
       * @param accum The current list of decimal digits, in reverse order.
       * @return The tuple with accumulated digit list, and 4 iteration parameters q, r, t, and i.
       */
      @tailrec
      def g(q:BigInt, r:BigInt, t:BigInt, i:Int, n:Int, accum:List[Int]):(List[Int], BigInt, BigInt, BigInt, Int) = {
        if (n == 0) (accum, q, r, t, i)
        else {
          val u:Int = 3 * (3 * i + 1) * (3 * i + 2)
          val y:BigInt = (q * (27 * i - 12) + 5 * r) / (5 * t)
          g(10 * q * i * (2 * i - 1), 10 * u * (q * (5 * i - 2) + r - y * t), t * u, i + 1, n - 1, y.toInt :: accum)
        }
      }

      /*
       * <!-- build --> Recursively build a BigInt from the new pi digits.
       * @param dList The list of partial pi decimal digits.
       * @param bi The next BigInt digit to add.
       * @return the BigInt representing the given digits.
       */
      @tailrec
      def build(dList: List[Int], bi:BigInt):BigInt = {
        if (dList.isEmpty) bi
        else build(dList.tail, bi * 10 + dList.head)
      }

      val (revList, q, r, t, i) = g(iter.q, iter.r, iter.t, iter.i ,newDigits, Nil)
      val dList = revList.reverse    // obtain the pi decimal digits in forward order
      val bi = build(dList.tail,dList.head)
      val prec = dList.length + (if (iter.value != 0) iter.value.mc.getPrecision else 0)
      val mc = new java.math.MathContext(prec)    // use the combined precision of input iteration value and new digits
      Iteration(
      		BigDecimal.decimal(
      				BigDecimal(bi,mc).bigDecimal.movePointLeft(prec - 1),
      				mc
      		)
      		+ iter.value,
      		q, r, t, i)
    }

    def value(mc:java.math.MathContext):BigDecimal = {
      val newDigits = mc.getPrecision + decimalPad - singleton.value.mc.getPrecision
      if (newDigits > 0)    // calculate extended precision pi
        singleton = sPIgot(newDigits, singleton)
      singleton.value(mc)
    }

    def value(precision:Int):BigDecimal = {
      value(new java.math.MathContext(precision))
    }
  }      // LazyPi

  /**
   * <!-- pi --> The constant value pi.
   * @param mc The MathContext with the desired precision.
   * @return The value of pi.
   */
  def pi(mc:java.math.MathContext) = LazyPi.value(mc)

  /**
   * <!-- pi --> The constant value pi.
   * @param precision The desired precision (number of digits).
   * @return The value of pi.
   */
  def pi(precision:Int) = LazyPi.value(precision)

  private object LazyLn2 {
    private val decimalPad = 3    // extend precision for internal calculation by this many digits
    private var singleton: BigDecimal = {
      val precision = 501 + decimalPad
      val mc = new java.math.MathContext(precision)
      BigDecimal("0." +
        "6931471805599453094172321214581765680755001343602552541206800094933936219696947156058633269964186875" +
        "4200148102057068573368552023575813055703267075163507596193072757082837143519030703862389167347112335" +
        "0115364497955239120475172681574932065155524734139525882950453007095326366642654104239157814952043740" +
        "4303855008019441706416715186447128399681717845469570262716310645461502572074024816377733896385506952" +
        "6066834113727387372292895649354702576265209885969320196505855476470330679365443254763274495125040606" +
        "9438",mc)
    }

    def value() = singleton

    /**
     * <!-- value -->
     * @see http://en.wikipedia.org/wiki/Natural_logarithm_of_2
     */
    def value(mc:java.math.MathContext):BigDecimal = {
      val requiredPrecision = mc.getPrecision + decimalPad
      if (requiredPrecision > singleton.mc.getPrecision) {    // calculate extended precision e
        val preciseMc = new java.math.MathContext(requiredPrecision)
        val bd0 = BigDecimal("0", preciseMc)
        val bd2 = BigDecimal("2", preciseMc)
        val bd3 = BigDecimal("3", preciseMc)
        val bd6 = BigDecimal("6", preciseMc)
        val an = (n:Int) => {val nm1 = n - 1; bd0 - (nm1 * nm1)}
        val bn = (n:Int) => {val nm1 = n - 1; bd3 + bd6 * nm1}
        singleton = cfFunction(bd0, bd2, bd3, an, bn, preciseMc).value
      }
      singleton(mc)
    }

    def value(precision:Int):BigDecimal = {
      value(new java.math.MathContext(precision))
    }

  }    // LazyLn2

  // TODO: generalize to find the nth root? (http://en.wikipedia.org/wiki/Nth_root_algorithm)
  /**
   * <!-- sqrt --> Find the square root of a BigDecimal.
   * @see http://en.wikipedia.org/wiki/Nth_root_algorithm
   * @param x The input number (square of the result).
   * @param bd1 BigDecimal 1 to the required precision.
   * @return The square root
   */
  private def sqrt(x:BigDecimal, bd1:BigDecimal) = {
    val tolerance:BigDecimal = BigDecimal(bd1.bigDecimal.movePointLeft(x.mc.getPrecision - 1))
    val a = if (x < 1) bd1 / x else x  // ensure a is > 1 (if a = 1/x then sqrt(x) = sqrt(1/a) = 1/sqrt(a))
    val intPart = a.toBigInt
    val hiLb = intPart.bitLength      // high and low log2
    val loLb = hiLb - 1
    val bd2 = bd1 + 1
    val half = bd1 / bd2

    def sqrtP2(lb: Int) = {            // find sqrt(2^lb)
      val halfPower = bd2.pow(lb / 2)
      if ((lb & 1) == 1) halfPower * sqrt2 else halfPower
    }

    val hi = sqrtP2(hiLb)              // high and low sqrt bounds for the initial guess
    val lo = sqrtP2(loLb)
    val lowerP2 = bd2.pow(loLb)        // next power of 2 below a
    val r = (a - lowerP2) / lowerP2    // relative distance from lower to higher power of 2
    var guess = (hi - lo) * r + lo    // first sqrt(a) estimate
    var previous = bd1
    var diff = bd1
    do {
      previous = guess
      guess = half * (guess + a / guess)
      diff = if (guess > previous) guess - previous else previous - guess
    } while (diff >= tolerance)
    if (x < 1) bd1 / guess else guess    // if x < 1, take reciprocal of the last guess as sqrt(x)
  }

  /**
   * <!-- sqrt --> Find the square root of a BigDecimal.
   * @param x The input number.
   * @return The square root with the same precision as x.
   */
  def sqrt(x:BigDecimal):BigDecimal = sqrt(x, BigDecimal("1",x.mc))

  private def splitBd(x:BigDecimal, mc:java.math.MathContext): (BigInt,BigDecimal) = {
    val intPart = x.toBigInt
    val fracPart = x - BigDecimal(intPart, mc)
    (intPart,fracPart)
  }

  private def cfExp(x:BigDecimal, mc:java.math.MathContext) = {
      val preciseMc = new java.math.MathContext(mc.getPrecision + 2)
      val bd0 = BigDecimal("0", preciseMc)
      val bd1 = BigDecimal("1", preciseMc)
      val bd2 = BigDecimal("2", preciseMc)
      val hx = x(preciseMc) / bd2
      val x2 = hx * hx
      val result = cfFunction(bd1, x, bd1 - hx, (n:Int) => x2, (n:Int) => bd2 * n - bd1, preciseMc)
      result.value(mc)
  }

  private val posExpLimit = BigDecimal("4944763835")
  private val negExpLimit = BigDecimal("-4944762640")

  /**
   * <!-- exp --> Find the BigDecimal power of e.
   * @see http://en.wikipedia.org/wiki/Exponential_function
   * Solve for e^(2a/b), where b = 1 and a = x/2.
   * @param x The value for which to find the power of e.
   * @return The exp result.
   */
  def exp(x:BigDecimal):NumVal = {
    val mc = x.mc
    if (x > posExpLimit) Double.PositiveInfinity    // prevent overflow
    else if (x < negExpLimit) BigDecimal("0", mc)    // prevent underflow
    else if (x == 0) BigDecimal("1", mc)
    else if (x == 1) LazyE.value(mc)
    else if (x == 2) {
      val e = LazyE.value(mc)
      e * e
    } else if (x > 1 || x < -1) {
      val (neg, absX) = if (x < 0) (true, -x) else (false, x)
      val preciseMc = new java.math.MathContext(mc.getPrecision + 19)
      val (intPart,fracPart) = splitBd(absX,preciseMc)
      val scale = 100000000

      def intExp(bd:BigDecimal):BigDecimal = {
        val s = bd / scale
        if (s < 1)
          cfExp(bd / scale,preciseMc).pow(scale)
        else if (s == 1)
          LazyE.value(preciseMc).pow(scale)
        else
          intExp(s).pow(scale)
      }

      val result = (intExp(BigDecimal(intPart,preciseMc)) * cfExp(fracPart,preciseMc))
      if (neg) (BigDecimal("1", preciseMc) / result)(mc) else (result)(mc)

    } else
      cfExp(x,mc)
  }

  /**
   * <!-- log2 --> Find the log base 2 of a BigDecimal > 0.
   * @param x The base 2 power of the result.  Must be > 0.
   * @return The log result.
   */
  private def lb(x:BigDecimal):BigDecimal = {
    val mc = x.mc
    val bd0 = BigDecimal("0", mc)
    val bd1 = BigDecimal("1", mc)
    val bd2 = BigDecimal("2", mc)
    val (intPart,fracPart) = splitBd(x,mc)
    var intBitLength = intPart.bitLength
    var recipFracBitLength = 0
    var recipFracLowBit = 0
    if (fracPart != bd0) {
      val recipFrac = (bd1 / fracPart).toBigInt
      recipFracLowBit = recipFrac.bigInteger.getLowestSetBit
      if (intBitLength == 0)
        recipFracBitLength = recipFrac.bitLength
    }
    var shift =
        if (recipFracLowBit != 0 && recipFracLowBit == recipFracBitLength - 1)
          intBitLength - recipFracLowBit - 1
        else if (recipFracBitLength == 0)
          intBitLength - 1
        else
          intBitLength - recipFracBitLength
    val intResult = BigDecimal(shift, mc)
    var rem = x * bd2.pow(-shift) // right shift (divide rem by 2) "shift" times
    if (rem == bd2) intResult + 1
    else if (rem == bd1) intResult
    else {
      val tolerance = "1.0E-" + mc.getPrecision // (remember value 0 is unlimited; all others are powers of 10!)
      val tol = BigDecimal(tolerance, mc)        // (-z.mc.getPrecision)
      var fp = bd1
      var fracResult = bd0
      while (fp >= tol) {
        fp /= bd2
        rem *= rem
        if (rem >= bd2) {
          rem /= bd2
          fracResult += fp
        }
      }
      intResult + fracResult
    }
  }      // lb

  /**
   * <!-- log2 --> Find the log base 2 of a BigDecimal.
   * @param x The base 2 exponent of the result.  If < 0 return NaN; if == 0 return -Inf.
   * @return The log result.
   */
  def log2(x:BigDecimal):NumVal = {
    if (x < 0) Double.NaN
    else if (x == 0) Double.NegativeInfinity
    else {
      val mc = x.mc
      val preciseMc = new java.math.MathContext(mc.getPrecision + 2)
      lb(x(preciseMc))(mc)
    }
  }

  /**
   * <!-- log10 --> Find the log base 10 of a BigDecimal.
   * @param x The value for which to find the base 10 log.
   * @return The log result.
   */
  def log10(x:BigDecimal) = {
    val mc = x.mc    // use the argument's precision for calculation
    val bd10 = BigDecimal("10", mc)
    log(x,bd10)
  }

  /**
   * <!-- ln --> Find the natural log of a BigDecimal using continued fractions.
   * @see http://functions.wolfram.com/ElementaryFunctions/Log/10/
   * @param x The value for which to find the natural log.
   * @return The log result.
   */
  private def ln(x:BigDecimal, bd0:BigDecimal, bd1:BigDecimal, mc:java.math.MathContext):BigDecimal = {

    val a = if (x < 1) bd1 / x else x    // ensure a is > 1
    val intPart = a.toBigInt

    def isPow2:Boolean = {    // test whether x is a (positive or negative) integer power of 2
      if (x - BigDecimal(intPart, mc) == 0)    // no fractional bits
        (intPart.bitCount) == 1
      else false
    }

    def cf(xm1:BigDecimal) = {    // continued fraction definition for ln(x); xm1 = x - 1
      cfFunction(bd0, xm1, bd1, (n:Int) => { val a = n/2; xm1*a*a }, (n:Int) => bd1 * n, mc).value
    }

    if (isPow2)        // use faster binary algorithm: ln(x) = ln(2) * log2(x)
      (LazyLn2.value(mc) * lb(x))
// TODO: Implement the First and Second AGM algorithms for ln, starting on p32 in file "brent_invited.pdf"
//    and compare performance at different precisions with the current implementation.
//    If and where there is a crossover (AGM becomes faster) add another conditional to use AGM.
    else if (a > 2) {    // reduce a using: ln(n * (a / n)) = ln(n) + ln(a / n), where n is the next power of 2 below a
      val lowLb = intPart.bitLength - 1    // log2(n)
      val n = BigDecimal("2",mc).pow(lowLb)  // 2^log2(n) = next lower power-of-2 from a
      val term1 = (LazyLn2.value(mc) * lowLb)    // natural log of next lower power-of-2 from a
      val term2 = cf(a / n - 1) 
      val abs = (term1 + term2)(mc)
      if (x < 1) -abs else abs    // correct sign if a = 1/x, using ln(1/x) = -ln(x)
    } else cf(x - 1)    // x is already in the range 0.5 .. 2, use continued fraction
  }

  /**
   * <!-- ln --> Find the natural log of a BigDecimal.
   * @param x The value for which to find the natural log.
   * @return The log result.
   */
  def ln(x:BigDecimal):NumVal = {
    val mc = x.mc
    if (x < 0) Double.NaN
    else if (x == 0) Double.NegativeInfinity
    else if (x == 1) BigDecimal("0", mc)
    else if (x == 2) LazyLn2.value(mc)
    else {
      val preciseMc = new java.math.MathContext(mc.getPrecision + 3)
      val bd0 = BigDecimal("0", preciseMc)
      val bd1 = BigDecimal("1", preciseMc)
      val result = ln(x(preciseMc),bd0,bd1,preciseMc)
      result(mc)
    }
  }

  /**
   * <!-- log --> Find the logarithm (arbitrary base) of a BigDecimal.
   * @param x The value for which to find the log.
   * @param base The BigDecimal base for the log.
   * @return The log result.
   */
  def log(x:BigDecimal, base:BigDecimal):NumVal = {
    val mc = x.mc
    if (x < 0) Double.NaN
    else if (x == 0) Double.NegativeInfinity
    else if (x == 1) BigDecimal("0", mc)
    else if (x == base) BigDecimal("1", mc)
    else if (base <= 0 || base == 1) Double.NaN    // nonsense base
    else {
      val preciseMc = new java.math.MathContext(mc.getPrecision + 3)
      val bd0 = BigDecimal("0", preciseMc)
      val bd1 = BigDecimal("1", preciseMc)
      val lnx = ln(x(preciseMc),bd0,bd1,preciseMc)    // TODO: parallelize ln(x) and ln(base)
      val lnb = ln(base(preciseMc),bd0,bd1,preciseMc)
      (lnx / lnb)(mc)    // by the relation logb(a) = logc(a) / logc(b); convert to original precision
    }
  }

  /**
   * <!-- pow --> Find the BigDecimal power of a BigDecimal.
   * @param x The base for which to find the power; must be a positive value.
   * @param expon The exponent with which to raise x.
   * @param mc The math context for precision
   * @return The power result.
   */
  private def pow(x:BigDecimal, expon:BigDecimal, mc:java.math.MathContext):NumVal = {
    val preciseMc = new java.math.MathContext(mc.getPrecision + 2)
    val bd0 = BigDecimal("0", preciseMc)
    val bd1 = BigDecimal("1", preciseMc)
    val lnx = ln(x(preciseMc),bd0,bd1,preciseMc)  // x must be positive
    exp(lnx * expon(preciseMc)) match {
      case bd:BigDecimalVal => bd.x(mc)
      case x => x
    }
  }

  /**
   * <!-- bigIntPow --> Find the BigInt power of a BigDecimal.
   * @param x The base for which to find the power.
   * @param expon The exponent with which to raise x.
   * @return The power result.
   */
  def bigIntPow(x:BigDecimal, expon:BigInt):NumVal = {
    if (expon > 999999999L) {    // exponent is out of range to use BigInt pow function
      val mc = x.mc
      pow(x, BigDecimal(expon,mc),mc)
    } else
      x.pow(expon.toInt)
  }

  /**
   * <!-- safePow --> Find the BigDecimal power of a BigDecimal, with sanity checks.
   * @param x The base for which to find the power.
   * @param expon The exponent with which to raise x.
   * @param mc The math context for precision
    * @return The power result.
   */
  private def safePow(x:BigDecimal, expon:BigDecimal, mc:java.math.MathContext):NumVal = {
    val bd1 = BigDecimal("1",mc)
    if (x < 0) {
      val intExp = expon.toLong
      if (expon != intExp) Double.NaN    // fract exponent of neg number; no support (yet) for imaginary numbers
      else {    // x is < 0 and expon is a non-zero integer
        val oddExp = (intExp & 1) == 1
        if (x == -bd1)
          if (oddExp) -1 else 1
        else {

          val result = -pow(-x,expon,mc)    // pass pos x and negate result

          if (oddExp) result    // odd exponent preserves sign
          else if (result < 0) -result else result
        }
      }
    } else {    // x is positive or 0
      val bd0 = BigDecimal("0",mc)
      (x,expon) match {
        case (`bd0`, b) => if (b > 0) 0 else Double.PositiveInfinity
        case (`bd1`, b) => 1
        case (a, b) => pow(x,expon,mc)
      }
    }
  }

  private def isValidInt(x:BigDecimal) =
  {
    if (x < Int.MinValue || x > Int.MaxValue) false
    else {
      val dx = x.toDouble
      val intPart = dx.toInt
      dx == intPart
    }
  }

  /**
   * <!-- pow --> Find the BigDecimal power of a BigDecimal.
   * @param x The base for which to find the power.
   * @param expon The exponent with which to raise x.
   * @return The power result.
   */
  def pow(x:BigDecimal, expon:BigDecimal):NumVal = {
    val mc = x.mc
    if (expon == 0) BigDecimal("1",mc)
    else if (expon == 1) x
//    else if (expon.isValidInt) {    // broken in Scala 2.9.2 for expon == -1.7976931348623157E+308
    else if (isValidInt(expon)) {
      val (negExp, absExp) = if (expon < 0) (true, -(expon.toLong)) else (false, expon.toLong)
      if (absExp <= 999999999L) {    // use built-in pow function
        val power = x.pow(absExp.toInt)
        if (negExp) {    // negative exponent so take reciprocal
          if (power == 0.0)    // reciprocol of zero is infinity
            if (x < 0.0)    // sign depends on even/odd exponent
              if ((absExp & 1) == 1) Double.NegativeInfinity else Double.PositiveInfinity
            else Double.PositiveInfinity
          else BigDecimal("1",mc) / power
        }
        else power
      } else safePow(x, expon, mc)
    } else safePow(x, expon, mc)
  }

  /**
   * <!-- normalizeRadians --> Reduce an angle to within the range -pi .. +pi.
   * @param x The angle to normalize, in radians.
   * @param mc The math context with the required precision.
   * @return The normalized result in radians.
   */
  private def normalizeRadians(x:BigDecimal, mc:java.math.MathContext) = {
    val px = x(mc)
    val pi = LazyPi.value(mc)
    val doublePi = pi * 2
    val normalize = px > pi || px < -pi
    if (normalize) {
      val mult = px / doublePi
      val rem = (mult - BigDecimal(mult.toBigInt, mc)) * doublePi
      if (rem > pi) rem - doublePi else if (rem < -pi) rem + doublePi else rem
    } else px
  }

  /**
   * <!-- cfSine --> Find the sine to a desired precision.
   * @see http://en.wikipedia.org/wiki/Sine
   * @param x The input angle, in radians.
   * @param mc The math context with the required precision.
   * @return The big decimal result, in the range -1 .. +1.
   */
  private def cfSine(x:BigDecimal, mc:java.math.MathContext) = {
    val bd0 = BigDecimal("0", mc)
    val bd1 = BigDecimal("1", mc)
    val preciseX = normalizeRadians(x,mc)
    val x2 = preciseX * preciseX
    cfFunction(
            bd0, preciseX, bd1,
            (n:Int) => { val fn = (n-2)*2; val a = if (fn == 0) 1 else fn*(fn+1); x2*a },
            (n:Int) => {val fn = (n-1)*2; val a = fn*(fn+1); -x2 + a},
            mc
    ).value
  }

  /**
   * <!-- sin --> Find the sine of a BigDecimal.
   * @param x The input angle, in radians.
   * @return The sine result.
   */
  def sin(x:BigDecimal) = {
    val mc = x.mc
    val preciseMc = new java.math.MathContext(mc.getPrecision + 4)
    cfSine(x,preciseMc)(mc)
  }

  /**
   * <!-- cos --> Find the cosine of a BigDecimal.
   * @param x The input angle, in radians.
   * @return The cosine result.
   */
  def cos(x:BigDecimal) = {
    val mc = x.mc
    val preciseMc = new java.math.MathContext(mc.getPrecision + 4)
    val preciseX = normalizeRadians(x,preciseMc)
    val result = cfSine(LazyPi.value(preciseMc) / 2 - preciseX, preciseMc)    // cos(x) = sin(pi/2 - x)
    result(mc)
  }

  /**
   * <!-- tan --> Find the tangent of a BigDecimal.
   * @see http://functions.wolfram.com/ElementaryFunctions/Tan/10/
   * @param x The input angle, in radians.
   * @return The tangent result.
   */
  def tan(x:BigDecimal) = {
    val mc = x.mc
    val preciseMc = new java.math.MathContext(mc.getPrecision + 3)
    val bd0 = BigDecimal("0", preciseMc)
    val bd1 = BigDecimal("1", preciseMc)
    val preciseX = normalizeRadians(x,preciseMc)
    val mx2 = -(preciseX * preciseX)
    cfFunction(
            bd0, preciseX, bd1,
            (n:Int) => { mx2 },
            (n:Int) => { BigDecimal(n*2-1,preciseMc) },
            preciseMc
    ).value(mc)
  }

  /**
   * <!-- cfArcsine --> Find the arc sine to a desired precision, using continued fractions.
   * @see http://functions.wolfram.com/ElementaryFunctions/ArcSin/10/
   * @param x The input value.
   * @param mc The math context with the required precision.
   * @return The angle result, in radians.
   */
  private def cfArcsine(x:BigDecimal, mc:java.math.MathContext) = {
    val preciseX = x(mc)
    val x2 = preciseX * preciseX
    val bd0 = BigDecimal("0", mc)
    val bd1 = BigDecimal("1", mc)
    val m2 = -2
    val a1 = preciseX * sqrt(-x2 + 1, bd1)    // x * sqrt(1-x2)
    val an = (n:Int) => { val a = n & m2; x2 * (-a * (a - 1)) } 
    val bn = (n:Int) => bd0 + (n * 2 - 1)
    cfFunction(bd0, a1, bd1, an, bn, mc).value
  }

  /**
   * <!-- asinLimit --> Obtain special values for x at or beyond valid arc sine limits.
   */
  private def asinLimit(x:BigDecimal, mc:java.math.MathContext): (Boolean, NumVal) = {
    if (x > 1 || x < -1)    // out of range gives NaN
      (true, Double.NaN)
    else if (x == 1)        // give exact values at input value limits
      (true, LazyPi.value(mc) / 2)
    else if (x == -1)
      (true, -LazyPi.value(mc) / 2)
    else
      (false, 0)
  }

  /**
   * <!-- acosLimit --> Obtain special values for x at or beyond valid arc cosine limits.
   */
  private def acosLimit(x:BigDecimal, mc:java.math.MathContext): (Boolean, NumVal) = {
    if (x > 1 || x < -1)    // out of range gives NaN
      (true, Double.NaN)
    else if (x == 1)        // give exact values at input value limits
      (true, BigDecimal("0", mc))
    else if (x == -1)
      (true, LazyPi.value(mc))
    else
      (false, 0)
  }

  /**
   * <!-- asin --> Find the arc sine of a BigDecimal to a given precision.
   * @see http://en.wikipedia.org/wiki/Inverse_trigonometric_functions
   */
  private def asin(x:BigDecimal, preciseMc:java.math.MathContext) = {
    if (x < 0.6 && x > -0.6)  // faster at small x and high precision, but blows up at x >= .993
      cfArcsine(x,preciseMc)
    else {    // faster at low precision and works for all x
      val bd1 = BigDecimal("1", preciseMc)
      val pX = x(preciseMc)
      (atan(pX / (sqrt(-pX * pX + 1, bd1) + 1)) * 2)
    }
  }

  /**
   * <!-- asin --> Find the arc sine of a BigDecimal.
   * @param x The value for which to find the arc sine (sine of the result).
   * @return The arc sine result.
   */
  def asin(x:BigDecimal):NumVal = {
    val mc = x.mc
    asinLimit(x,mc) match {
      case (true, y) => y
      case (false, y) => {
        val preciseMc = new java.math.MathContext(mc.getPrecision + 3)
        asin(x, preciseMc)(mc)
      }
    }
  }

  /**
   * <!-- acos --> Find the arc cosine of a BigDecimal.
   * @see http://en.wikipedia.org/wiki/Inverse_trigonometric_functions
   * @param x The value for which to find the arc cosine (cosine of the result).
   * @return The arc cosine result.
   */
  def acos(x:BigDecimal):NumVal = {
    val mc = x.mc
    acosLimit(x,mc) match {
      case (true, y) => y
      case (false, y) => {
        val preciseMc = new java.math.MathContext(mc.getPrecision + 4)
        (LazyPi.value(preciseMc) / 2 - asin(x, preciseMc))(mc)
      }
    }
  }

  /**
   * <!-- atan --> Find the arc tangent of a BigDecimal.
   * @see http://en.wikipedia.org/wiki/Inverse_trigonometric_functions
   * @param x The value for which to find the cosine.
   * @return The arc tangent result.
   */
  def atan(x:BigDecimal) = {
    val mc = x.mc
    val preciseMc = new java.math.MathContext(mc.getPrecision + 3)
    val bd0 = BigDecimal("0", preciseMc)
    val bd1 = BigDecimal("1", preciseMc)
    val b0 = bd0
    val b1 = bd1
    val neg = x < 0
    val absX = if (neg) -x(preciseMc) else x(preciseMc)
    val large = absX > 1    // normalize to within 0 .. 1
    val normX = if (large) bd1 / absX else absX
    def cf(pX:BigDecimal) = {
      val a1 = pX
      val x2 = pX * pX
      val an = (n:Int) => { val a = n - 1; x2 * a * a } 
      val bn = (n:Int) => bd0 + (n * 2 - 1)
      cfFunction(b0, a1, b1, an, bn, preciseMc).value(mc)
    }
    val result = cf(normX)
    if (large) {
      val halfPi = LazyPi.value(preciseMc) * BigDecimal("0.5",preciseMc)
      if (neg) -(halfPi - result)
      else halfPi - result
    }
    else if (neg) -result
    else result
  }

}      // Transcendental

//class DebugTd {}
//
//object DebugTd {
//
//  def main(args: Array[String]) {
//    println("hello world")
//    val mc500 = new java.math.MathContext(500)
//    val mc1000 = new java.math.MathContext(1000)
//    val mc2000 = new java.math.MathContext(2000)
//
//    // COS:
//    val th0 = System.currentTimeMillis
//    val testh1 = Transcendental.cos(BigDecimal("1", new java.math.MathContext(20)))
//    val th1 = System.currentTimeMillis
//    val testh2 = Transcendental.cos(BigDecimal("-1000", mc500))
//    val th2 = System.currentTimeMillis
//    val testh3 = Transcendental.cos(BigDecimal("-1000", mc1000))
//    val th3 = System.currentTimeMillis
//    val testhRef = Transcendental.cos(BigDecimal("-1000", mc2000))
//    println("cos test1=" + testh1 + ", time=" + (th1-th0))
//    println("cos test2=" + testh2 + ", time=" + (th2-th1))
//    println("cos test3=" + testh3 + ", time=" + (th3-th2))
//    println("cos testRef=" + testhRef)
//
//    val t0 = System.currentTimeMillis
//    val e1 = Transcendental.exp(BigDecimal("10000",mc500))
//    val t1 = System.currentTimeMillis
//    println("e^10000 =" + e1 + ", time=" + (t1 - t0))
//    val e2 = Transcendental.exp(BigDecimal("10000000",mc500))
//    val t2 = System.currentTimeMillis
//    println("e^10000000 =" + e2 + ", time=" + (t2 - t1))
//    val e3 = Transcendental.exp(BigDecimal("100000000",mc500))
//    val t3 = System.currentTimeMillis
//    println("e^100000000 =" + e3 + ", time=" + (t3 - t2))
//    val e4 = Transcendental.exp(BigDecimal("999999999",mc500))
//    val t4 = System.currentTimeMillis
//    println("e^999999999 =" + e4 + ", time=" + (t4 - t3))
//    val e5 = Transcendental.exp(BigDecimal("-4944762640",mc500))
//    val t5 = System.currentTimeMillis
//    println("e^4944763835 =" + e5 + ", time=" + (t5 - t4))
//  }
//
//}		// DebugTd class
