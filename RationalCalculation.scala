class Rational(n: Int, d: Int){
    require(d != 0)

    private val g = gcd(n.abs, d.abs)  
    val number = n / g
    val demon = d / g

    def this(n: Int) = this(n, 1)
  
    def +(that: Rational): Rational = 
        new Rational (
            number * that.demon + that.number * demon,
            demon * that.demon
    )

    def *(that: Rational): Rational = 
        new Rational(
            number * that.number,
            demon * that.demon
    )

    override def toString = {
        if (demon == 1)
            number.toString
        else
            number.toString + "/" + demon.toString
    }

    private def gcd(a: Int, b: Int): Int = {
        if (b == 0) 
            a 
        else 
            gcd(b, a%b)
    }
}

object RationalCalculation {
    def main(args: Array[String]): Unit = {

        val oneTwo = new Rational(1, 2) // Num : 1/2
        val twoThree = new Rational(2, 3) // Num : 2/3

        val fiftyEightSixtyTwo = new Rational(58, 62) // Num : 58/62
        val fortyTwoNinetyEight = new Rational(42, 98) // Num : 42/98

        val two = new Rational(2) // Num : 2
        val sixteen = new Rational(16) // Num : 16

        val oneHundredTwentyEight = new Rational(128) // Num : 128
        val twoHundredNinetyFour = new Rational(294) // Num : 294

        print("1/2 + 2/3 = ")
        println(oneTwo + twoThree)

        print("1/2 * 2/3 = ")
        println(oneTwo * twoThree)

        print("58/62 * 128 = ")
        println(fiftyEightSixtyTwo * oneHundredTwentyEight)

        print("42/98 * 294 = ")
        println(fortyTwoNinetyEight * twoHundredNinetyFour)

        print("1/2 + (1/2 * 2/3 * 2/3 * 1/2) * 2 = ")
        println(oneTwo + (oneTwo * twoThree * twoThree * oneTwo ) * two)

        print("16 + (2 * 58/62 + (1/2 * 58/62)) = ")
        println(sixteen + (two * fiftyEightSixtyTwo + (oneTwo * fiftyEightSixtyTwo)))

        print("(42/98 + 58/62) * 128 * 42/98 = ")
        println((fortyTwoNinetyEight + fiftyEightSixtyTwo) * oneHundredTwentyEight * fortyTwoNinetyEight)
    }
}


// ===========================================
//       Output Sample
// ===========================================
// $ scalac RationalCalculation.scala 
// $ scala RationalCalculation
// 1/2 + 2/3 = 7/6
// 1/2 * 2/3 = 1/3
// 58/62 * 128 = 3712/31
// 42/98 * 294 = 126
// 1/2 + (1/2 * 2/3 * 2/3 * 1/2) * 2 = 13/18
// 16 + (2 * 58/62 + (1/2 * 58/62)) = 1137/62
// (42/98 + 58/62) * 128 * 42/98 = 113664/1519
