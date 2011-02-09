package ProsodicParsing.util

object Util {
  // translated from nltk
  def log_add( ns:List[Double] ) = {
    import math.{log,exp}
    val maxVal = ns.max
    if( maxVal > scala.Double.NegativeInfinity )
      maxVal + log( ns.foldLeft(0D)( (a,b) => a + exp( b - maxVal ) ) )
    else
      maxVal
  }
}

