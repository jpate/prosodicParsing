package ProsodicParsing.runners
import ProsodicParsing.HMMs.PlainHMM
import ProsodicParsing.types._

object DevelopmentRunner {
  def main( args:Array[String]) {
    import scala.math.log

    val dataPath = args(0)

    val hiddenStates = Set(
      "Q_0",
      "Q_1",
      "Q_2"
    ).map(HiddenState(_))

    val trainingData = io.Source.fromFile( dataPath ).getLines().toList.map(
      _.split(" ").toList.map( ObservedState( _ ) )
    )

    val observationTypes = Set(
      trainingData.flatten
    ).flatten

    println( trainingData(0) )

    println( observationTypes )

    val h = new PlainHMM( hiddenStates , observationTypes )

    h.buildHMM( trainingData(0) )

    h.randomize( 1 )

    println( h )

    println( trainingData( 0 ) )
    var lastProb = log( h.generalProbability( trainingData(0) ) )
    println( "    " +lastProb )
    var lastGenProb = 0D
    for( n <- 0 to 100 ) {
      print( n + ":  " )
      val newProb = log( h.reestimate( trainingData(0) ) )
      println( "    " +newProb + " (" + ((newProb - lastProb)/lastProb) + ")" )
      // val ezpzProb = h.easyPeasyTotalProbability( trainingData(0) )
      // println( "    " + ezpzProb )
      // val generalProb = h.generalProbability( trainingData(0) )
      // println( "    " + generalProb + " (" + (( generalProb - lastGenProb )/lastGenProb) + ")")

      println( h )

      lastProb = newProb
      // lastGenProb = generalProb
    }
    println( h )

    println( "\n\n\n\n\n=======\n\n\n\n" )

    val nextObservationSet = Set(
      "wait",
      "whoa",
      "what",
      "?"
    ).map( ObservedState( _ ) )

    val nextHiddenStates = Set(
      "Q_0",
      "Q_1"
    ).map(HiddenState(_))

    val vShortSentence = List( "wait", "what" ).map( ObservedState( _ ) )

    val h2 = new PlainHMM( nextHiddenStates, nextObservationSet )

    println( h2.totalProbability( vShortSentence ) )
    println( h2.generalProbability( vShortSentence ) )
  }
}


