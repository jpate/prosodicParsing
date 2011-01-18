package ProsodicParsing.runners
import ProsodicParsing.HMMs.PlainHMM
import ProsodicParsing.types._

object DevelopmentRunner {
  def main( args:Array[String]) {

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

    

  }
}


