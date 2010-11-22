package ProsodicParsing.examples;

import collection.immutable.{HashSet,HashMap}
import ProsodicParsing.Types.HMM


object testHMM {
  def main( args:Array[String] ) {
    val t = new HMM( 4 , HashSet("whoa","wait","what","?") )

    println( t.TransitionMatrix("Q_1")("Q_2") )
    //t.randomize(0)


    
    //val newEmissionsMatrix = new HashMap[String,HashMap[String,Double]](
    val newEmissionsMatrix = HashMap(
      Pair(
        "Q_0",
        HashMap(
          "whoa" -> 0.5D,
          "wait" -> 0.5D,
          "what" -> 0D,
          "?" -> 0D
        )
      ),
      Pair(
        "Q_1",
        HashMap(
          "whoa" -> 0D,
          "wait" -> 0.5D,
          "what" -> 0.5D,
          "?" -> 0D
        )
      ),
      Pair(
        "Q_2",
        HashMap(
          "whoa" -> 0D,
          "wait" -> 0D,
          "what" -> 0.5D,
          "?" -> 0.5D
        )
      ),
      Pair(
        "Q_3",
        HashMap(
          "whoa" -> 0.5D,
          "wait" -> 0D,
          "what" -> 0D,
          "?" -> 0.5D
        )
      )
    )

    t.setEmissionMatrix( newEmissionsMatrix )

    //t.buildHMM( Array("whoa","wait","what","?")  )
    t.emStep(Array("whoa","wait","what","?"))
    //t.seeMarginals();
    //t.seeMarginals()
    //t.buildHMM( Array("whoa","wait","what","?") )
    //t.seeMarginals()
  }
}

