package ProsodicParsing.runners
import ProsodicParsing.HMMs.PlainHMM
import ProsodicParsing.HMMs.CoupledHMM
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

        // val uttStart = ObservedState("UTTSTART")
        // val uttEnd = ObservedState("UTTEND")

    val observationTypes =// Set( uttStart, uttEnd  ) ++
      Set(
        trainingData.flatten
      ).flatten

    println( trainingData(0) )

    println( observationTypes )

    val h = new PlainHMM( hiddenStates , observationTypes )

    h.buildHMM( trainingData(0) )

    h.randomize( 1 )

    println( h )

    println( trainingData( 0 ) )

    println( "argmax" )
    println( h.argmax( trainingData(0)::Nil ) )
    println( "viterbi" )
    println( h.viterbi( trainingData(0) ).mkString(""," ",".") )
    println( "argMaxUsingCreateForMaxProduct" )
    println( h.argMaxUsingCreateForMaxProduct( trainingData(0) ) )
    println( "marginalsForString" )
    println( h.marginalsForString( trainingData(0) ) )
    //var lastProb = log( h.generalProbability( trainingData(0) ) ) + log( h.generalProbability(
    //var lastProb = trainingData.map{ s => log( h.generalProbability( s ) ) }.sum
    //trainingData(1)))
    //println( "    " +lastProb )
    println( "BEGINNING EM" )
    var lastProb = 0D
    var deltaLogProb = 1D
    var n = 0
    /*
    while( math.abs( deltaLogProb ) > 0.00001 & n < 100 ) {
      //print( n + ":  " )

      val newProb = h.reestimate( trainingData )// List( trainingData head , trainingData last ) )
      //val newProb = h.reestimate( List( trainingData head ))//, trainingData last ) )
      //val newProb = h.reestimate( trainingData )
      // val newProb = h.reestimateSingle( trainingData last )
      // val newProb = h.reestimateSingle(
      //   //List( trainingData(0),trainingData(1) ).flatMap( ObservedState("#####")::_ )
      //   trainingData.flatMap{ List( uttStart ) ++ _ ++ List( uttEnd ) }
      // )
      deltaLogProb = ((newProb - lastProb)/lastProb)
      println( n +  ":    " +newProb + " (" +  deltaLogProb + ")" )
      //println( n +  ":    " + h.generalProbability( trainingData head ) + "\n\n" )
      //println( n +  ":        " + h.generalProbability( trainingData last ) + "\n\n" )
      //println( n +  ":    " + math.log( h.totalProbability( trainingData head ) ) + "\n\n" )
      //println( n +  ":        " + math.log( h.totalProbability( trainingData last ) ) + "\n\n" )
      // val ezpzProb = h.easyPeasyTotalProbability( trainingData(0) )
      // println( "    " + ezpzProb )
      // val generalProb = h.generalProbability( trainingData(0) )
      // println( "    " + generalProb + " (" + (( generalProb - lastGenProb )/lastGenProb) + ")")

      //println( h )

      lastProb = newProb
      n = n + 1
      // lastGenProb = generalProb
    }
    */

    println( n + ":   " + h.generalProbability( trainingData last ) )

    println( "EM RESULTS IN: " )

    println( h )

    /*
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
    */
  }
}

object CoupledDevelopmentRunner {
  def main( args:Array[String]) {
    import scala.math.log

    val dataPath = args(0)

    val hiddenStates =
      Set( "S_0", "S_1", "S_2" ).flatMap{ x =>
        Set( "P_1","P_2","P_3" ).map{ y =>
          HiddenStatePair( x, y )
        }
      }

    val trainingData = io.Source.fromFile( dataPath ).getLines().toList.map(
      _.split(" ").toList.map{ w =>
        val Array( word, prosody ) = w.split( "#")
        ObservedStatePair( word, prosody )
      }
    ).filter( _.size > 2 )

    val observationTypes =// Set( uttStart, uttEnd  ) ++
      Set(
        trainingData.flatten
      ).flatten

    //println( trainingData )


    val h = new CoupledHMM( hiddenStates, observationTypes )
    h.randomize( 10 )

    println( "Hidden States:" )
    println( hiddenStates.mkString("","\n","\n\n" ) )

    println( "Observed Types:" )
    println( observationTypes.mkString("","\n","\n\n" ) )

    /*
    h.buildHMM( trainingData(0) )

    h.buildSlicedHMM( trainingData(0) )
    */

    /*
    println( "first computePartialCounts for: " + trainingData(0) )
    println( h.computePartialCounts(trainingData(0) ) )
    println( "\n\n\n\ndone\n\n\n\n" )
    */


    println( "BEGINNING EM" )
    var lastLogProb = 0D
    var deltaLogProb = 1D
    var n = 0
    while( ( math.abs( deltaLogProb ) > 0.00001 ) & n < 100 ) {
      val newLogProb = h.reestimate( trainingData )
      deltaLogProb = ((newLogProb-lastLogProb)/lastLogProb)
      println( n + ": " + newLogProb + " ("+  deltaLogProb + ")")
      println( h )
      lastLogProb = newLogProb
      /*
      if( n % 5 == 0 ) 
        println(
          h.argmax( trainingData ).map{_.mkString(""," ",".")}.mkString("\t","\n\t","\n")
        )
      */
      n += 1
    }

    println( "EM RESULTS IN: " )

    println( h )

    h.argmax( trainingData ).map{_.mkString(""," ",".")}.mkString("\t","\n\t","\n")

  }
}

