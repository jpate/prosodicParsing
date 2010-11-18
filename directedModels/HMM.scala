package ProsodicParsing.Types

import cc.mallet.types.{LabelAlphabet,LabelSequence}
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm.inference.BruteForceInferencer
import cc.mallet.grmm.inference.Sampler;
import cc.mallet.grmm.inference.SamplingInferencer;
import cc.mallet.grmm.inference.GibbsSampler;
import scala.collection.immutable.{HashMap,HashSet}

abstract class ProbabilityDistribution {
  var pt:HashMap[String,Double]

  def toArray:Array[Double] = (
    pt.keySet.toList.sorted.map( element => pt( element ) )
  ).toArray

  def apply( s:String ) = pt(s)

  def randomize( centeredOn: Int ) {
    import scala.util.Random

    val r = new Random( 15 )

    pt = normalize(
        HashMap[String,Double]( pt.keySet.map( element =>
          element -> (r.nextDouble + centeredOn) ).toSeq: _*
        )
      )
  }

  def normalize( basisPT: HashMap[String,Double] ) = {
    val max = basisPT.values.reduceLeft(_+_)

    HashMap[String,Double](
      basisPT.keySet.map( element =>
        element -> basisPT(element)/max
      ).toSeq: _*
    )
  }
}

abstract class ConditionalProbabilityDistribution {
  var cpt:HashMap[String,HashMap[String,Double]]

  def apply( s:String ) = cpt(s)

  override def toString = cpt.toString

  def toArray:Array[Double] =
    cpt.keySet.toList.sorted.flatMap( fromKey =>
      cpt(fromKey).keySet.toList.sorted.map( toKey =>
        cpt(fromKey)(toKey)
      )
    ).toArray

  def randomize( centeredOn: Int ) {
    import scala.util.Random

    val r = new Random( 15 )

    cpt = normalize(
        HashMap[String,HashMap[String,Double]](
        cpt.keySet.map( fromStateName =>
            fromStateName -> (
              HashMap[String,Double]( cpt(fromStateName).keySet.map( toStateName =>
                toStateName -> (r.nextDouble + centeredOn) ).toSeq: _*
              )
            )
          ).toSeq: _*
        )
      )
  }

  def normalize( basisCPT: HashMap[String,HashMap[String,Double]] ) = {
    val maxes = HashMap[String,Double]() ++ basisCPT.keySet.map( thisKey =>
      thisKey -> basisCPT(thisKey).values.reduceLeft(_+_)
    )

    HashMap[String,HashMap[String,Double]](
      basisCPT.keySet.map( fromStateName =>
          fromStateName -> (
            HashMap[String,Double](
              basisCPT(fromStateName).keySet.map( toStateName =>
                toStateName -> basisCPT(fromStateName)(toStateName)/maxes(fromStateName)
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }
}

class HMM( numHiddenStates:Int, observationTypes:HashSet[String] ) {
  val stateNames = (1 to numHiddenStates) map ( "Q" + _ )
  object TransitionMatrix extends ConditionalProbabilityDistribution {
    // For now we'll initialize to a uniform transition matrix and define a
    // randomize method for people to have a random initialization whenever they
    // like
    var cpt = HashMap[String,HashMap[String,Double]](
      stateNames.map( fromStateName =>
          fromStateName -> (
            HashMap[String,Double](
              stateNames.map( toStateName =>
                toStateName -> 1D/numHiddenStates ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object EmissionMatrix extends ConditionalProbabilityDistribution {
    var cpt = HashMap[String,HashMap[String,Double]](
      stateNames.map( fromStateName =>
          fromStateName -> (
            HashMap[String,Double](
              observationTypes.map( toStateName =>
                toStateName -> 1D/observationTypes.size 
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
  }

  object InitialStateProbabilities extends ProbabilityDistribution {
    var pt = HashMap[String,Double] (
      stateNames.map( thisStateName =>
        thisStateName -> 1D/stateNames.size
      ).toSeq: _*
    )
  }

  def randomize(n:Int) {
    TransitionMatrix.randomize(n)
    EmissionMatrix.randomize(n)
  }

  val labels = new LabelAlphabet()// observationTypes.toArray )
  observationTypes.foreach( labels.lookupIndex( _, true ) )
  //val hmm = new FactorGraph()
  val hmm = new DirectedModel()
  //var observedAssignment = new LabelSequence( labels )
  var observedAssignment = new Assignment()
  def buildHMM( tokens:Array[String] ) {
    // TODO: CHECK TO MAKE SURE hmm SHOULD BE CLEARED. MAYBE WE CAN SAVE SOME
    // WORK BY JUST MODIFYING AN EXISTING FACTOR GRAPH IF THINGS ARE SLOW?
    hmm.clear();

    val hiddenStates = Array.tabulate(tokens.size)( _ => new Variable( numHiddenStates ) )
    val observations =
      Array.tabulate(tokens.size)( _ => new Variable( labels ) )
      //Array.tabulate(tokens.size)( _ => new Variable( observationTypes.size ) )

    ( 0 to tokens.size-1 ) foreach( i =>
      hiddenStates(i).setLabel("hidden."+i)
    )

    ( 0 to tokens.size-1 ) foreach{ i =>
      observations(i).setLabel("observed."+i)
      observedAssignment.addRow(
        new Assignment(
          Array( observations(i) ),
          Array( labels.lookupIndex( tokens(i) ) )
        )
      )
    }


    ( 1 to (tokens.size-1) ) foreach{ i =>
      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array(
              hiddenStates(i-1),
              hiddenStates(i)
            ),
            TransitionMatrix.toArray
          ),
          hiddenStates(i)
        )
      )
    }

    //print( EmissionMatrix.toArray.size )
    // println( EmissionMatrix )
    (0 to tokens.size-1 ) foreach ( i =>
      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array(
              hiddenStates(i),
              observations(i)
            ),
            EmissionMatrix.toArray
          ),
          observations(i)
        )
      )
    )



    //hmm.dump();
    (0 to labels.size-1).foreach( thisLabelIndex =>
      println( thisLabelIndex + ": " + labels.lookupLabel(thisLabelIndex))
    )
    //println( labels )
    //println( hmm )
  }

  //val inferencer = new BruteForceInferencer()
  //val inferencer = new SamplingInferencer( new GibbsSampler, 10 )

  def seeMarginals() {
    val inferencer = new JunctionTreeInferencer()
    inferencer.computeMarginals( hmm )
    
    observedAssignment.dump()
    //println( observedAssignment.asTable )
    inferencer.dumpLogJoint( observedAssignment )
    //println( "marginals")
    //hmm foreach ( hiddenVar =>
    //)

    //hmm.dump();
    //println( hmm.factors)
    // (hmm.factors).toArray.foreach{ thisFactor =>
    //   println( "<--" )
    //   println( thisFactor )
    //   println( "-->" )
    //   println( inferencer.lookupMarginal(  thisFactor.getVariable(0) ) )
    // }
  }
}

