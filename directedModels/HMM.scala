package ProsodicParsing.Types

import cc.mallet.types.{LabelAlphabet,LabelSequence}
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm.inference.BruteForceInferencer
import cc.mallet.grmm.inference.Sampler;
import cc.mallet.grmm.inference.SamplingInferencer;
import cc.mallet.grmm.inference.GibbsSampler;
//import scala.collection.JavaConversions._
import scala.collection.immutable.{HashMap,HashSet}
import scala.collection.mutable.{HashMap => MHashMap}

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

  def setPT( newPT:HashMap[String,Double] ) {
    pt = newPT
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

  def randomize( centeredOn:Int ) {
    randomize( centeredOn, 15 )
  }

  def randomize( centeredOn:Int, seed:Int ) {
    import scala.util.Random

    val r = new Random( seed )

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

  def setCPT( newCPT:HashMap[String,HashMap[String,Double]] ) {
    cpt = newCPT
  }

  def cptCopy = cpt
}

abstract class CPTDomain {
  def parent;
  def child;
}

case class StateTransitions( fromState:String, toState:String )
    //  extends CPTDomain {
    //  def parent = fromState
    //  def child = toState
    //}

case class Emissions( state:String, emission:String )
    //   extends CPTDomain {
    //   def parent = state
    //   def child = emission
    // }

class HMM( numHiddenStates:Int, observationTypes:HashSet[String] ) {
  val labels = new LabelAlphabet()
  observationTypes.foreach( labels.lookupIndex( _, true ) )

  val stateNames = (0 to (numHiddenStates-1)) map ( "Q_" + _ )


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
  

  def setTransitionMatrix( newTransitions:HashMap[String,HashMap[String,Double]] ) {
    TransitionMatrix.setCPT( newTransitions )
  }

  def setEmissionMatrix( newEmissions:HashMap[String,HashMap[String,Double]] ) {
    EmissionMatrix.setCPT( newEmissions )
  }

  def randomize(n:Int) {
    TransitionMatrix.randomize(n)
    EmissionMatrix.randomize(n)
  }

  def reestimate(
    stateProbs: MHashMap[String,Double],
    stateTransitions: MHashMap[StateTransitions,Double],
    emissions: MHashMap[Emissions,Double]
  ) {
    val newTransitionMatrix = TransitionMatrix.cptCopy

    stateTransitions.keysIterator.foreach{ case StateTransitions( fromState, toState ) =>
      newTransitionMatrix( fromState ).updated(
        toState,
        ( stateTransitions( StateTransitions( fromState, toState ) ) /
          stateProbs( fromState ) )
      )
    }

    val newEmissionsMatrix = EmissionMatrix.cptCopy

    emissions.keysIterator.foreach{ case Emissions( state, obs ) =>
      newEmissionsMatrix(state).updated(
        obs,
        emissions( Emissions( state, obs ) ) / stateProbs( state )
      )
    }

    setTransitionMatrix( newTransitionMatrix )

    setEmissionMatrix( newEmissionsMatrix )

    println( "Transitions!" )
    println( TransitionMatrix )
    println( "Emissions!" )
    println( EmissionMatrix )
  }



  //val hmm = new FactorGraph()
  var hmm = new DirectedModel()
  //var observedAssignment = new LabelSequence( labels )
  //var observedAssignment = new Assignment()
  var hiddenVariables:Array[Variable] = Array()
  var observations:Array[Variable] = Array()
  //var inferenceGraph = new FactorGraph()

  def buildHMM( tokens:Array[String] ) {
    // TODO: CHECK TO MAKE SURE hmm SHOULD BE CLEARED? MAYBE WE CAN SAVE SOME
    // WORK BY JUST MODIFYING AN EXISTING FACTOR GRAPH IF THINGS ARE SLOW?
    hmm.clear();

    hiddenVariables = Array.tabulate(tokens.size)( _ => new Variable( numHiddenStates ) )
    observations = Array.tabulate(tokens.size)( _ => new Variable( labels ) )

    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }


        // val observedAssignment = new Assignment(
        //   observations,
        //   tokens.map( labels.lookupIndex(_) )
        // )


    // state transitions
    ( 1 to (tokens.size-1) ) foreach{ i =>
      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array(
              hiddenVariables(i-1), hiddenVariables(i)),
              TransitionMatrix.toArray
          ),
          hiddenVariables(i)
        )
      )
    }

    //print( EmissionMatrix.toArray.size )
    // println( EmissionMatrix )
    ( 0 to tokens.size-1 ) foreach { i =>
      val thisObservation = new Assignment(
        observations(i) ,
        labels.lookupIndex( tokens(i) )
      )


      hmm.addFactor(
        new CPT(
          new TableFactor(
            Array(
              hiddenVariables(i),
              observations(i)
            ),
            EmissionMatrix.toArray
          ),
          observations(i),
          thisObservation
        )
      )
    }

      // hmm.dump();
    //println("<<")
    //println( observedAssignment )
    //observedAssignment.dump()
    //println(">>")

    //val test = hmm.slice( observedAssignment )
    //hmm = hmm.slice( observedAssignment )
    //println( "Before slice: " )
    //inferenceGraph = hmm
    //seeMarginals()

    //println( hmm.slice( observedAssignment ) )
    //hmm = hmm.slice( observedAssignment )
    //val factors = hmm.slice( observedAssignment )
    //inferenceGraph = new FactorGraph( Array( hmm.slice( observedAssignment ) ) )

    // println("===")
    // println( new DirectedModel( new FactorGraph( Array( factors ) ) ) )
    // println("===")

    //hmm = new DirectedModel()

    //factors.factors().foreach(
    //  hmm.addFactor( _ )
    //)

    //hmm = new DirectedModel( factors )

    //println( "done" )
    println( "After slice: " )
    seeMarginals()


    //hmm.dump();
    //(0 to labels.size-1).foreach( thisLabelIndex =>
    //  println( thisLabelIndex + ": " + labels.lookupLabel(thisLabelIndex))
    //)
    //println( labels )
    //println( hmm )
  }

  //val inferencer = new BruteForceInferencer()
  //val inferencer = new SamplingInferencer( new GibbsSampler, 10 )


  def emStep( sequence:Array[String] ) {

    val stateProbs = new MHashMap[String,Double] {
      override def default( s:String ) = 0D
    }

    val sequencePairs = new MHashMap[StateTransitions,Double] {
      override def default( s:StateTransitions ) = 0D
    }

    var newTransitionMatrix = new MHashMap[String,MHashMap[String,Double]] /*{
      override def default( s1:String ) = new MHashMap[String,Double] {
        override def default( s2:String) = 0D
      }
    } */

    val emissionsPairs = new MHashMap[Emissions,Double] {
      override def default( s:Emissions ) = 0D
    }

    val newEmissionMatrix = new MHashMap[String,MHashMap[String,Double]] {
      override def default( s:String ) = new MHashMap[String,Double]() {
        override def default( s:String) = 0D
      }
    }

    buildHMM( sequence )

    val inferencer = new JunctionTreeInferencer()
    inferencer.computeMarginals( hmm )

    // !!! DO NOT FORGET TO DO THE LAST OBSERVATION/HIDDEN STATE !!!
    val firstState = inferencer.lookupMarginal( hiddenVariables(0) )
    ( 0 to (numHiddenStates-1) ) foreach ( j =>
      stateProbs( "Q_" ) +=
        firstState.value( new Assignment( hiddenVariables(0) , j ) )
    )

    val firstTransition = inferencer.lookupMarginal(
      new HashVarSet( Array( hiddenVariables(0), hiddenVariables(1) ) )
    )

    ( 0 to (numHiddenStates-1) ) foreach ( j =>
      (0 to (numHiddenStates-1) ) foreach { k =>
        val p_0_1:Double = firstTransition.value(
            new Assignment(
              Array( hiddenVariables(0), hiddenVariables(1) ),
              Array( j, k )
            )
          )

        sequencePairs( StateTransitions( "Q_"+j, "Q_"+k) ) += p_0_1

        emissionsPairs( Emissions( "Q_"+j, sequence(0) ) ) +=
          p_0_1
      }
    )

    ( 1 to (hiddenVariables.length-2) ) foreach { i =>
      val singleState = inferencer.lookupMarginal( hiddenVariables(i) )

      ( 0 to (numHiddenStates-1) ) foreach ( j =>
        stateProbs( "Q_" + i ) +=
          singleState.value( new Assignment( hiddenVariables(i) , j ) )
      )

      val transition = inferencer.lookupMarginal(
        new HashVarSet( Array( hiddenVariables(i), hiddenVariables(i+1) ) )
      )


      ( 0 to (numHiddenStates-1) ) foreach ( j =>
        (0 to (numHiddenStates-1) ) foreach { k =>
          val p_i_j:Double = transition.value(
              new Assignment(
                Array( hiddenVariables(i), hiddenVariables(i+1) ),
                Array( j, k )
              )
            )

          sequencePairs( StateTransitions( "Q_"+j, "Q_"+k) ) += p_i_j

          emissionsPairs( Emissions( "Q_"+j, sequence(i) ) ) +=
            p_i_j
        }
      )
    }

    reestimate( stateProbs, sequencePairs, emissionsPairs )

    val assignments = hmm.assignmentIterator()


    var totalLogLik = 0D

    println( assignments.assignment.dumpToString() )
    totalLogLik += inferencer.lookupLogJoint( assignments.assignment )
    assignments.next
    println( assignments.assignment.dumpToString() )
    totalLogLik += inferencer.lookupLogJoint( assignments.assignment )
    assignments.next
    println( assignments.assignment.dumpToString() )
    totalLogLik += inferencer.lookupLogJoint( assignments.assignment )
    assignments.next
    println( assignments.assignment.dumpToString() )
    totalLogLik += inferencer.lookupLogJoint( assignments.assignment )
    assignments.next
    println( assignments.assignment.dumpToString() )
    totalLogLik += inferencer.lookupLogJoint( assignments.assignment )
    assignments.next
    println( assignments.assignment.dumpToString() )
    totalLogLik += inferencer.lookupLogJoint( assignments.assignment )
    /*while( assignments.hasNext() ) {
      val newAssn = assignments.next()
      println( newAssn )
      //totalLogLik += inferencer.lookupLogJoint( newAssn )
    }*/

    /*
    assignments.toArray.foreach( someAssignment =>
      println( inferencer.lookupLogJoint( someAssignment ) )
    )
    */

    println( "Log Joint: " + totalLogLik )

    //println( computeTotalProbability )
    //println( inferencer.lookupLogJoint( new Assignment ) )
  }


  def seeMarginals() {
    val inferencer = new JunctionTreeInferencer()
    inferencer.computeMarginals( hmm )

    hiddenVariables foreach ( someHiddenVar =>
      println( inferencer.lookupMarginal( someHiddenVar ).dumpToString() )
    )

    //hmm.dump()
    //observedAssignment.dump()
    //println( observedAssignment.asTable )
    //inferencer.dumpLogJoint( observedAssignment )
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

