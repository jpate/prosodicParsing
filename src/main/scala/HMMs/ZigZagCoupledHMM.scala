package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.types.LabelAlphabet
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.util.Maths
import collection.mutable.HashMap


/*
   a_1        a_2        a_3 ...
    ^          ^          ^
    |          |          |
    |          |          |
   p_1  -->_  p_2  -->_  p_3 ...
           /|  |      /|  |
          /    |     /    |
         /     |    /     |
        /      |   /      |
       /       v  /       v
   q_1  -->   q_2  -->   q_3 ...
    |          |          |
    |          |          |
    v          v          v
   o_1        o_2        o_3 ...

  */

class ZigZagCoupledHMM(
  hiddenStateTypesSet:Set[HiddenStatePair],
  observationTypesSet:Set[ObservedStatePair],
  idString:String
) extends CoupledHMM(hiddenStateTypesSet,observationTypesSet,idString) {


  override def buildHMM( tokens: List[ObservedStatePair] ) {
    localUniverse = new Universe()
    hmm = new DynamicBayesNet( tokens.size )

    stringLength = tokens.size

    val hiddenVarA = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, hiddAlphA, t ) )
    val hiddenVarB = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, hiddAlphB, t ) )

    hiddenVariables = hiddenVarA ++ hiddenVarB

    val obsVarA = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, obsAlphA, t ) )
    val obsVarB = Array.tabulate( tokens.size )( t => new DynamicVariable( localUniverse, obsAlphB, t ) )

    observations = obsVarA ++ obsVarB

    ( 0 to tokens.size - 1 ) foreach{ i =>
      hiddenVarA( i ).setLabel("hidden.XA"+i)
      hiddenVarB( i ).setLabel("hidden.B"+i)
      obsVarA( i ).setLabel("observation.XA"+i)
      obsVarB( i ).setLabel("observation.B"+i)
    }

    // initial states:
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVarA(0) ),
        initialStateProbabilitiesA.toLogArray
      )
    )
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVarB(0) ),
        initialStateProbabilitiesB.toLogArray
      )
    )

    ( 1 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i-1), hiddenVarB(i), hiddenVarA(i) ),
            transitionMatrixA.toLogArray
          ),
        hiddenVarA(i)
        ),
        Array(i-1,i),
        i
        //i-1
      )
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i-1), hiddenVarB(i-1), hiddenVarB(i) ),
            transitionMatrixB.toLogArray
          ),
          hiddenVarB(i)
        ),
        i-1
      )
    }

    // emissions
    ( 0 to tokens.size-1) foreach{ i =>
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarA(i), obsVarA(i) ),
            emissionMatrixA.toLogArray
          ),
          obsVarA(i)
        ),
        i
      )
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVarB(i), obsVarB(i) ),
            emissionMatrixB.toLogArray
          ),
          obsVarB(i)
        ),
        i
      )
    }
    //hmm.dump()
  }

  override def computePartialCounts( tokens:List[ObservedStatePair] ) = {
    buildSlicedHMM( tokens )

    inferencer.computeMarginals( hmm )

    val initialStateCountsA = HashMap(
      hiddATypes.map{ qs =>
        qs ->  Double.NegativeInfinity
      }.toSeq:_*
    )

    val initialStateCountsB = HashMap(
      hiddBTypes.map{ qs =>
        qs ->  Double.NegativeInfinity
      }.toSeq:_*
    )


    val emissionCountsA = HashMap(
      hiddATypes.map{ q =>
        q -> HashMap(
          obsATypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionCountsB = HashMap(
      hiddBTypes.map{ q =>
        q -> HashMap(
          obsBTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsA = HashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> HashMap(
          hiddATypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCountsB = HashMap(
      hiddenStateTypes.map{ qsFrom =>
        qsFrom -> HashMap(
          hiddBTypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    ( 0 to (tokens.size-2) ) foreach{ i =>
      val( fromVarA, fromVarB, toVarA, toVarB ) = (
        hiddenVariables(i), hiddenVariables(tokens.size+i),
        hiddenVariables(i+1), hiddenVariables(tokens.size+i+1)
      )

      val ObservedStatePair( obsA, obsB ) = tokens(i)

      val transitionA = inferencer.lookupMarginal(
        new HashVarSet( Array( fromVarA, toVarB, toVarA ) )
      )
      val transitionB = inferencer.lookupMarginal(
        new HashVarSet( Array( fromVarA, fromVarB, toVarB ) )
      )

      hiddenStateTypes.foreach{ qsFrom =>
        val HiddenStatePair( qFromAString, bString ) = qsFrom
        val qFromA = HiddenState( qFromAString )
        val qToB = HiddenState( bString )
        val qFromB = HiddenState( bString )

        // first add up for the A stream
        hiddATypes.foreach{ qToA =>

          val thisTransitionCountA = transitionA.logValue(
            new Assignment(
              Array( fromVarA, toVarB, toVarA ),
              Array(
                hiddAlphA.lookupIndex( qFromA ),
                hiddAlphB.lookupIndex( qToB ),
                hiddAlphA.lookupIndex( qToA )
              )
            )
          )

              // if( qFromA == HiddenState( "C_B" ) && qToA == HiddenState( "C_O" ) ) {
              //   println(
              //     "<<< HiddenState(\"C_B\") is " + hiddAlphA.lookupIndex(qFromA ) + " ;  " +
              //     "HiddenState(\"C_O\") is " + hiddAlphA.lookupIndex(qToA ) + " ;  " +
              //     "qFromB is " + qFromB + " ; " +
              //     thisTransitionCountA + " >>>" 
              //   )
              // }

          if( i == 0 ) {
            initialStateCountsA( qFromA ) = Maths.sumLogProb(
                initialStateCountsA( qFromA ),
                thisTransitionCountA
            )
          }

          transitionCountsA(qsFrom)(qToA) = Maths.sumLogProb(
              transitionCountsA(qsFrom)(qToA),
              thisTransitionCountA
          )

          emissionCountsA(qFromA)( ObservedState(obsA) ) = Maths.sumLogProb(
              emissionCountsA(qFromA)( ObservedState(obsA) ),
              thisTransitionCountA
          )

          if( i == tokens.size - 2 ) {
            val ObservedStatePair( lastObsAString, _ ) = tokens(i+1)

            emissionCountsA(qToA)( ObservedState(lastObsAString) ) = Maths.sumLogProb(
                emissionCountsA(qToA)( ObservedState(lastObsAString) ),
                thisTransitionCountA
            )
          }

        }

        hiddBTypes.foreach{ qToB =>

          val thisTransitionCountB = transitionB.logValue(
            new Assignment(
              Array( fromVarA, fromVarB, toVarB ),
              Array(
                hiddAlphA.lookupIndex( qFromA ),
                hiddAlphB.lookupIndex( qFromB ),
                hiddAlphB.lookupIndex( qToB )
              )
            )
          )

          if( i == 0 ) {
            initialStateCountsB( qFromB ) = Maths.sumLogProb(
                initialStateCountsB( qFromB ),
                thisTransitionCountB
            )
          }

          transitionCountsB(qsFrom)(qToB) = Maths.sumLogProb(
              transitionCountsB(qsFrom)(qToB),
              thisTransitionCountB
          )

          emissionCountsB(qFromB)( ObservedState(obsB) ) = Maths.sumLogProb(
              emissionCountsB(qFromB)( ObservedState(obsB) ),
              thisTransitionCountB
          )

          if( i == tokens.size - 2 ) {
            val ObservedStatePair( _, lastObsBString ) = tokens(i+1)

            emissionCountsB(qToB)( ObservedState(lastObsBString) ) = Maths.sumLogProb(
                emissionCountsB(qToB)( ObservedState(lastObsBString) ),
                thisTransitionCountB
            )
          }

        }
      }
    }

    CoupledHMMPartialCounts(
      generalProbability( tokens ),
      initialStateCountsA,
      initialStateCountsB,
      transitionCountsA,
      transitionCountsB,
      emissionCountsA,
      emissionCountsB
    )
  }

}

