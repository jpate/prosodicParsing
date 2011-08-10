package ProsodicParsing.HMMs

import ProsodicParsing.types._
import cc.mallet.types.LabelAlphabet
import cc.mallet.grmm._
import cc.mallet.grmm.types._
import cc.mallet.grmm.inference.ForwardBackwardInferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer
import cc.mallet.grmm.inference.JunctionTreeInferencer._
import cc.mallet.util.Maths
//import scala.collection.immutable.{HashMap,HashSet}
import scala.collection.mutable.{HashMap,HashSet}
//import scala.collection.mutable.{HashMap => HashMap}

class PlainHMFG(
  hiddenStateTypesSet:Set[HiddenState],
  observationTypesSet:Set[ObservedState],
  idString:String
) extends AbstractHMM[HiddenState,ObservedState] (hiddenStateTypesSet, observationTypesSet) {

  val hmmID = idString

  val observationAlphabet = new LabelAlphabet()
  val hiddenStateAlphabet = new LabelAlphabet()

  observationTypes.foreach( observationAlphabet.lookupIndex( _, true ) )

  val hiddenStateIndexToLabel = new HashMap[Int,HiddenState]
  hiddenStateTypes.foreach( q =>
    hiddenStateIndexToLabel( hiddenStateAlphabet.lookupIndex( q, true ) ) = q
  )

  //def hiddenIndexToLabel( index:Int ) = hiddenStateIndexToLabel( index )

  def assignmentToViterbiString( maxAssn:Assignment ) = {
    hiddenVariables.map{ hiddenVar =>
      hiddenStateIndexToLabel( maxAssn.get( hiddenVar ) )
    }.toList
  }


  val transitionMatrix =
    new ConditionalLogProbabilityDistribution( hiddenStateTypesSet, hiddenStateTypesSet )

  var emissionMatrix =
    new ConditionalLogProbabilityDistribution ( hiddenStateTypesSet, observationTypesSet )

  val initialStateProbabilities = new LogProbabilityDistribution( hiddenStateTypesSet )
  val finalStateProbabilities = new LogProbabilityDistribution( hiddenStateTypesSet )

  def parameters = List(
    initialStateProbabilities,
    finalStateProbabilities,
    transitionMatrix,
    emissionMatrix
  )

  def packageParameters = PlainHMFGParameters(
    initialStateProbabilities.pt,
    finalStateProbabilities.pt,
    transitionMatrix.cpt,
    emissionMatrix.cpt
  )

  def initialPartialCounts = PlainHMFGPartialCounts(
      0D,
      HashMap(
        hiddenStateTypes.map( thisStateName =>
          thisStateName -> Double.NegativeInfinity
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( thisStateName =>
          thisStateName -> Double.NegativeInfinity
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( fromStateName =>
          fromStateName -> (
            HashMap(
              hiddenStateTypes.map( toStateName =>
                toStateName -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      ),
      HashMap(
        hiddenStateTypes.map( qsFrom =>
          qsFrom -> (
            HashMap(
              observationTypes.map( obs =>
                obs -> Double.NegativeInfinity
              ).toSeq: _*
            )
          )
        ).toSeq: _*
      )
    )

  def setParams( newParams:Parameters ) {
    val PlainHMFGParameters(
      initialProbs,
      finalProbs,
      transitions,
      emissions
    ) = newParams
    initialStateProbabilities.setPT( initialProbs )
    finalStateProbabilities.setPT( finalProbs )
    transitionMatrix.setCPT( transitions )
    emissionMatrix.setCPT( emissions )
  }



  def buildHMM( tokens:List[ObservedState] ) {
    localUniverse = new Universe()
    hmm = new DynamicBayesNet(tokens.size)


    hiddenVariables = Array.tabulate(tokens.size)( t => new DynamicVariable( localUniverse,
    hiddenStateAlphabet, t ) )
    observations = Array.tabulate(tokens.size)( t => new DynamicVariable( localUniverse,
    observationAlphabet, t ) )


    ( 0 to tokens.size-1 ) foreach{ i =>
      hiddenVariables(i).setLabel("hidden."+i)
      observations(i).setLabel("observed."+i)
    }

    // initial states:
    hmm.addHiddenTimedFactor(
      new CPT(
        LogTableFactor.makeFromLogValues(
          Array( hiddenVariables(0), hiddenVariables(1) ),
          transitionMatrix.toLogArray
        ),
        hiddenVariables(1)
      )
    )
    hmm.addInitialStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVariables(0) ),
        initialStateProbabilities.toLogArray
      )
    )



    // state transitions
    ( 2 to (tokens.size-1) ) foreach{ i =>
      hmm.addHiddenTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVariables(i-1), hiddenVariables(i) ),
              transitionMatrix.toLogArray
          ),
          hiddenVariables(i)
        )
      )
    }

    // emissions
    ( 0 to tokens.size-1 ) foreach { i =>
      hmm.addObservedTimedFactor(
        new CPT(
          LogTableFactor.makeFromLogValues(
            Array( hiddenVariables(i), observations(i) ),
            emissionMatrix.toLogArray
          ),
          observations(i)
        )
      )
    }

    // Final states
    hmm.addFinalStateProbabilities(
      LogTableFactor.makeFromLogValues(
        Array( hiddenVariables.last ),
        finalStateProbabilities.toLogArray
      )
    )
  }


  def generateObservationSequence( tokens:List[ObservedState] ) = new Assignment(
      observations, tokens.map{ w =>
        observationAlphabet.lookupIndex( w )
      }.toArray
    )


  def computePartialCounts( sequence:List[ObservedState] ) = {
    val stringLogProb = generalProbability( sequence )
    buildSlicedHMM( sequence )
    //buildHMM( sequence )


    val inferencer = new ForwardBackwardInferencer()
    //println( "calling computeMarginals from within computePartialCounts directly" );
    //inferencer.computeMarginals( hmm )

    inferencer.computeMarginals( hmm )

    val initialStateCounts = HashMap(
      hiddenStateTypes.map{ _ -> Double.NegativeInfinity }.toSeq:_*
    )

    val finalStateCounts = HashMap(
      hiddenStateTypes.map{ _ -> Double.NegativeInfinity }.toSeq:_*
    )

    val emissionCounts = HashMap(
      hiddenStateTypes.map{ q =>
        q -> HashMap(
          observationTypes.map{ obs =>
            obs -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val transitionCounts = HashMap(
      hiddenStateTypes.map{ qFrom =>
        qFrom -> HashMap(
          hiddenStateTypes.map{ qTo =>
            qTo -> Double.NegativeInfinity
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    // loop through all transitions
    //( hiddenVariables.init zip hiddenVariables.tail ) foreach { case ( fromVar, toVar ) =>
    ( 0 to (hiddenVariables.size-2) ) foreach { i =>

      val (fromVar, toVar) = (hiddenVariables(i), hiddenVariables(i+1) )

      val thisTransition = inferencer.lookupMarginal( new HashVarSet( Array( fromVar, toVar ) ) )
      hiddenStateTypes foreach { qFrom =>
        hiddenStateTypes foreach { qTo =>


          val thisTransitionCount = thisTransition.logValue(
            new Assignment(
              Array( fromVar, toVar ),
              Array(
                hiddenStateAlphabet.lookupIndex( qFrom ),
                hiddenStateAlphabet.lookupIndex( qTo )
              )
            )
          )


          transitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              transitionCounts(qFrom)(qTo),
              thisTransitionCount
          )

          if( i == 0 )
            initialStateCounts(qFrom) = Maths.sumLogProb(
                initialStateCounts(qFrom),
                thisTransitionCount
            )


          emissionCounts(qFrom)(sequence(i)) = Maths.sumLogProb(
              emissionCounts(qFrom)(sequence(i)),
              thisTransitionCount
          )

          if( i == (hiddenVariables.size-2) ) {
            emissionCounts(qTo)(sequence(i+1)) = Maths.sumLogProb(
                emissionCounts(qTo)(sequence(i+1)),
                thisTransitionCount
            )
            finalStateCounts(qTo) = Maths.sumLogProb(
                finalStateCounts(qTo),
                thisTransitionCount
            )
          }
        }
      }
    }

    PlainHMFGPartialCounts(
      generalProbability( sequence ),
      HashMap(
        initialStateCounts.keySet.map{ q =>
          q -> initialStateCounts(q)
        }.toSeq:_*
      ),
      HashMap(
        finalStateCounts.keySet.map{ q =>
          q -> finalStateCounts(q)
        }.toSeq:_*
      ),
      HashMap(
        transitionCounts.keySet.map{ from =>
          from -> HashMap(
            transitionCounts(from).keySet.map{ to =>
              to -> transitionCounts(from)(to)
            }.toSeq:_*
          )
        }.toSeq:_*
      ),
      HashMap(
        emissionCounts.keySet.map{ q =>
          q -> HashMap(
            emissionCounts(q).keySet.map{ obs =>
              obs -> emissionCounts(q)(obs)
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
  }


  def reestimate( corpus:List[List[ObservedState]] ) = {
    import math.{log,exp}

    var corpusInitialStateDenominator = Double.NegativeInfinity
    val corpusInitialStateCounts = new HashMap[HiddenState,Double]{
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    var corpusFinalStateDenominator = Double.NegativeInfinity
    val corpusFinalStateCounts = new HashMap[HiddenState,Double]{
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    val corpusTransitionCounts = new HashMap[HiddenState,HashMap[HiddenState,Double]]{
      override def default( qFrom:HiddenState ) = {
        this += Pair(
          qFrom,
          new HashMap[HiddenState,Double]{
            override def default( qTo:HiddenState ) = {
              this += Pair( qTo, Double.NegativeInfinity )
              this(qTo)
            }
          }
        )
        this(qFrom)
      }
    }

    val corpusTransitionDenominator = new HashMap[HiddenState,Double]{
      override def default( qFrom:HiddenState ) = Double.NegativeInfinity
    }

    val corpusEmissionCounts = new HashMap[HiddenState,HashMap[ObservedState,Double]]{
      override def default( q:HiddenState ) = {
        this += Pair(
          q,
          new HashMap[ObservedState,Double]{
            override def default( o:ObservedState ) = {
              this += Pair( o, Double.NegativeInfinity )
              this(o)
            }
          }
        )
        this(q)
      }
    }

    val corpusEmissionDenominator = new HashMap[HiddenState,Double]{
      override def default( q:HiddenState ) = Double.NegativeInfinity
    }

    var totalCorpusLogProb = 0D

    var n = 0
    corpus.foreach{ string =>
      // sums over time
      val PlainHMFGPartialCounts(
        stringLogProb,
        initialStateCounts,
        finalStateCounts,
        transitionCounts,
        emissionCounts
      ) = computePartialCounts( string ) 


      var stringInitialStateDenominator = Double.NegativeInfinity

      val stringInitialStateCounts = new HashMap[HiddenState,Double]{
        override def default( q:HiddenState ) = Double.NegativeInfinity
      }

      var stringFinalStateDenominator = Double.NegativeInfinity

      val stringFinalStateCounts = new HashMap[HiddenState,Double]{
        override def default( q:HiddenState ) = Double.NegativeInfinity
      }


      val stringTransitionCounts = new HashMap[HiddenState,HashMap[HiddenState,Double]]{
        override def default( qFrom:HiddenState ) = {
          this += Pair(
            qFrom,
            new HashMap[HiddenState,Double]{
              override def default( qTo:HiddenState ) = {
                this += Pair( qTo, Double.NegativeInfinity )
                this(qTo)
              }
            }
          )
          this(qFrom)
        }
      }

      val stringTransitionDenominator = new HashMap[HiddenState,Double]{
        override def default( qFrom:HiddenState ) = Double.NegativeInfinity
      }

      val stringEmissionCounts = new HashMap[HiddenState,HashMap[ObservedState,Double]]{
        override def default( q:HiddenState ) = {
          this += Pair(
            q,
            new HashMap[ObservedState,Double]{
              override def default( o:ObservedState ) = {
                this += Pair( o, Double.NegativeInfinity )
                this(o)
              }
            }
          )
          this(q)
        }
      }

      val stringEmissionDenominator = new HashMap[HiddenState,Double]{
        override def default( q:HiddenState ) = Double.NegativeInfinity
      }

      // sum over strings, scaling the numerator and denominator summands by the string probability
      // initialStateCounts.keySet.foreach{ q =>
      // }

      transitionCounts.keySet.foreach{ qFrom =>
        transitionCounts(qFrom).keySet.foreach{ qTo =>
          stringTransitionDenominator(qFrom) = Maths.sumLogProb(
              stringTransitionDenominator(qFrom),
              transitionCounts(qFrom)(qTo)
          )
          stringTransitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              stringTransitionCounts(qFrom)(qTo),
              transitionCounts(qFrom)(qTo)
          )
        }

        stringInitialStateDenominator = Maths.sumLogProb(
            stringInitialStateDenominator,
            initialStateCounts(qFrom)
        )
        stringInitialStateCounts(qFrom) = Maths.sumLogProb(
            stringInitialStateCounts(qFrom),
            initialStateCounts(qFrom)
        )

        stringFinalStateDenominator = Maths.sumLogProb(
            stringFinalStateDenominator,
            finalStateCounts(qFrom)
        )
        stringFinalStateCounts(qFrom) = Maths.sumLogProb(
            stringFinalStateCounts(qFrom),
            finalStateCounts(qFrom)
        )

        emissionCounts(qFrom).keySet.foreach{ o =>
          stringEmissionDenominator(qFrom) = Maths.sumLogProb(
              stringEmissionDenominator(qFrom),
              emissionCounts(qFrom)(o)
          )
          stringEmissionCounts(qFrom)(o) = Maths.sumLogProb(
              stringEmissionCounts(qFrom)(o),
              emissionCounts(qFrom)(o)
          )
        }
      }

      corpusInitialStateDenominator = Maths.sumLogProb(
          corpusInitialStateDenominator,
          stringInitialStateDenominator
      )

      corpusFinalStateDenominator = Maths.sumLogProb(
          corpusFinalStateDenominator,
          stringFinalStateDenominator
      )

      stringTransitionCounts.keySet.foreach{ qFrom =>
        stringTransitionCounts(qFrom).keySet.foreach{ qTo =>
          corpusTransitionCounts(qFrom)(qTo) = Maths.sumLogProb(
              corpusTransitionCounts(qFrom)(qTo),
              stringTransitionCounts(qFrom)(qTo) //- stringLogProb
          )
        }
        corpusInitialStateCounts(qFrom) = Maths.sumLogProb(
            corpusInitialStateCounts(qFrom),
            stringInitialStateCounts(qFrom) //- stringLogProb
        )
        corpusTransitionDenominator(qFrom) = Maths.sumLogProb(
            corpusTransitionDenominator(qFrom),
            stringTransitionDenominator(qFrom) //- stringLogProb
        )
        stringEmissionCounts(qFrom).keySet.foreach{ obs =>
          corpusEmissionCounts(qFrom)(obs) = Maths.sumLogProb(
              corpusEmissionCounts(qFrom)(obs),
              stringEmissionCounts(qFrom)(obs) //- stringLogProb
          )
        }
        corpusEmissionDenominator(qFrom) = Maths.sumLogProb(
            corpusEmissionDenominator(qFrom),
            stringEmissionDenominator(qFrom) //- stringLogProb
        )
      }


      totalCorpusLogProb += stringLogProb
    }

    //val corpusInitialStateCountsTotal = corpusInitialStateCounts.values.sum
    val initialStateProbs = HashMap(
      corpusInitialStateCounts.keySet.map{ q =>
        q -> (
          corpusInitialStateCounts( q ) - corpusInitialStateDenominator
        )
      }.toSeq:_*
    )
    val finalStateProbs = HashMap(
      corpusInitialStateCounts.keySet.map{ q =>
        q -> (
          corpusFinalStateCounts( q ) - corpusFinalStateDenominator
        )
      }.toSeq:_*
    )

    val transitionProbs = HashMap(
      corpusTransitionCounts.keySet.map{ qFrom =>
        qFrom -> HashMap(
          corpusTransitionCounts(qFrom).keySet.map{ qTo =>
            qTo -> (
              corpusTransitionCounts( qFrom )( qTo ) - corpusTransitionDenominator( qFrom )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val emissionProbs = HashMap(
      corpusEmissionCounts.keySet.map{ q =>
        q -> HashMap(
          corpusEmissionCounts(q).keySet.map{ obs =>
            obs -> (
              corpusEmissionCounts( q )( obs ) - corpusEmissionDenominator( q )
            )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    initialStateProbabilities.setPT( initialStateProbs )
    finalStateProbabilities.setPT( finalStateProbs )
    transitionMatrix.setCPT( transitionProbs )
    emissionMatrix.setCPT( emissionProbs )


    //normalize

    totalCorpusLogProb
    //corpus.map( s => math.log( generalProbability( s ) ) ).sum
  }




  override def toString =
    "  == HMFG Parameters == \n" +
    "\nInitialProbabilities" +
    initialStateProbabilities +
    "\nFinalProbabilities" +
    finalStateProbabilities +
    "\nTransitions:" +
    transitionMatrix +
    "\nEmissions" +
    emissionMatrix
}

