import fpinscala.state.{State,RNG}

object p1 {
  def uniform(inf: Double, sup: Double): RNG.Rand[Double] =
    RNG.flatMap(RNG._double){ d =>                      // same pattern as nonNegativeLessThan
      if (d > 0) RNG.unit(inf + d * (sup - inf))   // return this number !
      else uniform(inf, sup)                   // d == 0 not OK for open interval (inf, sup)
    }

  var r=RNG.Simple(0)

  def rollDice()={ //part a
    State(RNG.nonNegativeInt)
      .map2(
        State(RNG.nonNegativeInt)
      )((x,y)=>{(1+x%6,1+y%6)})
  }

  def unitCirclePoint()={ //part b
    State(uniform(-1,1))
      .map2(State(RNG.boolean))((x:Double,y:Boolean)=>{
        (x,Math.sqrt(1-x*x)*(if(y) -1 else 1))
      })
  }

  def myPie(n:Int=1)={ //part c
    def isInside(n:Int)={
      State.sequence(
        List.fill(n)(
          State(uniform(0,1))
            .map2(State(uniform(0,1)))((x,y)=>{x*x+y*y<1})
        )
      )
    }
    def countVals[A](s:State[RNG,List[A]])(f:(A)=>Boolean)={
      s.map(lst=>lst.foldRight(0)((x,y)=>{y + (if(f(x)) 1 else 0)}))
    }
    def calcPi()= {
      val accuracyList=isInside(n)
      val count=countVals(accuracyList)((x)=>{x})
      count.map(x=>1.0*x/n)
    }
    calcPi.run(r)
  }

  def main(args:Array[String])={
    println(rollDice.run(r))
    println(rollDice.run(rollDice.run(r)._2))

    println(unitCirclePoint.run(r))

    println(myPie(100000))
    println(Math.PI/4)
  }
}