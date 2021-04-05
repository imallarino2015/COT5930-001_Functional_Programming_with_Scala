import fpinscala.state.{State,RNG}

case class Course(name:String, roster:List[String]) { //part d
  def enroll(student: String):
  State[Course, Unit] = State.modify {
    case Course(name, roster) =>
      if (roster.contains(student))
        Course(name, student::roster)
      else
        Course(name, roster)
  }

  def enrolled(student: String):
  State[Course, Boolean] = {
    for{
      c<-State.get
    }yield (c.roster.exists((x)=>{x==student}))
  }

  def count:
  State[Course, Int] = {
    for{
      c<-State.get
    }yield (c.roster.length)
  }
}

object p2 {
  def uniform(inf: Double, sup: Double): RNG.Rand[Double] =
    RNG.flatMap(RNG._double){ d =>                      // same pattern as nonNegativeLessThan
      if (d > 0) RNG.unit(inf + d * (sup - inf))   // return this number !
      else uniform(inf, sup)                   // d == 0 not OK for open interval (inf, sup)
    }

  def probCond[B](ps: State[RNG,Double],prob: Double, btrue: => B, bfalse: =>B):
  State[RNG,B] = { //part b
    ps.cond(
      (x) => {
        1.0-x <= prob
      },
      btrue,
      bfalse
    )
  }

  def unfairCoinTosses(n:Int, prob:Double):
  State[RNG,List[String]]= { //part c
    State.sequence(List.fill(n)(probCond(
      State(uniform(0.0,1.0)),
      prob,
      "H",
      "T"
    )))
  }

  def main(args:Array[String]){
    val r: RNG.Simple = RNG.Simple(0)
    val testState = State(uniform(0,1))
    println(testState.run(r)._1)

    println(testState.cond((x)=>{x * 100 % 2 == 0}, "even", "odd").run(r))
    println(testState.condMap((x)=>{x * 100 % 2 == 0}, "even", "odd").run(r))

    println(probCond(testState,.55,"Over 55%","Under 55%").run(r))

    println(unfairCoinTosses(10,testState.run(r)._1).run(r))
  }
}
