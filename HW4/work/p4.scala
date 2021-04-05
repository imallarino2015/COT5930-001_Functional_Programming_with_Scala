import java.util.concurrent.Executors

import fpinscala.parallelism.Par

object p4 {
  def parMap[A,B](ps: List[A])(f: A => B):
  Par.Par[List[B]] = Par.fork {
    val fbs: List[Par.Par[B]] = ps.map(Par.asyncF(f))
    Par.sequence(fbs)
  }

  def parZipWith[A,B,C](sa: List[A], sb: List[B])(f: (A, B) => C):
  Par.Par[List[C]] = { //Part a
    Par.
      sequence(sa.zip(sb).map(Par.asyncF((x) => {
        f(x._1, x._2)
      })))
  }

  def parOrElse[A, B >: A](opt: =>Option[A], alternative: => Option[B]):
  Par.Par[Option[B]]={
    Par.map2(Par.fork(Par.unit(opt)), Par.fork(Par.unit(alternative)))(
      (a:Option[A],b:Option[B])=>{
        a.orElse(b)
      }
    )
  }

  //  //Part d
  //  asyncF(f)(x) == unit(f(x))
  //  (a => lazyUnit(f(a)))(x) == unit(f(x))
  //  lazyUnit(f(x)) == unit(f(x))
  //  fork(unit(f(x))) == unit(f(x))
  //  unit(f(x)) == unit(f(x))

  //  //Part e
  //  map(map(y)(g))(f) == map(y)(f compose g)
  //  map(g(y))(f) == map(y)((x)=>{f(g(x))})
  //  f(g(y)) == f(g(y))

  def main(args:Array[String]){
    val es=Executors.newFixedThreadPool(500)

    println(Par.run(es)(parZipWith(List(0,2,4,6,8),List(1,3,5,7,9))(_+_)))

    println(Par.run(es)(parOrElse(Some(24),Some(0))))
    println(Par.run(es)(parOrElse(None,None)))
    println(Par.run(es)(parOrElse(None,Some(14))))
    println(Par.run(es)(parOrElse(Some(67),None)))

    es.shutdown()
  }
}