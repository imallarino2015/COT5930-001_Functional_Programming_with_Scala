import java.util.concurrent.Executors

import fpinscala.state.{State,RNG}
import fpinscala.parallelism.Par
import ch7parallelism.timeit.timeit

object p3 {
  def partition[A](s:IndexedSeq[A], pivot:A)(less: (A, A) =>Boolean):
  (IndexedSeq[A], IndexedSeq[A], IndexedSeq[A]) = { //part a
    @annotation.tailrec
    def part[A](s:IndexedSeq[A], pivot: A)(less: (A, A) => Boolean)
               (left: IndexedSeq[A], middle: IndexedSeq[A], right: IndexedSeq[A]):
    (IndexedSeq[A], IndexedSeq[A], IndexedSeq[A]) = {
      s match {
        case h +: t => {
          if(less(h, pivot)) part(t, pivot)(less)(left :+ h, middle, right)
          else if (h == pivot) part(t, pivot)(less)(left, middle :+ h, right)
          else part (t, pivot)(less)(left, middle, right :+ h)
        }
        case _ => (left, middle, right)
      }
    }

    part(s, pivot)(less)(Vector(), Vector(), Vector())
  }

  def parPartition[A](s: IndexedSeq[A], pivot: A, threshold: Int)(less: (A, A) =>
    Boolean): Par.Par[(IndexedSeq[A], IndexedSeq[A], IndexedSeq[A])] = Par.fork {
    if (s.isEmpty) Par.unit(Vector(), Vector(), Vector())
    else if (s.length < threshold) Par.unit(partition(s, pivot)(less))
    else {
      val (l, r) = s.splitAt(s.length /2)
      Par.map2(parPartition(l, pivot, threshold)(less), parPartition(r, pivot,
        threshold)(less))(((a,b) =>(a._1 ++ b._1,a._2++b._2,a._3++b._3)))
    }
  }

  def benchmark(threshold:Int,n:Int)={ //part c
    val vec:Vector[Int]=State.sequence(
      List.fill(n)(
        State(RNG.int)
      )
    ).run(RNG.Simple(0))._1.toVector
    val partTime=timeit.timeIt(partition(vec, 4)(_ < _),10)
    val parPartTime=timeit.timeIt(parPartition(vec, 4,threshold)(_ < _),10)
    partTime._2/parPartTime._2
  }

  def mkChartData()={

  }

  def main(args:Array[String]){
    val es=Executors.newFixedThreadPool(500)


    println(partition(Vector(5, 0, 3, 4, 1, 5, 1, 3, 9, 2, 7, 4, 3, 6), 4)(_ < _))
    println(Par.run(es)(parPartition(Vector(5, 0, 3, 4, 1, 5, 1, 3, 9, 2, 7, 4, 3, 6), 4,4)(_ < _)))

    println(benchmark(100,1000))

    es.shutdown()
  }
}
