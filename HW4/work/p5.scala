import java.util.concurrent.Executors

import fpinscala.parallelism.{Par}

object p5 {
  def partition[A](s:IndexedSeq[A], pivot:A)(less: (A, A) =>Boolean):
  (IndexedSeq[A], IndexedSeq[A], IndexedSeq[A]) = { //part a
    @annotation.tailrec
    def part[A](s:IndexedSeq[A], pivot: A)(less: (A, A) => Boolean)(l: IndexedSeq[A], e: IndexedSeq[A], g: IndexedSeq[A]): (IndexedSeq[A], IndexedSeq[A], IndexedSeq[A]) =
      s match {
        case h +: t => {
          if(less(h, pivot)) part(t, pivot)(less)(l :+ h, e, g)
          else if (h == pivot) part(t, pivot)(less)(l, e :+ h, g)
          else part (t, pivot)(less)(l, e, g :+ h)
        }
        case _ => (l, e, g)
      }

    part(s, pivot)(less)(Vector(), Vector(), Vector())
  }

  def parPartition[A](s: IndexedSeq[A], pivot: A, threshold: Int)(less: (A, A) => Boolean):
  Par.Par[(IndexedSeq[A], IndexedSeq[A], IndexedSeq[A])] = Par.fork { //part b
    if (s.isEmpty) Par.unit(Vector(), Vector(), Vector())
    else if (s.length < threshold) Par.unit(partition(s, pivot)(less))
    else {
      val (left, r) = s.splitAt(s.length /2)
      Par.map2(parPartition(s, pivot, threshold)(less), parPartition(r, pivot, threshold)(less))(((a,b) =>(a._1 ++ b._1,a._2++b._2,a._3++b._3)))
    }
  }

  def quicksort(s: IndexedSeq[Int]):
  IndexedSeq[Int]={ //part a
    val pivot:Int=s(s.length-1)
    val future=parPartition(s,pivot,1)(_<_)
    val es=Executors.newFixedThreadPool(500)
    val splitSeq=Par.run(es)(future).get()
    if(splitSeq._1.length>0)
      quicksort(splitSeq._1)
    if(splitSeq._3.length>0)
      quicksort(splitSeq._3)
    splitSeq._1++splitSeq._2++splitSeq._3
  }

  def main(args:Array[String]){

  }
}
