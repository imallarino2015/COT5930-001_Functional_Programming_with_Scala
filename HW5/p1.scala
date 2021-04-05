import fpinscala.testing.{Gen, Prop}
import fpinscala.state.{RNG}

object p1 {
  def partition[A](s:IndexedSeq[A], pivot:A)(less: (A, A) =>Boolean):
  (IndexedSeq[A], IndexedSeq[A], IndexedSeq[A]) = { //part a
    if(s.isEmpty) {
      (IndexedSeq[A](), IndexedSeq[A](), IndexedSeq[A]())
    }else{
      val h=s.head
      val t=s.tail
      val x=partition(t,pivot)(less)
      if(less(pivot,h)){
        (x._1:+h,x._2,x._3)
      }else if(pivot==h){
        (x._1,x._2:+h,x._3)
      }else{
        (x._1,x._2,x._3:+h)
      }
    }
  }

  def quicksort(s: IndexedSeq[Int]):
  IndexedSeq[Int]={ //part a
    if(s.length<2)
      s
    else{
      val pivot:Int=s(s.length-1)
      val splitSeq=partition(s,pivot)(_<_)
      val right=if(splitSeq._1.isEmpty) splitSeq._1 else quicksort(splitSeq._1)
      val left=if(splitSeq._3.isEmpty) splitSeq._3 else quicksort(splitSeq._3)
      left++splitSeq._2++right
    }
  }

  @annotation.tailrec
  def isSorted[A](s:IndexedSeq[A])(less: (A,A) => Boolean): Boolean={
    if(s.length<=1) {
      true
    } else {
      val h=s.head
      val t=s.tail
      if(less(h,t.head)){
        isSorted(t)(less)
      }else{
        false
      }
    }
  }

  def main (args:Array[String]): Unit={
    val r = RNG.Simple(0)
    val l = Gen.listOf(Gen.choose(0,50))

    val isSortedProperty  = Prop
      .forAll(l)((x)=>{quicksort(x.toVector) == x.toVector.sorted})
      .run(1000,1000,r)
      .isFalsified
    println(isSortedProperty)

    val isAscendingProperty = Prop
      .forAll(l)((x)=>{isSorted(quicksort(x.toVector))(_<=_)})
      .run(1000,1000,r)
      .isFalsified
    println(isAscendingProperty)
  }
}