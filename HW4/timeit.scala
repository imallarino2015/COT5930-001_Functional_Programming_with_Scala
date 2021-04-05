package ch7parallelism.timeit

object timeit {
  /**
    * Evaluates a non-strict expresion what iters times+1 times and reports the average execution time.
    * @param what the expression to time
    * @param iters the number of iterations to run for the average
    * @tparam A the expression result type
    * @return tuple (value, average_runtime), where value it the value returned from the function and
    *         average_runtime is the average runtime for one iteration, expressed in seconds.
    */
  def timeIt[A](what: => A, iters: Int): (A, Double) = {
    @annotation.tailrec
    def repeat(k: Int): A =
      if (k == 1) what      // return the last result
      else {
        val _ = what        // ignore the result for all iterations except the last one
        repeat(k - 1)
      }

    val _ = what    // run it once to 'warm up' the cache and to load any classes needed

    // run iters iterations and time these runs:
    val start: Long = System.nanoTime()
    val a = repeat(iters)                 // run computation iters time
    val stop : Long = System.nanoTime()
    val seconds = (stop - start) / 1E9 / iters   // convert to seconds double (1E9 is a Double)
    (a, seconds)
  }
}
