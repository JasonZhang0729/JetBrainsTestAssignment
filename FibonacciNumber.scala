class FibonacciNumber {
  def fib(i: Int): Int = {
    if (i <= 1) {
      return 1;
    }

    var table = new Array[Int](i)

    table(0) = 1
    table(1) = 1

    for (j <- 2 to (table.length - 1)) {
      table(j) = table(j-1) + table(j-2)
    }

    return table(i-1)
  }
}

/**
  * The runtime of fib() is O(n).
  *
  * The runtime cannot be improved because the number of subproblems is O(n) and in order to get the th
  * fibonacci number you need to solve the 1st, 2nd, ...(n-1)th fibonacci number first, which at least
  * takes O(n) time.
  *
  * The space complexity can be improved by using an array of length 2 instead of length n because when
  * you want to calculate the kth fibonacci number, all you need are the (k-2)th and (k-1)th terms and all
  * previous terms are not necessary, so you don't have to preserve all of them.
  */
