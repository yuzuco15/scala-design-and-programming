object MyModule {
  def main(args: Array[String]) {
    println(formatAbs(-41))
  }

  def formatAbs(n: Int): Unit = {
    val msg = "The absolute value of %d is %d"
    msg.format(n, abs(n))
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorialTailRec(n: Int): Int = { // tail recursion
    def go (m: Int, acc: Int): Int = {
      if (m == 1) acc
      else go(m - 1, m * acc)
    }
    go(n, 1)
  }

  def factorial(n: Int): Int = { // not tail recursion
    if (n == 1) 1
    else n * factorial(n - 1)
  }

  def fib(n: Int): Int = { // not tail recursion
    if (n < 2) 1
    else fib(n - 1) + fib(n - 2)
  }

  // 2.1

  def fibTailRec(n: Int): Int = { // tail recursion
    def loop(n: Int, a: Int, b: Int): Int = {
      if (n == 0) b
      else loop(n - 1, b, a + b)
    }
    loop(n, 0, 1)
  }

  // Fibonacci + (tail recursion) - (inner function)
  def fibTailRec2(n: Int, a: Int = 0, b: Int = 1): Int = {
    if (n == 0) b
    else fibTailRec2(n - 1, b, a + b)
  }

  def findFirstString(ss: Array[String], key: String): Int = {
    def loop(n: Int): Int = {
      if (n == ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A] (as: Array[A], f: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n == as.length) -1
      else if (f(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  // 2.2
  def isSorted[A] (as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop (n: Int): Boolean = {
      if (n == as.length - 1) ordered(as(n), as(n + 1))
      else if(ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }
    loop(0)
  }

  def formatResult(name: String, n: Int, f: Int => Int): Unit = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def partial1[A, B, C] (a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  // 2.3
  def curry[A, B, C] (f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C] (f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C] (f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}