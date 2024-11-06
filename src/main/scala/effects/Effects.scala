package effects

import scala.concurrent.Future
import scala.io.StdIn

object Effects {
  // local reasoning = type signature describes the kind of computation that it will be performed
  // referential transparency = ability to replace an expression with the value it evaluates to

  /*
  Effect Desires/Properties -> bridge the  concept of a side effect with our desire to write functional programs
  - a data structure to bridge gap
  - the type signature describes what KIND of computation it will perform
  - the type signature describes the type of VALUE that it will produce
  - if side effects are required, construction must be separate from the execution
   */

  /*
  Example => Options = possibly absent values
  - type signature describes the kind of computation = a possibly absent value
  - type signature says that the computation returns an A, if the computation does produce something
  - no side effects are needed
   */
  val anOption: Option[Int] = Option(42)

  val anEffect = println("effect")

  /*
  Example 2 => Future
  - describes an asynchronous computation
  - produces a value of type A, if it finishes and it is successful
  - side effects are required, construction is not SEPARATE from execution
  => Future is NOT an effect
   */

  import scala.concurrent.ExecutionContext.Implicits.global

  val aFuture: Future[Int] = Future(42)

  /*
  Example 3: MyIO
  - describes a computation which might perform side effects
  - produces a value of type A if the computation is successful
  - side effects are required, construction IS SEPARATE from execution
  MY IO IS AN EFFECT
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIOWithSideEffects: MyIO[Int] = MyIO(() => {
    println("producing effect")
    42
  })

  /**
   * Exercises - create some IO which
   * 1. measure the current time of the system
   * 2. measure the duration fo a computation
   *  - use exercise 1
   *  - use map/flatMap combinations of MyIO
   *    3. read something from the console
   *    4. print something to the console and then read , then print a welcome message
   */

  // 1
  val currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = for {
    startTime <- currentTime
    result <- computation
    endTime <- currentTime
  } yield (endTime - startTime, result)

  def measure_v2[A](computation: MyIO[A]): MyIO[(Long, A)] = {
    currentTime.flatMap { startTime =>
      computation.flatMap { result =>
        currentTime.map { endTime =>
          (endTime - startTime, result)
        }
      }
    }
  }

  def measure_v3[A](computation: MyIO[A]): MyIO[(Long, A)] = {
    MyIO { () =>
      val startTime = System.currentTimeMillis()
      val result = computation.unsafeRun()
      val endTime = System.currentTimeMillis()
      (endTime - startTime, result)
    }
  }

  def demoMeasurement(): Unit = {
    val computation = MyIO(() => {
      println("Crunching numbers...")
      Thread.sleep(1000)
      println("Done!")
      42
    })

    println(measure(computation).unsafeRun())
    println(measure_v3(computation).unsafeRun())
  }

  // 3
  val readLine: MyIO[String] = MyIO(() => StdIn.readLine())
  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  // 4
  val program: MyIO[Unit] = for {
    _ <- putStrLn("What's your name?")
    name <- readLine
    _ <- putStrLn(s"Welcome to Rock the JVM, $name")
  } yield ()

  def main(args: Array[String]): Unit = {
    demoMeasurement()
    program.unsafeRun()
  }
}
