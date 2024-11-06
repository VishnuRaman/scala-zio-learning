package effects

import zio.*

import scala.io.StdIn

object ZIOEffects {


  case class MyZIO[-R, +E, +A](unsafeRun: (R) => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      })

    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => f(v).unsafeRun(r)
      })
  }

  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))

  val smallProgram = for {
    _ <- ZIO.succeed(println("what's your name"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()


  // A lot of combinators
  // zip
  val anotherMOL = ZIO.succeed(100)
  val tupledZIO = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /**
   * Type aliases of ZIOs
   *
   */
  // UIO[Int] = ZIO[Any, Nothing, A] - no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)

  // URIO[R,A] = ZIO[R, Nothing, A] - cannot fail
  val anURIO: URIO[Int, Int] = ZIO.succeed(67)

  // RIO[R,A] = ZIO[R, Throwable, A] - can fail with a Throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))

  // Task[A] = ZIO[Any, Throwable, A] - no requirements, can fail with a Throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))

  // IO[E,A] = ZIO[Any,E,A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")

  /**
   * Exercises
   *
   */

  // 1. sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    zioa.flatMap(a => ziob.map(b => b))
  }

  def sequenceTakeLast_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    for {
      a <- zioa
      b <- ziob
    } yield b
  }

  def sequenceTakeLast_v3[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    zioa *> ziob
  }


  // 2. sequence two ZIOs and take the value of the first one
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = {
    zioa.flatMap(a => ziob.map(b => a))
  }

  def sequenceTakeFirst_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = {
    zioa <* ziob
  }

  // 3. run a ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    zio.flatMap(_ => runForever(zio))
  }

  def runForever_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    zio *> runForever_v2(zio)
  }

  val endlessLoop = runForever {
    ZIO.succeed {
      println("running...")
      Thread.sleep(1000)
    }
  }

  // 4. convert the value of a ZIO to something else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = {
    zio.map(_ => value)
  }

  def convert_v2[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = {
    zio.as(value)
  }

  // 5. discard the value of a ZIO to unit
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = {
    convert(zio, ())
  }

  def asUnit_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = {
    zio.unit
  }

  // 6 - recursion
  def sum(n: Int): Int = {
    if (n == 0) 0
    else n + sum(n - 1)
  }

  def sumZIO(n: Long): UIO[Long] =
    if (n == 0) ZIO.succeed(0)
    else for {
      current <- ZIO.succeed(n)
      prevSum <- sumZIO(n - 1)
    } yield current + prevSum

  // 7 - fibonacci
  def fibo(n: Int): BigInt =
    if (n <= 2) 1
    else fibo(n - 1) + fibo(n - 2)

  def fiboZIO(n: Int): UIO[BigInt] =
    if (n <= 2) ZIO.succeed(1)
    else for {
      last <- ZIO.suspendSucceed(fiboZIO(n-1))
      prev <- fiboZIO(n-2)
    } yield (last + prev)

  /*
  fiboZIO(n-1).flatMap(last => fiboZIO(n-2).map(prev => last + prev)
   */

  def main(args: Array[String]) = {
    val runtime = Runtime.default

    given trace: Trace = Trace.empty

    Unsafe.unsafely {
      val mol = runtime.unsafe.run(meaningOfLife)
      println(mol)
    }

    Unsafe.unsafely {
      val firstEffect = ZIO.succeed {
        println("computing first effect ...")
        Thread.sleep(1000)
        1
      }

      val secondEffect = ZIO.succeed {
        println("computing second effect ...")
        Thread.sleep(1000)
        2
      }

      //      println(runtime.unsafe.run(sequenceTakeLast_v3(firstEffect, secondEffect)))
      //
      //      println(runtime.unsafe.run(sequenceTakeFirst_v2(firstEffect, secondEffect)))

      //   println(runtime.unsafe.run(endlessLoop))

      println(runtime.unsafe.run(fiboZIO(2000000)))
    }

  }

}
