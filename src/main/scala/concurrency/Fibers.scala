package concurrency

import zio.*
import utils.*

import java.io.{File, FileWriter}

object Fibers extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")

  // Fiber  = lightweight thread
  def createFiber: Fiber[Throwable, String] = ??? // impossible to create manually

  val sameThreadIO = for {
    mol <- meaningOfLife.debugThread
    lang <- favLang.debugThread
  } yield (mol, lang)


  val differentThreadIO = for {
    _ <- meaningOfLife.debugThread.fork
    _ <- favLang.debugThread.fork
  } yield ()

  val meaningOfLifeFiber = meaningOfLife.fork

  // join a fiber
  def runOnAnotherThread[R, E, A](zio: ZIO[R, E, A]) = for {
    fib <- zio.fork
    result <- fib.join
  } yield result

  // awaiting a fiber
  def runOnAnotherThread_v2[R, E, A](zio: ZIO[R, E, A]) = for {
    fib <- zio.fork
    result <- fib.await
  } yield result match
    case Exit.Success(value) => s"succeeded with $value"
    case Exit.Failure(cause) => s"failed with $cause"

  // poll - peek at the result of the fiber RIGHT NOW, without blocking
  val peekFiber = for {
    fib <- ZIO.attempt {
      Thread.sleep(1000)
      42
    }.fork
    result <- fib.poll
  } yield result

  // compose fibers
  // zip
  val zippedFibers = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber = fib1.zip(fib2)
    tuple <- fiber.join
  } yield tuple

  // or else
  val chainedFibers = for {
    fib1 <- ZIO.fail("not good!").debugThread.fork
    fib2 <- ZIO.succeed("Rock the JVM!").debugThread.fork
    fibers = fib1.orElse(fib2)
    msg <- fibers.join
  } yield msg

  /**
   * Exercises
   */
  // 1. zip two fibers without using the zip combinator
  // hint: create a fiber that waits for both
  def zipFibers[E, A, B](fiber1: Fiber[E, A], fiber2: Fiber[E, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] =
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork

  val zippedFibers_2 = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber <- zipFibers(fib1, fib2)
    tuple <- fiber.join
  } yield tuple

  def zipFibersGeneral[E, A, B](fiber1: Fiber[E, A], fiber2: Fiber[E, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] =
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork

  // 2. same thing with orElse
  def chainFibers[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] = {
    fiber1.join.orElse(fiber2.join).fork
  }

  // 3. distributing a task in between many fibers
  // spawn n fibers, count the n of words in each file,
  // then aggregate all the results together in one big number
  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000 random words
    val content = (1 to nWords)
      .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString)
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  // part 1 - an effect which reads one file and count the words there
  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }
    
  // part 2 - spin up fibers for all the files
  def wordCountParallel(n: Int): UIO[Int] = {
    val effects = (1 to n)
      .map(i => s"src/main/resources/file_$i.txt") // path
      .map(countWords) // list of effects
      .map(_.fork) // list of effects returning fibers
      .map(fiberEff => fiberEff.flatMap(_.join)) // list of effects returning values (count of words)
    
    effects.reduce { (zioa, ziob) =>
      for {
        a <- zioa
        b <- ziob
      } yield (a + b)
      
    }
  }

  override def run = wordCountParallel(10).debugThread
}
