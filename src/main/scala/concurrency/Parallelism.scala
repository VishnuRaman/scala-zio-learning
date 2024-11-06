package concurrency

import zio.*

object Parallelism extends ZIOAppDefault{

  val meaningofLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")
  val combined = meaningofLife.zip(favLang) // combines/zips in a sequential manner

  // combine parallel
  val combinedPar = meaningofLife.zipPar(favLang) // combination is parallel

  /*
   - start each on fibers
   - what if one fails? the other should be interrupted
   - what if one is interrupted? the entire thing should be interrupted
   - what if the whole thing is interrupted? need to interrupt both effects
   */

  // try a zipPar combinator
  // hint: fork/join/await, interrupt
//  def myZipPar[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO [R, E, (A, B)] = {
//    for  {
//      fiba <- zioa.fork
//      fibb <- ziob.fork
//      exita <- fiba.await
//      _ <- exita match {
//        case Exit.Success(value) => fibb.await
//        
//      }
//    } yield ()
//  }
//
//
//
  def run = ???
}
