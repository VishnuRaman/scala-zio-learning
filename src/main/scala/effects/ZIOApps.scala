package effects

import zio.*

object ZIOApps extends ZIOAppDefault {

  val meaningOfLife: UIO[Int] = ZIO.succeed(42)

  override def run = {
    meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
    meaningOfLife.debug
  }

}
