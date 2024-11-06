package effects

import zio.*

import java.io.IOException
import java.net.NoRouteToHostException
import scala.util.{Failure, Success, Try}

object ZIOEErrorHandling extends ZIOAppDefault {

  // ZIOs can fail
  val aFailedZIO: IO[String, Nothing] = ZIO.fail("Something went wrong")
  val failedWithThrowable: IO[RuntimeException, Nothing] = ZIO.fail(new RuntimeException("Boom!"))
  val failedWithDescription: ZIO[Any, String, Nothing] = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an effect that might throw n exception
  val badZIO = ZIO.succeed {
    println("Trying something")
    val string: String = null
    string.length
  } //this is bad

  // use attempt if you're ever unsure whether your code might throw an exception
  val anAttempt: Task[RuntimeFlags] = ZIO.attempt {
    println("Trying something")
    val string: String = null
    string.length
  }

  // effectfully catch errors
  val catchError = anAttempt.catchAll(e => ZIO.attempt(s"Returning a different value because $e"))
  val catchSelectiveErrors = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"ignoring runtime exception: $e")
    case _ => ZIO.succeed("Ignoring everything else")
  }

  // chain effects
  val aBetterAttempt = anAttempt.orElse(ZIO.succeed(56))
  // fold: handle both success and failure
  val handleBoth: ZIO[Any, Nothing, String] = anAttempt.fold(ex => s"Something bad happened: $ex", value => s"Length of the string was $value")
  // effectful fold: foldZIO
  val handleBoth_v2 = anAttempt.foldZIO(
    ex => ZIO.succeed(s"Something bad happened: $ex"),
    value => ZIO.succeed(s"Length of the string was $value")
  )

  /*
  Conversions between Option/Try/Either to ZIO
   */

  val aTrytoZIO = ZIO.fromTry(Try(42 / 0))

  // either -> ZIO
  val anEither = Right("Success!")
  val anEitherToZIO = ZIO.fromEither(anEither)
  //ZIO -> ZIO with Either as the value channel
  val eitherZIO = anAttempt.either
  // reverse
  val anAttempt_v2 = eitherZIO.absolve

  // option -> ZIO
  val anOption = ZIO.fromOption(Some(42))

  /**
   * Exercise: implement a version of fromTry, fromOption, fromEither, either, absolve
   * using fold and foldZIO
   */

  def tryToZIO[A](aTry: Try[A]): ZIO[Any, Throwable, A] =
    aTry match
      case Failure(exception) => ZIO.fail(exception)
      case Success(value) => ZIO.succeed(value)

  def eitherToZIO[A, B](anEither: Either[A, B]): ZIO[Any, A, B] =
    anEither match
      case Left(value) => ZIO.fail(value)
      case Right(value) => ZIO.succeed(value)

  def optionToZIO[A](anOption: Option[A]): ZIO[Any, Option[Nothing], A] =
    anOption match
      case Some(value) => ZIO.succeed(value)
      case None => ZIO.fail(None)

  def zioToZIoEither[R, A, B](zio: ZIO[R, A, B]): ZIO[R, Nothing, Either[A, B]] =
    zio.foldZIO(
      error => ZIO.succeed(Left(error)),
      value => ZIO.succeed(Right(value))
    )

  def absolveZIO[R, A, B](zio: ZIO[R, Nothing, Either[A, B]]): ZIO[R, A, B] =
    zio.flatMap {
      case Left(e) => ZIO.fail(e)
      case Right(value) => ZIO.succeed(value)
    }

  /*
  Errors = failures present in the ZIO type signature ("checked" exception in Java)
  Defects = failures that are unrecoverable, unforeseen, NOT present in the ZIO type signature

  ZIO[R, E, A] can finish with Exit[E, A]
    - Success[A] containing A
    - Cause[E]
      - Fail[E] containing the error
      - Die(t: Throwable) which was unforeseen
   */

  val divisionByZero: UIO[Int] = ZIO.succeed(1 / 0)

  val failedInt: ZIO[Any, String, Int] = ZIO.fail("I failed!")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int] = failureCauseExposed.unsandbox
  // fold with cause
  val foldedWithCause = failedInt.foldCause(
    cause => s"this failed with ${cause.defects}",
    value => s"this succeeded with $value"
  )

  val foldedWithCause_v2 = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed(s"this succeeded with $value")
  )

  /*
  Good Practice:
  - at a lower level, your "errors" should be treated
  - at a higher level, you should hide "errors" and assume they are unrecoverable
   */

  def callHttpEndpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("no internet, dummy!"))

  val endPointCalledWithDefects: ZIO[Any, Nothing, String] =
    callHttpEndpoint("rockthejvm.com").orDie // all errors are Defects

  // refining the error channel
  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new IOException("No internet!"))

  def callHTTPEndpoint_v2(url: String): ZIO[Any, IOException, String] =
    callHTTPEndpointWideError(url).refineOrDie[IOException] {
      case e: IOException => e
      case _: NoRouteToHostException => new IOException(s"No route to host $url, can't reach page.")
    }

  // reverse: turn defects into the error channel
  val endpointCallWithError = endPointCalledWithDefects.unrefine {
    case e => e.getMessage
  }

  /*
  Combine effects with different errors
   */

  case class IndexError(message: String)
  case class DbError(message: String)
  val callAPI: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  val queryDb: ZIO[Any, DbError, Int] = ZIO.succeed(1)
  val combined = for {
    page <- callAPI
    rowsAffected <- queryDb
  } yield (page, rowsAffected) // lost type safety

  /*
  Solutions
  - design an error model -> create AppError trait and make models extends trait
  - use Scala 3 union types make type combined: ZIO[Any, IndexError | DbError, (String, Int)]
  - .mapError to some common error type
   */

  /**
   * Exercises
   *
   */

  // 1. make this effect fail with a TYPED error
  val aBadfailure = ZIO.succeed[Int](throw new RuntimeException("this is bad!"))
  val aBetterfailure = aBadfailure.sandbox // exposes the defect in the Cause
  val abetterFailure_v2 = aBadfailure.unrefine { // surfaces out the exception in the error channel
    case e => e
  }

  // 2. transform a zio into another zio with a narrower exception type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie {
      case ioe: IOException => ioe
    }

  // 3.
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    zio.foldZIO(
      e => ZIO.fail(Left(e)),
      either => either match
        case Left(a) => ZIO.fail(Right(a))
        case Right(b) => ZIO.succeed(b)
    )

  // 4.
  val database = Map(
    "daniel" -> 123,
    "alice" -> 789
  )

  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if (userId != userId.toLowerCase) ZIO.fail(QueryError("user ID is invalid"))
    else ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))

  // surface out all the failed cases of this API
  def betterLookupProfile(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupProfile(userId).foldZIO(
      error => ZIO.fail(Some(error)),
      profileOption => profileOption match
        case Some(profile) => ZIO.succeed(profile)
        case None => ZIO.fail(None)
    )

  def betterLookupProfile_v2(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupProfile(userId).some

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
}
