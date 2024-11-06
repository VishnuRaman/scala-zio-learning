package effects

import zio.*

import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault {

  // app to subscribe users to a Newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase): UserSubscription = {
      new UserSubscription(emailService, userDatabase)
    }

    val live: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] = ZLayer.fromFunction(create)
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}!").unit
  }

  object EmailService {
    def create() = new EmailService

    val live = ZLayer.succeed(create())
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] =
      for {
        conn <- connectionPool.get
        _ <- conn.runQuery(s"insert into the subscribers(name, email) values (${user.name}, ${user.email})")
      } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool): UserDatabase =
      new UserDatabase(connectionPool)

    val live = ZLayer.fromFunction(create)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int): ConnectionPool =
      new ConnectionPool(nConnections)

    def live(nConnections: Int) = ZLayer.succeed(create(nConnections))
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))
  }

  object Connection {
    def create(): Connection = new Connection

    val live = ZLayer.succeed(create())
  }

  // dependency injection
  val subscriptionService = ZIO.succeed(
    UserSubscription.create(EmailService.create(), UserDatabase.create(ConnectionPool.create(10)))
  )

  /*
  "clean DI" has drawbacks
  - does not scale for many services
  - DI can be 100x worse
    - pass dependencies partially
    - not having all the dependencies all in the same place
    - passing dependencies multiple times
   */

  def subscribe(user: User): ZIO[Any, Throwable, Unit] =
    for {
      sub <- subscriptionService // service is instantiated at the point of the call
      _ <- sub.subscribeUser(user)
    } yield ()

  // risk leaking resources if you subscribe multiple users in the same program

  val program = for {
    _ <- subscribe(User("vishnu", "vr@gmail.com"))
    - <- subscribe(User("raman", "raman@gmail.com"))
  } yield ()

  // alternative
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // requires ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("vishnu", "vr@gmail.com"))
    - <- subscribe_v2(User("raman", "raman@gmail.com"))
  } yield ()

  /*
  - we don't need to care about dependencies until the end of the world
  - all ZIOs requiring this dependency will use the same instance
  - can user different instances of the same type for different needs (e.g testing)
  - layers can be created and composed much like regular ZIOs + rich API
   */

  /**
   * ZLayers
   *
   */

  val connectionPoolLayer: ULayer[ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))

  // a layer that requires a dependency (higher layer) can be built with ZLayer.fromFunction
  // (and automatically fetch the function arguments and place them into ZLayer's dependency/environment type argument)
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] =
    ZLayer.fromFunction(UserDatabase.create)

  val emailServiceLayer = ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create)

  // composing layers
  // vertical composition
  val databaseLayerFull = connectionPoolLayer >>> databaseLayer

  // horizontal composition
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase with EmailService] =
    databaseLayerFull ++ emailServiceLayer

  val userSubscriptionLayer = subscriptionRequirementsLayer >>> userSubscriptionServiceLayer

  // best practice: write "factory" methods exposing layers in the companion objects of the service
  val runnableProgram = program_v2.provide(userSubscriptionLayer)

  // magic
  val runnableProgram_v2 = program_v2.provide(
    UserSubscription.live,
    EmailService.live,
    UserDatabase.live,
    ConnectionPool.live(10)
  ) // ZIO will tell you if you are missing a layer

  // magic v2
  val userSubscriptionLayer_v2: ZLayer[Any, Nothing, UserSubscription] =
    ZLayer.make[UserSubscription](
      UserSubscription.live,
      EmailService.live,
      UserDatabase.live,
      ConnectionPool.live(10)
    )

  // passththrough
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool with UserDatabase] =
    UserDatabase.live.passthrough

  // service = take a dep and expose it as a value to further layers
  val dbService = ZLayer.service[UserDatabase]

  // launch = creates a ZIO that uses the services and never finishes
  val subscriptionLaunch: ZIO[EmailService with UserDatabase, Nothing, Nothing] =
    UserSubscription.live.launch

  //memoization

  /*
  ALready provided services: Clock, Random, System, Console
   */

  val getTime = Clock.currentTime(TimeUnit.MICROSECONDS)
  val randomValue = Random.nextInt
  val sysVariable = System.env("HADOOP_HOME")
  val printlnEffect = Console.printLine("This is ZIO")

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    runnableProgram_v2

}
