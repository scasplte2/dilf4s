package xyz.kd5ujc.level_db

import cats.data.{EitherT, OptionT}
import cats.effect.{Resource, Sync}
import cats.implicits._
import io.circe.{Decoder, Encoder}
import org.iq80.leveldb._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.storage.Store

import java.nio.file.{Files, Path}

/**
 * A `Store` implementation backed by LevelDB. This code is forked from topl/bifrost (also under MPL 2.0 as of 20240511)
 * https://github.com/Topl/Bifrost/blob/dev/level-db-store/src/main/scala/co/topl/db/leveldb/LevelDbStore.scala
 */
object LevelDbStore {

  implicit class localEncoderOps[F[_]: JsonSerializer, T: Encoder](t: T) {
    def toJsonBytes: F[Array[Byte]] = JsonSerializer[F].serialize(t)
  }

  implicit class localDecoderOps[F[_]: JsonSerializer](bytes: Array[Byte]) {
    def fromJsonBytes[T: Decoder]: F[Either[Throwable, T]] = JsonSerializer[F].deserialize[T](bytes)
  }

  private val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private val javaFactory = "org.iq80.leveldb.impl.Iq80DBFactory"

  def make[F[_]: Sync: JsonSerializer, Key: Encoder: Decoder, Value: Encoder: Decoder](
    path: Path
  ): Resource[F, Store[F, Key, Value]] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass(LevelDbStore.getClass)

    for {
      ldbFactory <- LevelDbStore.makeFactory[F](useJni = true)
      db         <- LevelDbStore.makeDb[F](path, ldbFactory)
      store      <- LevelDbStore.make[F, Key, Value](db)
    } yield store
  }

  def make[F[_]: Sync: JsonSerializer, Key: Encoder: Decoder, Value: Encoder: Decoder](
    db: DB
  ): Resource[F, Store[F, Key, Value]] =
    Resource.pure[F, Store[F, Key, Value]] {
      new Store[F, Key, Value] {

        def put(id: Key, t: Value): F[Unit] =
          (id.toJsonBytes, t.toJsonBytes).tupled.flatMap { case (idB, tB) => useDb(_.put(idB, tB)) }

        def remove(id: Key): F[Unit] =
          id.toJsonBytes.flatMap(idB => useDb(_.delete(idB)))

        def get(id: Key): F[Option[Value]] =
          id.toJsonBytes
            .flatMap(idB => useDb(db => Option(db.get(idB))))
            .flatMap {
              case Some(raw) => raw.fromJsonBytes[Value].map(_.toOption)
              case None      => Sync[F].pure(None)
            }

        def contains(id: Key): F[Boolean] =
          id.toJsonBytes
            .flatMap(idB => useDb(_.get(idB) != null))

        /**
         * Use the instance of the DB within a blocking F context
         */
        private def useDb[R](f: DB => R): F[R] =
          Sync[F].blocking(f(db))

        private def loopWithConditionAndLimit(
          cond:  (Key, Value) => Boolean,
          limit: Int = Int.MaxValue
        ): F[List[(Key, Value)]] = {
          def loop(
            iter:  DBIterator,
            bf:    List[(Key, Value)],
            count: Int = 0
          ): F[List[(Key, Value)]] =
            if (!iter.hasNext || count >= limit) Sync[F].pure(bf)
            else
              for {
                entry       <- Sync[F].delay(iter.next())
                key         <- EitherT(entry.getKey.fromJsonBytes[Key]).rethrowT
                value       <- EitherT(entry.getValue.fromJsonBytes[Value]).rethrowT
                newBuffer   <- if (cond(key, value)) Sync[F].delay(bf :+ (key, value)) else Sync[F].pure(bf)
                finalBuffer <- loop(iter, newBuffer, count + 1)
              } yield finalBuffer

          createReadResource.use {
            case (iter, _) =>
              loop(iter, List.empty[(Key, Value)])
          }
        }

        override def get(
          keys: List[Key]
        ): F[List[(Key, Option[Value])]] =
          keys.traverse { key =>
            get(key).map(value => key -> value)
          }

        override def getWithFilter(
          cond: (Key, Value) => Boolean
        ): F[List[(Key, Value)]] = loopWithConditionAndLimit(cond)

        override def putBatch(updates: List[(Key, Value)]): F[Unit] =
          createWriteResource.use {
            case (batch, wo) =>
              for {
                _ <- updates.traverse_ {
                  case (id, t) =>
                    (id.toJsonBytes, t.toJsonBytes).tupled.map { case (idB, tB) => batch.put(idB, tB) }
                }
                _ <- Sync[F].delay(db.write(batch, wo))
              } yield ()
          }

        override def removeBatch(deletions: List[Key]): F[Unit] =
          createWriteResource.use {
            case (batch, wo) =>
              for {
                _ <- deletions.traverse_ { id =>
                  id.toJsonBytes.map(idB => batch.delete(idB))
                }
                _ <- Sync[F].delay(db.write(batch, wo))
              } yield ()
          }

        private def createWriteResource: Resource[F, (WriteBatch, WriteOptions)] =
          Resource.make {
            Sync[F].delay {
              (db.createWriteBatch(), new WriteOptions())
            }
          } {
            case (batch, _) =>
              Sync[F].delay {
                batch.close()
              }
          }

        private def createReadResource: Resource[F, (DBIterator, ReadOptions)] =
          Resource.make[F, (DBIterator, ReadOptions)] {
            Sync[F].delay {
              val ro = new ReadOptions()
              ro.snapshot(db.getSnapshot)
              val iter = db.iterator(ro)
              (iter, ro)
            }
          } {
            case (iter, ro) =>
              Sync[F].delay {
                iter.close()
                ro.snapshot().close()
              }
          }
      }
    }

  /**
   * Creates an instance of a DB from the given path
   */
  def makeDb[F[_]: Sync](
    baseDirectory:   Path,
    factory:         DBFactory,
    createIfMissing: Boolean = true,
    paranoidChecks:  Option[Boolean] = None,
    blockSize:       Option[Int] = None,
    cacheSize:       Option[Long] = None,
    maxOpenFiles:    Option[Int] = None,
    compressionType: Option[CompressionType] = None
  ): Resource[F, DB] = {
    val options = new Options
    options.createIfMissing(createIfMissing)
    paranoidChecks.foreach(options.paranoidChecks)
    blockSize.foreach(options.blockSize)
    cacheSize.foreach(options.cacheSize)
    maxOpenFiles.foreach(options.maxOpenFiles)
    compressionType.foreach(options.compressionType)

    val dbF =
      Sync[F].whenA(createIfMissing)(Sync[F].blocking(Files.createDirectories(baseDirectory))) >>
      Sync[F].blocking {
        factory.open(
          baseDirectory.toFile,
          options
        )
      }

    Resource.fromAutoCloseable(dbF)
  }

  def makeFactory[F[_]: Sync: Logger](useJni: Boolean = true): Resource[F, DBFactory] =
    Resource.eval {
      Sync[F].delay {
        if (useJni) {
          // As LevelDB-JNI has problems on Mac (see https://github.com/ergoplatform/ergo/issues/1067),
          // we are using only pure-Java LevelDB on Mac
          val isMac = System.getProperty("os.name").toLowerCase().indexOf("mac") >= 0
          if (isMac) List(javaFactory) else List(nativeFactory, javaFactory)
        } else
          List(javaFactory)
      }
        .flatMap(factories =>
          List(this.getClass.getClassLoader, ClassLoader.getSystemClassLoader)
            .zip(factories)
            .collectFirstSomeM {
              case (loader, factoryName) =>
                OptionT(
                  Sync[F]
                    .fromTry(
                      scala.util.Try(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory])
                    )
                    .map(_.some)
                    .recoverWith {
                      case e =>
                        Logger[F].warn(e)(s"Failed to load database factory $factoryName").as(None)
                    }
                ).map(factoryName -> _).value
            }
            .map(_.toRight(new RuntimeException(s"Could not load any of the factory classes: $factories")))
        )
        .rethrow
        .flatTap {
          case (`javaFactory`, _) =>
            Logger[F].warn(
              "Using the pure java LevelDB implementation which is experimental and slower than the native implementation."
            )
          case _ => ().pure[F]
        }
        .flatTap {
          case (name, factory) =>
            Logger[F].info(s"Loaded $name with $factory")
        }
        .map(_._2)
    }
}
