package xyz.kd5ujc.storage.store

import java.nio.file.{Files, Path}

import cats.data.{EitherT, OptionT}
import cats.effect.{Resource, Sync}
import cats.implicits._

import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.storage.Store

import io.circe.{Decoder, Encoder}
import org.iq80.leveldb._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

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
  ): Resource[F, Store[F, Key, Value]] = for {
    implicit0(logger: SelfAwareStructuredLogger[F]) <- Resource.eval(Slf4jLogger.fromClass[F](LevelDbStore.getClass))
    ldbFactory                                      <- LevelDbStore.makeFactory[F](useJni = true)
    db                                              <- LevelDbStore.makeDb[F](path, ldbFactory)
    store                                           <- LevelDbStore.make[F, Key, Value](db)
  } yield store

  def make[F[_]: Sync: JsonSerializer, Key: Encoder: Decoder, Value: Encoder: Decoder](
    db: DB
  ): Resource[F, Store[F, Key, Value]] =
    Resource.pure[F, Store[F, Key, Value]] {
      new Store[F, Key, Value] {

        def put(id: Key, t: Value): F[Unit] = for {
          idB <- id.toJsonBytes
          tB  <- t.toJsonBytes
          _   <- Sync[F].blocking(db.put(idB, tB))
        } yield ()

        def remove(id: Key): F[Unit] =
          id.toJsonBytes.flatMap { idB =>
            Sync[F].blocking(db.delete(idB))
          }

        def get(id: Key): F[Option[Value]] =
          id.toJsonBytes.flatMap { idB =>
            Sync[F]
              .blocking(Option(db.get(idB)))
              .flatMap(_.flatTraverse(_.fromJsonBytes[Value].map(_.toOption)))
          }

        def contains(id: Key): F[Boolean] =
          id.toJsonBytes.flatMap { idB =>
            Sync[F].blocking(db.get(idB)).map(_ != null)
          }

        def putBatch(updates: List[(Key, Value)]): F[Unit] =
          createWriteResource.use {
            case (batch, wo) =>
              for {
                _ <- updates.traverse_ {
                  case (id, t) =>
                    (id.toJsonBytes, t.toJsonBytes).tupled.map { case (idB, tB) => batch.put(idB, tB) }
                }
                _ <- Sync[F].blocking(db.write(batch, wo.sync(true)))
              } yield ()
          }

        def removeBatch(deletions: List[Key]): F[Unit] =
          createWriteResource.use {
            case (batch, wo) =>
              for {
                _ <- deletions.traverse_ { id =>
                  id.toJsonBytes.map(idB => batch.delete(idB))
                }
                _ <- Sync[F].blocking(db.write(batch, wo.sync(true)))
              } yield ()
          }

        def getBatch(keys: List[Key]): F[List[(Key, Option[Value])]] =
          createReadResource.use { readOptions =>
            keys.traverse { id =>
              id.toJsonBytes.flatMap { idB =>
                Sync[F]
                  .blocking(Option(db.get(idB, readOptions)))
                  .flatMap(_.flatTraverse(_.fromJsonBytes[Value].map(_.toOption)))
                  .map((id, _))
              }
            }
          }

        def getWithFilter(
          cond: (Key, Value) => Boolean
        ): F[List[(Key, Value)]] = loopWithConditionAndLimit(cond)

        private def loopWithConditionAndLimit(
          cond:  (Key, Value) => Boolean,
          limit: Int = Int.MaxValue
        ): F[List[(Key, Value)]] = {
          def loop(
            iter:  DBIterator,
            buf:   List[(Key, Value)],
            count: Int = 0
          ): F[List[(Key, Value)]] =
            Sync[F].tailRecM((iter, buf, count)) {
              case (_iter, _buf, _count) =>
                if (!_iter.hasNext || _count >= limit) Sync[F].pure(Right(_buf))
                else {
                  (for {
                    entry <- EitherT(Sync[F].delay(_iter.next()).attempt)
                    key   <- EitherT(entry.getKey.fromJsonBytes[Key])
                    value <- EitherT(entry.getValue.fromJsonBytes[Value])
                  } yield (key, value)).value.flatMap {
                    case Left(_) => Sync[F].pure(Right(_buf))
                    case Right((key, value)) =>
                      val newBuffer = if (cond(key, value)) _buf :+ (key, value) else _buf
                      Sync[F].pure(Left((_iter, newBuffer, _count + 1)))
                  }
                }
            }

          createIterResource.use {
            case (iter, _) =>
              loop(iter, List.empty[(Key, Value)])
          }
        }

        private def createReadResource: Resource[F, ReadOptions] =
          Resource.make {
            for {
              snapshot <- Sync[F].delay(db.getSnapshot)
              ro = new ReadOptions().snapshot(snapshot)
            } yield ro
          } { ro =>
            Sync[F].delay(ro.snapshot().close())
          }

        private def createWriteResource: Resource[F, (WriteBatch, WriteOptions)] =
          Resource.make {
            Sync[F].delay((db.createWriteBatch(), new WriteOptions()))
          } {
            case (batch, _) => Sync[F].delay(batch.close())
          }

        private def createIterResource: Resource[F, (DBIterator, ReadOptions)] =
          Resource.make {
            for {
              snapshot <- Sync[F].delay(db.getSnapshot)
              ro = new ReadOptions().snapshot(snapshot)
              iter <- Sync[F].delay(db.iterator(ro))
              _    <- Sync[F].delay(iter.seekToFirst())
            } yield (iter, ro)
          } {
            case (iter, ro) =>
              for {
                _ <- Sync[F].delay(iter.close())
                _ <- Sync[F].delay(ro.snapshot().close())
              } yield ()
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
