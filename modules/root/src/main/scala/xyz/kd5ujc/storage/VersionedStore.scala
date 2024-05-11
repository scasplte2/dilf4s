package xyz.kd5ujc.storage

import java.util.UUID

trait VersionedStore[F[_], Key, Value] {

  // retrieves the current version of a key
  def get(key: Key): F[Option[Value]]

  // retrieves current version of key-values satisfying condition
  def getWithFilter(cond: (Key, Value) => Boolean): F[List[(Key, Value)]]

  // quickly checks if the current version has a value for a key
  def contains(key: Key): F[Boolean]

  // retrieves the latest version ID
  def latest: F[UUID]

  // retrieves all version IDs
  def listVersions: F[List[UUID]]

  // retrieves all diffs of a specific version
  def dumpVersionDiff(ver: UUID): F[List[(Key, Option[Value])]]

  // retrieves the diff of a key updated at a specified version
  def getFromVersionDiff(key: Key, ver: UUID): F[Option[Value]]

  // Update the database, return true if successful
  def update(version: UUID, toRemove: List[Key], toUpdate: List[(Key, Value)]): F[Boolean]

  def insert(id: UUID, toInsert: List[(Key, Value)]): F[Boolean] = update(id, List.empty, toInsert)

  def remove(id: UUID, toRemove: List[Key]): F[Boolean] = update(id, toRemove, List.empty)

  // Rollback the database to a previous version, return true if successful
  def rollback(version: UUID): F[Boolean]
}
