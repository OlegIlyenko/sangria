package sangria.execution.deferred

import scala.collection.mutable.{Set ⇒ MutableSet, Map ⇒ MutableMap}
import scala.concurrent.Future

class Fetcher[Ctx, Res, RelRes, Id](
  val idFn: Res ⇒ Id,
  val fetch: (FetcherContext[Ctx], Seq[Id]) ⇒ Future[Seq[Res]],
  val fetchRel: (FetcherContext[Ctx], RelationIds[Res]) ⇒ Future[Seq[RelRes]],
  val config: FetcherConfig
) {
  def defer(id: Id) = FetcherDeferredOne(this, id)
  def deferOpt(id: Id) = FetcherDeferredOpt(this, id)
  def deferOpt(id: Option[Id]) = FetcherDeferredOptOpt(this, id)
  def deferSeq(ids: Seq[Id]) = FetcherDeferredSeq(this, ids)
  def deferSeqOpt(ids: Seq[Id]) = FetcherDeferredSeqOpt(this, ids)
  def deferSeqOptExplicit(ids: Seq[Id]) = FetcherDeferredSeqOptExplicit(this, ids)

  def deferRel[RelId](rel: Relation[Res, RelRes, RelId], relId: RelId) = FetcherDeferredRel(this, rel, relId)
  def deferRelOpt[RelId](rel: Relation[Res, RelRes, RelId], relId: RelId) = FetcherDeferredRelOpt(this, rel, relId)
  def deferRelSeq[RelId](rel: Relation[Res, RelRes, RelId], relId: RelId) = FetcherDeferredRelSeq(this, rel, relId)
  def deferRelSeqMany[RelId](rel: Relation[Res, RelRes, RelId], relIds: Seq[RelId]) = FetcherDeferredRelSeqMany(this, rel, relIds)

  def clearCache(deferredResolverState: Any) =
    findCache(deferredResolverState)(_.clear())

  def clearCachedId(deferredResolverState: Any, id: Id) =
    findCache(deferredResolverState)(_.clearId(id))

  def clearCachedRel(deferredResolverState: Any, rel: Relation[Res, _, _]) =
    findCache(deferredResolverState)(_.clearRel(rel))

  def clearCachedRelId[RelId](deferredResolverState: Any, rel: Relation[Res, _, RelId], relId: RelId) =
    findCache(deferredResolverState)(_.clearRelId(rel, relId))

  def updateCache(deferredResolverState: Any, id: Id, value: Res) =
    findCache(deferredResolverState)(_.update(id, value))

  def updateCacheRel[RelId](deferredResolverState: Any, rel: Relation[Res, _, RelId], relId: RelId, values: Seq[Res]) =
    findCache(deferredResolverState)(_.updateRel(rel, relId, idFn, values))

  private def findCache(deferredResolverState: Any)(op: FetcherCache ⇒ Unit): Unit =
    deferredResolverState match {
      case map: Map[AnyRef, FetcherCache] @unchecked ⇒ map.get(this) match {
        case Some(cache) ⇒ op(cache)
        case None ⇒ // just ignore
      }
      case _ ⇒ // just ignore
    }

  def ids(deferred: Vector[Deferred[Any]]): Vector[Id] = {
    val allIds =  MutableSet[Id]()

    deferred foreach {
      case FetcherDeferredOne(s, id) if s eq this ⇒ allIds += id.asInstanceOf[Id]
      case FetcherDeferredOpt(s, id) if s eq this ⇒ allIds += id.asInstanceOf[Id]
      case FetcherDeferredOptOpt(s, Some(id)) if s eq this ⇒ allIds += id.asInstanceOf[Id]
      case FetcherDeferredSeq(s, ids) if s eq this ⇒ allIds ++= ids.asInstanceOf[Seq[Id]]
      case FetcherDeferredSeqOpt(s, ids) if s eq this ⇒ allIds ++= ids.asInstanceOf[Seq[Id]]
      case FetcherDeferredSeqOptExplicit(s, ids) if s eq this ⇒ allIds ++= ids.asInstanceOf[Seq[Id]]
      case _ ⇒ // skip
    }

    allIds.toVector
  }

  def relIds(deferred: Vector[Deferred[Any]]): Map[Relation[Any, Any, Any], Vector[Any]] = {
    val allIds =  MutableMap[Relation[Any, Any, Any], MutableSet[Any]]()

    def addToSet(rel: Relation[Any, Any, Any], id: Any) =
      allIds.get(rel) match {
        case Some(set) ⇒ set += id
        case None ⇒
          val set = MutableSet[Any]()
          set += id
          allIds(rel) = set
      }

    deferred foreach {
      case FetcherDeferredRel(s, rel, relId) if s eq this ⇒ addToSet(rel, relId)
      case FetcherDeferredRelOpt(s, rel, relId) if s eq this ⇒ addToSet(rel, relId)
      case FetcherDeferredRelSeq(s, rel, relId) if s eq this ⇒ addToSet(rel, relId)
      case FetcherDeferredRelSeqMany(s, rel, relIds) if s eq this ⇒ relIds.foreach(addToSet(rel, _))
      case _ ⇒ // skip
    }

    allIds.map{case (k, v) ⇒ k → v.toVector}.toMap
  }

  def isRel(deferred: Deferred[Any]) = deferred match {
    case FetcherDeferredRel(_, _, _) |
         FetcherDeferredRelOpt(_, _, _) |
         FetcherDeferredRelSeq(_, _, _) |
         FetcherDeferredRelSeqMany(_, _, _) ⇒ true
    case _ ⇒ false
  }
}

object Fetcher {
  private def relationUnsupported[Ctx, Id, Res]: (FetcherContext[Ctx], RelationIds[Res]) ⇒ Future[Seq[Res]] =
    (_, _) ⇒ Future.failed(new RelationNotSupportedError)

  private def relationOnlySupported[Ctx, Id, Res]: (FetcherContext[Ctx], Seq[Id]) ⇒ Future[Seq[Res]] =
    (_, _) ⇒ Future.failed(new RelationOnlySupportedError)

  def apply[Ctx, Res, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Res, Id] =
    new Fetcher[Ctx, Res, Res, Id](i ⇒ id.id(i), (c, ids) ⇒ fetch(c.ctx, ids), relationUnsupported, config)

  def withContext[Ctx, Res, Id](fetch: (FetcherContext[Ctx], Seq[Id]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Res, Id] =
    new Fetcher[Ctx, Res, Res, Id](i ⇒ id.id(i), fetch, relationUnsupported, config)

  def rel[Ctx, Res, RelRes, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], fetchRel: (Ctx, RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), (c, ids) ⇒ fetch(c.ctx, ids), (c, ids) ⇒ fetchRel(c.ctx, ids), config)

  def relWithContext[Ctx, Res, RelRes, Id](fetch: (FetcherContext[Ctx], Seq[Id]) ⇒ Future[Seq[Res]], fetchRel: (FetcherContext[Ctx], RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), fetch, fetchRel, config)

  def relOnly[Ctx, Res, RelRes, Id](fetchRel: (Ctx, RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), relationOnlySupported, (c, ids) ⇒ fetchRel(c.ctx, ids), config)

  def relOnlyWithContext[Ctx, Res, RelRes, Id](fetchRel: (FetcherContext[Ctx], RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), relationOnlySupported, fetchRel, config)

  def caching[Ctx, Res, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Res, Id] =
    new Fetcher[Ctx, Res, Res, Id](i ⇒ id.id(i), (c, ids) ⇒ fetch(c.ctx, ids), relationUnsupported, config)

  def cachingWithContext[Ctx, Res, Id](fetch: (FetcherContext[Ctx], Seq[Id]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Res, Id] =
    new Fetcher[Ctx, Res, Res, Id](i ⇒ id.id(i), fetch, relationUnsupported, config)

  def relCaching[Ctx, Res, RelRes, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], fetchRel: (Ctx, RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), (c, ids) ⇒ fetch(c.ctx, ids), (c, ids) ⇒ fetchRel(c.ctx, ids), config)

  def relCachingWithContext[Ctx, Res, RelRes, Id](fetch: (FetcherContext[Ctx], Seq[Id]) ⇒ Future[Seq[Res]], fetchRel: (FetcherContext[Ctx], RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), fetch, fetchRel, config)

  def relOnlyCaching[Ctx, Res, RelRes, Id](fetchRel: (Ctx, RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), relationOnlySupported, (c, ids) ⇒ fetchRel(c.ctx, ids), config)

  def relOnlyCachingWithContext[Ctx, Res, RelRes, Id](fetchRel: (FetcherContext[Ctx], RelationIds[Res]) ⇒ Future[Seq[RelRes]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, RelRes, Id] =
    new Fetcher[Ctx, Res, RelRes, Id](i ⇒ id.id(i), relationOnlySupported, fetchRel, config)
}

case class FetcherConfig(cacheConfig: Option[() ⇒ FetcherCache] = None, maxBatchSizeConfig: Option[Int] = None) {
  def caching = copy(cacheConfig = Some(() ⇒ FetcherCache.simple))
  def caching(cache: FetcherCache) = copy(cacheConfig = Some(() ⇒ cache))

  def maxBatchSize(size: Int) = copy(maxBatchSizeConfig = Some(size))
}

object FetcherConfig {
  val empty = FetcherConfig()

  def caching = empty.caching
  def caching(cache: FetcherCache) = empty.caching(cache)

  def maxBatchSize(size: Int) = empty.maxBatchSize(size)
}

trait DeferredOne[+T, Id] extends Deferred[T] {
  def id: Id
}

trait DeferredOpt[+T, Id] extends Deferred[Option[T]] {
  def id: Id
}

trait DeferredOptOpt[+T, Id] extends Deferred[Option[T]] {
  def id: Option[Id]
}

trait DeferredSeq[+T, Id] extends Deferred[Seq[T]] {
  def ids: Seq[Id]
}

trait DeferredSeqOpt[+T, Id] extends Deferred[Seq[Option[T]]] {
  def ids: Seq[Id]
}

trait DeferredRel[T, RelId] extends Deferred[T] {
  def rel: Relation[T, _, RelId]
  def relId: RelId
}

trait DeferredRelOpt[T, RelId] extends Deferred[Option[T]] {
  def rel: Relation[T, _, RelId]
  def relId: RelId
}

trait DeferredRelSeq[T, RelId] extends Deferred[Seq[T]] {
  def rel: Relation[T, _, RelId]
  def relId: RelId
}

trait DeferredRelSeqMany[T, RelId] extends Deferred[Seq[T]] {
  def rel: Relation[T, _, RelId]
  def relIds: Seq[RelId]
}

case class FetcherDeferredOne[Ctx, T, RT, Id](source: Fetcher[Ctx, T, RT, Id], id: Id) extends DeferredOne[T, Id]
case class FetcherDeferredOpt[Ctx, T, RT, Id](source: Fetcher[Ctx, T, RT, Id], id: Id) extends DeferredOpt[T, Id]
case class FetcherDeferredOptOpt[Ctx, T, RT, Id](source: Fetcher[Ctx, T, RT, Id], id: Option[Id]) extends DeferredOptOpt[T, Id]
case class FetcherDeferredSeq[Ctx, T, RT, Id](source: Fetcher[Ctx, T, RT, Id], ids: Seq[Id]) extends DeferredSeq[T, Id]
case class FetcherDeferredSeqOpt[Ctx, T, RT, Id](source: Fetcher[Ctx, T, RT, Id], ids: Seq[Id]) extends DeferredSeq[T, Id]
case class FetcherDeferredSeqOptExplicit[Ctx, T, RT, Id](source: Fetcher[Ctx, T, RT, Id], ids: Seq[Id]) extends DeferredSeqOpt[T, Id]

case class FetcherDeferredRel[Ctx, RelId, T, Tmp, Id](source: Fetcher[Ctx, T, Tmp, Id], rel: Relation[T, Tmp, RelId], relId: RelId) extends DeferredRel[T, RelId]
case class FetcherDeferredRelOpt[Ctx, RelId, T, Tmp, Id](source: Fetcher[Ctx, T, Tmp, Id], rel: Relation[T, Tmp, RelId], relId: RelId) extends DeferredRelOpt[T, RelId]
case class FetcherDeferredRelSeq[Ctx, RelId, T, Tmp, Id](source: Fetcher[Ctx, T, Tmp, Id], rel: Relation[T, Tmp, RelId], relId: RelId) extends DeferredRelSeq[T, RelId]
case class FetcherDeferredRelSeqMany[Ctx, RelId, T, Tmp, Id](source: Fetcher[Ctx, T, Tmp, Id], rel: Relation[T, Tmp, RelId], relIds: Seq[RelId]) extends DeferredRelSeqMany[T, RelId]

trait Relation[T, Tmp, RelId] {
  def relIds(value: Tmp): Seq[RelId]
  def map(value: Tmp): T
}

object Relation {
  def apply[T, RelId](name: String, idFn: T ⇒ Seq[RelId]): Relation[T, T, RelId] =
    SimpleRelation[T, T, RelId](name)(idFn, identity)

  def apply[T, Tmp, RelId](name: String, idFn: Tmp ⇒ Seq[RelId], mapFn: Tmp ⇒ T): Relation[T, Tmp, RelId] =
    SimpleRelation[T, Tmp, RelId](name)(idFn, mapFn)
}

abstract class AbstractRelation[T, Tmp, RelId](idFn: Tmp ⇒ Seq[RelId], mapFn: Tmp ⇒ T) extends Relation[T, Tmp, RelId] {
  def relIds(value: Tmp) = idFn(value)
  def map(value: Tmp) = mapFn(value)
}

case class SimpleRelation[T, Tmp, RelId](name: String)(idFn: Tmp ⇒ Seq[RelId], mapFn: Tmp ⇒ T) extends AbstractRelation[T, Tmp, RelId](idFn, mapFn)

case class RelationIds[Res](rawIds: Map[Relation[Res, _, _], Seq[_]]) {
  def apply[RelId](relation: Relation[Res, _, RelId]): Seq[RelId] =
    get[RelId](relation).getOrElse(Vector.empty)

  def get[RelId](relation: Relation[Res, _, RelId]): Option[Seq[RelId]] =
    rawIds.get(relation).asInstanceOf[Option[Seq[RelId]]]
}

case class FetcherContext[Ctx](
  ctx: Ctx,
  fetcher: Fetcher[Ctx, _, _, _],
  cache: Option[FetcherCache],
  allFetcherCaches: Map[AnyRef, FetcherCache],
  allFetchers: Vector[Fetcher[Ctx, _, _, _]]
) {
  def cacheFor(fetcher: Fetcher[_, _, _, _]): Option[FetcherCache] =
    allFetcherCaches.get(fetcher)
}

class RelationNotSupportedError extends Exception(s"Relations are not supported by Fetcher.")

class RelationOnlySupportedError extends Exception(s"Only relations are supported by Fetcher.")
