package sangria.catseffect.schema

import cats.effect.Async
import sangria.schema.LeafAction

import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

case class AsyncValue[Ctx, Val, F[_]: Async](value: F[Val]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): AsyncValue[Ctx, NewVal, F] =
    new AsyncValue(Async[F].map(value)(fn))
}

object AsyncValue extends AsyncValueLowPriorityImplicits

trait AsyncValueLowPriorityImplicits {
  implicit def asyncAction[Ctx, Val, F[_]: Async](value: F[Val]): LeafAction[Ctx, Val] =
    AsyncValue(value)
}
