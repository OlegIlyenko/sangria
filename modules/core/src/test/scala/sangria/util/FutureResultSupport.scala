package sangria.util

import sangria.execution.{ErrorWithResolver, QueryAnalysisError}
import sangria.marshalling.ResultMarshallerForType

import language.postfixOps
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait FutureResultSupport {
  implicit class FutureResult[T](f: Future[T]) {
    def await = Await.result(f, 10.seconds)
    def await(duration: Duration) = Await.result(f, duration)

    def awaitAndRecoverQueryAnalysis(implicit m: ResultMarshallerForType[T]): T =
      Await.result(recoverQueryAnalysis, 10.seconds)

    def recoverQueryAnalysis(implicit m: ResultMarshallerForType[T]): Future[T] = f.recover {
      case analysisError: QueryAnalysisError =>
        analysisError.resolveError(m.marshaller).asInstanceOf[T]
    }

    def awaitAndRecoverQueryAnalysisScala(implicit ev: T =:= Any) =
      Await.result(recoverQueryAnalysisScala, 10.seconds)

    def recoverQueryAnalysisScala(implicit ev: T =:= Any) = f.recover {
      case analysisError: ErrorWithResolver => analysisError.resolveError
    }
  }

  object sync {
    val executionContext = ExecutionContext.fromExecutor(new java.util.concurrent.Executor {
      def execute(command: Runnable) = command.run()
    })
  }
}
