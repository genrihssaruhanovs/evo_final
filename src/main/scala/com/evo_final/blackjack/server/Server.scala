package com.evo_final.blackjack.server

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import fs2._
import fs2.concurrent.Queue
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.ExecutionContext

object Test extends App {
  def getAge(name: String): IO[Int] = IO(name.length)
  val testStream = Stream("test1", "test2", "test3")
  val ioStream = testStream.evalMap(getAge)
  val ioStream2 = testStream.evalFilter(name => IO(name.contains("2")))

}

object Server extends IOApp {

  // Let's build a WebSocket server using Http4s.

  private val webSocketRoute = HttpRoutes.of[IO] {

    // websocat "ws://localhost:9002/echo"
    case GET -> Root / "blackjack" =>
      // Pipe is a stream transformation function of type `Stream[F, I] => Stream[F, O]`. In this case
      // `I == O == WebSocketFrame`. So the pipe transforms incoming WebSocket messages from the client to
      // outgoing WebSocket messages to send to the client.
      val echoPipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
        _.collect {
          case WebSocketFrame.Text(message, _) => WebSocketFrame.Text(message)
        }

      for {
        // Unbounded queue to store WebSocket messages from the client, which are pending to be processed.
        // For production use bounded queue seems a better choice. Unbounded queue may result in out of
        // memory error, if the client is sending messages quicker than the server can process them.
        queue <- Queue.unbounded[IO, WebSocketFrame]
        response <- WebSocketBuilder[IO].build(
          // Sink, where the incoming WebSocket messages from the client are pushed to.
          receive = queue.enqueue,
          // Outgoing stream of WebSocket messages to send to the client.
          send = queue.dequeue.through(echoPipe),
        )
      } yield response

    case GET -> Root / "blackjack" / "bet"      => Ok("test")
    case GET -> Root / "blackjack" / "decision" => Ok("test")
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
//    gameRef <- Ref.of[IO, Game]
      _ <-
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9002, host = "localhost")
          .withHttpApp(webSocketRoute.orNotFound)
          .serve
          .compile
          .drain
    } yield ExitCode.Success
}
