package com.evo_final.blackjack.server

import org.http4s.websocket.WebSocketFrame.Text
import io.circe
import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.generic.JsonCodec
import io.circe.generic.extras.auto
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import com.evo_final.blackjack.{Amount, PlayerId}
import com.evo_final.blackjack.game_logic.{Game, PlayerDecision}
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import fs2._
import fs2.concurrent.Queue
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import cats.implicits._

import scala.concurrent.ExecutionContext

object Server extends IOApp {

  case class Bet(amount: Amount) {}

  case class ServerState(connectedClients: Map[PlayerId, Queue[IO, WebSocketFrame]], game: Game)
  // Let's build a WebSocket server using Http4s.

  private def webSocketRoute(stateRef: Ref[IO, ServerState]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      // websocat "ws://localhost:9002/echo"
      case GET -> Root / "blackjack" =>
//        def pipe: Pipe[IO, WebSocketFrame, WebSocketFrame] = { inputStream =>
//          inputStream
//            .collect {
//              case Text(message, _) => decode[ServerState](message)
//            }
//            .map(out => Text(out.asJson.noSpaces))
//        }
        // Pipe is a stream transformation function of type `Stream[F, I] => Stream[F, O]`. In this case
        // `I == O == WebSocketFrame`. So the pipe transforms incoming WebSocket messages from the client to
        // outgoing WebSocket messages to send to the client.
        def pipe: Pipe[IO, WebSocketFrame, WebSocketFrame] = ???
//          _.collect {
//            case WebSocketFrame.Text(message, _) =>
//              val request = List(decode[PlayerDecision](message).toOption, decode[Bet](message).toOption)
//                .collectFirstSome(identity)
//              val test = for {
//                serverState <- stateRef.get
////                currentState <- serverState.get
//                game = request match {
//                  case Some(PlayerDecision) => serverState
//                }
//              } yield game
//          }
        for {
          // Unbounded queue to store WebSocket messages from the client, which are pending to be processed.
          // For production use bounded queue seems a better choice. Unbounded queue may result in out of
          // memory error, if the client is sending messages quicker than the server can process them.
          queue <- Queue.unbounded[IO, WebSocketFrame]
          response <- WebSocketBuilder[IO].build(
            // Sink, where the incoming WebSocket messages from the client are pushed to.
            receive = queue.enqueue,
            // Outgoing stream of WebSocket messages to send to the client.
            send = queue.dequeue.through(pipe),
          )
        } yield response

//      case GET -> Root / "blackjack" / "bet"      => Ok("test")
//      case GET -> Root / "blackjack" / "decision" => Ok("test")
    }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      serverStateRef <- Ref.of[IO, ServerState](ServerState(Map(), Game.start))
      _ <-
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9002, host = "localhost")
          .withHttpApp(webSocketRoute(serverStateRef).orNotFound)
          .serve
          .compile
          .drain
    } yield ExitCode.Success
}
