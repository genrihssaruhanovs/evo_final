package com.evo_final.blackjack.server
//websocat "ws://127.0.0.1:9002/blackjack"
import org.http4s.websocket.WebSocketFrame.Text
import io.circe.parser._

import io.circe.syntax._
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import com.evo_final.blackjack.PlayerId
import com.evo_final.blackjack.game_logic.Game
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import io.circe.Error
import fs2._
import fs2.concurrent.Queue
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import com.evo_final.blackjack.game_logic.PossibleActions.CanPlaceBet
import com.evo_final.blackjack.server.output.ToClient
import com.evo_final.blackjack.server.output.ToClient.{Communication, Message}
import scala.concurrent.ExecutionContext
import com.evo_final.blackjack.server.JsonCodecs._

object Server extends IOApp {
  case class Session(id: PlayerId, outQueue: Queue[IO, ToClient], stateRef: Ref[IO, ServerState]) {
    def process(input: Either[Error, FromClient]): IO[Option[ToClient]] = {
      input match {
        case Right(request) =>
          for {
            state <- stateRef.get
            (responseIo, newState) = state.process(id, request)
            response <- responseIo
            _        <- stateRef.update(_ => newState)
          } yield response
        case Left(error) => IO(Some(Message(error.getMessage)))
      }

    }
  }
  object Session {
    def connect(id: PlayerId, outQueue: Queue[IO, ToClient], stateRef: Ref[IO, ServerState]): IO[Session] = {
      for {
        state <- stateRef.get
        outMessage = state.game.roundOpt match {
          case Some(_) => Message("Round in progress, please wait")
          case None    => Communication(List(CanPlaceBet), "Place your bet")
        }
        _ <- outQueue.enqueue1(outMessage)
        _ <- stateRef.update(_ => state.newConnection(id, outQueue))
      } yield Session(id, outQueue, stateRef)
    }
  }
  private def webSocketRoute(stateRef: Ref[IO, ServerState]): HttpRoutes[IO] =
    HttpRoutes.of[IO] {

      case GET -> Root / "blackjack" =>
        def pipe(
          session: Session
        ): Pipe[IO, WebSocketFrame, WebSocketFrame] = { inputStream =>
          inputStream
            .collect {
              case Text(message, _) => decode[FromClient](message)
            }
            .evalMap(session.process)
            .collect { case Some(out) => out }
            .merge(session.outQueue.dequeue)
            .map(out => Text(out.asJson.noSpaces))
        }

        for {
          clientQueue <- Queue.unbounded[IO, WebSocketFrame]
          serverQueue <- Queue.unbounded[IO, ToClient]
          playerId = java.util.UUID.randomUUID
          session <- Session.connect(playerId, serverQueue, stateRef)
//

          response <- WebSocketBuilder[IO].build(
            receive = clientQueue.enqueue,
            send = clientQueue.dequeue.through(pipe(session)),
          )
        } yield response
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
