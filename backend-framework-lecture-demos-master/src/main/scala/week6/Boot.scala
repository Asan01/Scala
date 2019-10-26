package week6

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.stream.{ActorMaterializer, Materializer}
import akka.util.Timeout
import week6.actor.{MovieManager, TestBot}
import week6.model.{ErrorResponse, Movie, Response, SuccessfulResponse}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._

object Boot extends App with SprayJsonSerializer {

  implicit val system: ActorSystem = ActorSystem("movie-service")
  implicit val materializer: Materializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  implicit val timeout: Timeout = Timeout(10.seconds)

  val movieManager = system.actorOf(MovieManager.props(), "movie-manager")



    val route =
    path("healthcheck") {
      get {
        complete {
          "OK"
        }
      }
    } ~
    pathPrefix("kbtu-cinema") {
      path("movie" / Segment) { movieId =>
        get {
            val output: Future[Either[ErrorResponse, Movie]] = (movieManager ? MovieManager.ReadMovie(movieId)).mapTo[Either[ErrorResponse, Movie]]
            onSuccess(output){
              case Left(error) => complete(error.status, error)
              case right(movie) => complete(200, movie)
            }
        }
            delete{
              val output: Future[Either[ErrorCodeResponse, SuccessfulResponse]] = (movieManager ? MovieManager.DeleteMovie(movieId.)).mapTo[Either[ErrorResponse, SuccessfulResponse]]
              handle(output)
            }
      } ~
      path("movie") {
        post {
          entity(as[Movie]) { movie =>
              val output: Future[Either[ErrorResponse, SuccessfulResponse]] = (movieManager ? MovieManager.CreateMovie(movie)).mapTo[Either[ErrorResponse, SuccessfulResponse]]
              handle(output)
          }
        }
          put{
            entity(as[Movie]) {  movie=>
              val output: Future[Either[ErrorResponse, SuccessfulResponse]] = (movieManager ? MovieManager.UpdateMovie(movie)).mapTo[Either[ErrorResponse, SuccessulResponse]]
              handle(output)
            }
          }
      }
    }

  val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080)

  def handle(output:Future[Either[ErrorResponse, SuccessfulResponse]]) = {
    onSuccess(output) {
      case left(error) => complete(error.status, error)
      case right(successful) => complete(successful.status, successful)
    }
  }





//  val testBot = system.actorOf(TestBot.props(movieManager), "test-bot")
//
//
//
//  // test create
//  testBot ! TestBot.TestCreate
//
//  // test conflict
//  testBot ! TestBot.TestConflict
//  testBot ! "bla-bla"
//
//  // test read
//  testBot ! TestBot.TestRead

}
