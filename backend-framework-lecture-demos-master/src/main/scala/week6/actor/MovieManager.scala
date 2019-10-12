package week6.actor

import com.sun.javaws.exceptions.ErrorCodeResponseException
import org.omg.PortableInterceptor.SUCCESSFUL

// props
// messages
object MovieManager {

  // Create
  case class CreateMovie(movie: Movie)

  // Read
  case class ReadMovie(id: String)

  // Update
  case class UpdateMovie(movie: Movie)

  // Delete
  case class DeleteMovie(id: String)

  def props() = Props(new MovieManager)
}

// know about existing movies
// can create a movie
// can manage movie
class MovieManager extends Actor with ActorLogging {

  // import companion OBJECT
  import MovieManager._

  var movies: Map[String, Movie] = Map()

  override def receive: Receive = {

    case CreateMovie(movie) =>
      movies.get(movie.id) match {
        case Some(existingMovie) =>
          log.warning(s"Could not create a movie with ID: ${movie.id} because it already exists.")
          sender() ! Left(ErrorResponse(409, s"Movie with ID: ${movie.id} already exists."))

        case None =>
          movies = movies + (movie.id -> movie)
          log.info("Movie with ID: {} created.", movie.id)
          sender() ! Right(SuccessfulResponse(201, s"Movie with ID: ${movie.id} created."))
      }

    case msg: ReadMovie =>
      movies.get(msg.id) match {
        case Some(movie) =>
          log.info("Movie with ID: {} is found", msg.id)
          sender() ! Right(movie)

        case None =>
          log.info("Movie with ID: {} is not found", msg.id)
          sender() ! Left(ErrorResponse(404, s"Movie with ID: ${msg.id} not found."))
      }

    case UpdateMovie(movie) => {
      movies.get(movie.id) match {
        case Some(existingMovie) =>
          movies = movies.filter(_._1 ! = existingMovie.id) + (movie.id -> movie)
          log.info("Movie with ID: {} is updated", movie.id)
          sender() ! SuccessfulResponse(201,s"Movie with ID: ${movie.id} updated" )

        case None =>
          log.info("Movie with ID: {} does not exist", movie.id)
          sender() ! ErrorResponse(409, s"Movie with ID: ${movie.id} does not exist")
      }
    }

    case DeleteMovie(id) => {
      movies.get(id) match {
        case Some(existingMovie) =>
          movies = movies.filter(_._1 != existingMovie.id)
          log.info("Movie with ID: {} is deleted", id)
          sender() ! SuccessfulResponse(206, s"Movie with ID: ${id} is deleted")

        case None =>
          log.info("Movie with ID: {} does not exist", id)
          sender() ! ErrorResponse(409, s"Movie with ID: ${id} does not exist")
      }
    }
  }

  def randomInt() =
    // FIXME: use random
    4
}
