import org.bson.types.ObjectId

//Any class that represents a Mongo document should be similar to this.
// All documents needs a unique _id field and an apply function that generates that field.
case class Movie(_id: ObjectId, title: String, us_gross: Int, world_gross: Int, budget: Int, release_date: String, release_date_in_seconds: Long,
                 genre: String, director: String, rotten_tomatoes: Int, imdb: Double, imdb_votes : Int) {}

object Movie {
  def apply(title: String, us_gross: Int, world_gross: Int, budget: Int, release_date: String, release_date_in_seconds: Long,
            genre: String, director: String, rotten_tomatoes: Int, imdb: Double, imdb_votes: Int)
  : Movie = Movie(new ObjectId(), title, us_gross, world_gross, budget, release_date, release_date_in_seconds, genre, director, rotten_tomatoes, imdb, imdb_votes)
}