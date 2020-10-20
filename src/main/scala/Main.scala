import com.mongodb.client.model.UpdateOptions
import org.mongodb.scala._
import org.mongodb.scala.bson.codecs.Macros._
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}

import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._
import org.mongodb.scala.model.Sorts._

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.{Source, StdIn}
import scala.io.StdIn.readLine

object Main extends App {

  // Demonstrate understanding of basic command line input.
  if (args.length == 0) {
    println("No command-line arguments were entered, but continuing.")
  }
  else {
    for (a <- args) {
      println(s"The command-line argument ${a} was entered.")
    }
  }

  // An UpdateOptions object that has upsert set to true, for convenience.
  val upsertTrue: UpdateOptions = new UpdateOptions().upsert(true)
  val upsertFalse: UpdateOptions = new UpdateOptions().upsert(false)

  // Helper functions for access and printing.
  def getResults[T](obs: Observable[T]): Seq[T] = {
    Await.result(obs.toFuture(), Duration(10, SECONDS))
  }

  def printResults[T](obs: Observable[T]): Unit = {
    getResults(obs).foreach(println(_))
  }

  def nthIndexOf(text: String, delimiter: Char, n: Int): Int = {
    var p = n
    for (i <- 0 until text.length) {
      if (text.charAt(i) == delimiter) {
        p -= 1
        if (p == 0) return i
      }
    }
    -1
  }

  val codecRegistry = fromRegistries(fromProviders(classOf[Movie]), MongoClient.DEFAULT_CODEC_REGISTRY)
  val mongoClient: MongoClient = MongoClient()
  val database: MongoDatabase = mongoClient.getDatabase("test").withCodecRegistry(codecRegistry)
  val master_collection: MongoCollection[Movie] = database.getCollection("main")

  var currentLine = 0
  var t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20 = 0
  var commas_in_title, colons_in_title = 0

  var t9m_b_actual, t9m_e_actual, t9d_b_actual, t9d_e_actual, t9y_b_actual, t9y_e_actual = 0

  // Open the JSON for processing. Pass in a command line here once it's finalized.
  val inputFile: Unit = Source.fromFile("C:\\Users\\Chu\\Desktop\\Movies\\movies.json").getLines.foreach {
    line: String => {

      // Increment the line we're checking and reset the number of extra commas and colons we've seen.
      commas_in_title = 0
      colons_in_title = 0
      currentLine += 1
      // println("Now processing line " + currentLine + ".")

      // Compute some indices that are as delimiters.
      t1 = nthIndexOf(line, ':', 1) + 3 // ok
      t2 = nthIndexOf(line, ',', 1) - 1 //

      // println("[t1]  : begin [title] with [:] at " + t1 + ".")
      // println("[t2]  : end [title] with [,] at " + t2 + ".")

      t3 = nthIndexOf(line, ':', 2) + 2

      // The 2nd, 3rd, 4th... comma cannot come before the ending " of the title field.
      // Likewise, the 2nd, 3rd, 4th... colon cannot come before the ending " of the title field.
      var ending_delimiter_of_title = nthIndexOf(line, '"', 4)
      // println("Our title should end at index [" + ending_delimiter_of_title + "].\n")

      while (t2 < ending_delimiter_of_title) {
        // println("t2 right inside the while is [" + t2 + "].")
        // println("The ending index of the title string index inside the t2 while was [" + ending_delimiter_of_title + "].")

        // Add a comma.
        commas_in_title += 1

        // Update the most recently seen t2.
        t2 = nthIndexOf(line, ',', 1 + commas_in_title)

        // println("t2 after update is [" + t2 + "]. This should be within the title bounds.")
        // println("There are now [" + commas_in_title + "] additional commas added to t3 and later.\n")
      }

      while (t3 < ending_delimiter_of_title) {
        // println("t3 right inside the while is [" + t3 + "].")
        // println("The ending index of the title string index inside the t3 while was [" + ending_delimiter_of_title + "].")

        // Add a colon.
        colons_in_title += 1

        // Update the most recently seen t3.
        t3 = nthIndexOf(line, ':', 2 + colons_in_title)

        // println("t3 after update is [" + t3 + "]. This should be within the title bounds.")
        // println("There are now [" + colons_in_title + "] additional colons added to t3 and later.\n")
      }

      t4 = nthIndexOf(line, ',', 2 + commas_in_title)

      // println("[t3]  : begin [domestic_gross] with [:] at " + t3 + ".")
      // println("[t4]  : end [domestic_gross] with [,] at " + t4 + ".")

      t5 = nthIndexOf(line, ':', 3) + 2
      t6 = nthIndexOf(line, ',', 3 + commas_in_title)

      // println("[t5]  : begin [international_gross] with [:] at " + t5 + ".")
      // println("[t6]  : end [international_gross] with [,] at " + t6 + ".")

      t7 = nthIndexOf(line, ':', 5 + colons_in_title) + 2
      t8 = nthIndexOf(line, ',', 5 + commas_in_title)

      // println("[t7]  : begin [budget] with [:] at " + t7 + ".")
      // println("[t8]  : end [budget] with [,] at " + t8 + ".")

      t9 = nthIndexOf(line, ':', 6 + colons_in_title) + 3
      t10 = nthIndexOf(line, ',', 6 + commas_in_title) - 1

      // println("[t9]  : begin [release_date] with [:] at " + t9 + ".")
      // println("[t10] : end [release_date] with [,] at " + t10 + ".")

      t9m_b_actual = t9
      t9m_e_actual = t9 + 3

      // println("[t9m_b_actual]  : begin [actual_month_string] with [:] at " + t9m_b_actual + ".")
      // println("[t9m_e_actual] : end [actual_month_string] with [,] at " + t9m_e_actual + ".")
      // println(line.substring(t9m_b_actual, t9m_e_actual))

      t9d_b_actual = t9m_e_actual + 1
      t9d_e_actual = t9m_e_actual + 3

      // println("[t9d_b_actual]  : begin [actual_day_string] with [:] at " + t9d_b_actual + ".")
      // println("[t9d_e_actual] : end [actual_day_string] with [,] at " + t9d_e_actual + ".")
      // println(line.substring(t9d_b_actual, t9d_e_actual))

      t9y_b_actual = t9d_e_actual + 1
      t9y_e_actual = t9d_e_actual + 5

      // println("[t9y_b_actual]  : begin [actual_year_string] with [:] at " + t9y_b_actual + ".")
      // println("[t9y_e_actual] : end [actual_year_string] with [,] at " + t9y_e_actual + ".")
      // println(line.substring(t9y_b_actual, t9y_e_actual))

      t11 = nthIndexOf(line, ':', 11 + colons_in_title) + 3
      t12 = nthIndexOf(line, ',', 11 + commas_in_title) - 1

      // println("[t11] : begin [genre] with [:] at " + t11 + ".")
      //println("[t12] : end [genre] with [,] at " + t12 + ".")

      t13 = nthIndexOf(line, ':', 13 + colons_in_title) + 3
      t14 = nthIndexOf(line, ',', 13 + commas_in_title) - 1

      // println("[t13] : begin [director] with [:] at " + t13 + ".")
      // println("[t14] : end [director] with [,] at " + t14 + ".")

      t15 = nthIndexOf(line, ':', 14 + colons_in_title) + 2
      t16 = nthIndexOf(line, ',', 14 + commas_in_title)

      // println("[t15] : begin [rt_score] with [:] at " + t15 + ".")
      // println("[t16] : end [rt_score] with [,] at " + t16 + ".")

      t17 = nthIndexOf(line, ':', 15 + colons_in_title) + 2
      t18 = nthIndexOf(line, ',', 15 + commas_in_title)

      // println("[t17] : begin [imdb_score] with [:] at " + t17 + ".")
      // println("[t18] : end [imdb_score] with [,] at " + t18 + ".")

      t19 = nthIndexOf(line, ':', 16 + colons_in_title) + 2
      t20 = nthIndexOf(line, ',', 16 + commas_in_title) - 1

      // println("[t19] : begin [imdb_votes] with [:] at " + t19 + ".")
      // println("[t20] : end [imdb_votes] with [,] at " + t20 + ".\n")

      // These variables hold the data we want from each line.
      var title_from_line = ""
      var domestic_gross_from_line, international_gross_from_line, budget_from_line = 0
      var release_date_from_line, genre_from_line, director_from_line = ""
      var rt_score_from_line = 0
      var imdb_score_from_line = 0.0
      var imdb_votes_from_line = 0

      var months_for_computed_time, days_for_computed_time, years_for_computed_time = 0L: Long
      var release_date_in_seconds_from_line = 0L: Long

      title_from_line = line.substring(t1, t2)
      try {
        domestic_gross_from_line = line.substring(t3, t4).toInt
      } catch {
        case n: NumberFormatException => domestic_gross_from_line = -1;
      }
      try {
        international_gross_from_line = line.substring(t5, t6).toInt
      } catch {
        case n: NumberFormatException => international_gross_from_line = -1;
      }
      try {
        budget_from_line = line.substring(t7, t8).toInt
      } catch {
        case n: NumberFormatException => budget_from_line = -1;
      }
      release_date_from_line = line.substring(t9, t10)

      // Compute in time in seconds when the movie released.
      line.substring(t9m_b_actual, t9m_e_actual) match {
        case "Jan" => months_for_computed_time = 1
        case "Feb" => months_for_computed_time = 2
        case "Mar" => months_for_computed_time = 3
        case "Apr" => months_for_computed_time = 4
        case "May" => months_for_computed_time = 5
        case "Jun" => months_for_computed_time = 6
        case "Jul" => months_for_computed_time = 7
        case "Aug" => months_for_computed_time = 8
        case "Sep" => months_for_computed_time = 9
        case "Oct" => months_for_computed_time = 10
        case "Nov" => months_for_computed_time = 11
        case "Dec" => months_for_computed_time = 12
      }

      days_for_computed_time = line.substring(t9d_b_actual, t9d_e_actual).toInt
      years_for_computed_time = line.substring(t9y_b_actual, t9y_e_actual).toInt
      release_date_in_seconds_from_line = (60 * 60 * 24 * 365 * years_for_computed_time) + (60 * 60 * 24 * 30 * months_for_computed_time) + (60 * 60 * 24 * days_for_computed_time)

      // For testing.
      // println("years_for_computed_line: [" + years_for_computed_time + "].")
      // println("months_for_computed_line: [" + months_for_computed_time + "].")
      // println("days_for_computed_line: [" + days_for_computed_time + "].")
      // println("release_date_in_seconds_from_line: [" + release_date_in_seconds_from_line + "].")

      // Catch formatting due to how the genre and data is formatted.
      if (line.substring(t11, t12).equals("ul")) {
        genre_from_line = "unknown genre"
      } else {
        genre_from_line = line.substring(t11, t12)
      }
      if (line.substring(t13, t14).equals("ul")) {
        director_from_line = "a unknown director"
      } else {
        director_from_line = line.substring(t13, t14)
      }

      try {
        rt_score_from_line = line.substring(t15, t16).toInt
      } catch {
        case n: NumberFormatException => rt_score_from_line = -1;
      }
      try {
        imdb_score_from_line = line.substring(t17, t18).toDouble
      } catch {
        case n: NumberFormatException => imdb_score_from_line = -1.0;
      }
      try {
        imdb_votes_from_line = line.substring(t19, t20).toInt
      } catch {
        case n: NumberFormatException => imdb_votes_from_line = -1;
      }

      /* What are we inserting into the document?
      println("Title: " + title_from_line)
      println("Domestic Gross: " + domestic_gross_from_line)
      println("Worldwide Gross: " + international_gross_from_line)
      println("Budget: " + budget_from_line)
      println("Release Date: " + release_date_from_line)
      println("Release Date in Seconds: " + release_date_in_seconds_from_line)
      println("Genre: " + genre_from_line)
      println("Director: " + director_from_line)
      println("Rotten Tomatoes Score: " + rt_score_from_line)
      println("IMDB Score: " + imdb_score_from_line)
      println("IMDB Votes: " + imdb_votes_from_line)
      */

      // Finally, insert the document.
      printResults(master_collection.insertOne(Movie(title_from_line, domestic_gross_from_line, international_gross_from_line, budget_from_line, release_date_from_line, release_date_in_seconds_from_line, genre_from_line, director_from_line, rt_score_from_line, imdb_score_from_line, imdb_votes_from_line)))
    }
  }

  println()
  println("Loading of [movies.json] has completed.\n")
  printMenuAndPrompt()

  def printOptions(): Unit = {
    println("**********************************************************************")
    println("*    Welcome to [movies]. Please select an option from the below.    *")
    println("**********************************************************************\n")
    println("[1]      Show All Movies                                             ")
    println("[2]      Search for Movie                                            ")
    println("[3]      Add Movie                                                   ")
    println("[4]      Edit Movie                                                  ")
    println("[5]      Remove Movie                                                ")
    println("[6]      About                                                       ")
    println("[7]      Quit the Program                                            \n")
  }

  def printMenuAndPrompt(): Unit = {

    var loop = true
    while (loop) {

      // This loop here will repeatedly prompt, listen, run code, and repeat.
      printOptions()

      // Get user input with StdIn.readLine and read directly from StdIn.
      StdIn.readLine() match {
        case "1" => showAllInDatabase()
        case "2" => searchMovie()
        case "3" => addMovie()
        case "4" => editMovie()
        case "5" => removeMovie()
        case "6" => showAbout()
        case "7" =>
          println("Thank you for using [movies].\n")
          mongoClient.close()
          System.exit(0)
        case notRecognized => println(s"[$notRecognized] was not a valid option. Please try again.\n")
      }
    }
  }

  def showAllInDatabase(s: MongoCollection[Movie] = master_collection): Any = {

    // Prints all the documents in the master collection.
    var listOfMovies = getResults(s.find()).toList
    for (movie <- listOfMovies) {
      println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
      println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
      println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
    }
  }

  // The most complex function, needs to implement everything on whiteboard.
  def searchMovie(): Any = {
    printFilterMenu()
  }

  // Implement exception handling.
  def addMovie(): Any = {

    var title_as = ""
    var genre_as, director_as = ""

    var domestic_gross_as, international_gross_as, budget_as = ""
    var release_day_as, release_month_as, release_year_as = ""
    var rt_score_as, imdb_score_as, imdb_votes_as = ""

    var domestic_gross_a, international_gross_a, budget_a = 0
    var release_day_a, release_year_a = 0
    var rt_score_a = 0
    var imdb_score_a = 0.0
    var imdb_votes_a = 0

    var release_date_string_as = ""
    var release_month_int = 0
    var release_date_in_seconds_as = 0: Long

    print("Title to Add: ")
    title_as = StdIn.readLine()
    if (title_as.equals("")) {
      println("[Notice]: A title must be entered.")
      println("Starting a new addition.\n")
      addMovie()
    }

    print("\nDomestic Gross: ")
    domestic_gross_as = StdIn.readLine()
    try {
      domestic_gross_a = domestic_gross_as.toInt
      if (domestic_gross_a < 0) {
        println("The domestic gross can't be negative, defaulting to -1.\n")
        domestic_gross_a = -1
      }
      if (domestic_gross_a > Int.MaxValue) {
        println("The domestic gross can't exceed the size of an Int, defaulting to Int.MaxValue.\n")
        domestic_gross_a = Int.MaxValue
      }
    } catch {
      case n: NumberFormatException =>
        println("The domestic gross wasn't recognized, defaulting to -1.\n")
        domestic_gross_a = -1
    }

    print("Worldwide Gross: ")
    international_gross_as = StdIn.readLine()
    try {
      international_gross_a = international_gross_as.toInt
      if (international_gross_a < 0) {
        println("The worldwide gross can't be negative, defaulting to -1.\n")
        domestic_gross_a = -1
      }
      if (international_gross_a > Int.MaxValue) {
        println("The international gross can't exceed the size of an Int, defaulting to Int.MaxValue.\n")
        international_gross_a = Int.MaxValue
      }
    } catch {
      case n: NumberFormatException =>
        println("The international gross wasn't recognized, defaulting to -1.\n")
        international_gross_a = -1
    }

    print("Budget: ")
    budget_as = StdIn.readLine()
    try {
      budget_a = budget_as.toInt
      if (budget_a < 0) {
        println("The budget can't be negative, defaulting to -1.\n")
        budget_a = -1
      }
      if (budget_a > Int.MaxValue) {
        println("The budget gross can't exceed the size of an Int, defaulting to Int.MaxValue.\n")
        budget_a = Int.MaxValue
      }
    } catch {
      case n: NumberFormatException =>
        println("The budget wasn't recognized, defaulting to -1.\n")
        budget_a = -1
    }

    print("Genre: ")
    genre_as = StdIn.readLine()
    if (genre_as.equals("")) {
      println("No genre was entered, defaulting to \"unknown genre\".\n")
      genre_as = "unknown genre"
    }

    print("Director: ")
    director_as = StdIn.readLine()
    if (director_as.equals("")) {
      println("No director was entered, defaulting to \"a unknown director\".\n")
      director_as = "a unknown director"
    }

    print("Month (1-12): ")
    StdIn.readLine match {
      case "1" => release_month_as = "Jan"
        release_month_int = 1
      case "2" => release_month_as = "Feb"
        release_month_int = 2
      case "3" => release_month_as = "Mar"
        release_month_int = 3
      case "4" => release_month_as = "Apr"
        release_month_int = 4
      case "5" => release_month_as = "May"
        release_month_int = 5
      case "6" => release_month_as = "Jun"
        release_month_int = 6
      case "7" => release_month_as = "Jul"
        release_month_int = 7
      case "8" => release_month_as = "Aug"
        release_month_int = 8
      case "9" => release_month_as = "Sep"
        release_month_int = 9
      case "10" => release_month_as = "Oct"
        release_month_int = 10
      case "11" => release_month_as = "Nov"
        release_month_int = 11
      case "12" => release_month_as = "Dec"
        release_month_int = 12
      case notRecognized =>
        println(s"[$notRecognized] was not a valid month. Defaulting to January.\n")
        release_day_as = "Jan"
        release_month_int = 1
    }

    print("Day (01-31): ")
    release_day_as = StdIn.readLine()
    try {
      release_day_a = release_day_as.toInt
      if (release_day_a < 1 || release_day_a > 31) {
        println(s"[Notice]: [$release_day_a] was not a valid day, defaulting to 1. Must be in range of [0 - 31].\n")
        release_day_a = 1
      }
    } catch {
      case n: NumberFormatException =>
        println(s"[$release_day_a] was not a valid day, defaulting to 1. Must be in range of [0 - 31].\n")
        release_day_a = 1
    }

    print("Year (YYYY): ")
    release_year_as = StdIn.readLine()
    try {
      release_year_a = release_year_as.toInt
      if (release_year_a < 0) {
        println("[Notice]: The release year can't be before the beginning of time, defaulting to 1970. Must be in range of [0 - Int.MaxValue].\n")
        release_year_a = 1970
      }
      if (release_year_a > Int.MaxValue) {
        println("[Notice]: The release year can't exceed Int.MaxValue, defaulting to Int.MaxValue. Must be in range of [0 - Int.MaxValue].\n")
        release_year_a = Int.MaxValue
      }
    } catch {
      case n: NumberFormatException =>
        println(s"[Notice]: [$release_year_a] was not a valid year. Defaulting to 1970.\n")
        release_year_a = 1970
    }

    // Compute the release date string.
    if (release_day_a < 10)
      release_date_string_as = release_month_as + " 0" + release_day_a + " " + release_year_a
    if (release_day_a >= 10)
      release_date_string_as = release_month_as + " " + release_day_a + " " + release_year_a

    // Compute the release date in seconds from the release date entered.
    release_date_in_seconds_as = (60 * 60 * 24 * 365 * release_year_a) + (60 * 60 * 24 * 30 * release_month_int) + (60 * 60 * 24 * release_day_a)

    print("Rotten Tomatoes Rating: ")
    rt_score_as = StdIn.readLine()
    try {
      rt_score_a = rt_score_as.toInt
      if (rt_score_a < 0) {
        println("[Notice]: The Rotten Tomatoes score can't be negative, defaulting to -1.\n")
        rt_score_a = -1
      }
      if (rt_score_a > 100) {
        println("[Notice]: The Rotten Tomatoes score can't be above 100, defaulting to 100.\n")
        rt_score_a = 100
      }
    } catch {
      case n: NumberFormatException =>
        println("The Rotten Tomatoes score was not recognized, defaulting to -1.\n")
        rt_score_a = -1
    }

    print("IMDB Rating: ")
    imdb_score_as = StdIn.readLine()
    try {
      imdb_score_a = imdb_score_as.toDouble
      if (imdb_score_a < 0.0) {
        println("[Notice]: The IMDB score can't be negative, defaulting to -1.0.\n")
        imdb_score_a = -1.0
      }
      if (imdb_score_a > 10.0) {
        println("[Notice]: The Rotten Tomatoes score can't be above 10.0, defaulting to 10.0.\n")
        imdb_score_a = 10.0
      }
    } catch {
      case n: NumberFormatException =>
        println("The IMDB score was not recognized, defaulting to -1.0.\n")
        imdb_score_a = -1.0
    }

    print("Number of IMDB Votes: ")
    imdb_votes_as = StdIn.readLine()
    try {
      imdb_votes_a = imdb_votes_as.toInt
      if (imdb_votes_a < 0) {
        println("[Notice]: The number of IMDB votes can't be negative, defaulting to -1.\n")
        imdb_votes_a = 0
      }
      if (imdb_votes_a > Int.MaxValue) {
        println("[Notice]: The number of IMDB votes has exceeded Int.MaxValue, defaulting to Int.MaxValue.\n")
        imdb_votes_a = Int.MaxValue
      }
    } catch {
      case n: NumberFormatException =>
        println("The number of IMDB votes was not recognized, defaulting to -1.\n")
        imdb_score_a = -1
    }

    // Add the movie, but only if it doesn't exist in the database.
    if (getResults(master_collection.find((equal("title", title_as)))).isEmpty) {
      master_collection.insertOne(Movie(title_as, domestic_gross_a, international_gross_a, budget_a, release_date_string_as, release_date_in_seconds_as, genre_as, director_as, rt_score_a, imdb_score_a, imdb_votes_a))
      println(s"[Notice]: [$title_as] was not previously found in the database and was added!.\n")
      printMenuAndPrompt()
    }

    else {
      println(s"[Notice]: [$title_as] was found in the database and was not added again.\n")
      printMenuAndPrompt()
    }
  }

  // Implement exception handling.
  def editMovie(): Any = {

    var title_to_search_for, title_edit_as, genre_edit_as, director_edit_as = ""
    var domestic_gross_edit_as, international_gross_edit_as, budget_edit_as = ""
    var release_day_edit_as, release_month_edit_as, release_year_edit_as, release_date_string_as = ""
    var rt_score_edit_as, imdb_score_edit_as, imdb_votes_edit_as = ""

    var domestic_gross_edit_a, international_gross_edit_a, budget_edit_a = 0
    var release_day_edit_a, release_year_edit_a = 0
    var rt_score_edit_a = 0
    var imdb_score_edit_a = 0.0
    var imdb_votes_edit_a = 0

    var release_month_edit_int = 0
    var release_date_in_seconds_as = 0: Long

    // A quick check to see if the title is in the master collections, respectively.
    print("Movie to Edit: ")
    title_to_search_for = StdIn.readLine()
    if (title_to_search_for.equals("")) {
      println("A search must be entered.")
      println("Starting a new edit.\n")
      editMovie()
    }

    // Check if the movie exists in the database.
    if (getResults(master_collection.find((equal("title", title_to_search_for)))).isEmpty) {
      println(s"[Notice]: [$title_to_search_for] was not found in the database. Please add [$title_to_search_for] first.\n")
      printMenuAndPrompt()
    }

    else {
      print("New Title: ")
      title_edit_as = StdIn.readLine()
      if (title_edit_as.equals("")) {
        println("A new title must be entered, starting a new edit.\n")
        editMovie()
      }

      print("\nDomestic Gross: ")
      domestic_gross_edit_as = StdIn.readLine()
      try {
        domestic_gross_edit_a = domestic_gross_edit_as.toInt
        if (domestic_gross_edit_a < 0) {
          println("The domestic gross can't be negative, setting the default value to -1.\n")
          domestic_gross_edit_a = -1
        }
        if (domestic_gross_edit_a > Int.MaxValue) {
          println("The domestic gross can't exceed Int.MaxValue, setting the default value to Int.MaxValue.\n")
          domestic_gross_edit_a = Int.MaxValue
        }
      } catch {
        case n: NumberFormatException =>
          println("Since the input was not recognized, setting the default value to -1.\n")
          domestic_gross_edit_a = -1
      }

      print("Worldwide Gross: ")
      international_gross_edit_as = StdIn.readLine()
      try {
        international_gross_edit_a = international_gross_edit_as.toInt
        if (international_gross_edit_a < 0) {
          println("The worldwide gross can't be negative, setting the default value to -1.\n")
          international_gross_edit_a = -1
        }
        if (international_gross_edit_a > Int.MaxValue) {
          println("The worldwide gross can't exceed Int.MaxValue, setting the default value to Int.MaxValue.\n")
          international_gross_edit_a = Int.MaxValue
        }
      } catch {
        case n: NumberFormatException =>
          println("Since the input was not recognized, setting the default value to -1.\n")
          international_gross_edit_a = -1
      }

      print("Budget: ")
      budget_edit_as = StdIn.readLine()
      try {
        budget_edit_a = budget_edit_as.toInt
        if (budget_edit_a < 0) {
          println("The budget can't be negative, setting the default value to -1.\n")
          budget_edit_a = -1
        }
        if (budget_edit_a > Int.MaxValue) {
          println("The budget can't exceed Int.MaxValue, setting the default value to Int.MaxValue.\n")
          budget_edit_a = Int.MaxValue
        }
      } catch {
        case n: NumberFormatException =>
          println("Since the input was not recognized, setting the default value to -1.\n")
          budget_edit_a = -1
      }

      print("Genre: ")
      genre_edit_as = StdIn.readLine()
      if (genre_edit_as.equals("")) {
        println("No genre was entered, setting the default value \"unknown genre\".\n")
        genre_edit_as = "unknown genre"
      }

      print("Director: ")
      director_edit_as = StdIn.readLine()
      if (director_edit_as.equals("")) {
        println("No director was entered, setting the default value \"a unknown director\".\n")
        director_edit_as = "a unknown director"
      }

      print("Month (1-12): ")
      StdIn.readLine match {
        case "1" => release_month_edit_as = "Jan"
          release_month_edit_int = 1;
        case "2" => release_month_edit_as = "Feb"
          release_month_edit_int = 2
        case "3" => release_month_edit_as = "Mar"
          release_month_edit_int = 3
        case "4" => release_month_edit_as = "Apr"
          release_month_edit_int = 4
        case "5" => release_month_edit_as = "May"
          release_month_edit_int = 5;
        case "6" => release_month_edit_as = "Jun"
          release_month_edit_int = 6
        case "7" => release_month_edit_as = "Jul"
          release_month_edit_int = 7
        case "8" => release_month_edit_as = "Aug"
          release_month_edit_int = 8
        case "9" => release_month_edit_as = "Sep"
          release_month_edit_int = 9
        case "10" => release_month_edit_as = "Oct"
          release_month_edit_int = 10
        case "11" => release_month_edit_as = "Nov"
          release_month_edit_int = 11
        case "12" => release_month_edit_as = "Dec"
          release_month_edit_int = 12
        case notRecognized =>
          println(s"[$notRecognized] was not a valid month. Setting the default value of 01 (January).\n")
          release_month_edit_as = "Jan"
          release_month_edit_int = 1
      }

      print("Day (01-31): ")
      release_day_edit_as = StdIn.readLine()
      try {
        release_day_edit_a = release_day_edit_as.toInt
        if (release_day_edit_a < 1 || release_day_edit_a > 31) {
          println(s"[$release_day_edit_a] was not a valid day. Setting the default value of 01.\n")
          release_day_edit_a = 1
        }
      } catch {
        case n: NumberFormatException =>
          println(s"[$release_day_edit_a] was not a valid day. Setting the default value of 01.\n")
          release_day_edit_a = 1
      }

      print("Year (YYYY): ")
      release_year_edit_as = StdIn.readLine()
      try {
        release_year_edit_a = release_day_edit_as.toInt
        if (release_year_edit_a < 0) {
          println("The release can't be before the beginning of time! Setting the default value of 1970.\n")
          release_year_edit_a = 1970
        }
        if (release_year_edit_a > Int.MaxValue) {
          println("You're breaking the space-time continuum! Setting the default value of Int.MaxValue.\n")
          release_year_edit_a = Int.MaxValue
        }
      } catch {
        case n: NumberFormatException =>
          println(s"[$release_year_edit_a] was not a valid year. Setting the default value of 1970.\n")
          release_year_edit_a = 1970
      }

      // Compute the release date string.
      release_date_string_as = release_month_edit_as + " " + release_day_edit_a + " " + release_year_edit_a

      // Compute the release date in seconds from the release date string.
      release_date_in_seconds_as = (60 * 60 * 24 * 365 * release_year_edit_a) + (60 * 60 * 24 * 30 * release_month_edit_int) + (60 * 60 * 24 * release_day_edit_a)

      print("Rotten Tomatoes Rating: ")
      rt_score_edit_as = StdIn.readLine()
      try {
        rt_score_edit_a = rt_score_edit_as.toInt
        if (rt_score_edit_a < 0) {
          println("The Rotten Tomatoes score can't be negative. Setting the default value of -1.\n")
          rt_score_edit_a = -1
        }
        if (rt_score_edit_a > 100) {
          println("The Rotten Tomatoes score can't be above 100. Setting the default value of 100.\n")
          rt_score_edit_a = 100
        }
      } catch {
        case n: NumberFormatException =>
          println("The score was not recognized between [0 - 100]. Setting the default value of -1.\n")
          rt_score_edit_a = -1
      }

      print("IMDB Rating: ")
      imdb_score_edit_as = StdIn.readLine()
      try {
        imdb_score_edit_a = imdb_score_edit_as.toDouble
        if (imdb_score_edit_a < 0.0) {
          println("The IMDB score can't be negative. Setting the default value of -1.0.\n")
          imdb_score_edit_a = -1.0
        }
        if (imdb_score_edit_a > 10.0) {
          println("The Rotten Tomatoes score can't be above 10.0. Setting the default value of 10.0.\n")
          imdb_score_edit_a = 10.0
        }
      } catch {
        case n: NumberFormatException =>
          println("The score was not recognized between [0.0 - 10.0]. Setting the default value of -1.0.\n")
          imdb_score_edit_a = -1.0
      }

      print("Number of IMDB Votes: ")
      imdb_votes_edit_as = StdIn.readLine()
      try {
        imdb_votes_edit_a = imdb_votes_edit_as.toInt
        if (imdb_votes_edit_a < 0) {
          println("The number of IMDB votes can't be negative. Setting the default value of -1.\n")
          imdb_votes_edit_a = -1
        }
        if (imdb_votes_edit_a > Int.MaxValue) {
          println("The number of IMDB votes can't exceed Int.MaxValue. Setting the default value of Int.MaxValue.\n")
          imdb_votes_edit_a = Int.MaxValue
        }
      } catch {
        case n: NumberFormatException =>
          println("The number of votes should be between [0 - Int.MaxValue]. Setting the default value of -1.\n")
          imdb_votes_edit_a = -1
      }

      // Update the movie, but only if it exists in the database.
      master_collection.updateOne(equal("title", title_to_search_for), combine(set("title", title_edit_as),
        set("us_gross", domestic_gross_edit_a),
        set("world_gross", international_gross_edit_a),
        set("budget", budget_edit_a),
        set("release_date", release_date_string_as),
        set("release_date_in_seconds", release_date_in_seconds_as),
        set("genre", genre_edit_as),
        set("director", director_edit_as),
        set("rotten_tomatoes", rt_score_edit_a),
        set("imdb", imdb_score_edit_a),
        set("imdb_votes", imdb_votes_edit_a)), upsertFalse)

      println(s"[Notice]: [$title_to_search_for] was found and edited successfully!\n")
      printMenuAndPrompt()
    }
  }

  // Implement exception handling.
  def removeMovie(): Any = {

    var title_remove_as = ""

    // Take in all the field values for the movie we want to add.
    print("Title: ")
    title_remove_as = StdIn.readLine()

    // Check if the movie exists in the database.
    if (getResults(master_collection.find((equal("title", title_remove_as)))).isEmpty) {
      println(s"[Notice]: [$title_remove_as] was not found in the database. Please add [$title_remove_as] first, then remove it.\n")
      printMenuAndPrompt()
    }

    // Remove the movie, but only if it exists in the database.
    else {
      master_collection.deleteOne(Filters.equal("title", title_remove_as))
      println(s"[Notice]: [$title_remove_as] was found in the database and removed.\n")
      printMenuAndPrompt()
    }
  }

  /*
  // If there's time, implement a simple bookmarking system with another collection.
  def viewOrModifyBookmarks(): Any = {

    // Format and print all the documents in the bookmarks collection.
    println("Here's a list of your bookmarked movies.")
    println()
    println(getResults(bookmarks_collection.find()))
    println()

    println("[1]      Add Movie to Bookmarks              ")
    println("[2]      Edit Movie in Bookmarks             ")
    println("[3]      Remove Movie from Bookmarks         ")
    println("[4]      Return to Top Menu                  ")
    println("[5]      Quit the Program                   \n")

    // Get user input with StdIn.readLine and read directly from StdIn.
    StdIn.readLine() match {
      case "1" => addMovie(bookmarks_collection)
      case "2" => editMovie(bookmarks_collection)
      case "3" => removeMovie(bookmarks_collection)
      case "4" => printMenuAndPrompt()
      case "5" =>
        println("Thank you for coming to the MoViEs. Please come again.")
        System.exit(0)
      case notRecognized =>
        println(s"[$notRecognized] was not a valid option. Please try again.\n")
        viewOrModifyBookmarks()
    }
  }*/

  // Expand this section if there's time.
  @tailrec
  def showAbout(): Any = {

    var key = ""

    println("**********************************************************************************************************************************")
    println("* [movies] is designed to take in a specific file called [movies.json] from the command line.                                    *")
    println("* It implements a simple movie searching program using MongoDB. You can search for, add, edit, and remove movies.                *")
    println("*                                                                                                                                *")
    println("* Author: Ernie Chu                                                                                                              *")
    println("*                                                                                                                                *")
    println("**********************************************************************************************************************************")
    println("Type [OK] to return to the main menu, or [exit] to quit the program.\n")

    key = StdIn.readLine()
    if (key.equalsIgnoreCase("OK")) {
      println()
      printMenuAndPrompt()
    } else if (key.equalsIgnoreCase("exit")) {
      println("Thank you for using [movies]. See you next time.")
      mongoClient.close()
      System.exit(0)
    } else {
      println(s"The entered input [$key] was not recognized.\n")
      showAbout()
    }
  }

  def printFilterMenu(): Any = {
    println("Please select a filter from below.\n")
    println("[1]      Search by Title or Genre")
    println("[2]      Search by Earnings ")
    println("[3]      Search by Date")
    println("[4]      Search by Director")
    println("[5]      Return to Top Menu")
    println("[6]      Quit the Program\n")

    // Get user input with StdIn.readLine and read directly from StdIn.
    StdIn.readLine() match {
      case "1" => titleSearch()
      case "2" => earningsSearch()
      case "3" => dateSearch()
      case "4" => directorSearch()
      case "5" => printMenuAndPrompt()
      case "6" =>
        println("Thank you for coming to the MoViEs. Please come again.")
        mongoClient.close()
        System.exit(0)
      case notRecognized => println(s"[$notRecognized] was not a valid option. Returning to top menu.\n")
    }
  }

  def titleSearch(): Any = {

    var title_string, genre_string = ""

    // Attempt to read in the title and genre to search for.
    print("Title: ")
    title_string = readLine()
    print("Genre: ")
    genre_string = readLine()

    // Case -1: No input has been entered for either the title or genre.
    if (title_string.equals("") && genre_string.equals("")) {
      println("\n[Notice]: Both fields can't be left blank. Enter at least a [title] and/or [genre].\n")
      titleSearch()
    }

    // Case 1: Only the title was entered - perform the search and return to the search menu.
    if (genre_string == "") {
      println()

      // Prints all the documents in the master collection.
      var listOfMovies_search_genreNull = getResults((master_collection.find(Filters.equal("title", title_string))))
      for (movie <- listOfMovies_search_genreNull) {
        println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
        println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
        println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
      }

      println()
      printFilterMenu()
    }

    // Case 2: Only the genre was entered.
    if (title_string == "") {
      println()
      println("The results below were found in the master collection.")

      // Prints all the documents in the master collection.
      var listOfMovies_search_titleNull = getResults((master_collection.find(Filters.equal("genre", genre_string))))
      for (movie <- listOfMovies_search_titleNull) {
        println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
        println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
        println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
      }

      println()
      printFilterMenu()
    }

    // Case 3: Both the title and genre were entered.
    if ((genre_string != "") && (title_string != "")) {
      println()
      println("The results below were found in the master collection.")

      // Prints all the documents in the master collection.
      var listOfMovies_search_neitherNull = getResults((master_collection.find(and(Filters.equal("genre", genre_string), (Filters.equal("title", title_string))))))
      for (movie <- listOfMovies_search_neitherNull) {
        println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
        println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
        println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
      }

      println()
      printFilterMenu()
    }
  }

  def earningsSearch(): Any = {

    var earnings_type = ""
    var lower_bound_as, upper_bound_as = ""
    var lower_bound, upper_bound = 0

    println("[1]      Domestic Earnings")
    println("[2]      Worldwide Earnings")
    println("[3]      Return to Search Menu\n")

    StdIn.readLine() match {
      case "1" => earnings_type = "1"
      case "2" => earnings_type = "2"
      case "3" => printFilterMenu()
      case notRecognized =>
        println(s"[$notRecognized] was not a valid option. Please try again.\n")
        earningsSearch()
    }

    print("Earnings Lower Bound: ")
    lower_bound_as = StdIn.readLine()
    try {
      lower_bound = lower_bound_as.toInt
      if (lower_bound < 0) {
        println("The lower bound can't be negative. Setting a default lower bound of 0.\n")
        lower_bound = 0
      }
      if (lower_bound > Int.MaxValue) {
        println("The lower bound can't exceed Int.MaxValue. Setting a default lower bound of Int.MaxValue.\n")
        lower_bound = Int.MaxValue
      }
    } catch {
      case n: NumberFormatException =>
        println("\nUnrecognized input was entered, setting a default lower bound of 0.\n")
        lower_bound = 0
    }

    print("Earnings Upper Bound: ")
    upper_bound_as = StdIn.readLine()
    try {
      upper_bound = upper_bound_as.toInt
      if (upper_bound < 0) {
        println("The upper bound can't be negative. Setting a default upper bound of 0.\n")
        upper_bound = 0
      }
      if (upper_bound > Int.MaxValue) {
        println("The upper bound can't exceed Int.MaxValue . Setting a default upper bound of Int.MaxValue.\n")
        upper_bound = Int.MaxValue
      }
    } catch {
      case n: NumberFormatException =>
        println("\nUnrecognized input was entered, setting a default upper bound of Int.MaxValue.\n")
        upper_bound = Int.MaxValue
    }

    // Switch the bounds if the upper bound ends up higher than the lower bound.
    if (upper_bound <= lower_bound || lower_bound >= upper_bound) {
      print("The upper bound was lower than the lower bound, so the bounds have now been switched.\n")
      upper_bound = lower_bound
      lower_bound = upper_bound
    }

    if (earnings_type == "1") {
      println()
      println("The results below were found in the master collection.")

      // Searches for movies that fall in the bounds of the domestic earnings.
      var listOfMovies_earnings_domestic = getResults((master_collection.find(and(gt("us_gross", lower_bound), lt("us_gross", upper_bound)))))
      for (movie <- listOfMovies_earnings_domestic) {
        println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
        println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
        println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
      }

      println()
      printFilterMenu()
    }

    if (earnings_type == "2") {
      println()
      println("The results below were found in the master collection.")

      // Searches for movies that fall in the bounds of the worldwide earnings.
      var listOfMovies_earnings_worldwide = getResults(master_collection.find(and(gt("world_gross", lower_bound), lt("world_gross", upper_bound))))
      for (movie <- listOfMovies_earnings_worldwide) {
        println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
        println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
        println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
      }

      println()
      printFilterMenu()
    }
  }

  def dateSearch(): Any = {

    var date_search_type = ""
    var year_a, month_a, day_a, year_b, month_b, day_b, year_e, month_e, day_e = ""
    var year_ai, day_ai, year_bi, day_bi, year_ei, day_ei = 0L: Long
    var date_string_a, date_string_b, date_string_e = ""

    var month_int_a, month_int_b = 0L: Long
    var lower_bound_in_seconds, upper_bound_in_seconds = 0: Long

    println("[1]      Search for Movies Released Between Two Dates")
    println("[2]      Search for Movies Released On A Exact Date")
    println("[3]      Return to Search Menu")
    println()

    StdIn.readLine() match {
      case "1" => date_search_type = "between"
      case "2" => date_search_type = "exact"
      case "3" => printFilterMenu()
      case notRecognized =>
        println(s"\n[$notRecognized] was not a valid option. Please try again.\n")
        dateSearch()
    }

    if (date_search_type.equals("between")) {

      // Attempt to read and match in the month for the lower bounded date.
      print("Month [1-12] for Lower Bound: ")
      StdIn.readLine() match {
        case "1" => month_a = "Jan"
          month_int_a = 1
        case "2" => month_a = "Feb"
          month_int_a = 2
        case "3" => month_a = "Mar"
          month_int_a = 3
        case "4" => month_a = "Apr"
          month_int_a = 4
        case "5" => month_a = "May"
          month_int_a = 5
        case "6" => month_a = "Jun"
          month_int_a = 6
        case "7" => month_a = "Jul"
          month_int_a = 7
        case "8" => month_a = "Aug"
          month_int_a = 8
        case "9" => month_a = "Sep"
          month_int_a = 9
        case "10" => month_a = "Oct"
          month_int_a = 10
        case "11" => month_a = "Nov"
          month_int_a = 11
        case "12" => month_a = "Dec"
          month_int_a = 12
        case notRecognized =>
          println(s"[$notRecognized] was not a valid month. Setting default lower bounded month to January (01).\n")
          month_a = "Jan"
          month_int_a = 1
      }

      // Attempt to read in and the day for the lower bounded date.
      print("Day [0-31] for Lower Bound: ")
      try {
        day_a = StdIn.readLine()
        day_ai = day_a.toInt
        if (day_ai < 1 || day_ai > 31) {
          println("An invalid day for the lower bound was entered, setting default day to 01.\n")
          day_ai = 1
        }
      } catch {
        case n: NumberFormatException =>
          println("An invalid day outside [1 - 31] was entered. Setting the default day to 01.\n")
          day_ai = 1
      }

      // Attempt to read in the year for a lower bounded date.
      print("Year [YYYY] for Lower Bound: ")
      try {
        year_a = StdIn.readLine()
        year_ai = year_a.toInt
        if (year_ai < 0) {
          println("The year cannot before 0 AD. Setting default year to 1970.\n")
          year_ai = 1970
        }
      } catch {
        case n: NumberFormatException =>
          println("An invalid year outside [0 - Int.MaxValue] was entered. Setting the default year to 1970.\n")
          year_ai = 1970
      }

      // Attempt to read and match in the month for the upper bounded date.
      print("Month [1-12] for Upper Bound: ")
      StdIn.readLine() match {
        case "1" => month_b = "Jan"
          month_int_b = 1
        case "2" => month_b = "Feb"
          month_int_b = 2
        case "3" => month_b = "Mar"
          month_int_b = 3
        case "4" => month_b = "Apr"
          month_int_b = 4
        case "5" => month_b = "May"
          month_int_b = 5
        case "6" => month_b = "Jun"
          month_int_b = 6
        case "7" => month_b = "Jul"
          month_int_b = 7
        case "8" => month_b = "Aug"
          month_int_b = 8
        case "9" => month_b = "Sep"
          month_int_b = 9
        case "10" => month_b = "Oct"
          month_int_b = 10
        case "11" => month_b = "Nov"
          month_int_b = 11
        case "12" => month_b = "Dec"
          month_int_b = 12
        case notRecognized =>
          println(s"[$notRecognized] was not a valid month. Setting default upper bounded month to January (01).\n")
          month_b = "Jan"
          month_int_a = 1
      }

      // Attempt to read in and the day for the upper bounded date.
      print("Day [0-31] for Upper Bound: ")
      try {
        day_b = StdIn.readLine()
        day_bi = day_b.toInt
        if (day_bi < 1 || day_bi > 31) {
          println("An invalid day for the upper bound was entered, setting default day to 01.\n")
          day_bi = 1
        }
      } catch {
        case n: NumberFormatException =>
          println("An invalid day outside [1 - 31] was entered. Setting the default day to 01.\n")
          day_bi = 1
      }

      // Add the leading 0 necessary to match the data format for days less than 10.
      if (day_ai < 10)
        day_a = "0" + day_ai
      if (day_bi < 10)
        day_b = "0" + day_bi

      // Attempt to read in the year for a upper bounded date.
      print("Year [YYYY] for Upper Bound: ")
      try {
        year_b = StdIn.readLine()
        year_bi = year_b.toInt
        if (year_bi < 0) {
          println("The year cannot before 0 AD. Setting default year to 1970.\n")
          year_bi = 1970
        }
      } catch {
        case n: NumberFormatException =>
          println("An invalid year outside [0 - Int.MaxValue] was entered. Setting the default year to 1970.")
          year_bi = 1970
      }

      // Compute the lower and upper bounds, in terms of seconds.
      lower_bound_in_seconds = (60 * 60 * 24 * 365 * year_ai) + (60 * 60 * 24 * 30 * month_int_a) + (60 * 60 * 24 * day_ai)
      upper_bound_in_seconds = (60 * 60 * 24 * 365 * year_bi) + (60 * 60 * 24 * 30 * month_int_b) + (60 * 60 * 24 * day_bi)

      println()
      println("The results below were found in the master collection.")

      // Search the collection, given the computed bounds.
      var listOfMovies_date_bounded = getResults(master_collection.find(and(gt("release_date_in_seconds", lower_bound_in_seconds), lt("release_date_in_seconds", upper_bound_in_seconds))))
      for (movie <- listOfMovies_date_bounded) {
        println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
        println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
        println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
      }

      println()
      printFilterMenu()
    }

    if (date_search_type.equals("exact")) {

      // Attempt to read and match the month for an exact date search.
      print("Month [1-12] for Exact Search: ")
      StdIn.readLine() match {
        case "1" => month_e = "Jan"
        case "2" => month_e = "Feb"
        case "3" => month_e = "Mar"
        case "4" => month_e = "Apr"
        case "5" => month_e = "May"
        case "6" => month_e = "Jun"
        case "7" => month_e = "Jul"
        case "8" => month_e = "Aug"
        case "9" => month_e = "Sep"
        case "10" => month_e = "Oct"
        case "11" => month_e = "Nov"
        case "12" => month_e = "Dec"
        case notRecognized =>
          println(s"[$notRecognized] was not a valid month. Setting default exact bounded month to January (01).\n")
          month_e = "Jan"
      }

      // Attempt to read in and the day for the exact bounded date.
      print("Day [0-31] for Exact Search: ")
      try {
        day_e = StdIn.readLine()
        day_ei = day_e.toInt
        if (day_ei < 1 || day_ei > 31) {
          println("An invalid day for the exact bound was entered, setting default day to 01.\n")
          day_ei = 1;
        }
      } catch {
        case n: NumberFormatException =>
          println("An invalid day outside [1 - 31] was entered. Setting the default day to 01.\n")
          day_ei = 1;
      }

      // Add the leading 0 necessary to match the data format for days less than 10.
      if (day_ei < 10)
        day_e = "0" + day_ei

      // Attempt to read in the year for a exact bounded date.
      print("Year [YYYY] for Exact Search: ")
      try {
        year_e = StdIn.readLine()
        year_ei = year_e.toInt
        if (year_ei < 0) {
          println("The year cannot before 0 AD. Setting the default year to 1970.\n")
          year_ei = 1970;
        }
      } catch {
        case n: NumberFormatException =>
          println("An invalid year outside [0 - Int.MaxValue] was entered. Setting the default year to 1970.")
          year_ei = 1970
      }

      // For testing.
      date_string_e = month_e + " " + day_e + " " + year_ei
      println(s"The exact date string is [$date_string_e].")

      println()
      println("The results below were found in the master collection.")

      // Search the collection, given the computed bounds.
      var listOfMovies_date_exact = getResults(master_collection.find(Filters.equal("release_date", date_string_e)))
      for (movie <- listOfMovies_date_exact) {
        println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
        println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
        println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
      }

      println()
      printFilterMenu()
    }
  }

  def directorSearch(): Any = {

    // Take in user input as parameters for the search.
    print("Firstname: ")
    var firstname: String = readLine()
    if (firstname.equals("")) {
      println(s"[Error]: Firstname cannot be blank, starting a new search by director.\n")
      directorSearch()
    }
    if (firstname.charAt(firstname.length - 1) == ' ') {
      println(s"[Error]: Extra space(s) at the end of the lastname were detected, so the query is [$firstname]. Please try again without extra spaces.\n")
      directorSearch()
    }

    print("Lastname: ")
    var lastname: String = readLine()
    if (lastname.equals("")) {
      println(s"[Error]: Lastname cannot be blank, starting a new search by director.\n")
      directorSearch()
    }
    if (lastname.charAt(lastname.length - 1) == ' ') {
      println(s"[Error]: Extra space(s) at the end of the lastname were detected, so the query is [$lastname]. Please try again without extra spaces.\n")
      directorSearch()
    }

    // Case -1: No input has been entered for either the title or genre.
    if (firstname.equals("") && lastname.equals("")) {
      println("[Notice]: Both fields can't be left blank. Enter at least a [firstname] and/or [lastname].")
      directorSearch()
    }

    var combined_namestring = firstname + " " + lastname

    println()
    println("The results below were found in the master collection.")

    // Search the collections on the given director name.
    var listOfMovies_director = getResults(master_collection.find(Filters.equal("director", combined_namestring)))
    for (movie <- listOfMovies_director) {
      println(s"[${movie.title}] is a [${movie.genre}] movie directed by [${movie.director}] that released on [${movie.release_date}].")
      println(s"It made [${movie.us_gross}] dollars in the US and [${movie.world_gross}] dollars worldwide with a budget of [${movie.budget}] dollars.")
      println(s"Overall, it has a Rotten Tomatoes score of [${movie.rotten_tomatoes}], a IMDB rating of [${movie.imdb}], and [${movie.imdb_votes}] IMDB votes.")
    }

    printFilterMenu()
  }

  /* Complete another search function if there's time.
  def ratingSearch(): Any = {

    var rating_system = ""
    var low_constraint, high_constraint = 0.0
    var rotten_tomatoes_weight, imdb_weight, low_weighted_constraint, high_weighted_constraint = 0.0
    var low_number_of_imdb_votes, high_number_of_imdb_votes = 0

    println("[1]      Search by Rotten Tomatoes Rating")
    println("[2]      Search by IMDB Rating")
    println("[3]      Search by Custom Weighting")
    println("[4]      Return to Search Menu\n")

    StdIn.readLine() match {
      case "1" => rating_system = "rotten_tomatoes"
      case "2" => rating_system = "imdb"
      case "3" => rating_system = "custom_weighted"
      case "4" => printSortmenu()
      case notRecognized =>
        println(s"[$notRecognized] was not a valid option. Please try again.\n")
        ratingSearch()
    }

    if (rating_system.equals("imdb")) {
      print("Lowest Number of IMDB Votes: ")
      try {
        low_number_of_imdb_votes = StdIn.readInt()
      } catch {
        case n : NumberFormatException =>
          println("You didn't enter a valid number of IMDB votes. Make sure it's an integer.")
          println("Starting a new ratings search.\n")
          ratingSearch()
      }
      print("Greatest Number of IMDB Votes: ")
      try {
        low_constraint = StdIn.readDouble()
      } catch {
        case n : NumberFormatException =>
          println("You didn't enter a valid number of IMDB votes. Make sure it's an integer.")
          println("Starting a new ratings search.\n")
          ratingSearch()
      }
    }

    print("Lowest Rating: ")
    try {
      low_constraint = StdIn.readDouble()
    } catch {
      case n : NumberFormatException =>
        if (rating_system.equals("rotten_tomatoes"))
          println("You didn't enter a valid lower score. Enter the score in the decimal range [0] to [100].")
        if (rating_system.equals("imdb"))
          println("You didn't enter a valid lower score. Enter the score in the decimal range [0] to [10].")
        println("Starting a new ratings search.\n")
        ratingSearch()
    }

    print("Highest Rating: ")
    try {
      high_constraint = StdIn.readDouble()
    } catch {
      case n : NumberFormatException =>
        if (rating_system.equals("rotten_tomatoes"))
          println("You didn't enter a valid upper score. Enter the score in the decimal range [0] to [100].")
        if (rating_system.equals("imdb"))
          println("You didn't enter a valid upper score. Enter the score in the decimal range [0] to [10].")
        println("Starting a new ratings search.\n")
        ratingSearch()
    }

    // Executes if we want to perform a weighted search.
    if (rating_system.equals("custom_weighted")) {
      println("[Tip]: Entering both ratings will create a bound between the lowest and highest rating, inclusive.\n")

      println("Now, enter the weight (from 0 to 100) you want to assign to each system.")
      println("Entering only one weight is necessary. The remainder of the weight will apply towards the other system.")
      println("Ex. Entering [60] for Rotten Tomatoes will automatically assign a weight of [40] to IMDB.\n")

      println("Note that the last entered weight always takes precedence over any previous entry.")
      println("Ex. Entering [20] for IMDB after entering [95] for Rotten Tomatoes will assign Rotten Tomatoes a weight of [80].\n")

      print("Weight for Rotten Tomatoes: ")
      try {
        rotten_tomatoes_weight = StdIn.readDouble()
        if (rotten_tomatoes_weight <= 0.0 || rotten_tomatoes_weight >= 100.0) {
          println("You didn't enter a valid weighting. Enter a weight in the decimal range from [0] to [100]. This is measured in percent.\n")
          ratingSearch()
        }
      } catch {
        case n: NumberFormatException =>
          println("You didn't enter a valid Rotten Tomatoes weight. Enter a decimal from [0] to [100].\n")
          ratingSearch()
      }

      // Determine the weight to assign to IMDB.
      imdb_weight = 1 - rotten_tomatoes_weight

      print("Weight for IMDB: ")
      try {
        imdb_weight = StdIn.readDouble()
        if ((imdb_weight < 0.0 || imdb_weight >= 100.0) && (rotten_tomatoes_weight <= 0.0 || rotten_tomatoes_weight >= 100.0)) {
          println("You didn't enter a valid score. Enter a bottom score in the decimal range from [0] to [100].\n")
          ratingSearch()
        }
      } catch {
        case n: NumberFormatException =>
          println("You didn't enter a valid IMDB weight. Enter enter a decimal from [0] to [100].\n")
          ratingSearch()
      }

      // Determine the weight to assign to Rotten Tomatoes.
      rotten_tomatoes_weight = 1 - imdb_weight

      print("Lowest Score: ")
      try {
        low_weighted_constraint = StdIn.readDouble()
        if (low_weighted_constraint <= 0.0 || low_weighted_constraint >= 100.0) {
          println("You didn't enter a valid score. Enter a bottom weighted score in the decimal range from [0] to [100].\n")
          ratingSearch()
        }
      } catch {
        case n: NumberFormatException =>
          println("You didn't enter a valid lower score.\n")
          ratingSearch()
      }

      print("Highest Score: ")
      try {
        high_weighted_constraint = StdIn.readDouble()
        if (low_weighted_constraint <= 0.0 || low_weighted_constraint >= 100.0) {
          println("You didn't enter a valid score. Enter a top score in the decimal range from [0] to [100].\n")
          ratingSearch()
        }
      } catch {
        case n: NumberFormatException =>
          println("You didn't enter a valid upper score. Enter a upper weighted score in the decimal range from [0] to [100].\n")
          ratingSearch()
      }

      println("[Tip]: Entering both ratings will create a bound between the lowest and highest rating, inclusive.\n")
    }
  }
  */
}