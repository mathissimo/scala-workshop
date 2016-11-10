package section3

object Section3 {

  case class User(
                   id: Int,
                   firstName: String,
                   lastName: String,
                   age: Option[Int],
                   gender: Option[String])

  object UserRepository {
    val male = Some("male")
    val female = Some("female")

    private val users = Map(
      1 -> User(1, "John", "Doe", Some(32), male),
      2 -> User(2, "Johanna", "Doe", Some(30), None),
      3 -> User(3, "Alice", "Doe", None, female),
      4 -> User(4, "Lisa", "Doe", Some(8), female),
      5 -> User(5, "Kathrin", "Doe", Some(18), female)
    )

    def findById(id: Int): Option[User] = users.get(id)

    def findAll = users.values

  }

  def isMale(id: Int): Boolean = {
    UserRepository.findById(id) match {
      case None => false
      case Some(user) => user.gender == UserRepository.male
    }
  }

  def isUserAllowed(id: Int): Boolean = {
    val allowedUser: Option[User] = for {
      user <- UserRepository.findById(id)
      gender <- user.gender
      age <- user.age
      if gender == "female"
      if age >= 18
    } yield user
    allowedUser.isDefined;
  }

}
