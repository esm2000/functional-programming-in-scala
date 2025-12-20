case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))


// map2() would have to be changed to evaluate it's first argument even when the Either object 
// it's a part of is an instance of Left()

// Either[String, Person] would have to be changed to Either[List[String], Person]

// map2() would also have to prepare a list of Left() and prepend/append to the list of Left()

// changing map2() would probably require creating a trait that inherits from Either and overwrites it's existing map2() function
def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))