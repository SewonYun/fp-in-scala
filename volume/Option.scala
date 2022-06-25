import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](d: B): B = this match {
    case None    => d
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    var a = this.map(f)
    a.getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this != None) this
    else ob
  }

  def filter(f: A => Boolean): Option[A] = {
    if (this.getOrElse(false) == false) this
    else None
  }

  
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

case class Employee(name: String, department: String)

def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

def lookupByName(name: String): Option[Employee] = name match {
  case "네철수" => Some(Employee(name, "네이비"))
  case "카철수" => Some(Employee(name, "카카어"))
  case "라철수" => Some(Employee(name, "라이"))
  case _ => None
}

def robotWorld(e: Employee): Employee = {
  println(e)
  Employee("로봇", e.department)
}

// def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
// }

// println(lookupByName("김철수").map(_.department).getOrElse("default dept."))
// println(lookupByName("네철수").map(_.department).getOrElse("default dept."))
// println(lookupByName("라철수").map(_.department).getOrElse("default dept."))
// println(lookupByName("네철수"))
