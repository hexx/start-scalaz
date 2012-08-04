import scalaz._
import Scalaz._

import java.util.Date

object vector {
  implicit def vectorShow[A: Show]: Show[Vector[A]] =
    Show.shows("Vector(" + _.map(_.shows).mkString(", ") + ")")
}

case class Rational(n: Int, d: Int) {
  def +(r: Rational) = Rational(n * r.d + r.n * d, d * r.d)
  override def toString = s"$n/$d"
}

object rational {
  implicit def rationalShow: Show[Rational] =
    Show.shows(r => r.n.shows + "/" + r.d.shows)

  implicit def rationalSemigroup: Semigroup[Rational] = new Semigroup[Rational] {
    def append(r1: Rational, r2: => Rational): Rational = r1 + r2
  }
}

object date {
  implicit def dateOrder: Order[Date] = Order.order[Date](_.getTime ?|? _.getTime)
}

case class Student(name: String, grade: Int, birthday: Date)

object student {
  implicit object StudentOrder extends Order[Student] {
    def order(s1: Student, s2: Student): Ordering =
      s1.grade ?|? s2.grade |+| s1.birthday ?|? s2.birthday
  }

  implicit val equalStudent: Equal[Student] = Equal.equalA

  implicit val equalShow: Show[Student] = Show.showA
}

case class User(id: String, pass: String)

object StartScalaz extends App {
  // ・Vectorに対するShowのインスタンス
  // ・以下のクラスに対するShowとSemigroupのインスタンス
  def exercise1 {
    import vector._
    import rational._

    assert(Vector(1).shows == "Vector(1)")
    assert(Vector("geso").shows == "Vector(geso)")

    assert(Rational(1, 2).shows == "1/2")
    assert((Rational(1, 2) |+| Rational(1, 2)) == Rational(4, 4))
  }

  // ・replicateを用いて偶数列からn個取得する関数evens
  // ・unfoldを用いて10進数から2進数へ変換する関数encode
  def exercise2 {
    def evens(n: Int): List[Int] = 0.replicate[List](n, _ + 2)

    def encode(n: Int): List[Int] = n.unfold[List] {
      case 0 => None
      case n => Some(n % 2, n / 2)
    }

    evens(5) assert_=== List(0, 2, 4, 6, 8)
    encode(13) assert_=== List(1, 0, 1, 1)
  }

  // ・java.util.Dateに対するOrderのインスタンス
  // ・以下のクラスに対するOrderのインスタンス
  //   ・gradeとbirthdayを用いる
  //   ・ただしgrade重きを置く
  def exercise3 {
    import student._

    val d1 = new Date
    val d2 = new Date(d1.getTime + 1)

    assert(d1 ?|? d2 == Ordering.LT)

    val format = new java.text.SimpleDateFormat("yyyy MM dd")
    val akari = Student("akari", 1, format.parse("1995 07 24"))
    val kyoko = Student("kyoko", 2, format.parse("1995 03 28"))
    val yui = Student("yui", 2, format.parse("1994 04 22"))
    val chinatsu = Student("chinatsu", 1, format.parse("1995 11 06"))

    List(akari, kyoko, yui, chinatsu).sorted(StudentOrder.toScalaOrdering) assert_=== List(akari, chinatsu, yui, kyoko)
  }

  // ・Map[String, String]から“id“と“pass“をキーとして値を取り出しUserを構築する
  def exercise4 {
    implicit val userEqual: Equal[User] = Equal.equalA
    implicit val userShow: Show[User] = Show.showA

    def user(m: Map[String, String]): Option[User] = {
      (m.get("id") |@| m.get("pass"))(User)
    }

    user(Map("id" -> "halcat0x15a", "pass" -> "gesogeso")) assert_=== Some(User("halcat0x15a", "gesogeso"))
    user(Map.empty) assert_=== None
  }

  exercise1
  exercise2
  exercise3
  exercise4
}
