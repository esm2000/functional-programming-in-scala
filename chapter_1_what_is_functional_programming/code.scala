class Cafe:
    def buyCoffee(cc: CreditCard): (Coffee, Charge) =
        val cup = Coffee()
        (cup, Charge(cc, cup.price))
    
    def buyCoffees(
        cc: CreditCard, n: Int
    ): (List[Coffee], Charge) = 
        val purchases: List[(Coffee, Charge)] = 
        List.fill(n)(buyCoffee(cc))
        val (coffees, charges) = purchases.unzip
        val reduced =
            charges.reduce((c1, c2) => c1.combine(c2))
        (coffees, reduced)

class CreditCard

case class Charge(cc: CreditCard, amount: Double):
    def combine(other: Charge): Charge = 
        if cc == other.cc then
            Charge(cc, amount + other.amount)
        else
            throw Exception("Can't combine charges with different cards")

    def coalesce(charges: List[Charge]): List[Charge] =
        charges.groupBy(_.cc).values.map(_.reduce(_.combine(_))).toList

trait Payments:
    def charge(cc: CreditCard, price: Double): Unit

class SimulatedPayments extends Payments:
    def charge(cc: CreditCard, price: Double): Unit = 
        println("charging " + price + " to " + cc)

class Coffee:
    val price: Double = 2.0

object code extends App {
    val cc = CreditCard()
    val p = new SimulatedPayments()
    val cafe = Cafe()
    // val (cup, charge) = cafe.buyCoffee(cc)
    // p.charge(cc, charge.amount)
    val (cup, charge) = cafe.buyCoffees(cc, 4)
    p.charge(cc, charge.amount)

}