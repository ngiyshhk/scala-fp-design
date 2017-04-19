package fp.design.chapter1

class Cafe {
  def buyCoffee1(cc: CreditCard): Coffee = {
    val cup = new Coffee
    cc.charge(cup.price)
    cup
  }

  def buyCoffee2(cc: CreditCard, p: Payments): Coffee = {
    val cup = new Coffee
    p.charge(cc, cup.price)
    cup
  }

  def buyCoffee3(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee3(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce(_ combine _))
  }
}
