package ngiy.fp.design.chapter1

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge) = {
    if (cc == other.cc) Charge(cc, amount + other.amount)
    else throw new Exception("Can't combine charges to different cards")
  }
}
