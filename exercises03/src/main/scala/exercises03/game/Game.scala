package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = {
    if (input.forall(_.isDigit)) {
      input.toInt match {
        case x if x < number => NumberIsBigger
        case x if x > number => NumberIsSmaller
        case _               => Guessed
      }
    } else {
      input match {
        case "I give up" => GiveUp
        case _           => WrongInput
      }
    }
  }

  def action(state: State, number: Int): GameController => Unit = state match {
    case GiveUp          => _.giveUp(number)
    case Guessed         => _.guessed()
    case NumberIsBigger  => _.numberIsBigger()
    case NumberIsSmaller => _.numberIsSmaller()
    case WrongInput      => _.wrongInput()
  }

  def completed(state: State): Boolean =
    state == Guessed || state == GiveUp
}
