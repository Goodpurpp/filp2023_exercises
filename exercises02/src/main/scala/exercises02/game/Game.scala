package exercises02.game

import scala.annotation.tailrec

class Game(controller: GameController) {

  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */
  @tailrec final def play(number: Int): Unit = {
    controller.askNumber()
    if (!checks(number, controller.nextLine()))
      play(number)
  }
  private def checks(number: Int, input: String): Boolean = {
    if (input == GameController.IGiveUp) {
      controller.giveUp(number)
      true
    } else {
      input.toIntOption match {
        case Some(x) => checkNumber(number, x)
        case None =>
          controller.wrongInput()
          false
      }
    }
  }

  private def checkNumber(number: Int, numberInput: Int): Boolean = {
    if (numberInput == number) {
      controller.guessed()
      true
    } else {
      if (number > numberInput)
        controller.numberIsBigger()
      else
        controller.numberIsSmaller()
      false
    }
  }
}
