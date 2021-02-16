package tennis


class TennisGame1 (val player1Name : String, val player2Name : String) extends TennisGame {
  var m_score1: Int = 0
  var m_score2: Int = 0

  def wonPoint(playerName : String) : Unit = {
    if (playerName == "player1")
      m_score1 += 1
    else
      m_score2 += 1
  }

  def calculateScore() : String = {
    DisplayScore.generate(m_score1, m_score2)
  }
}

object DisplayScore {
  def generate(score1: Int, score2: Int) : String = {

    val difference = score1 - score2
    val tieBreak = score1 >= 4 || score2 >= 4
    
    (score1, score2, difference, tieBreak) match {
      case (_, _, 0, _) =>  drawing(score1)
      case (score1, score2, _, false) => basicScore(score1, score2)
      case (score1, score2, 1, true) => "Advantage player1"
      case (score1, score2, -1, true) => "Advantage player2"
      case (_ , _, difference, true) if difference > 0  => "Win for player1"
      case (_ , _, difference, true) if difference < 0 => "Win for player2"
    }
  }

  private def drawing(score: Int) : String = {
    score match {
      case 0 | 1 | 2 => s"${scoreName(score)}-All"
      case _ => "Deuce"
    }
  }

  private def basicScore(score1: Int, score2: Int) : String = {
    s"${scoreName(score1)}-${scoreName(score2)}"
  }

  private def scoreName(score: Int) = {
    score match {
      case 0 => "Love"
      case 1 => "Fifteen"
      case 2 => "Thirty"
      case 3 => "Forty"
      case unknown => throw new RuntimeException(s"Unknown score: $unknown")
    }
  }
}
