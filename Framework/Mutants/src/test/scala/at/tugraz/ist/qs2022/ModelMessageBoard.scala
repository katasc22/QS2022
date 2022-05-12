package at.tugraz.ist.qs2022

import at.tugraz.ist.qs2022.messageboard.clientmessages.Reaction

case class ModelMessageBoard(
                              messages: List[ModelUserMessage],
                              reports: List[ModelReport],
                              lastCommandSuccessful: Boolean,
                              userBanned: Boolean
                            ) {
  private def successStr: String = {
    if (lastCommandSuccessful) "success" else "fail"
  }

  private def bannedStr: String = {
    if (userBanned) "banned" else "not banned"
  }

  override def toString: String = s"ModelMessageBoard([${messages.mkString(",")}], [${reports.mkString(",")}], $successStr, $bannedStr)"
}

case class ModelReport(clientName: String, reportedClientName: String)

case class ModelUserMessage(
                             author: String,
                             message: String,
                             likes: List[String],
                             dislikes: List[String],
                             reactions: collection.mutable.Map[String, collection.mutable.Set[Reaction.Emoji]],
                             points: Int
                           ) {
  override def toString = s"$author: $message, liked by : ${likes.sorted.mkString(",")}, disliked by : ${dislikes.sorted.mkString(",")}, points: ${points}"
//    override def toString = s"$author: $message; Likes: ${likes.sorted.mkString(",")}; Dislikes: ${dislikes.sorted.mkString(",")}"
}
