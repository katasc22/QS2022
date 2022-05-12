package at.tugraz.ist.qs2022

import at.tugraz.ist.qs2022.actorsystem.{Message, SimulatedActor}
import at.tugraz.ist.qs2022.messageboard.MessageStore.USER_BLOCKED_AT_COUNT
import at.tugraz.ist.qs2022.messageboard.UserMessage
import at.tugraz.ist.qs2022.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2022.messageboard.clientmessages._
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import scala.jdk.CollectionConverters._
import scala.util.Try

// Documentation: https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#stateful-testing

object MessageBoardSpecification extends Commands {
  override type State = ModelMessageBoard
  override type Sut = SUTMessageBoard

  override def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  override def newSut(state: State): Sut = new SUTMessageBoard

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = state.messages.isEmpty && state.reports.isEmpty

  override def genInitialState: Gen[State] = ModelMessageBoard(Nil, Nil, lastCommandSuccessful = false, userBanned = false)

  override def genCommand(state: State): Gen[Command] = Gen.oneOf(genPublish, genLike, genDislike, genReport, genRetrieve, genSearch)

  val genAuthor: Gen[String] = Gen.oneOf("Alice", "Bob")
  val genReporter: Gen[String] = Gen.oneOf("Alice", "Bob", "Lena", "Lukas", "Simone", "Charles", "Gracie", "Patrick", "Laura", "Leon")
  val genMessage: Gen[String] = Gen.oneOf("msg_w_9ch", "msg_w_10ch", "msg_w_11ch_")
  val genEmoji: Gen[Reaction.Emoji] = Gen.oneOf(Reaction.Emoji.COOL, Reaction.Emoji.CRYING, Reaction.Emoji.LAUGHING, Reaction.Emoji.SMILEY, Reaction.Emoji.SURPRISE, Reaction.Emoji.SKEPTICAL)

  def genPublish: Gen[PublishCommand] = for {
    author <- genAuthor
    message <- genMessage
  } yield PublishCommand(author, message)

  case class PublishCommand(author: String, message: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      // order of checks is important!

      // R1 A message may only be stored if its text contains less than or exactly MAX MESSAGE LENGTH
      // (= 10) characters. This check is performed in the Worker class.

      if (message.length > MAX_MESSAGE_LENGTH) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      // R7 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times,
      // he/she cannot send any further Publish, Like, Dislike or Report messages.

      if (state.reports.count(r => r.reportedClientName == author) >= USER_BLOCKED_AT_COUNT) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R2 A message may only be saved if no identical message has been saved yet. Two messages are
      // identical if both author and text of both messages are the same.

      if (state.messages.exists(um => um.author == author && um.message == message)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        messages = ModelUserMessage(author, message, Nil, Nil, collection.mutable.Map(), 0) :: state.messages
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        (reply.isInstanceOf[UserBanned] == newState.userBanned) && (reply.isInstanceOf[OperationAck] == newState.lastCommandSuccessful)
      } else {
        false
      }
    }

    override def toString: String = s"Publish($author, $message)"
  }

  def genReaction: Gen[ReactionCommand] = for {
    author <- genAuthor
    message <- genMessage
    rName <- genAuthor
    reactionType <- genEmoji
  } yield ReactionCommand(author, message, rName, reactionType)

  case class ReactionCommand(author: String, message: String, rName: String, reactionType: Reaction.Emoji) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)

      // message with selected author exists -> like that message, otherwise like an invalid message
      val foundMessages: FoundMessages = sut.getClient.receivedMessages.remove.asInstanceOf[FoundMessages]
      val messageToLike: UserMessage = foundMessages.messages.asScala.find(
        um => um.getMessage == message && um.getAuthor == author
      ).orNull
      val messageId: Long = if (messageToLike != null) messageToLike.getMessageId else -100

      worker.tell(new Reaction(rName, sut.getCommId, messageId, reactionType))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      // R7 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times,
      // he/she cannot send any further Publish, Like, Dislike, Reaction or Report messages.

      if (state.reports.count(r => r.reportedClientName == rName) >= USER_BLOCKED_AT_COUNT) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R3 A message may only be liked/disliked/reacted if it exists.

      // R11 A reactions of a user to a message are a set, not a bag.

      if (
        !state.messages.exists(um => um.author == author && um.message == message) ||
          state.messages.exists(um => um.author == author && um.message == message && um.reactions.keySet.contains(rName))
      ) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      if (
        state.messages.exists(um => um.author == author && um.message == message) &&
          state.messages.exists(um => um.author == author && um.message == message && um.reactions.keySet.contains(rName) && um.reactions(rName).contains(reactionType))
      ) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }



      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        messages = state.messages.map(um => {
          if (um.author == author && um.message == message) {
            val reactionsCopy = um.reactions
            if (um.reactions.keySet.contains(rName)) {
              val s = reactionsCopy(rName)
              s.add(reactionType)
              reactionsCopy.put(rName, s)
            } else reactionsCopy.put(rName, scala.collection.mutable.Set(reactionType))
            um.copy(reactions = reactionsCopy)
            um.copy(points = um.points + 2)
          }
          else um
        })
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        (reply.isInstanceOf[UserBanned] == newState.userBanned) && (reply.isInstanceOf[OperationAck] == newState.lastCommandSuccessful)
      } else {
        false
      }
    }

    override def toString: String = s"Reaction($author, $message, $rName, $reactionType)"
  }

  def genLike: Gen[LikeCommand] = for {
    author <- genAuthor
    message <- genMessage
    likeName <- genAuthor
  } yield LikeCommand(author, message, likeName)

  case class LikeCommand(author: String, message: String, likeName: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)

      // message with selected author exists -> like that message, otherwise like an invalid message
      val foundMessages: FoundMessages = sut.getClient.receivedMessages.remove.asInstanceOf[FoundMessages]
      val messageToLike: UserMessage = foundMessages.messages.asScala.find(
        um => um.getMessage == message && um.getAuthor == author
      ).orNull
      val messageId: Long = if (messageToLike != null) messageToLike.getMessageId else -100

      worker.tell(new Like(likeName, sut.getCommId, messageId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      // R7 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times,
      // he/she cannot send any further Publish, Like, Dislike or Report messages.

      if (state.reports.count(r => r.reportedClientName == likeName) >= USER_BLOCKED_AT_COUNT) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R3 A message may only be liked/disliked if it exists.

      // R4 A message may only be liked/disliked by users who have not yet liked/disliked the corresponding message.

      if (
        !state.messages.exists(um => um.author == author && um.message == message) ||
          state.messages.exists(um => um.author == author && um.message == message && um.likes.contains(likeName))
      ) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }


      var nextStateMessages = state.messages.map(um => {
        if (um.author == author && um.message == message) {
          um.copy(likes = likeName :: um.likes)
        }
        else um
      })

      nextStateMessages = nextStateMessages.map(um => {
        if (um.author == author && um.message == message) {
          um.copy(points = um.points + 2)
        }
        else um
      })

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        messages = nextStateMessages
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        (reply.isInstanceOf[UserBanned] == newState.userBanned) && (reply.isInstanceOf[OperationAck] == newState.lastCommandSuccessful)
      } else {
        false
      }
    }

    override def toString: String = s"Like($author, $message, $likeName)"
  }

  def genDislike: Gen[DislikeCommand] = for {
    author <- genAuthor
    message <- genMessage
    dislikeName <- genAuthor
  } yield DislikeCommand(author, message, dislikeName)

  case class DislikeCommand(author: String, message: String, dislikeName: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)

      // message with selected author exists -> like that message, otherwise like an invalid message
      val foundMessages: FoundMessages = sut.getClient.receivedMessages.remove.asInstanceOf[FoundMessages]
      val messageToDislike: UserMessage = foundMessages.messages.asScala.find(
        um => um.getMessage == message && um.getAuthor == author
      ).orNull
      val messageId: Long = if (messageToDislike != null) messageToDislike.getMessageId else -100

      worker.tell(new Dislike(dislikeName, sut.getCommId, messageId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {

      // R7 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times,
      // he/she cannot send any further Publish, Like, Dislike or Report messages.

      if (state.reports.count(r => r.reportedClientName == dislikeName) >= USER_BLOCKED_AT_COUNT) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R3 A message may only be liked/disliked if it exists.

      // R4 A message may only be liked/disliked by users who have not yet liked/disliked the corresponding message.

      if (
        !state.messages.exists(um => um.author == author && um.message == message) ||
          state.messages.exists(um => um.author == author && um.message == message && um.dislikes.contains(dislikeName))
      ) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      var nextStateMessages = state.messages.map(um => {
        if (um.author == author && um.message == message) {
          um.copy(dislikes = dislikeName :: um.dislikes)
        }
        else um
      })

      nextStateMessages = nextStateMessages.map(um => {
        if (um.author == author && um.message == message) {
          um.copy(points = um.points - 1)
        }
        else um
      })

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        messages = nextStateMessages
      )

    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        (reply.isInstanceOf[UserBanned] == newState.userBanned) && (reply.isInstanceOf[OperationAck] == newState.lastCommandSuccessful)
      } else false
    }

    override def toString: String = s"Dislike($author, $message, $dislikeName)"
  }

  def genReport: Gen[ReportCommand] = for {
    reporter <- genReporter
    reported <- genAuthor
  } yield ReportCommand(reporter, reported)

  case class ReportCommand(reporter: String, reported: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new Report(reporter, sut.getCommId, reported))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      // R7 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times,
      // he/she cannot send any further Publish, Like, Dislike or Report messages.

      if (state.reports.count(r => r.reportedClientName == reporter) >= USER_BLOCKED_AT_COUNT) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R6 A user may report another user only if he has not previously reported the user in question.

      if (state.reports.exists(report => report.clientName == reporter && report.reportedClientName == reported)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        reports = ModelReport(reporter, reported) :: state.reports
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        (reply.isInstanceOf[UserBanned] == newState.userBanned) && (reply.isInstanceOf[OperationAck] == newState.lastCommandSuccessful)
      } else {
        false
      }
    }

    override def toString: String = s"Report($reporter, $reported)"
  }

  def genRetrieve: Gen[RetrieveCommand] = for {
    author <- genAuthor
  } yield RetrieveCommand(author)

  case class RetrieveCommandResult(success: Boolean, messages: List[String])

  case class RetrieveCommand(author: String) extends Command {
    type Result = RetrieveCommandResult

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply match {
        case messages: FoundMessages =>
          RetrieveCommandResult(success = true, messages.messages.asScala.map(um => um.toString).sorted.toList)
        case _ =>
          RetrieveCommandResult(success = false, Nil)
      }
    }

    def nextState(state: State): State = state.copy(
      lastCommandSuccessful = true,
      userBanned = false
    )

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get

        // R5 It should be possible to retrieve a list of all existing messages of an author.

        if (reply.success) {
          val modelMessages = state.messages.filter(um => um.author == author).map(um => um.toString).sorted
          reply.messages.sorted == modelMessages
        } else false
      } else false
    }

    override def toString: String = s"Retrieve($author)"
  }

  def genSearch: Gen[SearchCommand] = for {
    searchText <- genMessage
  } yield SearchCommand(searchText)

  case class SearchCommandResult(success: Boolean, messages: List[String])

  case class SearchCommand(searchText: String) extends Command {
    type Result = SearchCommandResult

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new SearchMessages(searchText, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply match {
        case messages: FoundMessages =>
          SearchCommandResult(success = true, messages.messages.asScala.map(um => um.toString).sorted.toList)
        case _ =>
          SearchCommandResult(success = false, Nil)
      }
    }

    def nextState(state: State): State = state.copy(
      lastCommandSuccessful = true,
      userBanned = false
    )

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get

        if (reply.success) {
          val modelMessages = state.messages.filter(um => um.message == searchText).map(um => um.toString).sorted
          reply.messages.sorted == modelMessages
        } else false
      } else false
    }

    override def toString: String = s"Search($searchText)"
  }

}