package at.tugraz.ist.qs2022

import java.security.InvalidParameterException
import java.util

import at.tugraz.ist.qs2022.actorsystem.{Message, SimulatedActor}
import at.tugraz.ist.qs2022.messageboard.MessageStore.USER_BLOCKED_AT_COUNT
import at.tugraz.ist.qs2022.messageboard.UserMessage
import at.tugraz.ist.qs2022.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2022.messageboard.clientmessages._
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import scala.collection.mutable
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
      // init communication
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
      // R1 A message may only be stored if its text contains less than or exactly MAX MESSAGE LENGTH (= 10) characters.
      if (message.length > MAX_MESSAGE_LENGTH) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      // R8 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times, he/she cannot send
      //any further Publish, Like, Dislike or Report messages.
      if (state.reports.count(r => r.reportedClientName == author) >= USER_BLOCKED_AT_COUNT) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      //R2 A message may only be saved if no identical message has been saved yet. Two messages are
      //identical if both author and text of both messages are the same.
      if (state.messages.count(mum => (mum.message == message) && mum.author == author) > 0) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        messages = ModelUserMessage(author = author, message = message, likes = Nil, dislikes = Nil, reactions = new scala.collection.mutable.HashMap[String,  scala.collection.mutable.Set[Reaction.Emoji]](), points = 0) :: state.messages
        //reports = ModelReport(reporter, reported) :: state.reports
      )

    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)

        if ((reply.isInstanceOf[UserBanned] != newState.userBanned)
          || (reply.isInstanceOf[OperationAck] != newState.lastCommandSuccessful)) {
          return false
        }

        val message_added = newState.messages.count(m => m.author == author && m.message == message)
        if (newState.lastCommandSuccessful) {
          if (message_added != 1)
            return false
        }

        true
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
      // init communication
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val messageFromStore = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
        .filter(m => m.getMessage == message)
      if (messageFromStore.length != 1) {
        worker.tell(new Reaction(rName, sut.getCommId, -1, reactionType))
      } else {
        worker.tell(new Reaction(rName, sut.getCommId, messageFromStore.head.getMessageId, reactionType))
      }

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
      val fromStore = state.messages.filter(m => m.message == message && m.author == author)

      // R8 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times, he/she cannot send
      //any further Publish, Like, Dislike or Report messages.
      if (isBanned(state, rName)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R3 A message may only be liked/disliked or get a reaction if it exists
      if (fromStore.length != 1) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      val mesFromStore = fromStore.head
      // R12 A message can get more than one reaction from user, but all reaction to a message from the
      //same user should be different (set semantics).
      if (mesFromStore.reactions.get(rName).contains(reactionType)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      mesFromStore.reactions.get(rName) match {
        case Some(reactions) =>
          mesFromStore.reactions.put(rName, reactions += reactionType)
      }

      val messagesUpdated = state.messages.map(m =>
        if (m == mesFromStore) {
          m.copy(
            reactions = mesFromStore.reactions
          )
        } else m)

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        messages = messagesUpdated
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)

        // R9 Successful requests should be confirmed by sending OperationAck. Requests are considered
        //successful when a message has been saved, a Like or Dislike has been added to a message, or
        //a report for an author has been added.
        if (reply.isInstanceOf[ReactionResponse] != newState.lastCommandSuccessful) {
          return false
        }

        //R10 Requests that are not successful should be confirmed by sending OperationFailed or
        //UserBanned.
        if (!state.lastCommandSuccessful) {
          if ((reply.isInstanceOf[UserBanned] != newState.userBanned)
            || (reply.isInstanceOf[OperationFailed] == newState.lastCommandSuccessful)) {
            return false
          }
        }


        //R12 A message can get more than one reaction from user, but all reaction to a message from the
        //same user should be different (set semantics).
        val messageState = newState.messages.filter(m => m.author == author && m.message == message)
        if (newState.lastCommandSuccessful) {
          val reactionResponse = reply.asInstanceOf[ReactionResponse]
          if (messageState.head.reactions.get(rName).contains(reactionResponse.reaction)) {
            return false
          }
        }

        true
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
      // init communication
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val messageFromStore = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
        .filter(m => m.getMessage == message)
      if (messageFromStore.length != 1) {
        worker.tell(new Like(likeName, sut.getCommId, -1))
        //throw new IllegalArgumentException("Message to like occurs " + messageFromStore.length + " times in store!")
      } else {
        worker.tell(new Like(likeName, sut.getCommId, messageFromStore.head.getMessageId))
      }

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
      val fromStore = state.messages.filter(m => m.message == message && m.author == author)

      // R8 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times, he/she cannot send
      //any further Publish, Like, Dislike or Report messages.
      if (isBanned(state, likeName)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R3 A message may only be liked/disliked or get a reaction if it exists
      if (fromStore.length != 1) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      val mesFromStore = fromStore.head
      // R4 A message may only be liked/disliked by users who have not yet liked/disliked the corresponding message
      if (mesFromStore.likes.contains(likeName)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      val messagesUpdated = state.messages.map(m =>
        if (m == mesFromStore) {
          m.copy(
            likes = m.likes :+ likeName,
            points = m.points + 2
          )} else m)

       state.copy(
        lastCommandSuccessful = true,
         userBanned = false,
        messages = messagesUpdated
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)

        // R9 Successful requests should be confirmed by sending OperationAck. Requests are considered
        //successful when a message has been saved, a Like or Dislike has been added to a message, or
        //a report for an author has been added.
        if (reply.isInstanceOf[ReactionResponse] != newState.lastCommandSuccessful) {
          return false
        }

        //R10 Requests that are not successful should be confirmed by sending OperationFailed or
        //UserBanned.
        if (!state.lastCommandSuccessful) {
          if ((reply.isInstanceOf[UserBanned] != newState.userBanned)
            || (reply.isInstanceOf[OperationFailed] == newState.lastCommandSuccessful && !newState.userBanned)) {
            return false
          }
        }

        // R11 If a message has been liked, two points should be added to the messages points counter. If a
        //message has been disliked, one point should be removed.
        val messageState = newState.messages.filter(m => m.author == author && m.message == message)
        if (newState.lastCommandSuccessful) {
          val reactionResponse = reply.asInstanceOf[ReactionResponse]
          if (messageState.head.points != reactionResponse.points) {
            return false
          }
        }

        true
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
      // init communication
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val messageFromStore = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
        .filter(m => m.getMessage == message)
      if (messageFromStore.length != 1) {
        worker.tell(new Dislike(dislikeName, sut.getCommId, -1))
      } else {
        worker.tell(new Dislike(dislikeName, sut.getCommId, messageFromStore.head.getMessageId))
      }

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
      val fromStore = state.messages.filter(m => m.message == message && m.author == author)

      // R8 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times, he/she cannot send
      //any further Publish, Like, Dislike or Report messages.
      if (isBanned(state, dislikeName)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R3 A message may only be liked/disliked or get a reaction if it exists
      if (fromStore.length != 1) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      val mesFromStore = fromStore.head
      // R4 A message may only be liked/disliked by users who have not yet liked/disliked the corresponding message
      if (mesFromStore.dislikes.contains(dislikeName)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      val messagesUpdated = state.messages.map(m =>
        if (m == mesFromStore) {
          m.copy(
            dislikes = m.dislikes :+ dislikeName,
            points = m.points - 1
          )} else m)

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        messages = messagesUpdated
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)

        // R9 Successful requests should be confirmed by sending OperationAck. Requests are considered
        //successful when a message has been saved, a Like or Dislike has been added to a message, or
        //a report for an author has been added.
        if (reply.isInstanceOf[ReactionResponse] != newState.lastCommandSuccessful) {
          return false
        }

        //R10 Requests that are not successful should be confirmed by sending OperationFailed or
        //UserBanned.
        if (!state.lastCommandSuccessful) {
          if ((reply.isInstanceOf[UserBanned] != newState.userBanned)
            || (reply.isInstanceOf[OperationFailed] == newState.lastCommandSuccessful && !newState.userBanned)) {
            return false
          }
        }

        // R11 If a message has been liked, two points should be added to the messages points counter. If a
        //message has been disliked, one point should be removed.
        val messageState = newState.messages.filter(m => m.author == author && m.message == message)
        if (newState.lastCommandSuccessful) {
          val reactionResponse = reply.asInstanceOf[ReactionResponse]
          if (messageState.head.points != reactionResponse.points) {
            return false
          }
        }

        true
      } else {
        false
      }
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

  // just a suggestion, change it according to your needs.
  case class RetrieveCommandResult(success: Boolean, messages: List[String])

  case class RetrieveCommand(author: String) extends Command {
    type Result = RetrieveCommandResult

    def run(sut: Sut): Result = {
      // init communication
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val foundMessages = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val result = RetrieveCommandResult(success = true, foundMessages.messages.asScala.map(m => m.getMessage).toList)

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get

        if (!reply.success) {
          return false
        }

        if (!checkR5(state.messages, reply.messages, author)) {
          return false
        }


        true
      } else {
        false
      }
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
      // init communication
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new SearchMessages(searchText, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val foundMessages = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val result = SearchCommandResult(success = true, foundMessages.messages.asScala.map(m => m.getMessage).toList)

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get

        if (!reply.success) {
          return false
        }

        val messages = state.messages.filter(m => m.author.contains(searchText) || m.message.contains(searchText)).map(m => m.message)
        if (!reply.messages.equals(messages)) {
          return false
        }

        // R5 It should be possible to retrieve a list of all existing messages of an author.
        if (!checkR5(state.messages, reply.messages, searchText)) {
          return false
        }

        // R6 It should be possible to search for messages containing a given text and get back list of those messages.
        state.messages.filter(m => m.message.contains(searchText))
          .foreach(m =>
            if (!reply.messages.contains(m.message)) {
              return false
            })

        true
      } else {
        false
      }
    }

    override def toString: String = s"Search($searchText)"
  }

  def checkR5(messagesState: List[ModelUserMessage], messagesReply: List[String], searchText: String): Boolean = {
    val mesByAuthor = messagesState.filter(m => m.author == searchText)
    mesByAuthor.foreach(m =>
      if (!messagesReply.contains(m.message)) {
        return false
      })
    true
  }

  // R8 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times, he/she cannot send
  //any further Publish, Like, Dislike or Report messages.
  def isBanned(state: State, author: String): Boolean = {
    if (state.reports.count(r => r.reportedClientName == author) >= USER_BLOCKED_AT_COUNT) {
      return true
    }
    false
  }

}
