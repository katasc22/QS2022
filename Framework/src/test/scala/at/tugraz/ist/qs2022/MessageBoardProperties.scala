package at.tugraz.ist.qs2022

import at.tugraz.ist.qs2022.actorsystem.{Message, SimulatedActor}
import at.tugraz.ist.qs2022.messageboard.MessageStore.USER_BLOCKED_AT_COUNT
import at.tugraz.ist.qs2022.messageboard.UserMessage
import at.tugraz.ist.qs2022.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2022.messageboard.clientmessages._
import org.junit.runner.RunWith
import org.scalacheck.Prop.{classify, forAll}
import org.scalacheck.{Gen, Properties}

import scala.jdk.CollectionConverters._

@RunWith(classOf[ScalaCheckJUnitPropertiesRunner])
class MessageBoardProperties extends Properties("MessageBoardProperties") {

  val validMessageGen: Gen[String] = Gen.asciiPrintableStr.map(s =>
    if (s.length <= MAX_MESSAGE_LENGTH) s else s.substring(0, MAX_MESSAGE_LENGTH)
  )
  property("message length: Publish + Ack [R1]") = forAll { (author: String, message: String) =>
    // arrange-  initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the messages
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - define your property and check against it
    // The following classify is optional, it prints stats on the generated values.
    // But the check inside is required.
    classify(message.length <= MAX_MESSAGE_LENGTH, "valid message length", "invalid message length") {
      // if operationAck is received, the message length should be smaller or equal to 10
      reply.isInstanceOf[OperationAck] == message.length <= MAX_MESSAGE_LENGTH
    }
  }
  // TODO: add another properties for requirements R1-R13

 /* property("example property with generators") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      // here would be a worker.tell, e.g. in a loop

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      // here would be a check
      false
    }*/
  property("R2") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply2 = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    reply2.isInstanceOf[OperationFailed]
  }

  property("R3 like") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    //val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()


    worker.tell(new RetrieveMessages(author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val mess = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
      .filter(m => m.getMessage == message)

    if (mess.length != 1) {
      worker.tell(new Like(author, sut.getCommId, -1))
    } else {
      worker.tell(new Like(author, sut.getCommId, mess.head.getMessageId))
    }

    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()



    result.isInstanceOf[ReactionResponse]


  }

  property("R3 dislike") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    //val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()




    worker.tell(new RetrieveMessages(author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val mess = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
      .filter(m => m.getMessage == message)

    if (mess.length != 1) {
      worker.tell(new Dislike(author, sut.getCommId, -1))
    } else {
      worker.tell(new Dislike(author, sut.getCommId, mess.head.getMessageId))
    }

    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()



    result.isInstanceOf[ReactionResponse]


  }

  property("R5") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(mg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()



    worker.tell(new RetrieveMessages(author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    //val mess = sut.getClient.receivedMessages.remove()
    val mess = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
      .filter(m => m.getMessage == message)


    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()



   mess.head == mg


  }

  property("R6") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(mg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()


    worker.tell(new SearchMessages(message, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    //val mess = sut.getClient.receivedMessages.remove()
    val foundMessages = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
    val fmessages = foundMessages.messages.asScala.filter(m => m.getMessage == message)


    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()


    fmessages.head == mg


  }

  property("R4 Like") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(mg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()


    worker.tell(new RetrieveMessages(author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val mess = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
      .filter(m => m.getMessage == message)

    worker.tell(new Like(author, sut.getCommId, mess.head.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Like(author, sut.getCommId, mess.head.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result2 = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()


    result2.isInstanceOf[OperationFailed]


  }

  property("R4 Dislike") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(mg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()


    worker.tell(new RetrieveMessages(author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val mess = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
      .filter(m => m.getMessage == message)

    worker.tell(new Dislike(author, sut.getCommId, mess.head.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Dislike(author, sut.getCommId, mess.head.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result2 = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()


    result2.isInstanceOf[OperationFailed]


  }

  property("R4 Like + Dislike") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(mg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()


    worker.tell(new RetrieveMessages(author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val mess = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages].messages.asScala
      .filter(m => m.getMessage == message)

    worker.tell(new Like(author, sut.getCommId, mess.head.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Dislike(author, sut.getCommId, mess.head.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result2 = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()


    result2.isInstanceOf[ReactionResponse]


  }

  property("R7") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(mg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Report(author, sut.getCommId, "Joe"))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Report(author, sut.getCommId, "Joe"))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result2 = sut.getClient.receivedMessages.remove()


    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()


    result2.isInstanceOf[OperationFailed]


  }

  property("R7") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the message
    val mg : UserMessage = new UserMessage(author, message)
    worker.tell(new Publish(mg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val retval1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Report(author, sut.getCommId, "Joe"))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result1 = sut.getClient.receivedMessages.remove()

    worker.tell(new Report(author, sut.getCommId, "Joe"))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val result2 = sut.getClient.receivedMessages.remove()


    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()


    result2.isInstanceOf[OperationFailed]


  }


}

