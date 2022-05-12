package at.tugraz.ist.qs2022.messageboard.messagestoremessages;

/**
 * Message used to signal that a like should be added to a message.
 */
public class DeleteLikeOrDislike extends MessageStoreMessage {
    public enum Type{
        LIKE,
        DISLIKE
    }

    /**
     * user message id of the user message which should be liked
     */
    public final long messageId;

    /**
     * name of the person which likes the message
     */
    public final String clientName;

    /**
     * name of the person which likes the message
     */
    public final Type typeToDelete;

    public DeleteLikeOrDislike(String clientName, long commId, long messageId, Type typeToDelete) {
        this.clientName = clientName;
        this.messageId = messageId;
        this.communicationId = commId;
        this.typeToDelete = typeToDelete;
    }
}
