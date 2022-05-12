package at.tugraz.ist.qs2022.messageboard.messagestoremessages;

import at.tugraz.ist.qs2022.messageboard.clientmessages.Reaction;

/**
 * Message used to signal that a like should be added to a message.
 */
public class AddReaction extends MessageStoreMessage {
    /**
     * User message id of the user message which should be disliked
     */
    public final long messageId;
    /**
     * Name of the person which likes the message
     */
    public final String clientName;

    /**
     * The name of the person who adds reaction
     */
    public final Reaction.Emoji reaction;

    public AddReaction(String clientName, long messageId, long commId, Reaction.Emoji reaction) {
        this.clientName = clientName;
        this.reaction = reaction;
        this.messageId = messageId;
        this.communicationId = commId;
    }
}
