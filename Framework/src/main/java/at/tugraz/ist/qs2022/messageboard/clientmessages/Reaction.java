package at.tugraz.ist.qs2022.messageboard.clientmessages;

/**
 * Message sent from client to worker to signal that a like should be added to a given user message.
 */

public class Reaction extends ClientMessage {
    public enum Emoji {
        SMILEY,
        LAUGHING,
        FROWN,
        CRYING,
        HORROR,
        SURPRISE,
        SKEPTICAL,
        COOL
    }

    /**
     * The user message id of the message to be liked
     */
    public final long messageId;

    /**
     * The name of the person who adds reaction
     */
    public final String clientName;

    /**
     * The name of the person who adds reaction
     */
    public final Emoji reaction;

    public Reaction(String clientName, long communicationId, long mId, Emoji reaction) {
        super(communicationId);
        this.clientName = clientName;
        this.messageId = mId;
        this.reaction = reaction;
    }

    @Override
    public int getDuration() {
        return 1;
    }
}
