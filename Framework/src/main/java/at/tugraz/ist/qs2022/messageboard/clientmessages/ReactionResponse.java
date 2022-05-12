package at.tugraz.ist.qs2022.messageboard.clientmessages;

/**
 * Reply message sent from worker to client if a request succeeded.
 */
public class ReactionResponse extends Reply {

    public int points = 0;
    public String reaction = "";

    public ReactionResponse(long communicationId, int points) {
        super(communicationId);
        this.points = points;
    }

    public ReactionResponse(long communicationId, String reaction) {
        super(communicationId);
        this.reaction = reaction;
    }

    public ReactionResponse(long communicationId) {
        super(communicationId);
    }
}
