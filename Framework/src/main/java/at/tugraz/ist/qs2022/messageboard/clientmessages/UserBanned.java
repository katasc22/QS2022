package at.tugraz.ist.qs2022.messageboard.clientmessages;

/**
 * Reply message sent from worker to client to signal that he is banned.
 */
public class UserBanned extends Reply {
    public UserBanned(long communicationId) {
        super(communicationId);
    }
}
