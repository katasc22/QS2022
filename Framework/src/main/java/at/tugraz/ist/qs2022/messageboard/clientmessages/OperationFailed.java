package at.tugraz.ist.qs2022.messageboard.clientmessages;

/**
 * Reply message sent from worker to client if a request failed.
 */
public class OperationFailed extends Reply {
    public OperationFailed(long communicationId) {
        super(communicationId);
    }
}
