package at.tugraz.ist.qs2022.messageboard.dispatchermessages;

import at.tugraz.ist.qs2022.actorsystem.Message;

/**
 * Message sent from client to dispatcher to stop the system.
 * This message is then forwarded to all workers to stop them.
 */
public class Stop implements Message {
    public Stop() {
    }

    @Override
    public int getDuration() {
        return 2;
    }
}
