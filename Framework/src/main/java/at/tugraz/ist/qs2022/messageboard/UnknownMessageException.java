package at.tugraz.ist.qs2022.messageboard;

/**
 * Exception class used to signal that a worker does
 * not know a client given the communication ID
 */
public class UnknownMessageException extends Exception {
    /**
     * Constructs a new UnknownClientException with the specified detail message.
     *
     * @param message the detail message.
     */
    public UnknownMessageException(String message) {
        super(message);
    }
}
