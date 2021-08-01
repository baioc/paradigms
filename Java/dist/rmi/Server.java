import java.rmi.Naming;
import java.rmi.RemoteException;
import java.net.MalformedURLException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.atomic.AtomicInteger;

public class Server extends UnicastRemoteObject implements Counter {
	private AtomicInteger count = new AtomicInteger(0);

	protected Server() throws RemoteException {
		super();
	}

	@Override
	public int getCount() {
		return count.addAndGet(1);
	}

	public static void main(String[] args) throws RemoteException, MalformedURLException {
		Naming.rebind(args[0], new Server());
	}
}
