import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Counter extends Remote {
	public int getCount() throws RemoteException;
}
