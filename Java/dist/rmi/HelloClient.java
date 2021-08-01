import java.rmi.Naming;
import java.net.MalformedURLException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public class HelloClient {
	public static void main(String[] args)
		throws NotBoundException, RemoteException, MalformedURLException
	{
		Counter stub = (Counter) Naming.lookup(args[0]);
		for (int repeat = Integer.parseInt(args[1]); repeat > 0; repeat--)
			System.out.println(stub.getCount());
	}
}
