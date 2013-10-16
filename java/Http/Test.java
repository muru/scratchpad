import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.Authenticator;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;

class ProxyAuthenticator extends Authenticator {
// Taken from http://stackoverflow.com/questions/14113341/http-407-proxy-authentication-required-how-to-handle-in-java-code
    private String user, password;

    public ProxyAuthenticator(String user, String password) {
        this.user = user;
        this.password = password;
    }

    protected PasswordAuthentication getPasswordAuthentication() {
        return new PasswordAuthentication(user, password.toCharArray());
    }
}

public class Test {
	public static void main(String[] args) throws IOException {
		URL url = new URL("http://recordings.kookoo.in/vishwajeet/kookoo_audio1377860094001.wav");
		
		// Create a proxy object and set the authentication details.
		Proxy netmon = new Proxy(Proxy.Type.HTTP, new InetSocketAddress("netmon.iitb.ac.in",80));
		Authenticator.setDefault(new ProxyAuthenticator("ldapuser", "ldappass"));
		
		// Open an URL Connection through the proxy.
		URLConnection urlc = url.openConnection(netmon);
		
		// Create a byte buffer to get the data.
		int numBytes = urlc.getContentLength();
		byte buffer[] = new byte[numBytes];
		
		// Get the input stream and create an output stream.
		InputStream is = urlc.getInputStream();
		PrintStream pout = new PrintStream(new File("test.mp3"));
		
		// If the content MIME-type is audio/x-wav, then we succeeded.
		System.out.println(urlc.getContentType());
		
		is.read(buffer);
		pout.write(buffer);
		
		pout.close();
		is.close();
	}

}
