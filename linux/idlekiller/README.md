# Installation

Install `xprintidle`:

    sudo apt-get install xprintidle

Copy the script to `/usr/bin/idle-killer.sh`, and the Upstart file to `/etc/init/idle-killer.conf`.

Start the Upstart service:

	sudo service idle-killer start

# Configuration
It doesn't support many options now. :)
You can set:

- The idle timeout using the variable `IDLE`. This is an integer in seconds.
- The frequency of checking using the sleep timeout variable `TIMEOUT`. 
Since this is passed to `sleep` as is, you can use syntax that your `sleep` will understand,
such as 10m and 2h.
- The grace period after the warning is shown, using the `GRACE` variable. Also passed to `sleep`.
- The warning message using the `IDLE_MESSAGE` variable.

To set them, create `/etc/default/idle-killer` and set the variables in it. The file is sourced,
so `bash` syntax can be used.

I haven't added a way to exclude users from this yet.

# Logging
The script logs to `syslog` using the `IDLEKILLER` tag.

