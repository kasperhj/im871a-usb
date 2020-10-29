
## Notes for running in Ubuntu
If you get the error 
> Process terminated. Couldn't find a valid ICU package installed on the system.

run `export DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1`

Accessing the serial port requires `sudo usermod -a -G tty [username]`, and then a logout/login.