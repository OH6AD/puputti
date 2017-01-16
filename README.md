# puputti
Radio link fleet management tool

Requirements:

	sudo apt install ghc libghc-cassava-dev libghc-parsec3-dev

If you want to perform speed tests over radio link, you need `sshpass`
as well. Running speedtest in MikroTik RouterOS requires password in
plain text anyway so it's rational to use plain text passwords in
automated login, too.

	sudo apt install sshpass

## Speed testing

Test between *laajavet* and *hacklalu*

	SSHPASS=tester_password runghc SpeedTestMain hosts.csv laajavet hacklalu
