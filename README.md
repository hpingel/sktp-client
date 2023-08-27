# sktp-client
SKTP Client for Commodore 64 in 6502 Assembler for WiC64

This assembler program for the C64 depends on a network device available to the Commodore 64. Currently, it depends on a WiC64 connected to the userport of the C64. Without real hardware this setup can be emulated by using a C64 emulator software that supports WiC64 emulation like Kernal64 oder Vice (at this time a current development snapshot is necessary).

This SKTP client allows the C64 to communicate with an "SKTP server" in the same way as the Sidekick64 network kernel does.

I have started my work on this assembler program by extending the "WiC64 Test PHP Directory Example" written by Sven Oliver "KiWi" Arke available from here https://www.wic64.de/downloads/. The routines to communicate with the WiC64 via the userport is probably the only unchanged code that is still the same as in the example. 

# Resources:

WiC64 - The Wireless interface for C64 - SX64 - C128 - VIC20:
https://www.wic64.de/

Kernal64 - Commodore 64 Emulator (supports WiC64 emulation): 
https://github.com/abbruzze/kernal64

Vice Emulator (Development snapshots contain WiC64 emulation):
https://github.com/VICE-Team/svn-mirror/releases

Sidekick64 network kernel Readme: 
https://github.com/hpingel/Sidekick64/blob/net-rebase-on-v0.51c/README_network.md

Sidekick64 network kernel:
https://github.com/hpingel/Sidekick64

SKTP introduction and discussion (German language)
https://www.forum64.de/index.php?thread/120021-demo-textbasiertes-web-browsing-für-sidekick64-sktp-browser

# License
sktp-client is licensed under GNU General Public License v3.0.
