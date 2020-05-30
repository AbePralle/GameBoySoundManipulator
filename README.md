# Game Boy Sound Manipulator
A GameBoy Color ROM that provides on-device/on-emulator sound parameter editing.

## Version
- v1.0.1 - May 30, 2020
- [MIT License](LICENSE)
- By Abe Pralle

![GBSound](Media/GBSound.gif)

## ROM
- A compiled ROM is included: [ROM/GBSound.gb](ROM/GBSound.gb).
- Run it on a Game Boy Color emulator such as [SameBoy](https://sameboy.github.io).

## Compiling
1. Install the [Rogue](https://github.com/AbePralle/Rogue) language to utilize its Rogo build system.
2. If on Windows or Linux, install [RGBDS](https://github.com/rednex/rgbds). On macOS Rogo will install it for you.
3. Run `rogo` to build the ROM from source using RGBDS.

## Usage
If you are programming Game Boy or Game Boy Color, Game Boy Sound Manipulator (AKA GBSound) lets you adjust sound parameters with immediate feedback. Once you have dialed in a sound you like, copying the sound's bytes to the appropriate Game Boy memory location will play that sound. For example, in the screen below you would copy bytes `02 00 F2 00 C1` to locations `FF10-FF14`.

