# Building Ekho on macOS (Apple Silicon)

These steps build a native arm64 `ekho` with Homebrew libraries. No PulseAudio is needed: Ekho writes a WAV file with `-o`, and macOS's built-in `afplay` plays it.

Tested on 2026-11-17: Ekho master at commit 9dedb67, macOS 26.7, Apple clang 21, Homebrew espeak-ng 1.52.0, libsndfile 1.2.2_1, mpg123 1.33.7, utf8cpp 4.0.8. The same recipe also worked in October 2026 on an older Ekho commit (74fdf64), which did not need the `-std=c++17` flag.

## 1. Homebrew packages

You need [Homebrew](https://brew.sh) and Apple's compiler tools (Xcode, or the Command Line Tools from `xcode-select --install`). Both were already installed on the test Mac.

```sh
brew install espeak-ng libsndfile mpg123 utf8cpp autoconf automake libtool pkgconf
```

Homebrew's `espeak-ng` is enough. Ekho includes only espeak-ng's public header (`espeak-ng/speak_lib.h`), and this build worked against the Homebrew package, so you don't need to build espeak-ng or pcaudiolib yourself.

## 2. sonic (optional, needed for the `-s` speed option)

Use the original sonic repository. The examples install into `$HOME/.local`; any prefix works.

```sh
git clone https://github.com/waywardgeek/sonic.git
cd sonic
make CFLAGS="-O2 -Wall -fPIC -I$(brew --prefix)/include"
mkdir -p $HOME/.local/lib $HOME/.local/include
cp -p libsonic.a $HOME/.local/lib/
cp -p sonic.h $HOME/.local/include/
cd ..
```

Only the static library is copied, so Ekho links sonic statically, with no extra dylib to find at run time.

## 3. Ekho

```sh
git clone https://github.com/hgneng/ekho.git
cd ekho
autoreconf -fi
B=$(brew --prefix)
./configure --without-pulseaudio \
  CPPFLAGS="-I$B/include -I$B/include/utf8cpp -I$HOME/.local/include" \
  LDFLAGS="-L$B/lib -L$HOME/.local/lib" \
  CXXFLAGS="-O2 -std=c++17"
make -j8
```

Notes:

- `-std=c++17` is required: `src/libekho.cpp` uses `std::filesystem`, and Apple clang doesn't enable it by default. Without the flag the build stops with `no member named 'is_directory' in namespace 'std::filesystem'`.
- configure prints `WARNING: unrecognized options: --without-pulseaudio`. You can ignore it: the flag still turns PulseAudio off, and `config.h` ends up with `HAVE_PULSEAUDIO` undefined. If you leave the flag off, configure looks for libpulse and stops if it is missing.
- If sonic was found, `config.h` has `#define HAVE_SONIC 1`.
- `otool -L ekho` should show only Homebrew's mpg123, espeak-ng and libsndfile, plus system libraries (libcurl, libc++, libSystem).

## 4. Voice data

Voice files are not in git; see README.md for the download page. Put them under `ekho-data/` (for example, the Toisanese clips go in `ekho-data/toisanese/`).

## 5. Run

From the build folder, or anywhere with `EKHO_DATA_PATH` pointing at the data folder:

```sh
EKHO_DATA_PATH=ekho-data ./ekho -v toisanese -o test.wav 一二三四五六七八九十零 && afplay test.wav
EKHO_DATA_PATH=ekho-data ./ekho -v toisanese -s -40 -o slow.wav 一二三四五六七八九十零 && afplay slow.wav
EKHO_DATA_PATH=ekho-data ./ekho -v Cantonese -o hello.wav 你好 && afplay hello.wav
```

In this test, the first line made a 3.6-second WAV and the slowed one 6.0 seconds.

Always use `-o file.wav`. The `ekho` command plays sound directly only through PulseAudio (that code in `src/audio.cpp` is inside `#ifdef HAVE_PULSEAUDIO`). This build turns PulseAudio off, so write a file and play it with `afplay`.

One thing to know for Toisanese: `ekho-data/toisanese_list` uses mostly simplified characters, and a character with no entry is skipped silently. For example, 個 is skipped but 个 is spoken.
